use std::{convert::TryFrom, ops::Deref, path::Path, process::Command};

use crate::{CompileOpts, OutFormat, Type, ast::{Ast, FunProto, Attributes}, lex::Op, types::Container};
use hashbrown::HashMap;
use inkwell::{IntPredicate, OptimizationLevel, builder::Builder, context::Context, module::Module, passes::PassManager, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, types::{AnyTypeEnum, BasicType, BasicTypeEnum, StructType}, values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue}};

/// The `namespace` struct holds an amount of items and serves to namespace them
#[derive(Clone, Default)]
struct Namespace<'c> {
    /// A hash map of identifiers to defined struct types
    pub struct_types: HashMap<String, (StructType<'c>, Container)>,

    /// A hash map of identifiers to defined union types
    pub union_types: HashMap<String, (StructType<'c>, Container)>,

    /// A map of function names to function prototypes
    pub funs: HashMap<String, (FunctionValue<'c>, FunProto)>,

    /// The name of this namespace
    pub name: String,

    /// Further nested namespaces
    nested: HashMap<String, Namespace<'c>>,

    /// A list of currently used namespaces
    pub using: Vec<String>,

    /// A map of identifiers imported to the fully qualified names of the identifiers
    pub used_items: HashMap<String, String>,

    /// A map of user - defined type definitions to real types
    pub typedefs: HashMap<String, Type>,
}


/// The `Compiler` struct is used to generate an executable with LLVM from the parsed AST.
pub struct Compiler<'c> {
    /// The LLVM context
    ctx: &'c Context,

    /// A hash map of identifiers to defined struct types
    struct_types: HashMap<String, (StructType<'c>, Container)>,

    /// A hash map of identifiers to defined union types
    union_types: HashMap<String, (StructType<'c>, Container)>,

    /// A map of function names to function prototypes
    funs: HashMap<String, (FunctionValue<'c>, FunProto)>,

    /// The LLVM module that we will be writing code to
    module: Module<'c>,

    /// The IR builder that we use to build LLVM IR
    build: Builder<'c>,

    /// The function that we are currently generating code in
    current_fn: Option<FunctionValue<'c>>,

    /// The signature of the current function
    current_proto: Option<FunProto>,

    /// A list of currently used namespaces
    using: Vec<String>,

    /// A map of identifiers imported to the fully qualified names of the identifiers
    used_items: HashMap<String, String>,

    /// A map of user - defined type definitions to real types
    typedefs: HashMap<String, Type>,

    /// The stack of namespaces that we are currently in
    ns: Vec<String>,

    /// A map of variable / argument names to LLVM values
    vars: HashMap<String, (PointerValue<'c>, Type)>,
}

impl<'c> Compiler<'c> {
    /// Create a new `Compiler` from an LLVM context struct
    pub fn new(ctx: &'c Context) -> Self {
        Self {
            ctx,
            struct_types: HashMap::new(),
            union_types: HashMap::new(),
            build: ctx.create_builder(),
            module: ctx.create_module("spark_llvm_module"),
            current_fn: None,
            funs: HashMap::new(),
            vars: HashMap::new(),
            current_proto: None,
            ns: Vec::new(),
            using: Vec::new(),
            typedefs: HashMap::new(),
            used_items: HashMap::new(),
        }
    }

    /// Convert the AST types to LLVM types
    pub fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'c> {
        match ty {
            Type::Integer{
                width, 
                signed: _
            } => self.ctx.custom_width_int_type(*width as u32).as_basic_type_enum(),
            Type::Ptr(internal) => self.llvm_type(internal).ptr_type(inkwell::AddressSpace::Generic).as_basic_type_enum(),
            Type::Struct(con) => {
                self.ctx.struct_type(con.fields.iter().map(|(_, f)| self.llvm_type(f)).collect::<Vec<_>>().as_slice(), true).as_basic_type_enum()
            },
            Type::Union(con) => {
                let largest = con.fields.iter().max_by(|(_, prev), (_, this)| prev.size().cmp(&this.size())).expect("Union type with no fields!");
                self.ctx.struct_type(&[self.llvm_type(&largest.1)], false).as_basic_type_enum()
            },
            Type::UnknownStruct(name) => match self.get_struct(name) {
                Some(s_ty) => s_ty.0.as_basic_type_enum(),
                None => panic!("Unknown struct type {}", name),
            },
            Type::UnknownUnion(name) => match self.get_union(name) {
                Some(u_ty) => u_ty.0.as_basic_type_enum(),
                None => panic!("Unknown union type {}", name),
            },
            Type::Unknown(name) => match (self.get_union(name), self.get_struct(name), self.get_typedef(name)) {
                (Some(_), Some(_), _) => panic!("Type {} can be both a union and a struct, prefix with struct or union keywords to remove abiguity", name),
                (Some(u), _, _) => u.0.as_basic_type_enum(),
                (_, Some(s), _) => s.0.as_basic_type_enum(),
                (_, _, Some(ty)) => self.llvm_type(&ty),
                (None, None, None) => panic!("Unknown union or struct type {}", name),
            },
            Type::Void => panic!("Cannot create void type in LLVM!"),
        }
    }

    /// Generate code for a function prototype
    pub fn gen_fun_proto(&mut self, mut proto: FunProto) -> Result<FunctionValue<'c>, String> {
        if !proto.attrs.contains(Attributes::EXT) {
            proto.name = self.apply_ns(proto.name); //Apply namespacing if the function is not defined as external
        }
        if self.module.get_function(proto.name.as_str()).is_some() {
            return Err(format!("Function {} defined twice", proto.name));
        }

        if proto.ret == Type::Void {
            let proto_clone = proto.clone();
            let fun = self.module.add_function(
                proto.name.as_str(),
                self.ctx.void_type().fn_type(
                    proto
                        .args
                        .iter()
                        .map(|(ty, _)| self.llvm_type(ty))
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                ),
                None,
            );
            self.funs.insert(proto.name, (fun, proto_clone));
            Ok(fun)
        }
        else {
            let proto_clone = proto.clone();
            let fun = self.module.add_function(
                proto.name.as_str(),
                self.llvm_type(&proto.ret).fn_type(
                    proto
                        .args
                        .iter()
                        .map(|(ty, _)| self.llvm_type(ty))
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                ),
                None,
            );
            self.funs.insert(proto.name, (fun, proto_clone));
            Ok(fun)
        }
    }

    /// Apply all namespaces to an identifier
    #[inline]
    pub fn apply_ns(&self, name: impl AsRef<str>) -> String {
        let base = self.ns.clone().join("::"); //Get the base namespace
        base + "::" + name.as_ref() //Append the identifier to the namespace base
    }

    /// Attempt to search used namespaces for a value in a hashmap
    fn resolve<T: Clone>(&self, name: impl AsRef<str>, map: &HashMap<String, T>) -> Option<T> {
        let name = name.as_ref();
        let self_ns = self.ns.join("::") + "::" + name; //See if we are using a function from the current namespace
        match map.get(&self_ns) {
            //We have this function in our current namespace, return it
            Some(val) => Some(val.clone()),
            //Try all used namespaces to see if we have this function in another namesapce
            None => {
                for namespace in self.using.iter() {
                    let using_ns = namespace.clone() + "::" + name;
                    if let Some(val) = map.get(&using_ns) {
                        return Some(val.clone())
                    }
                }
                None
            }
        }
    }

    /// Enter a namespace and generate identifiers for the namespace
    #[inline(always)]
    pub fn enter_ns(&mut self, name: impl ToString) {
        self.ns.push(name.to_string());
    }

    /// Build an alloca for a variable in the current function
    fn entry_alloca(&self, name: &str, ty: BasicTypeEnum<'c>) -> PointerValue<'c> {
        let entry_builder = self.ctx.create_builder();
        let f = self
            .current_fn
            .expect("Not in a function, can't allocate on stack");
        let bb = f
            .get_first_basic_block()
            .expect("Function has no entry block to allocate in");
        if let Some(ref ins) = bb.get_first_instruction() {
            entry_builder.position_at(bb, ins);
        } else {
            entry_builder.position_at_end(bb);
        }

        entry_builder.build_alloca(ty, name)
    }

    /// Generate code for a binary expression
    fn gen_bin(&mut self, lhs: &Ast, rhs: &Ast, op: &Op) -> AnyValueEnum<'c> {
        match op {
            //Handle assignment separately
            Op::Assign => {
                let lhs = self.gen(lhs, true).into_pointer_value();
                let rhs = BasicValueEnum::try_from(self.gen(rhs, false))
                    .expect("Right hand side of assignment expression is not a basic type!");

                self.build.build_store(lhs, rhs).as_any_value_enum()
            }
            op => {
                use std::mem::discriminant;
                let lhs = self.gen(lhs, false);
                let rhs = self.gen(rhs, false);
                if discriminant(&lhs.get_type()) != discriminant(&rhs.get_type()) {
                    panic!("Left hand side of '{}' expression does not match types with right hand side! LHS: {:?}, RHS: {:?}", op, lhs.get_type(), rhs.get_type());
                }
                let ty = lhs.get_type();
                match (ty, op) {
                    (AnyTypeEnum::IntType(_), Op::Plus) => {
                        let lhs = lhs.into_int_value();
                        let rhs = rhs.into_int_value();
                        self.build
                            .build_int_add(lhs, rhs, "tmp_iadd")
                            .as_any_value_enum()
                    }
                    (AnyTypeEnum::IntType(_), Op::Greater) => self
                        .build
                        .build_int_compare(
                            IntPredicate::SGT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_greater_than_cmp",
                        )
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Less) => self
                        .build
                        .build_int_compare(
                            IntPredicate::SLT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_less_than_cmp",
                        )
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Equal) => self
                        .build
                        .build_int_compare(
                            IntPredicate::EQ,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_eq_cmp",
                        )
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::GreaterEq) => self
                        .build
                        .build_int_compare(
                            IntPredicate::SGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_greater_than_eq_cmp",
                        )
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::NEqual) => self
                        .build
                        .build_int_compare(
                            IntPredicate::NE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_not_eq_cmp",
                        )
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::LessEq) => self
                        .build
                        .build_int_compare(
                            IntPredicate::SLE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_less_than_eq_cmp",
                        )
                        .as_any_value_enum(),

                    (AnyTypeEnum::IntType(_), Op::And) => self
                        .build
                        .build_and(lhs.into_int_value(), rhs.into_int_value(), "bit_and")
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Or) => self
                        .build
                        .build_or(lhs.into_int_value(), rhs.into_int_value(), "bit_or")
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Xor) => self
                        .build
                        .build_xor(lhs.into_int_value(), rhs.into_int_value(), "bit_xor")
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Star) => self
                        .build
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "int_mul")
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Divide) => self
                        .build
                        .build_int_signed_div(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_div",
                        )
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Modulo) => self
                        .build
                        .build_int_signed_rem(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "int_modulo",
                        )
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), Op::Minus) => self
                        .build
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "int_sub")
                        .as_any_value_enum(),

                    (AnyTypeEnum::IntType(_), Op::AndAnd) => {
                        let lhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            lhs.into_int_value(),
                            self.ctx.bool_type().const_zero(),
                            "and_and_cond_check_lhs",
                        );
                        let rhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            rhs.into_int_value(),
                            self.ctx.bool_type().const_zero(),
                            "and_and_cond_check_rhs",
                        );
                        self.build
                            .build_and(lhs, rhs, "cond_and_and_cmp")
                            .as_any_value_enum()
                    }
                    (AnyTypeEnum::IntType(_), Op::OrOr) => {
                        let lhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            lhs.into_int_value(),
                            self.ctx.bool_type().const_zero(),
                            "or_or_cond_check_lhs",
                        );
                        let rhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            rhs.into_int_value(),
                            self.ctx.bool_type().const_zero(),
                            "or_or_cond_check_rhs",
                        );
                        self.build
                            .build_or(lhs, rhs, "cond_or_or_cmp")
                            .as_any_value_enum()
                    },

                    //---------- Pointer Operations
                    (AnyTypeEnum::PointerType(ptr), op) => {
                        let lhs = self.build.build_ptr_to_int(lhs.into_pointer_value(), self.ctx.i64_type(), "ptr_cmp_cast_lhs");
                        let rhs = self.build.build_ptr_to_int(rhs.into_pointer_value(), self.ctx.i64_type(), "ptr_cmp_cast_rhs");

                        match op {
                            Op::NEqual => self.build.build_int_compare(IntPredicate::NE, lhs, rhs, "ptr_nequal_cmp").as_any_value_enum(),
                            Op::Equal => self.build.build_int_compare(IntPredicate::NE, lhs, rhs, "ptr_equal_cmp").as_any_value_enum(),

                            Op::Plus => self.build.build_int_to_ptr(self.build.build_int_add(lhs, rhs, "ptr_add"), ptr, "ptr_add_cast_back_to_ptr").as_any_value_enum(),
                            Op::Minus => self.build.build_int_to_ptr(self.build.build_int_sub(lhs, rhs, "ptr_sub"), ptr, "ptr_sub_cast_back_to_ptr").as_any_value_enum(),
                            Op::Star => self.build.build_int_to_ptr(self.build.build_int_mul(lhs, rhs, "ptr_mul"), ptr, "ptr_mul_cast_back_to_ptr").as_any_value_enum(),
                            Op::Divide => self.build.build_int_to_ptr(self.build.build_int_unsigned_div(lhs, rhs, "ptr_div"), ptr, "ptr_div_cast_back_to_ptr").as_any_value_enum(),
                            other => panic!("Cannot use operator {} on pointers", other),
                        }
                    }
                    other => panic!("Unable to use operator '{}' on type {:?}", op, other),
                }
            }
        }
    }

    /// Get a function by name from our hashmap, searching used namespaces if the function is not found
    fn get_fun(&self, name: impl AsRef<str>) -> Option<(FunctionValue<'c>, FunProto)> {
        let mut name = name.as_ref().to_owned();
        //See if we have a fully qualified imported name
        if let Some(item) = self.used_items.get(&name) {
            name = item.clone();
        }
        match self.funs.get(&name) {
            Some(val) => Some(val.clone()),
            None => self.resolve(name, &self.funs),
        }
    }

    /// Search for a typedef by name 
    fn get_typedef(&self, name: impl AsRef<str>) -> Option<Type> {
        let mut name = name.as_ref().to_owned();
        //See if we have a fully qualified imported name
        if let Some(item) = self.used_items.get(&name) {
            name = item.clone();
        }
        match self.typedefs.get(&name) {
            Some(val) => Some(val.clone()),
            None => self.resolve(name, &self.typedefs),
        }
    }

    /// Get a struct type, resolving not found errors with namespace lookup
    fn get_struct(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        let mut name = name.as_ref().to_owned();
        //See if we have a fully qualified imported name
        if let Some(item) = self.used_items.get(&name) {
            name = item.clone();
        }
        match self.struct_types.get(&name) {
            Some(val) => Some(val.clone()),
            None => self.resolve(name, &self.struct_types),
        }
    }

    /// Get a union type, resolving not found errors with namespace lookup
    fn get_union(&self, name: impl AsRef<str>) -> Option<(StructType<'c>, Container)> {
        let mut name = name.as_ref().to_owned();
        //See if we have a fully qualified imported name
        if let Some(item) = self.used_items.get(&name) {
            name = item.clone();
        }
        match self.union_types.get(&name) {
            Some(val) => Some(val.clone()),
            None => self.resolve(name, &self.union_types),
        }
    }

    fn gen_fundef(&mut self, proto: &FunProto, body: &Vec<Ast>) {
        if self.current_fn.is_some() {
            panic!("Nested functions are not currently supported, function {} must be moved to the top level", proto.name);
        }

        let old_vars = self.vars.clone();

        //Apply namespacing if the function isn't extern
        let name = if !proto.attrs.contains(Attributes::EXT) {
            self.apply_ns(proto.name.clone())
        } else {
            proto.name.clone()
        };
        
        let f = match self.module.get_function(name.as_str()) {
            Some(f) => f,
            None => self.gen_fun_proto(proto.clone()).unwrap(),
        };
        self.current_fn = Some(f);
        self.current_proto = Some(proto.clone());

        let bb = self.ctx.append_basic_block(f, "fn_entry"); //Add the first basic block
        self.build.position_at_end(bb); //Start inserting into the function

        //Add argument names to the list of variables we can use
        for (arg, (ty, proto_arg)) in f.get_param_iter().zip(proto.args.iter()) {
            let alloca = self.entry_alloca(
                proto_arg.clone().unwrap_or("".to_owned()).as_str(),
                self.llvm_type(ty),
            );
            self.build.build_store(alloca, arg); //Store the initial value in the function parameters

            if let Some(name) = proto_arg {
                self.vars.insert(name.clone(), (alloca, ty.clone()));
            }
        }

        //Generate code for the function body
        for ast in body {
            self.gen(ast, false);
        }

        self.vars = old_vars; //Reset the variables
        self.current_fn = None;
        self.current_proto = None;
    }

    /// Generate code for one expression, only used for generating function bodies, no delcarations
    pub fn gen(&mut self, node: &Ast, lval: bool) -> AnyValueEnum<'c> {
        match node {
            Ast::NumLiteral(ty, num) => self
                .llvm_type(ty)
                .into_int_type()
                .const_int_from_string(num.as_str(), inkwell::types::StringRadix::Decimal)
                .unwrap()
                .as_any_value_enum(),
            Ast::Ret(node) => {
                match self.current_proto.as_ref().expect("Must be in a function to return from one!").ret {
                    Type::Void => self.build.build_return(None).as_any_value_enum(),
                    _ => {
                        let ret = self.gen(node.deref().as_ref().unwrap(), false);
                        self.build
                            .build_return(Some(&BasicValueEnum::try_from(ret).unwrap()))
                            .as_any_value_enum()
                    }
                }
            }
            Ast::FunCall(name, args) => match self.get_fun(&name) {
                Some((f, _)) => {
                    let args = args.iter().map(|n| BasicValueEnum::try_from(self.gen(n, false)).expect("Failed to convert any value enum to basic value enum when calling function")).collect::<Vec<_>>();
                    self.build
                        .build_call(f.clone(), args.as_ref(), "tmp_fncall")
                        .as_any_value_enum()
                }
                None => panic!("Calling unknown function {}", name),
            },
            Ast::AssocFunAccess(item, name, args) => match self.get_fun(name.as_str()) {
                Some((f, _)) => {
                    let item = BasicValueEnum::try_from(self.gen(item.deref(), false)).unwrap(); //Generate code for the first expression 
                    let mut real_args = vec![item];
                    real_args.extend(args.iter().map(|n| BasicValueEnum::try_from(self.gen(n, false)).expect("Failed to convert any value enum to basic value enum when calling function")) );
                    self.build
                        .build_call(f.clone(), real_args.as_ref(), "tmp_assoc_fncall")
                        .as_any_value_enum()
                }
                None => panic!("Calling unknown associated function {}", name),
            },
            Ast::If {
                cond,
                true_block,
                else_block,
            } => {
                let cond = self.gen(cond, false).into_int_value();
                let fun = self.current_fn.expect("Conditional outside of function");
                
                let true_bb = self.ctx.append_basic_block(fun, "if_true_bb");
                let false_bb = self.ctx.append_basic_block(fun, "if_false_bb");
                let after_bb = self.ctx.append_basic_block(fun, "after_if_branch_bb");
                self.build.build_conditional_branch(cond, true_bb, false_bb);

                self.build.position_at_end(true_bb);
                for stmt in true_block {
                    self.gen(stmt, false);
                }
                //true_bb = self.build.get_insert_block().unwrap();
                self.build.build_unconditional_branch(after_bb);

                self.build.position_at_end(false_bb);

                match else_block.is_some() {
                    true => {
                        for stmt in else_block.as_ref().unwrap().iter() {
                            self.gen(stmt, false);
                        }
                        self.build.build_unconditional_branch(after_bb);
                        //false_bb = self.build.get_insert_block().unwrap();
                    }
                    false => {
                        self.build.build_unconditional_branch(after_bb);
                    }
                };

                self.build.position_at_end(after_bb);
                cond.as_any_value_enum()
            },
            Ast::While{cond, block} => {
                let fun = self.current_fn.expect("While loop outside of function");

                let cond_bb = self.ctx.append_basic_block(fun, "while_cond_bb");
                let while_bb = self.ctx.append_basic_block(fun, "while_loop_bb");
                let after_bb = self.ctx.append_basic_block(fun, "after_while_bb");

                self.build.build_unconditional_branch(cond_bb); //Jump to the condition block for the first check
                self.build.position_at_end(cond_bb);
                let cond = self.gen(cond, false).into_int_value();

                

                self.build.build_conditional_branch(cond, while_bb, after_bb);
                self.build.position_at_end(while_bb);

                let old_vars = self.vars.clone();
                for stmt in block {
                    self.gen(stmt, false);
                }
                self.vars = old_vars; //Drop values that were enclosed in the while loop

                let br = self.build.build_unconditional_branch(cond_bb); //Branch back to the condition to check it
                self.build.position_at_end(after_bb); //Continue condegen after the loop block
                br.as_any_value_enum()
            }
            Ast::VarDecl { ty, name, attrs: _ } => {
                let var = self.entry_alloca(name.as_str(), self.llvm_type(ty));
                self.vars.insert(name.clone(), (var, ty.clone()));
                var.as_any_value_enum()
            }
            Ast::VarAccess(name) => match self.vars.get(name) {
                Some((val, _)) => match lval {
                    false => self.build.build_load(*val, "ssa_load").as_any_value_enum(),
                    true => val.as_any_value_enum(),
                },
                None => panic!(
                    "Accessing unknown variable {}{}",
                    name,
                    match self.current_fn {
                        Some(f) => format!(
                            " in function {}",
                            f.get_name()
                                .to_str()
                                .expect("Failed to convert function name: invalid UTF-8")
                        ),

                        None => "".to_owned(),
                    }
                ),
            },
            Ast::StructLiteral { name, fields } => {
                let (ty, def) = self.get_struct(name).unwrap_or_else(|| {
                    panic!(
                        "Using unknown struct type {} when defining struct literal",
                        name
                    )
                });
                let ty = ty.clone();
                let def = def.clone();

                let mut pos_vals = Vec::with_capacity(def.fields.len());
                unsafe { pos_vals.set_len(def.fields.len()) };
                for field in fields {
                    let pos = def
                        .fields
                        .iter()
                        .position(|s| s.0 == field.0)
                        .unwrap_or_else(|| {
                            panic!(
                                "In struct literal for struct type {}: No field named {}",
                                name, field.0
                            )
                        });
                    let val = self.gen(&field.1, false);     
                    pos_vals[pos] = 
                        BasicValueEnum::try_from(val)
                            .expect("Failed to convert struct literal field to a basic value");
                }

                let literal = self.entry_alloca("struct_literal", ty.as_basic_type_enum()); //Create an alloca for the struct literal
                //Store the fields in the allocated struct literal
                for (idx, val) in pos_vals.iter().enumerate() {
                    let field = self.build.build_struct_gep(literal, idx as u32, "struct_literal_field").unwrap();
                    self.build.build_store(field, *val);
                }
                self.build.build_load(literal, "load_struct_literal").as_any_value_enum()
            }
            Ast::MemberAccess(val, field) => {
                let col = val
                    .get_type(
                        &self.funs,
                        &self.struct_types,
                        &self.union_types,
                        &self.vars,
                    )
                    .expect("Failed to get type of lhs when accessing member of struct or union");
                let (_, s_ty, is_struct) = match col {
                    Type::UnknownStruct(name) => {
                        let (ref s_ty, con) = self
                        .struct_types
                        .get(&name)
                        .unwrap_or_else(|| panic!("Using unknown struct type {}", name));
                        (s_ty, con, true)
                    },
                    Type::UnknownUnion(name) => {
                        let (s_ty, con) = self
                        .union_types
                        .get(&name)
                        .unwrap_or_else(|| panic!("Using unknown union type {}", name));
                        (s_ty, con, false)
                    }
                    Type::Unknown(name) => match self.struct_types.get(&name) {
                        Some((s_ty, con)) => (s_ty, con, true),
                        None => {
                            let (ref u_ty, ref con) = self.union_types
                            .get(&name)
                            .unwrap_or_else(|| panic!("Using unknown type {}", name));
                            (u_ty, con, false)
                        },
                    },
                    _ => panic!("Not a structure type"),
                };

                match is_struct {
                    true => {
                        let field_idx = s_ty
                        .fields
                        .iter()
                        .position(|(name, _)| name == field)
                        .unwrap_or_else(|| {
                            panic!("Struct type {} has no field named {}", s_ty.name, field)
                        });
                        let s = self.gen(val, true);
                        let field = self
                            .build
                            .build_struct_gep(s.into_pointer_value(), field_idx as u32, "struct_gep")
                            .unwrap();

                        //Return the pointer value if we are generating an assignment
                        match lval {
                            false => self
                                .build
                                .build_load(field, "load_struct_field")
                                .as_any_value_enum(),
                            true => field.as_any_value_enum(),
                        }
                    },
                    false => {
                        let (_, field_ty) = s_ty.fields.iter().find(|(name, _)| name == field).unwrap_or_else(|| panic!("Union type {} has no field named {}", s_ty.name, field));
                        let field_ty = self.llvm_type(field_ty);
                        match lval {
                            true => {
                                let u = self.gen(val, true);
                                self.build.build_pointer_cast(u.into_pointer_value(), field_ty.ptr_type(inkwell::AddressSpace::Generic), "union_member_access_lval_cast").as_any_value_enum()
                            },
                            false => {
                                let u = self.gen(val, false);
                                self.build.build_bitcast(u.into_struct_value().as_basic_value_enum(), field_ty, "union_member_access_rval_cast").as_any_value_enum()
                            }
                        }
                    }
                }

                
            }
            Ast::StrLiteral(string) => {
                let s = self
                    .build
                    .build_global_string_ptr(string.as_str(), "const_string_literal");
                unsafe {
                    self.build
                        .build_gep(
                            s.as_pointer_value(),
                            &[self.ctx.i64_type().const_zero()],
                            "string_literal_gep",
                        )
                        .as_any_value_enum()
                }
            }
            Ast::Cast(expr, ty) => {
                let lhs = self.gen(expr, false);
                match (lhs.get_type(), self.llvm_type(ty)) {
                    (AnyTypeEnum::IntType(_), BasicTypeEnum::PointerType(ptr)) => self
                        .build
                        .build_int_to_ptr(lhs.into_int_value(), ptr, "int_to_ptr_cast")
                        .as_any_value_enum(),
                    (AnyTypeEnum::IntType(_), BasicTypeEnum::IntType(ity2)) => self
                        .build
                        .build_int_cast(lhs.into_int_value(), ity2, "int_to_int_cast")
                        .as_any_value_enum(),
                    (AnyTypeEnum::PointerType(_), BasicTypeEnum::IntType(ity)) => self
                        .build
                        .build_ptr_to_int(lhs.into_pointer_value(), ity, "ptr_to_int_cast")
                        .as_any_value_enum(),
                    (one, two) => panic!("Cannot cast type {:?} to {:?}", one, two),
                }
            }
            Ast::Unary(op, val) => match op {
                Op::And => self.gen(val, true),
                Op::Star => {
                    let ptr = self.gen(val, false).into_pointer_value();
                    match lval {
                        false => self.build
                        .build_load(ptr, "deref_pointer_load")
                        .as_any_value_enum(),
                        true => ptr.as_any_value_enum(),
                    }
                    
                }
                other => panic!("Unknown unary operator {} being applied", other),
            },
            Ast::Bin(lhs, op, rhs) => self.gen_bin(lhs, rhs, op),
            other => unimplemented!("Cannot use expression {:?} inside of a function", other),
        }
    }

    /// Get all struct and union type declarations from the top level of the AST and remove them
    fn get_type_decls(&mut self, ast: Vec<Ast>) -> Vec<Ast> {
        ast.into_iter()
            .filter_map(|node| match node {
                Ast::StructDef(mut c) => {
                    c.name = self.apply_ns(c.name);
                    let ty = self.llvm_type(&Type::Struct(c.clone())).into_struct_type();
                    self.struct_types.insert(c.name.clone(), (ty, c));
                    None
                },
                Ast::UnionDef(mut c) => {
                    c.name = self.apply_ns(c.name);
                    self.union_types.insert(
                        c.name.clone(),
                        (
                            self.llvm_type(&Type::Union(c.clone())).into_struct_type(),
                            c.clone(),
                        ),
                    );
                    None
                },
                //Insert a user-defined typedef
                Ast::TypeDef(name, ty) => {
                    self.typedefs.insert(self.apply_ns(name), ty); 
                    None
                },
                Ast::Namespace(names, body) => {
                    for name in names.iter() {
                        self.enter_ns(name); //Add the namespace
                    }

                    let usings = self.using.clone(); //Get a list of used namesapces inside this namespace
                    self.get_type_decls(body.clone()); //Get all function prototypes in the namespace, applying the namespace
                    self.using = usings; //Stop using nested namespace's used namespaces
                    for _ in names.iter() {
                        self.ns.pop(); //Pop the namespace from our stack
                    } //Pop the namespace from our namespace stack
                    Some(Ast::Namespace(names, body))
                },
                Ast::Using(all, item) => {
                    match all {
                        true => { self.using.push(item.clone()); },
                        false => { self.used_items.insert(item.clone().split("::").last().unwrap().to_owned(), item.clone()); },
                    };
                    Some(Ast::Using(all, item))
                }
                other => Some(other),
            })
            .collect::<Vec<_>>()
    }

    /// Filter the AST, adding function prototypes to the module ahead of time
    fn get_fn_protos(&mut self, ast: Vec<Ast>) -> Vec<Ast> {
        ast.into_iter()
            .filter_map(|node| match node {
                Ast::FunProto(p) => {
                    self.gen_fun_proto(p).unwrap();
                    None
                }
                Ast::FunDef(proto, body) => {
                    self.gen_fundef(&proto, &body);
                    None
                }
                Ast::Namespace(names, body) => {
                    for name in names.iter() {
                        self.enter_ns(name); //Add the namespace
                    }
                    let usings = self.using.clone(); //Get a list of used namesapces inside this namespace
                    self.get_fn_protos(body); //Get all function prototypes in the namespace, applying the namespace
                    self.using = usings; //Stop using the namespaces of nested namespaces
                    for _ in names.iter() {
                        self.ns.pop(); //Pop the namespace from our stack
                    }
                    None
                },
                Ast::Using(all, item) => {
                    match all {
                        true => { self.using.push(item.clone()); },
                        false => { self.used_items.insert(item.clone().split("::").last().unwrap().to_owned(), item); },
                    };
                    None
                },
                other => Some(other),
            })
            .collect::<Vec<_>>()
    }

    /// Generate all code for a LLVM module and return it
    pub fn finish(mut self, ast: Vec<Ast>) -> Module<'c> {
        let ast = self.get_type_decls(ast);
        let ast = self.get_fn_protos(ast);
        for node in ast {
            self.gen(&node, false);
        }
        self.module
    }

    /// Compile the code into an executable file
    pub fn compile(self, ast: Vec<Ast>, opts: CompileOpts) {
        const LINKER: &str = "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\bin\\Hostx64\\x64\\link.exe";
        use std::process::Stdio;

        let module = self.finish(ast); //Compile the actual AST into LLVM IR
        module.verify().unwrap_or_else(|e| panic!("Failed to verify the LLVM module: {}", e));

        let fpm: PassManager<Module<'c>> = PassManager::create(());

        match opts.opt_lvl {
            crate::OptLvl::Debug => (),
            crate::OptLvl::Medium => { 
                fpm.add_demote_memory_to_register_pass();
                fpm.add_promote_memory_to_register_pass();
                fpm.add_constant_merge_pass();
                fpm.add_instruction_combining_pass();
                fpm.add_global_optimizer_pass();
            },
            crate::OptLvl::Aggressive => {
                fpm.add_demote_memory_to_register_pass();
                fpm.add_promote_memory_to_register_pass();
                fpm.add_constant_merge_pass();
                fpm.add_instruction_combining_pass();
                fpm.add_global_optimizer_pass();


                fpm.add_loop_rotate_pass();
                fpm.add_argument_promotion_pass(); 
                fpm.add_function_inlining_pass();
                fpm.add_memcpy_optimize_pass();
                fpm.add_loop_deletion_pass();
                fpm.add_loop_vectorize_pass();
                fpm.add_constant_propagation_pass();
                fpm.add_simplify_lib_calls_pass();
                fpm.add_strip_symbol_pass();
            }
        }
        
        match opts.output_ty {
            OutFormat::IR => module.print_to_file(opts.out_file).unwrap(),
            other=> {
                Target::initialize_all(&InitializationConfig::default());
                let opt = OptimizationLevel::Aggressive;
                let reloc = RelocMode::Default;
                let model = CodeModel::Default;
                let target = Target::from_triple(&TargetMachine::get_default_triple()).unwrap();
                let machine = target
                    .create_target_machine(
                        &TargetMachine::get_default_triple(),
                        &TargetMachine::get_host_cpu_name().to_str().unwrap(),
                        &TargetMachine::get_host_cpu_features().to_str().unwrap(),
                        opt,
                        reloc,
                        model,
                    )
                    .unwrap();

                machine.add_analysis_passes(&fpm);
                
                match other {
                    OutFormat::Asm => machine
                        .write_to_file(
                            &module,
                            FileType::Assembly,
                            &opts.out_file,
                        )
                        .unwrap(),
                    OutFormat::Obj => machine
                        .write_to_file(
                            &module,
                            FileType::Object,
                            &opts.out_file,
                        )
                        .unwrap(),
                    OutFormat::Lib => {
                        let obj = opts.out_file.with_extension("obj");
                        machine
                        .write_to_file(
                            &module,
                            FileType::Object,
                            &obj,
                        )
                        .unwrap();

                        let out = format!("/OUT:{}", opts.out_file.display());
                        let mut args = vec!["/LIB", obj.to_str().unwrap(), "/NOLOGO", out.as_str()];
                        args.extend(opts.libraries.iter().map(|s| s.as_str())); //Add all linked libraries

                        let cmd = Command::new(Path::new(LINKER)).args(args).stderr(Stdio::piped()).stdout(Stdio::piped()).spawn().unwrap(); //Link the file into a library
                        println!(
                            "{}",
                            String::from_utf8(cmd.wait_with_output().unwrap().stdout).unwrap()
                        );
                        
                        std::fs::remove_file(obj).unwrap();
                    },
                    OutFormat::Exe => {
                        let obj = opts.out_file.with_extension("obj");
                        machine
                        .write_to_file(
                            &module,
                            FileType::Object,
                            &obj,
                        )
                        .unwrap();

                        let out = format!("/OUT:{}", opts.out_file.display());
                        let mut args = vec![obj.to_str().unwrap(), "/ENTRY:main", "/SUBSYSTEM:console", "/NOLOGO", out.as_str()];
                        args.extend(opts.libraries.iter().map(|s| s.as_str())); //Add all linked libraries

                        let cmd = Command::new(Path::new(LINKER)).args(args).stderr(Stdio::piped()).stdout(Stdio::piped()).spawn().unwrap(); //Link the file into a library
                        println!(
                            "{}",
                            String::from_utf8(cmd.wait_with_output().unwrap().stdout).unwrap()
                        );
                        std::fs::remove_file(obj).unwrap();
                    }
                    _ => unreachable!()
                }
                
                
            }
        }
        
    }
}
