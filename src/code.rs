use std::{convert::TryFrom, mem::MaybeUninit, path::Path, process::Command};

use crate::{
    ast::{Ast, FunProto},
    lex::Op,
    types::Container,
    Type,
};
use hashbrown::HashMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    object_file::Relocation,
    passes::{PassManager, PassManagerBuilder},
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    OptimizationLevel,
};

/// The `Compiler` struct is used to generate an executable with LLVM from the parsed AST.
pub struct Compiler<'c> {
    /// The LLVM context
    ctx: &'c Context,

    /// A hash map of identifiers to defined struct types
    struct_types: HashMap<String, (StructType<'c>, Container)>,

    /// A hash map of identifiers to defined union types
    union_types: HashMap<String, (StructType<'c>, Container)>,

    /// The LLVM module that we will be writing code to
    module: Module<'c>,

    /// The IR builder that we use to build LLVM IR
    build: Builder<'c>,

    /// The function that we are currently generating code in
    current_fn: Option<FunctionValue<'c>>,

    /// A map of variable / argument names to LLVM values
    vars: HashMap<String, BasicValueEnum<'c>>,
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
            vars: HashMap::new(),
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
                self.ctx.struct_type(con.fields.iter().map(|(_, f)| self.llvm_type(f)).collect::<Vec<_>>().as_slice(), false).as_basic_type_enum()
            },
            Type::Union(con) => {
                let largest = con.fields.iter().max_by(|(_, prev), (_, this)| prev.size().cmp(&this.size())).expect("Union type with no fields!");
                self.ctx.struct_type(&[self.llvm_type(&largest.1)], false).as_basic_type_enum()
            },
            Type::UnknownStruct(name) => match self.struct_types.get(name) {
                Some(s_ty) => s_ty.0.as_basic_type_enum(),
                None => panic!("Unknown struct type {}", name),
            },
            Type::UnknownUnion(name) => match self.union_types.get(name) {
                Some(u_ty) => u_ty.0.as_basic_type_enum(),
                None => panic!("Unknown union type {}", name),
            },
            Type::Unknown(name) => match (self.union_types.get(name), self.struct_types.get(name)) {
                (Some(_), Some(_)) => panic!("Type {} can be both a union and a struct, prefix with struct or union keywords to remove abiguity", name),
                (Some(u), _) => u.0.as_basic_type_enum(),
                (_, Some(s)) => s.0.as_basic_type_enum(),
                (None, None) => panic!("Unknown union or struct type {}", name),
            },
            Type::Void => panic!("Cannot create void type in LLVM!"),
        }
    }

    /// Generate code for a function prototype
    pub fn gen_fun_proto(&mut self, proto: FunProto) -> Result<FunctionValue<'c>, String> {
        if self.module.get_function(proto.name.as_str()).is_some() {
            return Err(format!("Function {} defined twice", proto.name));
        }
        Ok(self.module.add_function(
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
        ))
    }

    /// Build an alloca for a variable in the current function
    fn entry_alloca(&self, name: &str, ty: BasicTypeEnum<'c>) -> PointerValue<'c> {
        let mut entry_builder = self.ctx.create_builder();
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

    /// Generate code for one expression
    pub fn gen(&mut self, node: &Ast) -> AnyValueEnum<'c> {
        match node {
            Ast::NumLiteral(ty, num) => self
                .llvm_type(ty)
                .into_int_type()
                .const_int_from_string(num.as_str(), inkwell::types::StringRadix::Decimal)
                .unwrap()
                .as_any_value_enum(),
            Ast::Ret(node) => {
                let ret = self.gen(node);
                self.build
                    .build_return(Some(&BasicValueEnum::try_from(ret).unwrap()))
                    .as_any_value_enum()
            }
            Ast::FunCall(name, args) => match self.module.get_function(name.as_str()) {
                Some(f) => {
                    let args = args.iter().map(|n| BasicValueEnum::try_from(self.gen(n)).expect("Failed to convert any value enum to basic value enum when calling function")).collect::<Vec<_>>();
                    self.build
                        .build_call(f, args.as_ref(), "tmp_fncall")
                        .as_any_value_enum()
                }
                None => panic!("Calling unknown function {}", name),
            },
            Ast::FunDef(proto, body) => {
                if self.current_fn.is_some() {
                    panic!("Nested functions are not currently supported, function {} must be moved to the top level", proto.name);
                }

                let old_vars = self.vars.clone();

                let f = match self.module.get_function(proto.name.as_str()) {
                    Some(f) => f,
                    None => self.gen_fun_proto(proto.clone()).unwrap(),
                };
                self.current_fn = Some(f);

                let bb = self.ctx.append_basic_block(f, "fn_entry"); //Add the first basic block
                self.build.position_at_end(bb); //Start inserting into the function

                //Add argument names to the list of variables we can use
                for (arg, (_, proto_arg)) in f.get_param_iter().zip(proto.args.iter()) {
                    if let Some(name) = proto_arg {
                        self.vars.insert(name.clone(), arg);
                    }
                }

                //Generate code for the function body
                for ast in body {
                    self.gen(ast);
                }

                self.vars = old_vars; //Reset the variables
                self.current_fn = None;

                f.as_any_value_enum()
            }
            Ast::VarDecl { ty, name, attrs: _ } => {
                let var = self.entry_alloca(name.as_str(), self.llvm_type(ty));
                self.vars.insert(name.clone(), var.as_basic_value_enum());
                var.as_any_value_enum()
            }
            Ast::VarAccess(name) => match self.vars.get(name) {
                Some(val) => self
                    .build
                    .build_load(val.into_pointer_value(), "ssa_load")
                    .as_any_value_enum(),
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
                let (ty, def) = self.struct_types.get(name).unwrap_or_else(|| {
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
                    let val = self.gen(&field.1);
                    pos_vals.insert(
                        pos,
                        BasicValueEnum::try_from(val)
                            .expect("Failed to convert struct literal field to a basic value"),
                    );
                }
                ty.const_named_struct(pos_vals.as_ref()).as_any_value_enum()
            }
            /*Ast::MemberAccess(name, field) => match self.gen(val) {
                AnyValueEnum::StructValue(s) => {
                    let ty = s.get_type();
                    let ty = self.struct_types.values().find(|s_ty| s_ty.1 == ty).unwrap();

                    self.build.build_struct_gep(s, index, name)

                },
                other => panic!("Can't access members of type {:?}", other),
            }*/
            Ast::Bin(lhs, op, rhs) => match op {
                //Handle assignment separately
                Op::Assign => {
                    let lhs = match self.gen(lhs) {
                        AnyValueEnum::PointerValue(p) => p,
                        _ => panic!("Left hand side of assignment expression is not assignable"),
                    };
                    let rhs = BasicValueEnum::try_from(self.gen(rhs))
                        .expect("Right hand side of assignment expression is not a basic type!");

                    self.build.build_store(lhs, rhs).as_any_value_enum()
                }
                op => {
                    use std::mem::discriminant;
                    let lhs = self.gen(lhs);
                    let rhs = self.gen(rhs);
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
                        other => panic!("Unable to use operator '{}' on type {:?}", op, other),
                    }
                }
            },
            other => unimplemented!("No {:?}", other),
        }
    }

    /// Get all struct and union type declarations from the top level of the AST and remove them
    fn get_type_decls(&mut self, ast: Vec<Ast>) -> Vec<Ast> {
        ast.into_iter()
            .filter_map(|node| match node {
                Ast::StructDef(c) => {
                    let ty = self.llvm_type(&Type::Struct(c.clone())).into_struct_type();
                    self.struct_types.insert(c.name.clone(), (ty, c));
                    None
                }
                Ast::UnionDef(c) => {
                    self.union_types.insert(
                        c.name.clone(),
                        (
                            self.llvm_type(&Type::Union(c.clone())).into_struct_type(),
                            c.clone(),
                        ),
                    );
                    None
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
                    self.gen_fun_proto(proto.clone()).unwrap();
                    Some(Ast::FunDef(proto, body))
                }
                other => Some(other),
            })
            .collect::<Vec<_>>()
    }

    /// Generate all code for a LLVM module and return it
    pub fn finish(mut self, ast: Vec<Ast>) -> Module<'c> {
        let ast = self.get_type_decls(ast);
        let ast = self.get_fn_protos(ast);
        for node in ast {
            self.gen(&node);
        }
        self.module
    }

    /// Compile the code into an executable file
    pub fn compile(self, ast: Vec<Ast>, file: impl AsRef<Path>) {
        use std::process::Stdio;
        let file = file.as_ref();
        let module = self.finish(ast);

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

        let fpm: PassManager<Module<'c>> = PassManager::create(());
        fpm.add_instruction_combining_pass();
        fpm.add_loop_rotate_pass();
        fpm.add_argument_promotion_pass();
        fpm.add_constant_merge_pass();
        fpm.add_function_inlining_pass();
        fpm.add_global_optimizer_pass();
        fpm.add_memcpy_optimize_pass();
        fpm.add_demote_memory_to_register_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_loop_deletion_pass();
        fpm.add_loop_vectorize_pass();

        machine.add_analysis_passes(&fpm);
        machine
            .write_to_file(&module, FileType::Assembly, file.with_extension("s").as_path())
            .unwrap();
        machine
            .write_to_file(&module, FileType::Object, file.with_extension("obj").as_path())
            .unwrap();

        let cmd = Command::new(Path::new("C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.29.30037\\bin\\Hostx64\\x64\\link.exe")).args(&[file.with_extension("obj").to_str().unwrap(), "/ENTRY:main", "/SUBSYSTEM:console", "/NOLOGO", format!("/OUT:{}.exe", file.display()).as_str()]).stderr(Stdio::piped()).stdout(Stdio::piped()).spawn().unwrap();
        println!("{}", String::from_utf8(cmd.wait_with_output().unwrap().stdout).unwrap());
    }
}
