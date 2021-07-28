pub mod compile;
pub mod types;
use bumpalo::Bump;
use log::{debug, error, trace, warn};
use std::{cell::Cell, convert::TryFrom, ops::Deref};

use crate::{
    ast::{Ast, AstPos, FunProto},
    lex::Op,
    types::Container,
    Type,
};
use hashbrown::HashMap;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate,
};

use super::ns::Ns;

/// The `Compiler` struct is used to generate an executable with LLVM from the parsed AST.
pub struct Compiler<'a, 'c> {
    /// The LLVM context
    ctx: &'c Context,

    /// The arena allocator for namespaces
    arena: &'a Bump,

    /// The root namespace
    root: &'a Ns<'a, 'c>,

    /// The current namespace
    current_ns: Cell<&'a Ns<'a, 'c>>,

    /// The LLVM module that we will be writing code to
    module: Module<'c>,

    /// The IR builder that we use to build LLVM IR
    build: Builder<'c>,

    /// The function that we are currently generating code in
    current_fn: Option<FunctionValue<'c>>,

    /// If we just wrote a return instruction
    just_ret: bool,

    /// The signature of the current function
    current_proto: Option<FunProto>,

    /// A map of variable / argument names to LLVM values
    pub vars: HashMap<String, (PointerValue<'c>, Type)>,
}

impl<'a, 'c> Compiler<'a, 'c> {
    /// Create a new `Compiler` from an LLVM context struct
    pub fn new(ctx: &'c Context, arena: &'a Bump) -> Self {
        let root = arena.alloc(Ns::new_empty(String::new()));
        Self {
            ctx,
            build: ctx.create_builder(),
            module: ctx.create_module("spark_llvm_module"),
            current_fn: None,
            vars: HashMap::new(),
            current_proto: None,
            arena,
            root,
            just_ret: false,
            current_ns: Cell::new(root),
        }
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
    fn gen_bin(&mut self, lhs: &AstPos, rhs: &AstPos, op: &Op) -> Option<AnyValueEnum<'c>> {
        match op {
            //Handle assignment separately
            Op::Assign => {
                let lhs = self.gen(lhs, true)?.into_pointer_value();
                let rhs = match BasicValueEnum::try_from(self.gen(rhs, false)?) {
                    Ok(val) => val,
                    Err(()) => {
                        error!("{}: Failed to generate code for assignment expression; the right hand side is not a basic value", rhs.1);
                        return None;
                    }
                };

                Some(self.build.build_store(lhs, rhs).as_any_value_enum())
            }
            op => {
                use std::mem::discriminant;
                let lhs_ty = lhs.get_type(self);
                let rhs_ty = rhs.get_type(self);
                if discriminant(&lhs_ty) != discriminant(&rhs_ty) {
                    error!("{}: Left hand side of '{}' expression does not match types with right hand side! LHS: {:?}, RHS: {:?}", lhs.1, op, lhs.ast(), rhs_ty);
                    return None;
                }
                let pos = lhs.1.clone();
                let lhs = self.gen(lhs, false)?;
                let rhs = self.gen(rhs, false)?;
                let ty = lhs.get_type();
                Some(match (ty, op) {
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
                        .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "int_div")
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
                    }

                    //---------- Pointer Operations
                    (AnyTypeEnum::PointerType(ptr), op) => {
                        let lhs = self.build.build_ptr_to_int(
                            lhs.into_pointer_value(),
                            self.ctx.i64_type(),
                            "ptr_cmp_cast_lhs",
                        );
                        let rhs = self.build.build_ptr_to_int(
                            rhs.into_pointer_value(),
                            self.ctx.i64_type(),
                            "ptr_cmp_cast_rhs",
                        );

                        match op {
                            Op::NEqual => self
                                .build
                                .build_int_compare(IntPredicate::NE, lhs, rhs, "ptr_nequal_cmp")
                                .as_any_value_enum(),
                            Op::Equal => self
                                .build
                                .build_int_compare(IntPredicate::NE, lhs, rhs, "ptr_equal_cmp")
                                .as_any_value_enum(),

                            Op::Plus => self
                                .build
                                .build_int_to_ptr(
                                    self.build.build_int_add(lhs, rhs, "ptr_add"),
                                    ptr,
                                    "ptr_add_cast_back_to_ptr",
                                )
                                .as_any_value_enum(),
                            Op::Minus => self
                                .build
                                .build_int_to_ptr(
                                    self.build.build_int_sub(lhs, rhs, "ptr_sub"),
                                    ptr,
                                    "ptr_sub_cast_back_to_ptr",
                                )
                                .as_any_value_enum(),
                            Op::Star => self
                                .build
                                .build_int_to_ptr(
                                    self.build.build_int_mul(lhs, rhs, "ptr_mul"),
                                    ptr,
                                    "ptr_mul_cast_back_to_ptr",
                                )
                                .as_any_value_enum(),
                            Op::Divide => self
                                .build
                                .build_int_to_ptr(
                                    self.build.build_int_unsigned_div(lhs, rhs, "ptr_div"),
                                    ptr,
                                    "ptr_div_cast_back_to_ptr",
                                )
                                .as_any_value_enum(),
                            other => {
                                error!("{}: Cannot use operator {} when operand types are pointer types", pos, other);
                                return None;
                            }
                        }
                    }
                    other => {
                        error!(
                            "{}: Cannot use operator '{}' with operand type {:?}",
                            pos, other.1, lhs_ty
                        );
                        return None;
                    }
                })
            }
        }
    }

    /// Generate code for one expression, only used for generating function bodies, no delcarations.
    /// ## Note
    /// Returns [None] on error, and writes error message to stderr
    pub fn gen(&mut self, node: &AstPos, lval: bool) -> Option<AnyValueEnum<'c>> {
        match node.ast() {
            Ast::NumLiteral(ty, num) => Some(
                self.llvm_type(ty, &node.1)
                    .into_int_type()
                    .const_int_from_string(num.as_str(), inkwell::types::StringRadix::Decimal)
                    .unwrap()
                    .as_any_value_enum(),
            ),
            Ast::Ret(node) => {
                match self
                    .current_proto
                    .as_ref()
                    .expect("Must be in a function to return from one!")
                    .ret
                {
                    Type::Void => {
                        self.just_ret = true;
                        Some(self.build.build_return(None).as_any_value_enum())
                    }
                    ref ty => {
                        let ty = ty.clone();
                        let ret = self.gen(&node.deref().as_ref().unwrap(), false)?;
                        if node.deref().as_ref().unwrap().get_type(self)? != ty {
                            error!(
                                "{}: In function {}: Returning the incorrect type: {} expected, {} returned",
                                node.deref().as_ref().unwrap().1,
                                self.current_fn.unwrap().get_name().to_str().unwrap(),
                                ty,
                                node.deref().as_ref().unwrap().get_type(self).unwrap()
                            );
                            return None;
                        }
                        self.just_ret = true;
                        Some(
                            self.build
                                .build_return(Some(&BasicValueEnum::try_from(ret).unwrap()))
                                .as_any_value_enum(),
                        )
                    }
                }
            }
            Ast::FunCall(name, args) => match self.get_fun(&name) {
                Some((f, p)) => {
                    //Do initial argument count length
                    if p.args.len() != args.len() {
                        error!("{}: Incorrect number of arguments passed to function {}; {} expected, {} passed", node.1, name, p.args.len(), args.len());
                        return None;
                    }

                    //Do type checking of function arguments
                    for (i, (arg, (parg, _))) in args.iter().zip(p.args.iter()).enumerate() {
                        if let Some(ref ty) = arg.get_type(self) {
                            if ty != parg {
                                error!("{}: Incorrect type of argument {} in function call {}; {} expected, {} passed", arg.1, i + 1, p.name, parg, ty);
                                return None;
                            }
                        } else {
                            error!(
                                "{}: Failed to get type of argument {} in function call {}",
                                arg.1,
                                i + 1,
                                p.name
                            );
                            return None;
                        }
                    }
                    let args = args
                        .iter()
                        .enumerate()
                        .map(|(argnum, n)| {
                            match BasicValueEnum::try_from(self.gen(n, false)?) {
                                Ok(val) => Some(val),
                                Err(()) => {
                                    error!("{}: Failed to generate code for argument #{} in function {} as the argument was not a basic type", n.1, argnum, name);
                                    None
                                }
                            }
                        }).collect::<Option<Vec<_>>>()?;
                    Some(
                        self.build
                            .build_call(f, args.as_ref(), "tmp_fncall")
                            .as_any_value_enum(),
                    )
                }
                None => panic!("Calling unknown function {}", name),
            },
            Ast::AssocFunAccess(item, name, args) => match self.get_fun(name.as_str()) {
                Some((f, _)) => {
                    let item = BasicValueEnum::try_from(self.gen(&item.deref(), false)?).unwrap(); //Generate code for the first expression
                    let mut real_args = vec![item];
                    real_args.extend(
                        args
                            .iter()
                            .map(|n| Some(BasicValueEnum::try_from(self.gen(&n, false)?).expect("Failed to convert any value enum to basic value enum when calling function"))).collect::<Option<Vec<_>>>()? );
                    Some(
                        self.build
                            .build_call(f, real_args.as_ref(), "tmp_assoc_fncall")
                            .as_any_value_enum(),
                    )
                }
                None => panic!("Calling unknown associated function {}", name),
            },
            Ast::If {
                cond: condition,
                true_block,
                else_block,
            } => {
                self.just_ret = false;
                let cond = self.gen(&condition, false)?.into_int_value();
                let fun = match self.current_fn {
                    Some(fun) => fun,
                    None => {
                        error!(
                            "{}: Cannot use an if expression outside of a function body",
                            condition.1
                        );
                        return None;
                    }
                };

                let true_bb = self.ctx.append_basic_block(fun, "if_true_bb");
                let false_bb = self.ctx.append_basic_block(fun, "if_false_bb");
                let after_bb = self.ctx.append_basic_block(fun, "after_if_branch_bb");
                self.build.build_conditional_branch(cond, true_bb, false_bb);

                self.build.position_at_end(true_bb);
                for stmt in true_block {
                    self.gen(&stmt, false);
                    if self.just_ret {
                        break;
                    }
                }
                if !self.just_ret {
                    self.build.build_unconditional_branch(after_bb);
                } else {
                    self.just_ret = false;
                    trace!("Encountered an early return from an if block, so not inserting a jump");
                }
                //true_bb = self.build.get_insert_block().unwrap();
                //self.build.build_unconditional_branch(after_bb);

                self.build.position_at_end(false_bb);

                match else_block.is_some() {
                    true => {
                        for stmt in else_block.as_ref().unwrap().iter() {
                            self.gen(&stmt, false);
                            if self.just_ret {
                                break;
                            }
                        }
                        if !self.just_ret {
                            self.build.build_unconditional_branch(after_bb);
                        } else {
                            self.just_ret = false;
                        }
                        //false_bb = self.build.get_insert_block().unwrap();
                    }
                    false => {
                        self.build.build_unconditional_branch(after_bb);
                    }
                };

                self.build.position_at_end(after_bb);
                Some(cond.as_any_value_enum())
            }
            Ast::While { cond, block } => {
                let fun = self.current_fn.expect("While loop outside of function");

                let cond_bb = self.ctx.append_basic_block(fun, "while_cond_bb");
                let while_bb = self.ctx.append_basic_block(fun, "while_loop_bb");
                let after_bb = self.ctx.append_basic_block(fun, "after_while_bb");

                self.build.build_unconditional_branch(cond_bb); //Jump to the condition block for the first check
                self.build.position_at_end(cond_bb);
                let cond = self.gen(&cond, false)?.into_int_value();

                self.build
                    .build_conditional_branch(cond, while_bb, after_bb);
                self.build.position_at_end(while_bb);

                let old_vars = self.vars.clone();
                for stmt in block {
                    self.gen(&stmt, false);
                }
                self.vars = old_vars; //Drop values that were enclosed in the while loop

                let br = self.build.build_unconditional_branch(cond_bb); //Branch back to the condition to check it
                self.build.position_at_end(after_bb); //Continue condegen after the loop block
                Some(br.as_any_value_enum())
            }
            Ast::VarDecl { ty, name, attrs: _ } => {
                let var = self.entry_alloca(name.as_str(), self.llvm_type(ty, &node.1));
                self.vars.insert(name.clone(), (var, ty.clone()));
                Some(var.as_any_value_enum())
            }
            Ast::VarAccess(name) => match self.vars.get(name) {
                Some((val, _)) => match lval {
                    false => Some(
                        self.build
                            .build_load(*val, "var_access_ssa_load")
                            .as_any_value_enum(),
                    ),
                    true => Some(val.as_any_value_enum()),
                },
                None => {
                    error!("{}: Accessing unknown variable {}", node.1, name,);
                    None
                }
            },
            Ast::StructLiteral { name, fields } => {
                let (ty, def) = match self.get_struct(name) {
                    Some((ty, def)) => (ty, def),
                    None => {
                        error!(
                            "{}: Using unknown struct type {} when defining struct literal",
                            node.1, name
                        );
                        return None;
                    }
                };

                if def.fields.is_none() {
                    error!(
                        "{}: Cannot have literal of opaque struct type {}",
                        node.1, def.name
                    );
                    return None;
                }
                let def_fields = def.fields.as_ref().unwrap(); //Safe, we already checked that the struct is not opaque

                let mut pos_vals = Vec::with_capacity(def_fields.len());
                unsafe { pos_vals.set_len(def_fields.len()) };
                for field in fields {
                    let pos = match def_fields.iter().position(|s| s.0 == field.0) {
                        Some(pos) => {
                            let ty = field.1.get_type(self)?;
                            if def_fields[pos].1 != ty {
                                error!("{}: In struct literal for type {}; incorrect type of field {}, {} expected but value of type {} given", node.1, name, def_fields[pos].0, def_fields[pos].1, ty);
                                return None;
                            }
                            pos
                        }
                        None => {
                            error!(
                                "{}: In struct literal for struct type {}: No field named {}",
                                node.1, name, field.0
                            );
                            return None;
                        }
                    };
                    let val = self.gen(&field.1, false)?;
                    pos_vals[pos] = BasicValueEnum::try_from(val)
                        .expect("Failed to convert struct literal field to a basic value");
                }

                let literal = self.entry_alloca("struct_literal", ty.as_basic_type_enum()); //Create an alloca for the struct literal
                                                                                            //Store the fields in the allocated struct literal
                for (idx, val) in pos_vals.iter().enumerate() {
                    let field = self
                        .build
                        .build_struct_gep(literal, idx as u32, "struct_literal_field")
                        .unwrap();
                    self.build.build_store(field, *val);
                }
                Some(
                    self.build
                        .build_load(literal, "load_struct_literal")
                        .as_any_value_enum(),
                )
            }
            Ast::MemberAccess(val, field, deref) => {
                let mut col = val
                    .get_type(self)
                    .expect("Failed to get type of lhs when accessing member of struct or union");

                if *deref {
                    col = col.deref_type().unwrap();
                }

                let (s_ty, is_struct) = match col {
                    Type::Struct(con) => (con, true),
                    Type::Union(con) => (con, false),
                    other => {
                        error!(
                            "{}: Cannot access member {} of non-struct or union type",
                            node.1, field
                        );
                        debug!("Cannot generate code for member access expression because the prefix type is {:?}", other);
                        return None;
                    }
                };

                match is_struct {
                    true => {
                        let field_idx = s_ty
                            .fields
                            .as_ref()
                            .unwrap()
                            .iter()
                            .position(|(name, _)| name == field)
                            .unwrap_or_else(|| {
                                panic!("Struct type {} has no field named {}", s_ty.name, field)
                            });
                        let mut s = self.gen(val, true)?;
                        if *deref {
                            s = self
                                .build
                                .build_load(s.into_pointer_value(), "deref_lhs_member_access")
                                .as_any_value_enum()
                        }

                        let field = self
                            .build
                            .build_struct_gep(
                                s.into_pointer_value(),
                                field_idx as u32,
                                "struct_member_access_gep",
                            )
                            .unwrap();

                        //Return the pointer value if we are generating an assignment
                        Some(match lval {
                            false => self
                                .build
                                .build_load(field, "load_struct_field")
                                .as_any_value_enum(),
                            true => field.as_any_value_enum(),
                        })
                    }
                    false => {
                        let (_, field_ty) = match s_ty
                            .fields
                            .as_ref()
                            .unwrap()
                            .iter()
                            .find(|(name, _)| name == field)
                        {
                            Some(val) => val,
                            None => {
                                error!(
                                    "{}: Union type {} has no field named {}",
                                    node.1, s_ty.name, field
                                );
                                return None;
                            }
                        };
                        let field_ty = self.llvm_type(field_ty, &node.1);
                        Some(match lval {
                            true => {
                                trace!(
                                    "{}: Generating union member lvalue access with type punning",
                                    val.1
                                );
                                let u = self.gen(val, true)?;
                                self.build
                                    .build_pointer_cast(
                                        u.into_pointer_value(),
                                        field_ty.ptr_type(inkwell::AddressSpace::Generic),
                                        "union_member_access_lval_punning",
                                    )
                                    .as_any_value_enum()
                            }
                            false => {
                                trace!("{}: Generating union member rvalue access with type punning and load", val.1);
                                let u = self.gen(val, true)?;
                                let ptr = self.build.build_pointer_cast(
                                    u.into_pointer_value(),
                                    field_ty.ptr_type(inkwell::AddressSpace::Generic),
                                    "union_member_access_rval_punning",
                                );
                                self.build
                                    .build_load(ptr, "union_member_access_rval_load")
                                    .as_any_value_enum()
                                /*self.build
                                .build_bitcast(
                                    u.into_struct_value().as_basic_value_enum(),
                                    field_ty,
                                    "union_member_access_rval_cast",
                                )
                                .as_any_value_enum()*/
                            }
                        })
                    }
                }
            }
            Ast::StrLiteral(string) => {
                let s = self
                    .build
                    .build_global_string_ptr(string.as_str(), "const_string_literal");
                unsafe {
                    Some(
                        self.build
                            .build_gep(
                                s.as_pointer_value(),
                                &[self.ctx.i64_type().const_zero()],
                                "string_literal_gep",
                            )
                            .as_any_value_enum(),
                    )
                }
            }
            Ast::Cast(expr, ty) => {
                let lhs = self.gen(expr, false)?;
                Some(match (lhs.get_type(), self.llvm_type(ty, &node.1)) {
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
                    (AnyTypeEnum::PointerType(_), BasicTypeEnum::PointerType(ptr2)) => self
                        .build
                        .build_pointer_cast(lhs.into_pointer_value(), ptr2, "ptr_to_ptr_cast")
                        .as_any_value_enum(),
                    (one, two) => panic!("Cannot cast type {:?} to {:?}", one, two),
                })
            }
            Ast::Unary(op, val) => match op {
                Op::And => self.gen(val, true),
                Op::Star => {
                    let ptr = self.gen(val, false)?.into_pointer_value();
                    Some(match lval {
                        false => self
                            .build
                            .build_load(ptr, "deref_pointer_load")
                            .as_any_value_enum(),
                        true => ptr.as_any_value_enum(),
                    })
                }
                other => panic!("Unknown unary operator {} being applied", other),
            },
            Ast::Bin(lhs, op, rhs) => self.gen_bin(lhs, rhs, op),

            other => unimplemented!("Cannot use expression {:?} inside of a function", other),
        }
    }
}
