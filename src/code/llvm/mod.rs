pub mod compile;
pub mod types;
use bumpalo::Bump;
use log::{debug, error, trace, warn};
use std::{cell::Cell, convert::TryFrom, ops::Deref};
use either::Either;

use crate::{
    ast::{Ast, AstPos, FunProto},
    lex::Op,
    types::Container,
    Type,
};
use hashbrown::HashMap;
use inkwell::{AddressSpace, IntPredicate, basic_block::BasicBlock, builder::Builder, context::Context, module::Module, types::{AnyTypeEnum, BasicType, BasicTypeEnum, StructType}, values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, CallableValue, FunctionValue, PointerValue}};
use std::cell::RefCell;

use super::ns::Ns;

/// The `Compiler` struct is used to generate an executable with LLVM from the parsed AST.
pub struct Compiler<'a, 'c> {
    /// The LLVM context
    pub ctx: &'c Context,

    /// The right hand side expression type, if any (used for variable declaration type inference)
    pub rhs_ty: Option<Type>,

    /// The arena allocator for namespaces
    arena: &'a Bump,

    /// A map of type ID's to struct types in LLVM
    struct_types: RefCell<Vec<StructType<'c>>>,

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

    /// The label we will go to when a break statement is encountered
    break_lbl: Option<BasicBlock<'c>>,

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
            break_lbl: None,
            //Hack to get a dynamic array with uninitialized values
            struct_types: RefCell::new(vec![
                unsafe {
                    std::mem::transmute([0u8; std::mem::size_of::<StructType>()])
                };
                crate::parser::TYPEID
                    .load(std::sync::atomic::Ordering::Relaxed)
                    + 1
            ]),
            rhs_ty: None,
        }
    }

    /// Build an alloca for a variable in the current function
    fn entry_alloca(&self, name: &str, ty: BasicTypeEnum<'c>) -> PointerValue<'c> {
        let entry_builder = self.ctx.create_builder();
        let bb = self.build.get_insert_block().unwrap();
        if let Some(ref ins) = bb.get_first_instruction() {
            entry_builder.position_at(bb, ins);
        } else {
            entry_builder.position_at_end(bb);
        }

        entry_builder.build_alloca(ty, name)
    }

    /// Generate code for a binary expression
    fn gen_bin(
        &mut self,
        lhs_node: &AstPos,
        rhs_node: &AstPos,
        op: &Op,
    ) -> Option<AnyValueEnum<'c>> {
        match op {
            //Handle assignment separately
            Op::Assign => {
                self.rhs_ty = rhs_node.get_type(self);
                let lhs = self.gen(lhs_node, true)?.into_pointer_value();
                let rhs = match BasicValueEnum::try_from(self.gen(rhs_node, false)?) {
                    Ok(val) => val,
                    Err(()) => {
                        error!("{}: Failed to generate code for assignment expression; the right hand side is not a basic value", rhs_node.1);
                        return None;
                    }
                };

                let lhs_ty = lhs_node.get_type(self);
                let rhs_ty = rhs_node.get_type(self);

                self.rhs_ty = None;

                let rhs = match (&lhs_ty, &rhs_ty) {
                    (
                        Some(Type::Integer { width, signed: _ }),
                        Some(Type::Integer {
                            width: rwidth,
                            signed: _,
                        }),
                    ) => {
                        if width != rwidth {
                            if rwidth > width {
                                warn!("{}: Right hand side of assignment expression is casted to type of lesser width (narrowing conversion)", rhs_node.1);
                            }
                            self.build
                                .build_int_cast(
                                    rhs.into_int_value(),
                                    self.llvm_type(&lhs_ty.unwrap(), &lhs_node.1)
                                        .into_int_type(),
                                    "rhs_assign_cast",
                                )
                                .as_basic_value_enum()
                        } else {
                            rhs
                        }
                    }
                    (Some(other), Some(rother)) if other != rother => {
                        error!("{}: Cannot assign value of type {} to variable of type {} (consider adding an explicit cast)", lhs_node.1, rother, other);
                        return None;
                    }
                    (_, _) => rhs,
                };

                Some(self.build.build_store(lhs, rhs).as_any_value_enum())
            }
            op => {
                let lhs_ty = lhs_node
                    .get_type(self)
                    .expect("failed to get lhs type of binary expression");
                let rhs_ty = rhs_node
                    .get_type(self)
                    .expect("failed to get rhs type of binary expression");

                let lhs = self.gen(lhs_node, false)?;
                let lhs = match lhs_ty {
                    Type::Ptr(_) => self.build.build_ptr_to_int(
                        lhs.into_pointer_value(),
                        self.ctx.custom_width_int_type(usize::BITS),
                        "ptr_to_int_lhs",
                    ),
                    Type::Integer {
                        signed: _,
                        width: _,
                    } => lhs.into_int_value(),
                    _ => {
                        error!("{}: Left hand side of {} expression is not an integer or pointer value", lhs_node.1, op);
                        return None;
                    }
                };

                let rhs = self.gen(rhs_node, false)?;
                let rhs = match rhs_ty {
                    Type::Ptr(_) => self.build.build_ptr_to_int(
                        rhs.into_pointer_value(),
                        self.ctx.custom_width_int_type(usize::BITS),
                        "ptr_to_int_rhs",
                    ),
                    Type::Integer {
                        signed: _,
                        width: _,
                    } => rhs.into_int_value(),
                    _ => {
                        error!("{}: Right hand side of {} expression is not an integer or pointer value", rhs_node.1, op);
                        return None;
                    }
                };

                //Cast rhs to be the same integer type
                let rhs = if rhs.get_type().get_bit_width() != lhs.get_type().get_bit_width() {
                    self.build
                        .build_int_cast(rhs, lhs.get_type(), "rhs_operand_int_cast")
                } else {
                    rhs
                };

                let (res, cmp) = match op {
                    Op::Plus => (self.build.build_int_add(lhs, rhs, "tmp_iadd"), false),
                    Op::Greater => (
                        self.build.build_int_compare(
                            IntPredicate::SGT,
                            lhs,
                            rhs,
                            "int_greater_than_cmp",
                        ),
                        true,
                    ),
                    Op::Less => (
                        self.build.build_int_compare(
                            IntPredicate::SLT,
                            lhs,
                            rhs,
                            "int_less_than_cmp",
                        ),
                        true,
                    ),
                    Op::Equal => (
                        self.build
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "int_eq_cmp"),
                        true,
                    ),
                    Op::GreaterEq => (
                        self.build.build_int_compare(
                            IntPredicate::SGE,
                            lhs,
                            rhs,
                            "int_greater_than_eq_cmp",
                        ),
                        true,
                    ),
                    Op::NEqual => (
                        self.build
                            .build_int_compare(IntPredicate::NE, lhs, rhs, "int_not_eq_cmp"),
                        true,
                    ),
                    Op::LessEq => (
                        self.build.build_int_compare(
                            IntPredicate::SLE,
                            lhs,
                            rhs,
                            "int_less_than_eq_cmp",
                        ),
                        true,
                    ),
                    Op::And => (self.build.build_and(lhs, rhs, "bit_and"), false),
                    Op::Or => (self.build.build_or(lhs, rhs, "bit_or"), false),
                    Op::Xor => (self.build.build_xor(lhs, rhs, "bit_xor"), false),
                    Op::Star => (self.build.build_int_mul(lhs, rhs, "int_mul"), false),
                    Op::Divide => (self.build.build_int_signed_div(lhs, rhs, "int_div"), false),
                    Op::Modulo => (
                        self.build.build_int_signed_rem(lhs, rhs, "int_modulo"),
                        false,
                    ),
                    Op::Minus => (self.build.build_int_sub(lhs, rhs, "int_sub"), false),
                    Op::AndAnd => {
                        let lhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            lhs,
                            self.ctx.bool_type().const_zero(),
                            "and_and_cond_check_lhs",
                        );
                        let rhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            rhs,
                            self.ctx.bool_type().const_zero(),
                            "and_and_cond_check_rhs",
                        );
                        (self.build.build_and(lhs, rhs, "cond_and_and_cmp"), true)
                    }
                    Op::OrOr => {
                        let lhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            lhs,
                            self.ctx.bool_type().const_zero(),
                            "or_or_cond_check_lhs",
                        );
                        let rhs = self.build.build_int_compare(
                            IntPredicate::SGT,
                            rhs,
                            self.ctx.bool_type().const_zero(),
                            "or_or_cond_check_rhs",
                        );
                        (self.build.build_or(lhs, rhs, "cond_or_or_cmp"), true)
                    }

                    Op::ShLeft => (
                        self.build.build_left_shift(lhs, rhs, "int_left_shift"),
                        false,
                    ),
                    Op::ShRight => (
                        self.build
                            .build_right_shift(lhs, rhs, false, "int_right_shift"),
                        false,
                    ),
                    other => {
                        error!("{}: Cannot use operator '{}'", lhs_node.1, other);
                        return None;
                    }
                };

                Some(match lhs_ty {
                    Type::Ptr(_) if !cmp => self
                        .build
                        .build_int_to_ptr(
                            res,
                            self.llvm_type(&lhs_ty, &lhs_node.1).into_pointer_type(),
                            "bin_res_to_ptr",
                        )
                        .as_any_value_enum(),
                    _ => res.as_any_value_enum(),
                })
            }
        }
    }

    /// Generate code for an if / while / switch statement body, clearing local variables declared in the body and
    /// not inserting a jump to the after_bb if a return statement was encountered
    fn gen_body(&mut self, body: &[AstPos], after_bb: BasicBlock<'c>) -> Option<()> {
        let old_vars = self.vars.clone();
        let old_break_lbl = self.break_lbl;
        self.break_lbl = Some(after_bb);

        for stmt in body {
            self.gen(stmt, false)?;
            if self.just_ret {
                break;
            }
        }
        if !self.just_ret {
            self.build.build_unconditional_branch(after_bb);
        } else {
            self.just_ret = false;
            trace!("Encountered an early return from a block, so not inserting a jump");
        }

        self.vars = old_vars;
        self.break_lbl = old_break_lbl;
        Some(())
    }

    /// Generate code for one expression, only used for generating function bodies, no delcarations.
    /// ## Note
    /// Returns [None] on error, and writes error message to stderr
    pub fn gen(&mut self, node: &AstPos, lval: bool) -> Option<AnyValueEnum<'c>> {
        match node.ast() {
            Ast::NumLiteral(num) => Some(
                self.ctx
                    .custom_width_int_type(num.width as u32)
                    .const_int_from_string(
                        num.val.to_str_radix(10).as_str(),
                        inkwell::types::StringRadix::Decimal,
                    )
                    .unwrap()
                    .as_any_value_enum(),
            ),
            Ast::Ret(node) => {
                match self
                    .current_proto
                    .as_ref()
                    .expect("Must be in a function to return from one!")
                    .ty
                    .ret
                {
                    Type::Void => {
                        self.just_ret = true;
                        Some(self.build.build_return(None).as_any_value_enum())
                    }
                    ref ty => {
                        let ty = ty.clone();
                        let ret = self.gen(node.deref().as_ref().unwrap(), false)?;
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
            Ast::Break => {
                if let Some(break_lbl) = self.break_lbl {
                    self.just_ret = true;
                    Some(
                        self.build
                            .build_unconditional_branch(break_lbl)
                            .as_any_value_enum(),
                    )
                } else {
                    error!(
                        "{}: Cannot use break statement here; there is nowhere to break to",
                        node.1
                    );
                    None
                }
            }
            Ast::FunCall(name, args) => match self.get_fun(&name) {
                Some((f, p)) => {
                    let f = match f {
                        Either::Left(fun) => fun.into(),
                        Either::Right(ptr) => match CallableValue::try_from(ptr) {
                            Ok(callable) => callable,
                            Err(_) => {
                                error!("{}: Cannot call pointer {}", node.1, name);
                                return None
                            }
                        }
                    };
                    //Do initial argument count length
                    if p.args.len() != args.len() {
                        error!("{}: Incorrect number of arguments passed to function {}; {} expected, {} passed", node.1, name, p.args.len(), args.len());
                        return None;
                    }

                    //Do type checking of function arguments
                    for (i, (arg, parg)) in args.iter().zip(p.args.iter()).enumerate() {
                        if let Some(ref ty) = arg.get_type(self) {
                            if ty != parg {
                                error!("{}: Incorrect type of argument {} in function call {}; {} expected, {} passed", arg.1, i + 1, name, parg, ty);
                                return None;
                            }
                        } else {
                            error!(
                                "{}: Failed to get type of argument {} in function call {}",
                                arg.1,
                                i + 1,
                                name
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
                None => {
                    error!("{}: Calling unknown function {}", node.1, name);
                    None
                }
            },
            Ast::If {
                cond: condition,
                true_block,
                else_block,
            } => {
                self.just_ret = false;
                let cond = self.gen(condition, false)?.into_int_value();
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

                let old_vars = self.vars.clone();

                for stmt in true_block {
                    self.gen(stmt, false)?;
                    if self.just_ret {
                        break;
                    }
                }
                if !self.just_ret {
                    self.build.build_unconditional_branch(after_bb);
                } else {
                    self.just_ret = false;
                    trace!("Encountered an early return from a block, so not inserting a jump");
                }

                self.vars = old_vars;

                self.build.position_at_end(false_bb);

                match else_block.is_some() {
                    true => {
                        let old_vars = self.vars.clone();
                        for stmt in else_block.as_ref().unwrap() {
                            self.gen(stmt, false)?;
                            if self.just_ret {
                                break;
                            }
                        }
                        if !self.just_ret {
                            self.build.build_unconditional_branch(after_bb);
                        } else {
                            self.just_ret = false;
                            trace!(
                                "Encountered an early return from a block, so not inserting a jump"
                            );
                        }
                        self.vars = old_vars;
                    }
                    false => {
                        self.build.build_unconditional_branch(after_bb);
                    }
                };

                self.build.position_at_end(after_bb);
                Some(cond.as_any_value_enum())
            }
            Ast::While { cond, block } => {
                self.just_ret = false;
                let fun = self.current_fn.expect("While loop outside of function");

                let cond_bb = self.ctx.append_basic_block(fun, "while_cond_bb");
                let while_bb = self.ctx.append_basic_block(fun, "while_loop_bb");
                let after_bb = self.ctx.append_basic_block(fun, "after_while_bb");

                self.build.build_unconditional_branch(cond_bb); //Jump to the condition block for the first check
                self.build.position_at_end(cond_bb);
                let cond = self.gen(cond, false)?.into_int_value();

                self.build
                    .build_conditional_branch(cond, while_bb, after_bb);
                self.build.position_at_end(while_bb);

                self.gen_body(block, cond_bb)?;

                self.build.position_at_end(after_bb); //Continue condegen after the loop block
                Some(cond.as_any_value_enum())
            }
            Ast::VarDecl { ty, name, attrs: _ } => {
                let ty = match ty {
                    Some(ty) => ty.clone(),
                    None => match self.rhs_ty {
                        Some(ref ty) => ty.clone(),
                        None => {
                            error!("{}: In variable declaration {}; Type not given and cannot be inferred from the context", node.1, name);
                            return None;
                        }
                    },
                };
                let ty = self.resolve_unknown(ty, &node.1);
                let var = self.entry_alloca(name.as_str(), self.llvm_type(&ty, &node.1));
                self.vars.insert(name.clone(), (var, ty));
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
                None => match self.get_const(name) {
                    Some((val, _)) => match lval {
                        false => Some(
                            self.build
                                .build_load(val.as_pointer_value(), "global_access_ssa_load")
                                .as_any_value_enum(),
                        ),
                        true => Some(val.as_any_value_enum()),
                    },
                    None => match self.get_fun(name) {
                        Some((f, _)) => Some(match f {
                            Either::Left(fun) => fun.as_any_value_enum(),
                            Either::Right(ptr) => ptr.as_any_value_enum()
                        }),
                        None => {
                            error!("{}: No variable, global, or function named {} found in the current namespace", node.1, name);
                            None
                        }
                    },
                },
            },
            Ast::StructLiteral { name, fields } => {
                let def = match self.get_struct(name) {
                    Some(def) => def,
                    None => {
                        error!(
                            "{}: Using unknown struct type {} when defining struct literal",
                            node.1, name
                        );
                        return None;
                    }
                };
                let ty = *self.struct_types.borrow().get(def.typeid).unwrap();

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
                        trace!("{}: For member access {}; Index is {}, field count is {}", node.1, field, field_idx, s_ty.fields.unwrap_or(vec![]).len());

                        let field = match self
                            .build
                            .build_struct_gep(
                                s.into_pointer_value(),
                                field_idx as u32,
                                "struct_member_access_gep",
                            ) {
                                Ok(field) => field,
                                Err(()) => {
                                    log::error!("{}: Internal error: failed to issue LLVM GEP instruction because the pointer is either not a struct or the index is OOB", node.1);
                                    return None
                                }
                            };

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
                    (AnyTypeEnum::ArrayType(_), BasicTypeEnum::PointerType(_)) => unsafe {
                        self.build
                            .build_in_bounds_gep(
                                lhs.into_pointer_value(),
                                &[self.ctx.i64_type().const_zero()],
                                "array_to_ptr_cast_gep",
                            )
                            .as_any_value_enum()
                    },
                    (one, two) => panic!("Cannot cast type {:?} to {:?}", one, two),
                })
            }
            Ast::Unary(op, val) => match op {
                Op::And => self.gen(val, true),
                Op::Star => {
                    let ty = val.get_type(self)?;
                    if !matches!(ty, crate::types::Type::Ptr(_)) {
                        error!("{}: Cannot dereference non-pointer type {}", node.1, ty);
                        return None;
                    }
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
            Ast::Array(expr, idx) => {
                let ty = expr.get_type(self)?;
                if let Type::Integer {
                    signed: _,
                    width: _,
                } = idx.get_type(self)?
                {
                } else {
                    error!(
                        "{}: Cannot index array with expression of type {}",
                        node.1,
                        idx.get_type(self)?
                    );
                    return None;
                }

                let ptr = if let Type::Array(ty, _) = ty {
                    let ptr = self.gen(expr, true)?.into_pointer_value();
                    self.build
                        .build_bitcast(
                            ptr,
                            self.llvm_type(ty.deref(), &node.1)
                                .ptr_type(AddressSpace::Generic),
                            "array_index_bitcast_to_ptr",
                        )
                        .into_pointer_value()
                } else if let Type::Ptr(_) = ty {
                    self.gen(expr, true)?.into_pointer_value()
                } else {
                    error!("{}: Array index operation on prefix type that is not an array or pointer type", node.1);
                    return None;
                };

                let idx = self.gen(idx, false)?.into_int_value();
                let ptr = unsafe { self.build.build_gep(ptr, &[idx], "array_index_gep") };
                let res = Some(match lval {
                    true => ptr.as_any_value_enum(),
                    false => self
                        .build
                        .build_load(ptr, "array_index_load_idx")
                        .as_any_value_enum(),
                });
                trace!("{}: Generated code for array index", node.1);
                res
            }
            Ast::Bin(lhs, op, rhs) => self.gen_bin(lhs, rhs, op),
            Ast::Switch(cond, cases, default) => {
                let cond = BasicValueEnum::try_from(self.gen(cond, false)?).ok()?;
                let current_bb = self.build.get_insert_block()?;
                let default_bb = self
                    .ctx
                    .append_basic_block(self.current_fn?, "switch_default_bb");
                let after_bb = self
                    .ctx
                    .append_basic_block(self.current_fn?, "after_switch_bb");

                let mut llvm_cases = vec![];
                for (case_vals, body) in cases {
                    self.just_ret = false;
                    let case_bb = self
                        .ctx
                        .append_basic_block(self.current_fn?, "switch_case_bb");
                    self.build.position_at_end(case_bb);
                    self.gen_body(body, after_bb)?;

                    for case_val in case_vals {
                        let case_val = self
                            .ctx
                            .custom_width_int_type(case_val.width as u32)
                            .const_int_from_string(
                                case_val.val.to_str_radix(10).as_str(),
                                inkwell::types::StringRadix::Decimal,
                            )
                            .unwrap();
                        llvm_cases.push((case_val, case_bb));
                    }
                }

                self.build.position_at_end(default_bb);
                match default {
                    Some(body) => {
                        self.gen_body(body, after_bb)?;
                    }
                    None => {
                        self.build.build_unconditional_branch(after_bb);
                    }
                }

                self.build.position_at_end(current_bb);
                let switch = self.build.build_switch(
                    cond.into_int_value(),
                    default_bb,
                    llvm_cases.as_slice(),
                );

                self.build.position_at_end(after_bb);

                Some(switch.as_any_value_enum())
            }

            other => unimplemented!("Cannot use expression {:?} inside of a function", other),
        }
    }
}
