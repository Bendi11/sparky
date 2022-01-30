use codespan_reporting::diagnostic::{Diagnostic, Label};
use inkwell::{types::IntType, IntPredicate, FloatPredicate};
use num_bigint::Sign;

use crate::{ast::{Ast, AstNode, ElseExpr, IfExpr, NumberLiteral, NumberLiteralAnnotation}, parse::token::Op, util::files::FileId};

use super::*;

impl<'ctx, 'files> LlvmCodeGenerator<'ctx, 'files> {
    /// Generate code for a single AST statement
    pub fn gen_stmt(
        &mut self,
        file: FileId,
        module: ModId,
        ast: &Ast<TypeId>,
    ) -> Result<(), Diagnostic<FileId>> {
        match &ast.node {
            AstNode::IfExpr(if_expr) => { self.gen_if_expr(file, module, if_expr)?; },
            AstNode::Assignment { lhs, rhs } => {
                let rhs_ty = self.ast_type(file, module, rhs)?;

                let lhs_ty = if let AstNode::VarDeclaration { ty: None, .. } = &lhs.node {
                    rhs_ty
                } else {
                    self.ast_type(file, module, lhs)?
                };
                if lhs_ty != rhs_ty {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Value of type {} cannot be assigned to type of {}",
                            self.spark.get_type_name(rhs_ty),
                            self.spark.get_type_name(lhs_ty),
                        ))
                        .with_labels(vec![
                            Label::primary(file, ast.span)
                                .with_message("Assignee encountered here"),
                            Label::secondary(file, ast.span)
                                .with_message("Assigned value encountered here"),
                        ]));
                }

                let lhs = if let AstNode::VarDeclaration {
                    name,
                    ty: _,
                    mutable: _,
                } = &lhs.node
                {
                    let llvm_ty = self.llvm_ty(lhs_ty);
                    if let Ok(llvm_ty) = BasicTypeEnum::try_from(llvm_ty) {
                        let pv = self.builder.build_alloca(llvm_ty, "var_dec_aloca");
                        self.current_scope
                            .define(*name, ScopeDef::Value(lhs_ty, pv));
                        pv
                    } else {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Cannot declare a variable of type {}",
                                self.spark.get_type_name(lhs_ty)
                            ))
                            .with_labels(vec![Label::primary(file, lhs.span)]));
                    }
                } else {
                    self.gen_lval(file, module, lhs)?
                };

                let rhs = self.gen_expr(file, module, rhs)?;

                self.builder.build_store(lhs, rhs);
            }
            AstNode::VarDeclaration { name, ty, mutable } => {
                if let Some(ty) = ty {
                    let llvm_ty = self.llvm_ty(*ty);
                    if let Ok(llvm_ty) = BasicTypeEnum::try_from(llvm_ty) {
                        let pv = self.builder.build_alloca(llvm_ty, name.as_str());
                        self.current_scope
                            .define(*name, ScopeDef::Value(*ty, pv.into()));
                    } else {
                        return Err(Diagnostic::error()
                            .with_message("Cannot declare variable of unit type")
                            .with_labels(vec![Label::primary(file, ast.span)]));
                    }
                } else {
                    return Err(Diagnostic::error()
                        .with_message("Must provide type of variable or assign a value")
                        .with_labels(vec![Label::primary(file, ast.span)
                            .with_message("In this variable declaration")])
                        .with_notes(vec![format!(
                            "Provide an explicit type in parenthesis after the '{}' keyword",
                            if *mutable { "mut" } else { "let " }
                        )]));
                }
            }
            AstNode::Return(returned) => {
                let returned_ty = self.ast_type(file, module, returned).map_err(|e| {
                    e.with_labels(vec![
                        Label::secondary(file, ast.span).with_message("In this return statement")
                    ])
                })?;

                let current_fun = &self.spark[self.current_fun.unwrap().1];

                if returned_ty != current_fun.ty.return_ty {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                                "Returned value of type '{}' is not compatible with declared return type of '{}'",
                                self.spark.get_type_name(returned_ty),
                                self.spark.get_type_name(current_fun.ty.return_ty),
                            )
                        )
                    );
                }

                if current_fun.ty.return_ty != SparkCtx::UNIT {
                    let returned = self.gen_expr(file, module, returned)?;
                    self.builder.build_return(Some(&returned));
                } else {
                    self.builder.build_return(None);
                }
            }
            AstNode::PhiExpr(phi) => {
                if let Some(break_data) = self.break_data {
                    if let Some(phi_data) = break_data.phi_data {
                        let phid_ty = self.ast_type(file, module, phi)?;

                        if phid_ty != phi_data.phi_ty {
                            return Err(Diagnostic::error()
                                .with_message("Phi statement returns a value with type different to expected type")
                                .with_labels(vec![
                                    Label::primary(file, phi.span)
                                        .with_message(format!("Phi statement of type '{}' encountered here", self.spark.get_type_name(phid_ty)))
                                ])
                            );
                        }

                        let phi_val = self.gen_expr(file, module, phi)?;
                        self.builder.build_store(phi_data.alloca, phi_val);
                        self.builder
                            .build_unconditional_branch(break_data.return_to_bb);
                    } else {
                        return Err(Diagnostic::error()
                            .with_message("Phi statement encountered but not used")
                            .with_labels(vec![Label::primary(file, ast.span)
                                .with_message("Phi statement encountered here")])
                            .with_notes(vec![
                                "Replace with a break or continue statement if value is not used"
                                    .to_owned(),
                            ]));
                    }
                } else {
                    return Err(Diagnostic::error()
                        .with_message("Phi statement not in a block")
                        .with_labels(vec![Label::primary(file, ast.span)]));
                }
            }
            other => {
                return Err(Diagnostic::error()
                    .with_message(format!("Invalid statement: {:#?}", other))
                    .with_labels(vec![Label::primary(file, ast.span)]))
            }
        }

        Ok(())
    }

    /// Generate code for a single AST expression
    fn gen_expr(
        &mut self,
        file: FileId,
        module: ModId,
        ast: &Ast<TypeId>,
    ) -> Result<BasicValueEnum<'ctx>, Diagnostic<FileId>> {
        Ok(match &ast.node {
            AstNode::IfExpr(..) | AstNode::Block(..) => {
                let phi = self.gen_lval(file, module, ast)?;
                self.builder.build_load(phi, "load_phi")
            } 
            AstNode::CastExpr(to, rhs) => self.gen_cast(file, module, *to, rhs)?,
            AstNode::Access(path) => {
                let access = self.gen_access(file, ast.span, path)?;
                if access.get_type().get_element_type().is_function_type() {
                    access.into()
                } else {
                    self.builder.build_load(access, "var_rval_load").into()
                }
            }
            AstNode::UnaryExpr(op, rhs) => {
                let rhs_ty = self.ast_type(file, module, rhs)?;
                match op {
                    Op::AND => {
                        let lval = self.gen_lval(file, module, rhs)?;
                        lval.into()
                    }
                    Op::Star => {
                        if let TypeData::Pointer(_) = &self.spark[rhs_ty].data {
                            let pv = self.gen_expr(file, module, rhs)?.into_pointer_value();
                            let deref = self.builder.build_load(pv, "deref_load");
                            deref
                        } else {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "Expression of type {} cannot be dereferenced",
                                    self.spark.get_type_name(rhs_ty),
                                ))
                                .with_labels(vec![Label::primary(file, ast.span)]));
                        }
                    }
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!("Invalid unary operand {}", op))
                            .with_labels(vec![Label::primary(file, ast.span)]))
                    }
                }
            },
            AstNode::BinExpr(lhs, op, rhs) => return self.gen_bin_expr(file, module, lhs, *op, rhs),
            AstNode::BooleanLiteral(b) => match b {
                true => self.ctx.bool_type().const_all_ones(),
                false => self.ctx.bool_type().const_zero(),
            }.into(),
            AstNode::StringLiteral(s) => {
                let glob = self.builder.build_global_string_ptr(s.as_str(), "const_str");
                glob.as_pointer_value().into()
            },
            AstNode::NumberLiteral(n) => {
                match n {
                    NumberLiteral::Integer(num, annot) => {
                        match annot {
                            Some(annot) => {
                                let sign = num.to_u64_digits().0 == Sign::Plus;

                                let n =
                                    match annot {
                                        NumberLiteralAnnotation::U8
                                        | NumberLiteralAnnotation::I8 => self.ctx.i8_type(),
                                        NumberLiteralAnnotation::U16
                                        | NumberLiteralAnnotation::I16 => self.ctx.i16_type(),
                                        NumberLiteralAnnotation::U32
                                        | NumberLiteralAnnotation::I32 => self.ctx.i32_type(),
                                        NumberLiteralAnnotation::U64
                                        | NumberLiteralAnnotation::I64 => self.ctx.i64_type(),
                                        NumberLiteralAnnotation::F32
                                        | NumberLiteralAnnotation::F64 => self.ctx.i64_type(),
                                    }
                                    .const_int(num.to_u64_digits().1[0], sign);

                                match annot {
                                    NumberLiteralAnnotation::F32 => {
                                        if sign {
                                            self.builder
                                                .build_signed_int_to_float(
                                                    n,
                                                    self.ctx.f32_type(),
                                                    "numliteral_cast",
                                                )
                                                .into()
                                        } else {
                                            self.builder
                                                .build_unsigned_int_to_float(
                                                    n,
                                                    self.ctx.f32_type(),
                                                    "numliteral_cast",
                                                )
                                                .into()
                                        }
                                    }
                                    NumberLiteralAnnotation::F64 => {
                                        if sign {
                                            self.builder
                                                .build_signed_int_to_float(
                                                    n,
                                                    self.ctx.f64_type(),
                                                    "numliteral_cast",
                                                )
                                                .into()
                                        } else {
                                            self.builder
                                                .build_unsigned_int_to_float(
                                                    n,
                                                    self.ctx.f64_type(),
                                                    "numliteral_cast",
                                                )
                                                .into()
                                        }
                                    }
                                    _ => n.into(),
                                }
                            }
                            None => self
                                .ctx
                                .i32_type()
                                .const_int(
                                    num.to_u64_digits().1[0],
                                    num.to_u64_digits().0 == Sign::Plus,
                                )
                                .into(),
                        }
                    }
                    NumberLiteral::Float(f, annot) => match annot {
                        Some(annot) => {
                            if let NumberLiteralAnnotation::F32 = annot {
                                self.ctx.f32_type().const_float(*f).into()
                            } else {
                                let f = self.ctx.f64_type().const_float(*f);

                                match annot {
                                    NumberLiteralAnnotation::U8 => self
                                        .builder
                                        .build_float_to_unsigned_int(
                                            f,
                                            self.ctx.i8_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::U16 => self
                                        .builder
                                        .build_float_to_unsigned_int(
                                            f,
                                            self.ctx.i16_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::U32 => self
                                        .builder
                                        .build_float_to_unsigned_int(
                                            f,
                                            self.ctx.i32_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::U64 => self
                                        .builder
                                        .build_float_to_unsigned_int(
                                            f,
                                            self.ctx.i64_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::I8 => self
                                        .builder
                                        .build_float_to_signed_int(
                                            f,
                                            self.ctx.i8_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::I16 => self
                                        .builder
                                        .build_float_to_signed_int(
                                            f,
                                            self.ctx.i16_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::I32 => self
                                        .builder
                                        .build_float_to_signed_int(
                                            f,
                                            self.ctx.i32_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::I64 => self
                                        .builder
                                        .build_float_to_signed_int(
                                            f,
                                            self.ctx.i64_type(),
                                            "numberliteral_cast",
                                        )
                                        .into(),
                                    NumberLiteralAnnotation::F64 => f.into(),
                                    NumberLiteralAnnotation::F32 => unreachable!(),
                                }
                            }
                        }
                        None => self.ctx.f64_type().const_float(*f).into(),
                    },
                }
            }
            _ => {
                return Err(Diagnostic::error()
                    .with_message("Expression not yet implemented")
                    .with_labels(vec![Label::primary(file, ast.span)]))
            }
        })
    }

    /// Generate code for a single binary expression
    fn gen_bin_expr(
        &mut self,
        file: FileId,
        module: ModId,
        lhs: &Ast<TypeId>,
        op: Op,
        rhs: &Ast<TypeId>,
    ) -> Result<BasicValueEnum<'ctx>, Diagnostic<FileId>> {
        let lhs_ty = self.ast_type(file, module, lhs)?;
        let rhs_ty = self.ast_type(file, module, rhs)?;

        let llvm_lhs = self.gen_expr(file, module, lhs)?;
        let llvm_rhs = self.gen_expr(file, module, rhs)?;

        if lhs_ty == rhs_ty {
            match (op, self.spark[lhs_ty].data.clone()) {
                (Op::Star, TypeData::Integer { .. }) => {
                    return Ok(self
                        .builder
                        .build_int_mul(llvm_lhs.into_int_value(), llvm_rhs.into_int_value(), "imul")
                        .into())
                }
                (Op::Div, TypeData::Integer { signed: true, .. }) => {
                    return Ok(self
                        .builder
                        .build_int_signed_div(
                            llvm_lhs.into_int_value(),
                            llvm_rhs.into_int_value(),
                            "sidiv",
                        )
                        .into())
                }
                (Op::Div, TypeData::Integer { signed: false, .. }) => {
                    return Ok(self
                        .builder
                        .build_int_unsigned_div(
                            llvm_lhs.into_int_value(),
                            llvm_rhs.into_int_value(),
                            "uidiv",
                        )
                        .into())
                }
                (Op::Add, TypeData::Integer { .. }) => {
                    return Ok(self
                        .builder
                        .build_int_add(llvm_lhs.into_int_value(), llvm_rhs.into_int_value(), "iadd")
                        .into())
                }
                (Op::Sub, TypeData::Integer { .. }) => {
                    return Ok(self
                        .builder
                        .build_int_sub(llvm_lhs.into_int_value(), llvm_rhs.into_int_value(), "isub")
                        .into())
                }
                (Op::Mod, TypeData::Integer { signed: true, .. }) => {
                    return Ok(self
                        .builder
                        .build_int_signed_rem(
                            llvm_lhs.into_int_value(),
                            llvm_rhs.into_int_value(),
                            "simod",
                        )
                        .into())
                }
                (Op::Mod, TypeData::Integer { signed: false, .. }) => {
                    return Ok(self
                        .builder
                        .build_int_unsigned_rem(
                            llvm_lhs.into_int_value(),
                            llvm_rhs.into_int_value(),
                            "uimod",
                        )
                        .into())
                }

                (
                    Op::Eq | Op::Greater | Op::GreaterEq | Op::Less | Op::LessEq,
                    TypeData::Integer { signed, .. },
                ) => {
                    return Ok(self
                        .builder
                        .build_int_compare(
                            match (op, signed) {
                                (Op::Eq, _) => IntPredicate::EQ,
                                (Op::Greater, true) => IntPredicate::SGT,
                                (Op::Greater, false) => IntPredicate::UGT,
                                (Op::GreaterEq, true) => IntPredicate::SGE,
                                (Op::GreaterEq, false) => IntPredicate::UGE,
                                (Op::Less, true) => IntPredicate::SLT,
                                (Op::Less, false) => IntPredicate::ULT,
                                (Op::LessEq, true) => IntPredicate::SLE,
                                (Op::LessEq, false) => IntPredicate::ULE,
                                _ => unreachable!(),
                            },
                            llvm_lhs.into_int_value(),
                            llvm_rhs.into_int_value(),
                            "icmp",
                        )
                        .into())
                },

                (
                    Op::Eq | Op::Greater | Op::GreaterEq | Op::Less | Op::LessEq,
                    TypeData::Float {..}
                ) => {
                    return Ok(self
                        .builder
                        .build_float_compare(match op {
                            Op::Eq => FloatPredicate::OEQ,
                            Op::Greater => FloatPredicate::OGT,
                            Op::GreaterEq => FloatPredicate::OGE,
                            Op::Less => FloatPredicate::OLT,
                            Op::LessEq => FloatPredicate::OLE,
                            _ => unreachable!(),
                        },
                        llvm_lhs.into_float_value(),
                        llvm_rhs.into_float_value(),
                        "fcmp",
                    )
                    .into())
                }

                (Op::Star, TypeData::Float { .. }) => {
                    return Ok(self
                        .builder
                        .build_float_mul(
                            llvm_lhs.into_float_value(),
                            llvm_rhs.into_float_value(),
                            "fmul",
                        )
                        .into())
                }
                (Op::Div, TypeData::Float { .. }) => {
                    return Ok(self
                        .builder
                        .build_float_div(
                            llvm_lhs.into_float_value(),
                            llvm_rhs.into_float_value(),
                            "fdiv",
                        )
                        .into())
                }
                (Op::Add, TypeData::Float { .. }) => {
                    return Ok(self
                        .builder
                        .build_float_add(
                            llvm_lhs.into_float_value(),
                            llvm_rhs.into_float_value(),
                            "fadd",
                        )
                        .into())
                }
                (Op::Sub, TypeData::Float { .. }) => {
                    return Ok(self
                        .builder
                        .build_float_sub(
                            llvm_lhs.into_float_value(),
                            llvm_rhs.into_float_value(),
                            "fsub",
                        )
                        .into())
                }
                (Op::Mod, TypeData::Float { .. }) => {
                    return Ok(self
                        .builder
                        .build_float_rem(
                            llvm_lhs.into_float_value(),
                            llvm_rhs.into_float_value(),
                            "fmod",
                        )
                        .into())
                }
                _ => (),
            }
        }

        Ok(
            match (
                self.spark[lhs_ty].data.clone(),
                op,
                self.spark[rhs_ty].data.clone(),
            ) {
                (TypeData::Integer { .. }, Op::ShLeft, TypeData::Integer { .. }) => self
                    .builder
                    .build_left_shift(llvm_lhs.into_int_value(), llvm_rhs.into_int_value(), "ishl")
                    .into(),
                (TypeData::Integer { signed, .. }, Op::ShRight, TypeData::Integer { .. }) => self
                    .builder
                    .build_right_shift(
                        llvm_lhs.into_int_value(),
                        llvm_rhs.into_int_value(),
                        signed,
                        "ishr",
                    )
                    .into(),
                _ => {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Binary operator {} cannot be applied to the given types",
                            op
                        ))
                        .with_labels(vec![
                            Label::primary(file, lhs.span).with_message(format!(
                                "Left hand side is found to be of type {}",
                                self.spark.get_type_name(lhs_ty)
                            )),
                            Label::primary(file, rhs.span).with_message(format!(
                                "Right hand side is found to be of type {}",
                                self.spark.get_type_name(rhs_ty)
                            )),
                        ]))
                }
            },
        )
    }

    /// Generate an lvalue expression, returning a [PointerValue] to the lval
    fn gen_lval(
        &mut self,
        file: FileId,
        module: ModId,
        ast: &Ast<TypeId>,
    ) -> Result<PointerValue<'ctx>, Diagnostic<FileId>> {
        Ok(match &ast.node {
            AstNode::Access(path) => return self.gen_access(file, ast.span, path),
            AstNode::Block(block) => {
                let body_bb = self.ctx.append_basic_block(self.current_fun.unwrap().0, "block");
                let after_bb = self.ctx.append_basic_block(self.current_fun.unwrap().0, "after");

                if let Some(pv) = self.gen_body(file, module, block, body_bb, after_bb)? {
                    pv
                } else {
                    return Err(Diagnostic::error()
                        .with_message("Cannot use block without phi statement as expression")
                        .with_labels(vec![
                            Label::primary(file, ast.span)
                        ])
                    )
                }
            },
            AstNode::IfExpr(if_expr) => {
                if let Some(pv) = self.gen_if_expr(file, module, if_expr)? {
                    pv
                } else {
                    return Err(Diagnostic::error()
                        .with_message("Cannot use if block with no phi nodes as expression")
                        .with_labels(vec![
                            Label::primary(file, ast.span)
                        ])
                    )
                }
            }
            _ => {
                let expr = self.gen_expr(file, module, ast)?;
                let alloca = self.builder.build_alloca(expr.get_type(), "lvalue_alloca");
                alloca
            }
        })
    }

    /// Generate LLVM IR for a single symbol access
    fn gen_access(
        &mut self,
        file: FileId,
        span: Span,
        path: &SymbolPath,
    ) -> Result<PointerValue<'ctx>, Diagnostic<FileId>> {
        let def = self.find_in_scope(file, span, path)?;
        Ok(match def {
            ScopeDef::Def(SparkDef::FunDef(_, fun)) => {
                let llvm_fun = self.llvm_funs[&fun];
                llvm_fun.as_global_value().as_pointer_value()
            }
            ScopeDef::Value(_, ptr) => ptr,
            _ => {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "Cannot use {} as an expression value",
                        match def {
                            ScopeDef::Def(SparkDef::ModDef(submod)) =>
                                format!("module '{}'", self.spark[submod].name),
                            ScopeDef::Def(SparkDef::TypeDef(_, ty)) =>
                                format!("type '{}'", self.spark.get_type_name(ty)),
                            ScopeDef::Value(..) => unreachable!(),
                            ScopeDef::Def(SparkDef::FunDef(..)) => unreachable!(),
                        }
                    ))
                    .with_labels(vec![Label::primary(file, span)]))
            }
        })
    }

    fn gen_cast(
        &mut self,
        file: FileId,
        module: ModId,
        to_ty: TypeId,
        rhs: &Ast<TypeId>,
    ) -> Result<BasicValueEnum<'ctx>, Diagnostic<FileId>> {
        let rhs_ty = self
            .ast_type(file, module, rhs)
            .map_err(|d| d.with_notes(vec!["In cast expression".to_owned()]))?;

        let to = self.spark[to_ty].clone().data;
        let from = self.spark[rhs_ty].clone().data;

                
        //Generate an enum literal from a cast to an enum that contains the casted
        //type as a variant
        if let TypeData::Enum {parts} = &self.spark[self.spark.unwrap_alias(to_ty)].data {
            let idx = parts.iter().enumerate().find_map(|(idx, ty)| {
                if *ty == rhs_ty {
                    Some(idx)
                } else {
                    None
                }
            });

            if let Some(idx) = idx {
                let rhs_size = self.size_of_type(rhs_ty);
                let llvm_rhs = self.gen_lval(file, module, rhs)?;
                
                //Round a number up to the nearest multiple of two
                let next_pow = |num: u32| {
                    let mut pow = 1;
                    while pow < num {
                        pow *= 2;
                    }
                    pow
                };

                let enum_ty = BasicTypeEnum::try_from(self.llvm_ty(to_ty)).unwrap();

                let enum_literal = self.builder.build_alloca(
                    enum_ty, 
                    "enum_literal_alloca"
                );

                let discrim = self.builder.build_struct_gep(enum_literal, 0, "enum_literal_get_discrim").unwrap();
                self.builder.build_store(discrim, self.ctx.i8_type().const_int(idx as u64, false));

                let array_size = self.size_of_type(to_ty) - 1;
                let rhs_ptr_bc = self.builder.build_bitcast(llvm_rhs, self.ctx.i8_type().ptr_type(AddressSpace::Generic), "enum_variant_store_bc").into_pointer_value();

                let variant = self.builder.build_struct_gep(enum_literal, 1, "enum_literal_get_variant").unwrap();

                let variant_ptr = self.builder.build_bitcast(
                    variant, 
                    self.ctx.i8_type().ptr_type(AddressSpace::Generic), 
                    "enum_variant_bc"
                ).into_pointer_value();
                
                let array_align = next_pow(array_size);
                let src_align = next_pow(rhs_size);

                self.builder.build_memcpy(
                    variant_ptr,
                    array_align,
                    rhs_ptr_bc,
                    src_align,
                    self.ctx.ptr_sized_int_type(&self.target, None)
                        .const_int(rhs_size as u64,  false)
                ).unwrap();
                
                let enum_literal_load = self.builder.build_load(enum_literal, "enum_lit_load");
                return Ok(enum_literal_load.into())
                
            } else {
                return Err(Diagnostic::error()
                    .with_message(
                        "Attempting to cast to an enum type that does not contain castee type"
                    )
                    .with_labels(vec![
                        Label::primary(file, rhs.span)
                            .with_message(format!(
                                "Attempted to cast type {} to enum type {}",
                                self.spark.get_type_name(rhs_ty),
                                self.spark.get_type_name(to_ty)
                            ))
                    ])
                )
            }
        }

        let llvm_rhs = self.gen_expr(file, module, rhs)?;


        Ok(match (from, to) {
            (
                TypeData::Integer {
                    width: from_width,
                    signed: _,
                },
                TypeData::Integer {
                    signed: to_sign,
                    width: to_width,
                },
            ) => {
                let llvm_to = self.llvm_int_ty(to_width);

                if let BasicValueEnum::IntValue(iv) = llvm_rhs {
                    if from_width == to_width {
                        iv.into()
                    } else if from_width < to_width && !to_sign {
                        self.builder
                            .build_int_z_extend(iv, llvm_to, "zext_upcast")
                            .into()
                    } else if from_width < to_width && to_sign {
                        self.builder
                            .build_int_s_extend(iv, llvm_to, "sext_upcast")
                            .into()
                    } else {
                        self.builder
                            .build_int_truncate(iv, llvm_to, "itrunc_downcast")
                            .into()
                    }
                } else {
                    println!("{:?}", llvm_rhs.get_type());
                    unreachable!()
                }
            }
            (TypeData::Integer { .. }, TypeData::Pointer(_)) => {
                let llvm_to = self.llvm_ty(to_ty).into_pointer_type();
                if let BasicValueEnum::IntValue(iv) = llvm_rhs {
                    self.builder
                        .build_int_to_ptr(iv, llvm_to, "int_to_ptr")
                        .into()
                } else {
                    unreachable!()
                }
            }
            (TypeData::Integer { signed, .. }, TypeData::Float { .. }) => {
                let llvm_to = self.llvm_ty(to_ty).into_float_type();
                if let BasicValueEnum::IntValue(iv) = llvm_rhs {
                    if signed {
                        self.builder
                            .build_signed_int_to_float(iv, llvm_to, "s_to_f")
                            .into()
                    } else {
                        self.builder
                            .build_unsigned_int_to_float(iv, llvm_to, "u_to_f")
                            .into()
                    }
                } else {
                    unreachable!()
                }
            }
            (TypeData::Float { .. }, TypeData::Integer { signed, width }) => {
                let llvm_to = self.llvm_int_ty(width);
                if let BasicValueEnum::FloatValue(fv) = llvm_rhs {
                    if signed {
                        self.builder
                            .build_float_to_signed_int(fv, llvm_to, "f_to_s")
                            .into()
                    } else {
                        self.builder
                            .build_float_to_unsigned_int(fv, llvm_to, "f_to_u")
                            .into()
                    }
                } else {
                    unreachable!()
                }
            }
            (TypeData::Pointer(..), TypeData::Pointer(..)) => {
                let llvm_to = self.llvm_ty(to_ty).into_pointer_type();
                if let BasicValueEnum::PointerValue(pv) = llvm_rhs {
                    self.builder
                        .build_pointer_cast(pv, llvm_to, "ptr_to_ptr")
                        .into()
                } else {
                    unreachable!()
                }
            }
            (TypeData::Pointer(..), TypeData::Integer { signed, width }) => {
                let llvm_to = self.llvm_int_ty(width);
                if let BasicValueEnum::PointerValue(pv) = llvm_rhs {
                    let int = self.builder.build_ptr_to_int(pv, llvm_to, "ptr_to_u");
                    if signed {
                        self.builder
                            .build_int_s_extend_or_bit_cast(int, llvm_to, "ptr_to_u_to_i")
                            .into()
                    } else {
                        int.into()
                    }
                } else {
                    unreachable!()
                }
            }
            _ => {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "Cannot cast value of type {} to {}",
                        self.spark.get_type_name(rhs_ty),
                        self.spark.get_type_name(to_ty)
                    ))
                    .with_labels(vec![Label::primary(file, rhs.span)]))
            }
        })
    }
        
    /// Generate code for a single if expression or statement
    fn gen_if_expr(
        &mut self, 
        file: FileId, 
        module: ModId,
        if_expr: &IfExpr<TypeId>
    ) -> Result<Option<PointerValue<'ctx>>, Diagnostic<FileId>> {
        let start_bb = self.builder.get_insert_block().unwrap();

        let cond_ty = self.ast_type(file, module, &if_expr.cond)?;
        if let TypeData::Bool = &self.spark[cond_ty].data {
            let cond = self.gen_expr(file, module, &if_expr.cond)?.into_int_value();
            let if_body_block = self.ctx.append_basic_block(self.current_fun.unwrap().0, "if_body");
            
            match &if_expr.else_expr {
                Some(else_expr) => {
                    let else_bb = self.ctx.append_basic_block(self.current_fun.unwrap().0, "else_bb");
                    let after_bb = self.ctx.append_basic_block(self.current_fun.unwrap().0, "after_bb");

                    let if_phi = self.gen_body(
                        file, 
                        module, 
                        &if_expr.body, 
                        if_body_block, 
                        after_bb
                    )?;
                    
                    match else_expr {
                        ElseExpr::ElseIf(elif_expr) => {
                            self.builder.position_at_end(else_bb);
                            let else_phi = self.gen_if_expr(file, module, elif_expr)?;
                            if let (Some(if_pv), Some(else_pv)) = (if_phi, else_phi) {
                                let else_phi = self.builder.build_load(else_pv, "elif_phi");
                                self.builder.build_store(if_pv, else_phi);
                            }
                        },
                        ElseExpr::Else(else_body) => {
                            if let (Some(if_pv), Some(pv)) = (if_phi, self.gen_body(
                                file, 
                                module, 
                                else_body,
                                else_bb, 
                                after_bb
                            )?) {
                                let else_phi = self.builder.build_load(pv, "else_expr_phi");
                                self.builder.build_store(if_pv, else_phi);
                            }
                        }
                    }

                    self.builder.position_at_end(start_bb);
                    self.builder.build_conditional_branch(cond, if_body_block, else_bb);
                    Ok(if_phi)
                },
                None => {
                    let after_bb = self.ctx.append_basic_block(self.current_fun.unwrap().0, "after_if");
                    let if_phi = self.gen_body(
                        file, 
                        module, 
                        &if_expr.body, 
                        if_body_block, 
                        after_bb
                    )?;
                    self.builder.position_at_end(start_bb);
                    self.builder.build_conditional_branch(cond, if_body_block, after_bb);
                    self.builder.position_at_end(after_bb);
                    Ok(if_phi)
                }
            }

        } else {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "Using value of type {} as boolean condition for if expression",
                    self.spark.get_type_name(cond_ty)
                ))
                .with_labels(vec![
                    Label::primary(file, if_expr.cond.span)
                        .with_message("Non-boolean value here")
                ])
            )
        }
    }
    
    /// Generate LLVM IR for a block of statements 
    fn gen_body(
        &mut self,
        file: FileId, 
        module: ModId,
        body: &[Ast<TypeId>],
        to_bb: BasicBlock<'ctx>,
        after_bb: BasicBlock<'ctx>,
    ) -> Result<Option<PointerValue<'ctx>>, Diagnostic<FileId>> {
        let from_bb = self.builder.get_insert_block().unwrap();

        let phi_data = match Self::phi_node(file, body) {
            Err(_) => None,
            Ok(phi_node) => {
                let ty = self.ast_type(file, module, phi_node).unwrap();
                let llvm_ty = BasicTypeEnum::try_from(self.llvm_ty(ty)).unwrap();
                let phi_alloca = self.builder.build_alloca(llvm_ty, "phi_alloca");

                Some(PhiData {
                    phi_ty: ty,
                    alloca: phi_alloca
                })
            }
        };
        
        let old_break_data = self.break_data;
        self.builder.position_at_end(to_bb);

        self.break_data = Some(BreakData {
            return_to_bb: after_bb,
            phi_data,
        });

        self.current_scope.push_layer();
        
        for stmt in body.iter() {
            if let Err(e) = self.gen_stmt(file, module, stmt) {
                self.break_data = old_break_data;
                self.current_scope.pop_layer();
                self.builder.position_at_end(from_bb);
                return Err(e)
            }
        }

        self.break_data = old_break_data;
        self.current_scope.pop_layer();
        self.builder.build_unconditional_branch(after_bb);

        Ok(phi_data.map(|d| d.alloca))
    }

    /// Generate an LLVM integer type to match an IR integer type
    fn llvm_int_ty(&self, width: IntegerWidth) -> IntType<'ctx> {
        match width {
            IntegerWidth::Eight => self.ctx.i8_type(),
            IntegerWidth::Sixteen => self.ctx.i16_type(),
            IntegerWidth::ThirtyTwo => self.ctx.i32_type(),
            IntegerWidth::SixtyFour => self.ctx.i64_type(),
        }
    }

    /// Get the type of an AST expression
    fn ast_type(
        &mut self,
        file: FileId,
        module: ModId,
        ast: &Ast<TypeId>,
    ) -> Result<TypeId, Diagnostic<FileId>> {
        Ok(match &ast.node {
            AstNode::UnitLiteral => SparkCtx::UNIT,
            AstNode::NumberLiteral(num) => match num.annotation() {
                Some(ann) => match ann {
                    NumberLiteralAnnotation::I8 => SparkCtx::I8,
                    NumberLiteralAnnotation::I16 => SparkCtx::I16,
                    NumberLiteralAnnotation::I32 => SparkCtx::I32,
                    NumberLiteralAnnotation::I64 => SparkCtx::I64,
                    NumberLiteralAnnotation::U8 => SparkCtx::U8,
                    NumberLiteralAnnotation::U16 => SparkCtx::U16,
                    NumberLiteralAnnotation::U32 => SparkCtx::U32,
                    NumberLiteralAnnotation::U64 => SparkCtx::U64,
                    NumberLiteralAnnotation::F32 => SparkCtx::F32,
                    NumberLiteralAnnotation::F64 => SparkCtx::F64,
                },
                None => if let NumberLiteral::Float(..) = num {
                    SparkCtx::F64
                } else {
                    SparkCtx::I32
                },
            },
            AstNode::StringLiteral(_) => self.spark.new_type(TypeData::Pointer(SparkCtx::U8)),
            AstNode::BooleanLiteral(_) => SparkCtx::BOOL,
            AstNode::TupleLiteral(parts) => {
                let part_types = parts
                    .iter()
                    .map(|part| self.ast_type(file, module, part))
                    .collect::<Result<Vec<_>, _>>()?;
                self.spark.new_type(TypeData::Tuple(part_types))
            }
            AstNode::ArrayLiteral(parts) => {
                let first_type = self.ast_type(file, module, parts.first().ok_or_else(||
                    Diagnostic::error()
                        .with_message("Failed to infer type of array literal because there are no elements")
                        .with_labels(vec![Label::primary(file, ast.span)])
                )?)?;
                self.spark.new_type(TypeData::Array {
                    element: first_type,
                    len: parts.len() as u64,
                })
            }
            AstNode::CastExpr(ty, ..) => *ty,
            AstNode::FunCall(called, ..) => {
                let called_ty = self.ast_type(file, module, called)?;
                if let TypeData::Function(f_ty) = &self.spark[called_ty].data {
                    f_ty.return_ty
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Attempting to call a value of type '{}' as a function",
                            self.spark.get_type_name(called_ty)
                        ))
                        .with_labels(vec![Label::primary(file, called.span).with_message(
                            format!(
                                "Called value of type '{}' here",
                                self.spark.get_type_name(called_ty)
                            ),
                        )]));
                }
            }
            AstNode::Access(path) => {
                let def = self.find_in_scope(file, ast.span, path)?;

                match def {
                    ScopeDef::Def(SparkDef::FunDef(_, f)) => self.spark[f].ty.return_ty,
                    ScopeDef::Value(ty, _) => ty,
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message("Cannot infer type of definition")
                            .with_labels(vec![Label::primary(file, ast.span)]))
                    }
                }
            }
            AstNode::MemberAccess(lhs, name) => {
                let lhs_ty = self.ast_type(file, module, lhs)?;
                if let TypeData::Struct { fields } = &self.spark[lhs_ty].data {
                    fields.iter().find_map(|(ty, field_name)| if name == field_name {
                        Some(*ty)
                    } else {
                        None
                    }).ok_or_else(|| Diagnostic::error()
                        .with_message(format!(
                                "Attempting to index field '{}' of type '{}' but no such field exists",
                                name,
                                self.spark.get_type_name(lhs_ty)
                            )
                        )
                        .with_labels(vec![
                            Label::primary(file, lhs.span)
                                .with_message(format!("This expression is found to be of type '{}'", self.spark.get_type_name(lhs_ty)))
                        ])
                        
                    )?
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Attempting to access field {} of non-struct type '{}'",
                            name,
                            self.spark.get_type_name(lhs_ty)
                        ))
                        .with_labels(vec![Label::primary(file, lhs.span).with_message(format!(
                            "this expression is found to be of type '{}'",
                            self.spark.get_type_name(lhs_ty)
                        ))]));
                }
            }
            AstNode::Index { object, index: _ } => {
                let object_ty = self.ast_type(file, module, object)?;
                if let TypeData::Array { element, len: _ } = self.spark[object_ty].data {
                    element
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Attempting to index into a value of type '{}'",
                            self.spark.get_type_name(object_ty)
                        ))
                        .with_labels(vec![Label::primary(file, object.span).with_message(
                            format!(
                                "This expression is found to be of type '{}'",
                                self.spark.get_type_name(object_ty)
                            ),
                        )]));
                }
            }
            AstNode::BinExpr(
                _,
                Op::Greater | Op::GreaterEq | Op::Less | Op::LessEq | Op::Eq,
                _,
            ) => SparkCtx::BOOL,
            AstNode::BinExpr(lhs, ..) => self.ast_type(file, module, lhs)?,
            AstNode::UnaryExpr(op, rhs) => {
                let rhs_ty = self.ast_type(file, module, rhs)?;
                match op {
                    Op::Star => {
                        if let TypeData::Pointer(pointee) = self.spark[rhs_ty].data {
                            pointee
                        } else {
                            return Err(Diagnostic::error()
                                .with_message(
                                    "Attempting to dereference expression of non-pointer type",
                                )
                                .with_labels(vec![Label::primary(file, ast.span).with_message(
                                    format!(
                                        "This expression is found to be of type '{}'",
                                        self.spark.get_type_name(rhs_ty)
                                    ),
                                )]));
                        }
                    }
                    Op::AND => self.spark.new_type(TypeData::Pointer(rhs_ty)),
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!("Unsupported unary operator '{}' used", op))
                            .with_labels(vec![Label::primary(file, ast.span)]))
                    }
                }
            }
            AstNode::IfExpr(if_expr) => {
                let phi_node = Self::phi_node(file, &if_expr.body).map_err(|e| {
                    e.with_labels(vec![
                        Label::secondary(file, ast.span).with_message("In if body here")
                    ])
                })?;
                let phi_ty = self.ast_type(file, module, phi_node)?;
                phi_ty
            }

            AstNode::VarDeclaration { ty: Some(ty), .. } => *ty,
            AstNode::PhiExpr(phid) => self.ast_type(file, module, phid)?,
            AstNode::Return(..)
            | AstNode::Break
            | AstNode::Continue
            | AstNode::VarDeclaration { .. }
            | AstNode::Assignment { .. }
            | AstNode::PhiExpr(..) => {
                return Err(Diagnostic::error()
                    .with_message("Cannot find type of statement")
                    .with_labels(vec![Label::primary(file, ast.span)]))
            }
            AstNode::Loop(body) | AstNode::Block(body) => {
                let phi_node = Self::phi_node(file, &body).map_err(|e| {
                    e.with_labels(vec![
                        Label::secondary(file, ast.span).with_message("In loop body here")
                    ])
                })?;
                self.ast_type(file, module, phi_node)?
            }
            AstNode::Match { matched: _, cases } => {
                let case_1 = cases.first().ok_or_else(|| {
                    Diagnostic::error()
                        .with_message("Failed to infer type of match expression")
                        .with_labels(vec![Label::primary(file, ast.span)])
                })?;
                self.ast_type(file, module, &case_1.1)?
            }
        })
    }

    /// Get the phi node from a block of AST nodes
    fn phi_node(file: FileId, body: &[Ast<TypeId>]) -> Result<&Ast<TypeId>, Diagnostic<FileId>> {
        body.iter()
            .find_map(|stmt| {
                if let AstNode::PhiExpr(_) = &stmt.node {
                    Some(stmt)
                } else {
                    None
                }
            })
            .ok_or_else(|| {
                Diagnostic::error()
                    .with_message("Failed to locate phi node in block of statements")
                    .with_labels(if let Some(first) = body.first() {
                        vec![Label::primary(file, first.span)
                            .with_message("First expression of block here")]
                    } else {
                        vec![]
                    })
            })
    }
}
