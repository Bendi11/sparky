use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::{Ast, AstNode, NumberLiteralAnnotation, NumberLiteral},
    parse::token::Op, util::files::FileId,
};

use super::*;

impl<'ctx, 'files> LlvmCodeGenerator<'ctx, 'files> {
    /// Generate code for a single AST statement
    pub fn gen_stmt(&mut self, file: FileId, module: ModId, ast: Ast<TypeId>) -> Result<(), Diagnostic<FileId>> {
        match &ast.node {
            AstNode::Assignment { lhs, rhs } => {

            },
            AstNode::Return(returned) => {
                let returned_ty = self.ast_type(file, module, returned).map_err(|e| e.with_labels(vec![
                    Label::secondary(file, ast.span)
                        .with_message("In this return statement")
                ]))?;

                let current_fun = &self.spark[self.current_fun.unwrap().1];
                
                if returned_ty != current_fun.ty.return_ty {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                                "Returned value of type '{}' is not compatible with declared return type of '{}'",
                                self.spark.get_type_name(returned_ty),
                                self.spark.get_type_name(current_fun.ty.return_ty),
                            )
                        )
                    )
                }
                
                if current_fun.ty.return_ty != SparkCtx::UNIT {
                    let returned = self.gen_expr(file, module, returned)?;
                    self.builder.build_return(Some(&returned)); 
                } else {
                    self.builder.build_return(None);
                }
            },
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
                            )
                        }
                        
                        let phi_val = self.gen_expr(file, module, phi)?;
                        self.builder.build_store(phi_data.alloca, phi_val);
                        self.builder.build_unconditional_branch(break_data.return_to_bb);
                    } else {
                        return Err(Diagnostic::error()
                            .with_message("Phi statement encountered but not used")
                            .with_labels(vec![
                                Label::primary(file, ast.span)
                                    .with_message("Phi statement encountered here")
                            ])
                            .with_notes(vec!["Replace with a break or continue statement if value is not used".to_owned()])
                        )
                    }
                } else {
                    return Err(Diagnostic::error()
                        .with_message("Phi statement not in a block")
                        .with_labels(vec![Label::primary(file, ast.span)])
                    )
                }
            },
            other => return Err(Diagnostic::error()
                    .with_message(format!("Invalid statement: {:#?}", other))
                    .with_labels(vec![Label::primary(file, ast.span)])
                ),
        }

        Ok(())
    }
    
    /// Generate code for a single AST expression
    fn gen_expr(&mut self, file: FileId, module: ModId, ast: &Ast<TypeId>) -> Result<BasicValueEnum<'ctx>, Diagnostic<FileId>> {
        match &ast.node {
            AstNode::NumberLiteral(n) => match n {
                NumberLiteral::Integer(num, annot) => match annot {
                    NumberLiteralAnnotation::U8 => self.ctx.i8_type().const_int(num.to_u64_digits(), sign_extend)
                }
            }
            _ => ()
        }
        todo!()
    }
    
    /// Get the type of an AST expression
    fn ast_type(&mut self, file: FileId, module: ModId, ast: &Ast<TypeId>) -> Result<TypeId, Diagnostic<FileId>> {
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
                None => SparkCtx::I32,
            },
            AstNode::StringLiteral(_) => self.spark.new_type(TypeData::Pointer(SparkCtx::U8)),
            AstNode::BooleanLiteral(_) => SparkCtx::BOOL,
            AstNode::TupleLiteral(parts) => {
                let part_types = parts
                    .iter()
                    .map(|part| self.ast_type(file, module, part))
                    .collect::<Result<Vec<_>, _>>()?;
                self.spark.new_type(TypeData::Tuple(part_types))
            },
            AstNode::ArrayLiteral(parts) => {
                let first_type = self.ast_type(file, module, parts.first().ok_or_else(||
                    Diagnostic::error()
                        .with_message("Failed to infer type of array literal because there are no elements")
                        .with_labels(vec![Label::primary(file, ast.span)])
                )?)?;
                self.spark.new_type(TypeData::Array {
                    element: first_type,
                    len: parts.len() as u64
                })
            },
            AstNode::CastExpr(ty, ..) => *ty,
            AstNode::FunCall(called, ..) => {
                let called_ty = self.ast_type(file, module, called)?;
                if let TypeData::Function(f_ty) = &self.spark[called_ty].data {
                    f_ty.return_ty
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!("Attempting to call a value of type '{}' as a function", self.spark.get_type_name(called_ty)))
                        .with_labels(vec![
                            Label::primary(file, called.span)
                                .with_message(format!("Called value of type '{}' here", self.spark.get_type_name(called_ty))),
                        ])
                    )
                }
            },
            AstNode::Access(path) => {
                let def = self.spark.get_def(module, path).map_err(|_| {
                    Diagnostic::error()
                        .with_message(format!("Failed to find symbol '{}' in module {}", path, self.spark[module].name))
                        .with_labels(vec![Label::primary(file, ast.span)])
                })?;

                match def {
                    SparkDef::FunDef(_, f) => self.spark[f].ty.return_ty,
                    _ => return Err(Diagnostic::error()
                        .with_message("Cannot infer type of definition")
                        .with_labels(vec![Label::primary(file, ast.span)])
                    )
                }
            },
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
                            )
                        )
                        .with_labels(vec![
                            Label::primary(file, lhs.span)
                                .with_message(format!("this expression is found to be of type '{}'", self.spark.get_type_name(lhs_ty)))
                        ])
                    )
                }
            },
            AstNode::Index { object, index } => {
                let object_ty = self.ast_type(file, module, object)?;
                if let TypeData::Array { element, len: _ } = self.spark[object_ty].data {
                    element
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                                "Attempting to index into a value of type '{}'",
                                self.spark.get_type_name(object_ty)
                            )
                        )
                        .with_labels(vec![
                            Label::primary(file, object.span)
                                .with_message(format!("This expression is found to be of type '{}'", self.spark.get_type_name(object_ty)))
                        ])
                    )
                }
            },
            AstNode::BinExpr(lhs, ..) => self.ast_type(file, module, lhs)?,
            AstNode::UnaryExpr(op, rhs) => {
                let rhs_ty = self.ast_type(file, module, rhs)?;
                match op {
                    Op::Star => if let TypeData::Pointer(pointee) = self.spark[rhs_ty].data {
                        pointee
                    } else {
                        return Err(Diagnostic::error()
                            .with_message("Attempting to dereference expression of non-pointer type")
                            .with_labels(vec![
                                Label::primary(file, ast.span)
                                    .with_message(format!("This expression is found to be of type '{}'", self.spark.get_type_name(rhs_ty)))
                            ])
                        )
                    },
                    Op::AND => self.spark.new_type(TypeData::Pointer(rhs_ty)),
                    _ => return Err(Diagnostic::error()
                        .with_message(format!("Unsupported unary operator '{}' used", op))
                        .with_labels(vec![Label::primary(file, ast.span)])
                    ),
                }
            },
            AstNode::IfExpr(if_expr) => {
                let phi_node = Self::phi_node(file, &if_expr.body)
                    .map_err(|e| e.with_labels(vec![
                        Label::secondary(file, ast.span)
                            .with_message("In if body here")
                    ])
                )?;
                let phi_ty = self.ast_type(file, module, phi_node)?;
                phi_ty
            },
            
            AstNode::Return(..) |
                AstNode::Break |
                AstNode::Continue |
                AstNode::VarDeclaration { .. } |
                AstNode::Assignment { .. } | 
                AstNode::PhiExpr(..) => {
                    return Err(Diagnostic::error()
                        .with_message("Cannot find type of statement")
                        .with_labels(vec![
                            Label::primary(file, ast.span)
                        ])
                    )
                }
            AstNode::Loop(body) | AstNode::Block(body) => {
                let phi_node = Self::phi_node(file, &body).map_err(|e| e.with_labels(vec![
                        Label::secondary(file, ast.span)
                            .with_message("In loop body here")
                    ])
                )?;
                self.ast_type(file, module, phi_node)?
            },
            AstNode::Match { matched: _, cases } => {
                let case_1 = cases.first().ok_or_else(|| Diagnostic::error()
                    .with_message("Failed to infer type of match expression")
                    .with_labels(vec![Label::primary(file, ast.span)])
                )?;
                self.ast_type(file, module, &case_1.1)?
            },
        })
    }
    
    /// Get the phi node from a block of AST nodes
    fn phi_node(file: FileId, body: &[Ast<TypeId>]) -> Result<&Ast<TypeId>, Diagnostic<FileId>> {
        body
            .iter()
            .find_map(|stmt| if let AstNode::PhiExpr(_) = &stmt.node {
                    Some(stmt)
                } else { None }
            )
            .ok_or_else(|| Diagnostic::error()
                .with_message("Failed to locate phi node in block of statements")
                .with_labels(if let Some(first) = body.first() {
                        vec![Label::primary(file, first.span)
                            .with_message("First expression of block here")
                        ]
                    } else { vec![] }
                )
            )
    }
} 
