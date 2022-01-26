use codespan_reporting::diagnostic::{Diagnostic, Label};
use inkwell::values::AnyValueEnum;

use crate::{
    ast::{Ast, AstNode, NumberLiteralAnnotation},
    parse::token::Op, util::files::FileId,
};

use super::*;

impl<'ctx, 'files> LlvmCodeGenerator<'ctx, 'files> {
    /// Generate code for a single AST statement
    fn gen_stmt(&mut self, ast: &Ast<TypeId>) {
        match &ast.node {
            AstNode::Assignment { lhs, rhs } => {},
            _ => (),
        }
        todo!()
    }
    
    /// Generate code for a single AST expression
    fn gen_expr(&mut self, file: FileId, module: ModId, ast: &Ast<TypeId>) -> Result<AnyValueEnum<'ctx>, Diagnostic<FileId>> {
        match &ast.node {
            AstNode::Return(returned) => {
                let returned_ty = self.ast_type(module, returned).ok_or_else(|| Diagnostic::error()
                    .with_message("Failed to infer type of returned value")
                    .with_labels(vec![Label::primary(file, returned.span)])
                );
                
            },
            _ => ()
        }
        todo!()
    }
    
    /// Get the type of an AST expression
    fn ast_type(&mut self, module: ModId, ast: &Ast<TypeId>) -> Option<TypeId> {
        Some(match &ast.node {
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
                    .map(|part| self.ast_type(module, part))
                    .collect::<Option<Vec<_>>>()?;
                self.spark.new_type(TypeData::Tuple(part_types))
            },
            AstNode::ArrayLiteral(parts) => {
                let first_type = self.ast_type(module, parts.first()?)?;
                self.spark.new_type(TypeData::Array {
                    element: first_type,
                    len: parts.len() as u64
                })
            },
            AstNode::CastExpr(ty, ..) => *ty,
            AstNode::FunCall(called, ..) => {
                let called_ty = self.ast_type(module, called)?;
                if let TypeData::Function(f_ty) = &self.spark[called_ty].data {
                    f_ty.return_ty
                } else {
                    return None
                }
            },
            AstNode::Access(path) => {
                let def = self.spark.get_def(module, path).ok()?;
                match def {
                    SparkDef::FunDef(f) => self.spark[f].ty.return_ty,
                    _ => return None
                }
            },
            AstNode::MemberAccess(lhs, name) => {
                let lhs_ty = self.ast_type(module, lhs)?;
                if let TypeData::Struct { fields } = &self.spark[lhs_ty].data {
                    fields.iter().find_map(|(ty, field_name)| if name == field_name {
                        Some(*ty)
                    } else { None })?
                } else {
                    return None
                }
            },
            AstNode::Index { object, index } => {
                let object_ty = self.ast_type(module, object)?;
                if let AstNode::NumberLiteral(_) = &index.node {
                    if let TypeData::Array { element, len: _ } = self.spark[object_ty].data {
                        element
                    } else {
                        return None
                    }
                } else {
                    return None
                }
            },
            AstNode::BinExpr(lhs, ..) => self.ast_type(module, lhs)?,
            AstNode::UnaryExpr(op, rhs) => {
                let rhs_ty = self.ast_type(module, rhs)?;
                match op {
                    Op::Star => if let TypeData::Pointer(pointee) = self.spark[rhs_ty].data {
                        pointee
                    } else { return None },
                    Op::AND => self.spark.new_type(TypeData::Pointer(rhs_ty)),
                    _ => return None,
                }
            },
            AstNode::IfExpr(if_expr) => {
                let phi_node = Self::phi_node(&if_expr.body)?;
                let phi_ty = self.ast_type(module, phi_node)?;
                phi_ty
            },
            
            AstNode::Return(..) |
                AstNode::Break |
                AstNode::Continue => {
                    return None
                }
            AstNode::VarDeclaration { name: _, ty: _, mutable: _ } => return None,
            AstNode::Assignment { lhs: _, rhs: _ } => return None,
            AstNode::PhiExpr(_) => return None,
            AstNode::Loop(body) | AstNode::Block(body) => {
                let phi_node = Self::phi_node(&body)?;
                self.ast_type(module, phi_node)?
            },
            AstNode::Match { matched: _, cases } => {
                let case_1 = cases.first()?;
                self.ast_type(module, &case_1.1)?
            },
        })
    }
    
    /// Get the phi node from a block of AST nodes
    fn phi_node(body: &[Ast<TypeId>]) -> Option<&Ast<TypeId>> {
        body
            .iter()
            .find_map(|stmt| if let AstNode::PhiExpr(_) = &stmt.node {
                Some(stmt)
            } else { None })
    }
} 
