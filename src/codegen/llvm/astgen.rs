use inkwell::values::AnyValueEnum;

use crate::ast::{Ast, AstNode, NumberLiteralAnnotation};

use super::*;

impl<'ctx, 'files> LlvmCodeGenerator<'ctx, 'files> {
    /// Generate code for a single AST statement
    fn gen_stmt(&mut self, ast: &Ast<TypeId>) {
        match &ast.node {
            AstNode::Assignment { lhs, rhs } => {}
        }
    }
    
    /// Generate code for a single AST expression
    fn gen_expr(&mut self, ast: &Ast<TypeId>) -> AnyValueEnum<'ctx> {
        match &ast.node {
            AstNode::Return(returned) => {
                
            }
        }
    }
    
    /// Get the type of an AST expression
    fn ast_type(&mut self, ast: &Ast<TypeId>) -> Option<TypeId> {
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
                let part_types = parts.iter().map(|part| self.ast_type(part)).collect::<Vec<_>>();
                self.spark.new_type(TypeData::Tuple(part_types))
            },
            AstNode::ArrayLiteral(parts) => {
                let first_type = 
            }
        })
    }
} 
