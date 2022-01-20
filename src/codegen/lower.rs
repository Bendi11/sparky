use crate::{
    ast::{Ast, AstNode, Def, DefData, FunProto, IntegerWidth, ParsedModule, UnresolvedType}, 
    error::{DiagnosticManager, Diagnostic}, 
    util::{files::Files, loc::Span}
};

use super::ir::{SparkCtx, ModId, FunId, TypeId, FunctionType, TypeData, SparkDef};

/// Structure for lowering a parsed AST's types
pub struct Lowerer<'ctx, 'files> {
    ctx: &'ctx mut SparkCtx,
    diags: DiagnosticManager<'files>,
}

impl<'ctx, 'files> Lowerer<'ctx, 'files> {
    /// Create a new AST lowerer
    pub fn new(ctx: &'ctx mut SparkCtx, files: &'files Files) -> Self {
        Self {
            ctx,
            diags: DiagnosticManager::new(files),
        }
    }
    
    /// Lower a parsed module's definitions and return an ID for the lowered
    /// module
    pub fn lower_module(&mut self, parsed: &ParsedModule) -> ModId {
        let module_id = self.ctx.new_module(parsed.name);
        let module = &mut self.ctx[module_id];
        for def in parsed.defs.iter().map(|(_, v)| v) {
            match def {
                DefData::FunDef(proto, body) => {
                    
                }
            }
        }
    }
    
    /// Lower AST types to type IDs
    pub fn lower_ast(&self, module: ModId, ast: &Ast) -> Ast<TypeId> {
        Ast {
            span: ast.span,
            node: match &ast.node {
                AstNode::Access(path) => AstNode::Access(path.clone()),
                AstNode::MemberAccess(accessing, name) => 
                    AstNode::MemberAccess(Box::new(self.lower_ast(module, accessing)), name.clone()),
                AstNode::Index { object, index } => AstNode::Index {
                    object: Box::new(self.lower_ast(module, object)),
                    index: Box::new(self.lower_ast(module, index)),
                },
                AstNode::FunCall(called, args) => AstNode::FunCall(
                    Box::new(self.lower_ast(module, called)),
                    args.iter().map(|arg| self.lower_ast(module, arg)).collect()
                ),
                AstNode::VarDeclaration { name, ty, mutable } => AstNode::VarDeclaration {
                    name: name.clone(),
                    ty: ty.map(|ty| self.lower_type(module, Some(ast.span), &ty)),
                    mutable: *mutable,
                },
                AstNode::Assignment { lhs, rhs } => AstNode::Assignment {
                    lhs: Box::new(self.lower_ast(module, lhs)),
                    rhs: Box::new(self.lower_ast(module, rhs)),
                },
                AstNode::ArrayLiteral(elems) => AstNode::ArrayLiteral(
                    elems.iter().map(|elem| self.lower_ast(module, elem)).collect()    
                ),
                AstNode::StringLiteral(s) => AstNode::StringLiteral(s.clone()),
                AstNode::NumberLiteral(num) => AstNode::NumberLiteral(num.clone()),
                AstNode::BooleanLiteral(b) => AstNode::BooleanLiteral(*b),
                AstNode::TupleLiteral(elems) => AstNode::TupleLiteral(
                    elems.iter().map(|elem| self.lower_ast(module, elem)).collect()
                ),
                AstNode::BinExpr(lhs, op, rhs) => AstNode::BinExpr(
                    Box::new(self.lower_ast(module, lhs)),
                    *op,
                    Box::new(self.lower_ast(module, rhs)),
                ),
                AstNode::UnaryExpr(op, rhs) => AstNode::UnaryExpr(
                    *op,
                    Box::new(self.lower_ast(module, rhs))
                ),
                AstNode::CastExpr(ty, rhs) => AstNode::CastExpr(
                    self.lower_type(module, Some(ast.span), ty),
                    Box::new(self.lower_ast(module, rhs)),
                ),
            }
        }
    }
    
    /// Lower a single function prototype to a function with no body
    fn lower_funproto(&mut self, proto: &FunProto<UnresolvedType>) -> FunId {

    }
    
    /// Lower a type either by resolving the path to the type or 
    /// converting an integral type
    fn lower_type(&mut self, module: ModId, span: Option<Span>, ty: &UnresolvedType) -> TypeId {
        match ty {
            UnresolvedType::Integer { width, signed } => match signed {
                true => match width {
                    IntegerWidth::Eight => SparkCtx::I8,
                    IntegerWidth::Sixteen => SparkCtx::I16,
                    IntegerWidth::ThirtyTwo => SparkCtx::I32,
                    IntegerWidth::SixtyFour => SparkCtx::I64
                },
                false => match width {
                    IntegerWidth::Eight => SparkCtx::U8,
                    IntegerWidth::Sixteen => SparkCtx::U16,
                    IntegerWidth::ThirtyTwo => SparkCtx::U32,
                    IntegerWidth::SixtyFour => SparkCtx::U64 
                }
            },
            UnresolvedType::Float { doublewide } => match doublewide {
                true => SparkCtx::F64,
                false => SparkCtx::F32,
            },
            UnresolvedType::Unit => SparkCtx::UNIT,
            UnresolvedType::Bool => SparkCtx::BOOL,
            UnresolvedType::Fun(ty) => self.ctx.new_type(TypeData::Function(FunctionType {
                return_ty: self.lower_type(module, span, &ty.return_ty),
                args: ty.arg_tys.iter().map(|ty| self.lower_type(module, span, ty)).collect()
            })),
            UnresolvedType::Pointer(ty) => self.ctx.new_type(TypeData::Pointer(self.lower_type(module, span, ty))),
            UnresolvedType::Array { elements, len } => self.ctx.new_type(TypeData::Array {
                element: self.lower_type(module, span, elements),
                len: *len,
            }),
            UnresolvedType::Tuple { elements } => self.ctx.new_type(TypeData::Tuple(elements.iter().map(|ty| self.lower_type(module, span, ty)).collect())),
            UnresolvedType::UserDefined { name } => match self.ctx.get_def(module, name) {
                Ok(SparkDef::TypeDef(type_id)) => type_id,
                _ => {
                    self.diags.emit({
                        let mut diag = Diagnostic::error(format!("type {} not found", name), self.ctx[module].file);
                        if let Some(span) = span {
                            diag.with_span(span)
                        } else {
                            diag
                        }
                    });
                    panic!()
                }
            },
        }
    }
}
