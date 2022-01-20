use crate::{
    ast::{Ast, AstNode, Def, DefData, FunProto, IfExpr, ElseExpr, IntegerWidth, ParsedModule, UnresolvedType}, 
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
        let module_id = self.ctx.new_module(parsed.name, parsed.file);
        let module = &mut self.ctx[module_id];
        for def in parsed.defs.iter().map(|(_, v)| v) {
            match &def.data {
                DefData::FunDef(proto, body) => {
                    
                },
                _ => unimplemented!()
            }
        }
        unimplemented!()
    }
    
    /// Lower AST types to type IDs
    pub fn lower_ast(&mut self, module: ModId, ast: &Ast) -> Ast<TypeId> {
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
                    ty: ty.as_ref().map(|ty| self.lower_type(module, Some(ast.span), ty)),
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
                //AstNode:: => AstNode::UnitLiteral,
                AstNode::Continue => AstNode::Continue,
                AstNode::Break => AstNode::Break,
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
                AstNode::PhiExpr(expr) => AstNode::PhiExpr(
                    Box::new(self.lower_ast(module, expr))
                ),
                AstNode::Return(expr) => AstNode::Return(
                    Box::new(self.lower_ast(module, expr))
                ),
                AstNode::Loop(body) => AstNode::Loop(body.iter().map(|expr| self.lower_ast(module, expr)).collect()),
                AstNode::Block(body) => AstNode::Block(body.iter().map(|expr| self.lower_ast(module, expr)).collect()),
                AstNode::Match { matched, cases } => AstNode::Match {
                    matched: Box::new(self.lower_ast(module, matched)),
                    cases: cases.iter().map(|(constant, case)| (self.lower_ast(module, constant), self.lower_ast(module, case))).collect()
                },
                AstNode::IfExpr(if_expr) => AstNode::IfExpr(self.lower_if_ast(module, if_expr)),
            }
        }
    }

    /// Lower a parsed if expression's types to TypeIds
    fn lower_if_ast(&mut self, module: ModId, if_expr: &IfExpr<UnresolvedType>) -> IfExpr<TypeId> {
        let cond = Box::new(self.lower_ast(module, &if_expr.cond));
        let body = if_expr.body.iter().map(|expr| self.lower_ast(module, expr)).collect();
        let else_expr = match &if_expr.else_expr {
            Some(ElseExpr::ElseIf(else_if_expr)) => Some(ElseExpr::ElseIf(
                    Box::new(self.lower_if_ast(module, else_if_expr))
            )),
            Some(ElseExpr::Else(body)) => Some(ElseExpr::Else(
                body.iter().map(|expr| self.lower_ast(module, expr)).collect()
            )),
            None => None
        };
        IfExpr {
            cond,
            body,
            else_expr,
        }
    }
    
    /// Lower a single function prototype to a function with no body
    fn lower_funproto(&mut self, module: ModId, span: Span, proto: &FunProto<UnresolvedType>) -> FunId {
        let fun_ty = FunctionType {
            return_ty: self.lower_type(module, Some(span), &proto.return_ty),
            args: proto.args.iter().map(|(_, ty)| self.lower_type(module, Some(span), ty)).collect(),
        };

        self.ctx.new_fun(
            proto.name,
            fun_ty,
            proto.args.iter().map(|(name, _)| Some(name.clone())).collect()
        )
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
            UnresolvedType::Fun(ty) => {
                let return_ty = self.lower_type(module, span, &ty.return_ty);
                let args = ty.arg_tys.iter().map(|ty| self.lower_type(module, span, ty)).collect();
                self.ctx.new_type(TypeData::Function(FunctionType {
                    return_ty,
                    args,
                }))
            },
            UnresolvedType::Pointer(ty) => {
                let pointee = self.lower_type(module, span, ty);
                self.ctx.new_type(TypeData::Pointer(pointee))
            },
            UnresolvedType::Array { elements, len } => {
                let element = self.lower_type(module, span, elements);
                self.ctx.new_type(TypeData::Array {
                    element,
                    len: *len,
                })
            },
            UnresolvedType::Tuple { elements } => {
                let elements = elements.iter().map(|ty| self.lower_type(module, span, ty)).collect();
                self.ctx.new_type(TypeData::Tuple(elements))
            }
            UnresolvedType::UserDefined { name } => match self.ctx.get_def(module, name) {
                Ok(SparkDef::TypeDef(type_id)) => type_id,
                _ => {
                    self.diags.emit({
                        let diag = Diagnostic::error(format!("type {} not found", name), self.ctx[module].file);
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
