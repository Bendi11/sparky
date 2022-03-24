use codespan_reporting::diagnostic::{Diagnostic, Label};
use hashbrown::HashMap;

use crate::{Symbol, ast::{
        Ast, AstNode, DefData, ElseExpr, FunProto, IfExpr, IntegerWidth, Literal, ParsedModule,
        UnresolvedType,
    }, error::DiagnosticManager, util::{
        files::{FileId, Files},
        loc::Span,
    }};

use super::ir::{FunId, FunctionType, ModId, SparkCtx, SparkDef, TypeData, TypeId};

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

    pub fn lower_defs(&mut self, parsed: &ParsedModule, id: ModId) {
        for def in parsed.defs.iter().map(|(_, v)| v) {
            match &def.data {
                DefData::FunDef(proto, body) => {
                    let fun = if let SparkDef::FunDef(_, id) =
                        self.ctx[id].defs.get(&proto.name).unwrap()
                    {
                        *id
                    } else {
                        unreachable!()
                    };

                    let body = body
                        .iter()
                        .map(|expr| self.lower_ast(id, expr, def.file))
                        .collect();
                    self.ctx[fun].body = Some(body);
                }
                DefData::AliasDef { name, aliased, tparams } => {
                    let ty = if let SparkDef::TypeDef(_, id) = self.ctx[id].defs.get(name).unwrap()
                    {
                        *id
                    } else {
                        unreachable!()
                    };
                    if tparams.is_none() {
                        let aliased = self.lower_type(id, Some(def.span), aliased, def.file, None);
                        self.ctx[ty] = TypeData::Alias(*name, aliased);
                    }
                }
                _ => continue,
            }
        }

        for child in &parsed.children {
            let child_id = *self.ctx[id].defs.get(child.0).unwrap();
            if let SparkDef::ModDef(child_id) = child_id {
                self.lower_defs(child.1, child_id);
            }
        }
    }

    pub fn lower_module(&mut self, parsed: &ParsedModule) -> ModId {
        let id = self.gen_forward_decls(parsed);
        self.lower_defs(parsed, id);
        id
    }

    /// Generate forward declarations for all function definitions and declarations
    fn gen_forward_funs(&mut self, parsed: &ParsedModule, module_id: ModId) {
        for def in parsed.defs.iter().map(|(_, v)| v) {
            match &def.data {
                DefData::FunDec(proto) | DefData::FunDef(proto, _) => {
                    let fun_id = self.lower_funproto(module_id, def.span, proto, def.file);
                    self.ctx[module_id]
                        .defs
                        .define(proto.name, SparkDef::FunDef(def.file, fun_id));
                }
                _ => (),
            }
        }

        for child in parsed.children.iter().map(|(_, c)| c) {
            let child_def = self.ctx[module_id].defs.get(&child.name).unwrap();
            if let SparkDef::ModDef(child_id) = child_def {
                let child_id = *child_id;
                drop(child_def);
                self.gen_forward_funs(child, child_id);
            } else {
                unreachable!()
            }
        }
    }

    /// Generate forward declarations for all type definitions
    fn gen_forward_types(&mut self, parsed: &ParsedModule) -> ModId {
        let module_id = self.ctx.new_module(parsed.name);

        for def in parsed.defs.iter().map(|(_, v)| v) {
            match &def.data {
                DefData::AliasDef { name, tparams, aliased } => {
                    let ty = self.ctx.new_empty_type();
                    self.ctx[module_id]
                        .defs
                        .define(name.clone(), SparkDef::TypeDef(def.file, ty));
                }
                _ => continue,
            }
        }

        for child in parsed.children.iter().map(|(_, c)| c) {
            let child_id = self.gen_forward_types(child);
            self.ctx[module_id]
                .defs
                .define(child.name.clone(), SparkDef::ModDef(child_id));
        }

        module_id
    }

    /// Resolve all imports of a module
    fn gen_imports(&mut self, parsed: &ParsedModule, module_id: ModId) {
        for def in parsed.defs.iter().map(|(_, v)| v) {
            if let DefData::ImportDef { name } = &def.data {
                let imported = match self.ctx.get_def(module_id, name) {
                    Ok(id) => id,
                    Err(name) => {
                        self.diags.emit(
                            Diagnostic::error()
                                .with_message(format!("Imported item '{}' not found", name))
                                .with_labels(vec![Label::primary(def.file, def.span)]),
                        );
                        panic!()
                    }
                };
                self.ctx[module_id].defs.define(name.last(), imported);
            }
        }

        for child in parsed.children.iter().map(|(_, c)| c) {
            let child_def = self.ctx[module_id].defs.get(&child.name).unwrap();
            if let SparkDef::ModDef(child_id) = child_def {
                let child_id = *child_id;
                drop(child_def);
                self.gen_imports(child, child_id);
            } else {
                unreachable!()
            }
        }
    }

    /// Lower a parsed module's definitions and return an ID for the lowered
    /// module
    fn gen_forward_decls(&mut self, parsed: &ParsedModule) -> ModId {
        let module_id = self.gen_forward_types(parsed);
        for child in &parsed.children {
            let child_id = self.gen_forward_types(&child.1);
            self.ctx[module_id]
                .defs
                .define(child.0.clone(), SparkDef::ModDef(child_id));
        }
        self.gen_forward_funs(parsed, module_id);
        for child in &parsed.children {
            let child_id = *self.ctx[module_id].defs.get(child.0).unwrap();
            if let SparkDef::ModDef(child_id) = child_id {
                self.gen_forward_funs(child.1, child_id);
            }
        }
        self.gen_imports(parsed, module_id);
        for child in &parsed.children {
            let child_id = *self.ctx[module_id].defs.get(child.0).unwrap();
            if let SparkDef::ModDef(child_id) = child_id {
                self.gen_imports(child.1, child_id);
            }
        }

        module_id
    }

    /// Lower AST types to type IDs
    pub fn lower_ast(&mut self, module: ModId, ast: &Ast, file: FileId) -> Ast<TypeId> {
        Ast {
            span: ast.span,
            node: match &ast.node {
                AstNode::Access(path) => AstNode::Access(path.clone()),
                AstNode::MemberAccess(accessing, name) => AstNode::MemberAccess(
                    Box::new(self.lower_ast(module, accessing, file)),
                    name.clone(),
                ),
                AstNode::Index { object, index } => AstNode::Index {
                    object: Box::new(self.lower_ast(module, object, file)),
                    index: Box::new(self.lower_ast(module, index, file)),
                },
                AstNode::FunCall(called, args) => AstNode::FunCall(
                    Box::new(self.lower_ast(module, called, file)),
                    args.iter()
                        .map(|arg| self.lower_ast(module, arg, file))
                        .collect(),
                ),
                AstNode::VarDeclaration { name, ty, mutable } => AstNode::VarDeclaration {
                    name: name.clone(),
                    ty: ty
                        .as_ref()
                        .map(|ty| self.lower_type(module, Some(ast.span), ty, file, None)),
                    mutable: *mutable,
                },
                AstNode::Assignment { lhs, rhs } => AstNode::Assignment {
                    lhs: Box::new(self.lower_ast(module, lhs, file)),
                    rhs: Box::new(self.lower_ast(module, rhs, file)),
                },
                AstNode::Literal(l) => {
                    AstNode::Literal(self.lower_literal(module, ast.span, l, file))
                } //AstNode:: => AstNode::UnitLiteral,
                AstNode::Continue => AstNode::Continue,
                AstNode::Break => AstNode::Break,
                AstNode::BinExpr(lhs, op, rhs) => AstNode::BinExpr(
                    Box::new(self.lower_ast(module, lhs, file)),
                    *op,
                    Box::new(self.lower_ast(module, rhs, file)),
                ),
                AstNode::UnaryExpr(op, rhs) => {
                    AstNode::UnaryExpr(*op, Box::new(self.lower_ast(module, rhs, file)))
                }
                AstNode::CastExpr(ty, rhs) => AstNode::CastExpr(
                    self.lower_type(module, Some(ast.span), ty, file, None),
                    Box::new(self.lower_ast(module, rhs, file)),
                ),
                AstNode::PhiExpr(expr) => {
                    AstNode::PhiExpr(Box::new(self.lower_ast(module, expr, file)))
                }
                AstNode::Return(expr) => {
                    AstNode::Return(Box::new(self.lower_ast(module, expr, file)))
                }
                AstNode::Block(body) => AstNode::Block(
                    body.iter()
                        .map(|expr| self.lower_ast(module, expr, file))
                        .collect(),
                ),
                AstNode::Match { matched, cases } => AstNode::Match {
                    matched: Box::new(self.lower_ast(module, matched, file)),
                    cases: cases
                        .iter()
                        .map(|(arm, case)| {
                            (
                                self.lower_type(module, Some(ast.span), arm, file, None),
                                self.lower_ast(module, case, file),
                            )
                        })
                        .collect(),
                },
                AstNode::IfExpr(if_expr) => {
                    AstNode::IfExpr(self.lower_if_ast(module, if_expr, file))
                }
            },
        }
    }

    /// Lower a literal AST
    fn lower_literal(
        &mut self,
        module: ModId,
        span: Span,
        literal: &Literal<UnresolvedType>,
        file: FileId,
    ) -> Literal<TypeId> {
        match literal {
            Literal::Array(elems) => Literal::Array(
                elems
                    .iter()
                    .map(|elem| self.lower_ast(module, elem, file))
                    .collect(),
            ),
            Literal::String(s) => Literal::String(s.clone()),
            Literal::Number(num) => Literal::Number(num.clone()),
            Literal::Bool(b) => Literal::Bool(*b),
            Literal::Unit => Literal::Unit,
            Literal::Struct {
                ty,
                fields
            } => Literal::Struct {
                    ty: ty.as_ref().map(|ty| self.lower_type(module, Some(span), ty, file, None)),
                    fields: fields
                        .iter()
                        .map(|(name, field)| (name.clone(), self.lower_ast(module, field, file)))
                        .collect()
                }
        }
    }

    /// Lower a parsed if expression's types to TypeIds
    fn lower_if_ast(
        &mut self,
        module: ModId,
        if_expr: &IfExpr<UnresolvedType>,
        file: FileId,
    ) -> IfExpr<TypeId> {
        let cond = Box::new(self.lower_ast(module, &if_expr.cond, file));
        let body = if_expr
            .body
            .iter()
            .map(|expr| self.lower_ast(module, expr, file))
            .collect();
        let else_expr = match &if_expr.else_expr {
            Some(ElseExpr::ElseIf(else_if_expr)) => Some(ElseExpr::ElseIf(Box::new(
                self.lower_if_ast(module, else_if_expr, file),
            ))),
            Some(ElseExpr::Else(body)) => Some(ElseExpr::Else(
                body.iter()
                    .map(|expr| self.lower_ast(module, expr, file))
                    .collect(),
            )),
            None => None,
        };
        IfExpr {
            cond,
            body,
            else_expr,
        }
    }

    /// Lower a single function prototype to a function with no body
    fn lower_funproto(
        &mut self,
        module: ModId,
        span: Span,
        proto: &FunProto<UnresolvedType>,
        file: FileId,
    ) -> FunId {
        let fun_ty = FunctionType {
            return_ty: self.lower_type(module, Some(span), &proto.return_ty, file, None),
            args: proto
                .args
                .iter()
                .map(|(_, ty)| self.lower_type(module, Some(span), ty, file, None))
                .collect(),
        };

        self.ctx.new_fun(
            proto.name,
            fun_ty,
            proto.flags,
            proto
                .args
                .iter()
                .map(|(name, _)| Some(name.clone()))
                .collect(),
            span,
        )
    }

    /// Lower a type either by resolving the path to the type or
    /// converting an integral type
    fn lower_type(
        &mut self,
        module: ModId,
        span: Option<Span>,
        ty: &UnresolvedType,
        file: FileId,
        targs: Option<&HashMap<Symbol, TypeId>>,
    ) -> TypeId {
        match ty {
            UnresolvedType::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(|(ty, name)| (self.lower_type(module, span, ty, file, targs), name.clone()))
                    .collect();
                self.ctx.new_type(TypeData::Struct { fields })
            }
            UnresolvedType::Enum { variants } => {
                let parts = variants
                    .iter()
                    .map(|ty| self.lower_type(module, span, ty, file, targs))
                    .collect();
                self.ctx.new_type(TypeData::Enum { parts })
            }
            UnresolvedType::Integer { width, signed } => match signed {
                true => match width {
                    IntegerWidth::Eight => SparkCtx::I8,
                    IntegerWidth::Sixteen => SparkCtx::I16,
                    IntegerWidth::ThirtyTwo => SparkCtx::I32,
                    IntegerWidth::SixtyFour => SparkCtx::I64,
                },
                false => match width {
                    IntegerWidth::Eight => SparkCtx::U8,
                    IntegerWidth::Sixteen => SparkCtx::U16,
                    IntegerWidth::ThirtyTwo => SparkCtx::U32,
                    IntegerWidth::SixtyFour => SparkCtx::U64,
                },
            },
            UnresolvedType::Float { doublewide } => match doublewide {
                true => SparkCtx::F64,
                false => SparkCtx::F32,
            },
            UnresolvedType::Unit => SparkCtx::UNIT,
            UnresolvedType::Bool => SparkCtx::BOOL,
            UnresolvedType::Fun(ty) => {
                let return_ty = self.lower_type(module, span, &ty.return_ty, file, targs);
                let args = ty
                    .arg_tys
                    .iter()
                    .map(|ty| self.lower_type(module, span, ty, file, targs))
                    .collect();
                self.ctx
                    .new_type(TypeData::Function(FunctionType { return_ty, args }))
            }
            UnresolvedType::Pointer(ty) => {
                let pointee = self.lower_type(module, span, ty, file, targs);
                self.ctx.new_type(TypeData::Pointer(pointee))
            }
            UnresolvedType::Array { elements, len } => {
                let element = self.lower_type(module, span, elements, file, targs);
                self.ctx.new_type(TypeData::Array { element, len: *len })
            }
            UnresolvedType::UserDefined { name } => match self.ctx.get_def(module, name) {
                Ok(SparkDef::TypeDef(_, type_id)) => type_id,
                Ok(..) => {
                    self.diags.emit({
                        let diag = Diagnostic::error()
                            .with_message(format!("definition '{}' found but is not a type", name));
                        if let Some(span) = span {
                            diag.with_labels(vec![Label::primary(file, span)])
                        } else {
                            diag
                        }
                    });
                    std::process::exit(-1);
                }
                Err(_) => if let Some(targ) = targs.map(|targs| targs.get(&name.last())).flatten() {
                    *targ 
                } else {
                    self.diags.emit({
                        let diag =
                            Diagnostic::error().with_message(format!(
                                "type '{}' not found (generics: {:#?})",
                                name,
                                targs
                            ));
                        if let Some(span) = span {
                            diag.with_labels(vec![Label::primary(file, span)])
                        } else {
                            diag
                        }
                    });
                    std::process::exit(-1);
                }
            },
        }
    }
}
