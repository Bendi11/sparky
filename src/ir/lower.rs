use quickscope::ScopeMap;

use crate::{
    ast::{ParsedModule, DefData, Def, UnresolvedType, Ast, IfExpr, ElseExpr, NumberLiteralAnnotation}, 
    util::{files::{Files, FileId}, loc::Span}, 
    error::{DiagnosticManager, Diagnostic},
    Symbol,
};
use super::*;


/// A structure responsible for lowering a parsed AST into
/// intermediate representation, resolving all unknown symbols
#[derive(Debug)]
pub struct AstLowerer<'ctx, 'files> {
    ctx: &'ctx mut IRContext,
    /// A map of module names to their IDs in the context
    forward_modules: HashMap<Symbol, ModuleId>,
    /// Structure responsible for outputting emitted diagnostics
    diagnostic: DiagnosticManager<'files>,
}

pub type SemanticResult<T> = Result<T, ()>;

impl<'ctx,'files> AstLowerer<'ctx, 'files> {
    /// Create a new lowerer from an ir context
    pub fn new(
        ctx: &'ctx mut IRContext, 
        files: &'files Files, 
    ) -> Self {
        Self {
            ctx,
            diagnostic: DiagnosticManager::new(files),
            forward_modules: HashMap::new()
        }
    }
    
    /// Generate code for all passed modules
    pub fn codegen(&mut self, modules: &[ParsedModule]) -> SemanticResult<()> {
        self.forward_modules = self.forward_modules(modules)?;
        self.walk_modules_with(modules, &mut Self::forward_types)?;
        self.walk_modules_with(modules, &mut Self::resolve_types)?;
        self.walk_modules_with(modules, &mut Self::gen_fundecls)?;
        self.walk_modules_with(modules, &mut Self::gen_fundefs)?;
        println!("{:#?}", self.ctx);
        Ok(())
    }
    
    /// Generate forward definitions of all modules in the IR context and return a map of module
    /// names to more module IDs
    pub fn forward_modules(&mut self, modules: &[ParsedModule]) -> SemanticResult<HashMap<Symbol, ModuleId>> {
        fn forward_modules_for(ctx: &mut IRContext, list: &HashMap<Symbol, ParsedModule>) -> SemanticResult<HashMap<Symbol, ModuleId>> {
            let mut submods = HashMap::new();
            for submod in list.values() {
                let irmod_id = ctx.new_module(submod.name);
                ctx.modules[irmod_id].children = forward_modules_for(ctx, &submod.children)?;
                submods.insert(submod.name, irmod_id);
            }
            Ok(submods)
        }
        let mut modules_map = HashMap::new();
        for module in modules.iter() {
            let irmod_id = self.ctx.new_module(module.name);
            let children = forward_modules_for(&mut self.ctx, &module.children)?;
            self.ctx.modules[irmod_id].children = children;

            modules_map.insert(module.name, irmod_id);
        }
        Ok(modules_map)
    }
    
    /// Create Type entries for all type definitions in a module
    fn forward_types(&mut self, (parsed, id): (&ParsedModule, ModuleId)) -> SemanticResult<()> {
        //Keep a map of recorded definitions to check for duplicate names
        let mut remembered_defs: HashMap::<Symbol, Def>  = HashMap::new();
        
        for def in parsed.defs.values() {
            match def.data {
                DefData::StructDef { name, fields: _ }
                | DefData::EnumDef { name, variants: _ }
                | DefData::AliasDef { name, aliased: _ } => {
                    //Ensure that two type definitions don't have colliding names
                    if let Some(ref other_def) = remembered_defs.get(&name) {
                        self.diagnostic.emit(
                            Diagnostic::error(
                                format!("In module {}: two type definitions with the same name", self.symbol(parsed.name)), 
                                parsed.file
                            )
                            .with_span(def.span)
                            .with_span(other_def.span)
                        );
                        return Err(())
                    }
                    remembered_defs.insert(name, def.clone());

                    self.ctx.modules[id].typedefs.insert(name, self.ctx.invalid_id);
                },
                _ => (),
            }
        }
        Ok(())

    }
    
    /// Generate code for function declarations, leaving entry blocks as None
    fn gen_fundecls(&mut self, (parsedmod, mod_id): (&ParsedModule, ModuleId)) -> SemanticResult<()> {
        for def in parsedmod.defs.values() {
            match &def.data {
                DefData::FunDec(proto) | DefData::FunDef(proto, _) => {
                    let fun_id = self.ctx.new_fun(proto.name);
                    
                    let args = proto.args
                        .iter()
                        .map(|(name, ty)| match self.resolve_type(def.span, parsedmod.file, ty, mod_id) {
                            Ok(ty) => Ok((ty, Some(*name))),
                            Err(e) => Err(e)
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    let ret = self.resolve_type(def.span, parsedmod.file, &proto.return_ty, mod_id)?;
                    
                    let fun = &mut self.ctx.funs[fun_id];
                    fun.args = args;
                    fun.return_ty = ret;
                    drop(fun);
                    self.ctx.modules[mod_id].funs.insert(proto.name, fun_id);
                }
                _ => (),
            }
        }
        Ok(())
    }
    
    /// Generate function body ASTs for previously declared functions
    fn gen_fundefs(&mut self, (parsedmod, mod_id): (&ParsedModule, ModuleId)) -> SemanticResult<()> {
        for def in parsedmod.defs.values() {
            match &def.data {
                DefData::FunDef(proto, body) => {
                    let mut scope = ScopeMap::new();
                    let fun_id = *self.ctx.modules[mod_id].funs.get(&proto.name).expect("Forward declarations already created");
                    let body = self.resolve_ast_body(mod_id, parsedmod.file, &mut scope, body.to_owned())?;
                    self.ctx.funs[fun_id].body = Some(body);
                },
                _ => ()
            }
        }
        Ok(())
    }
    
    /// Transform the AST by resolving all types to their type IDs, creating variable Ids for
    /// variables instead of names
    fn resolve_ast(
        &mut self, 
        module: ModuleId, 
        file: FileId, 
        ast: Ast, 
        scope: &mut ScopeMap<Symbol, VarId>, 
        type_hint: Option<TypeId>
    ) -> SemanticResult<Node> {
        Ok(match ast.node {
            AstNode::Access(name) => match scope.get(&name.last()) {
                Some(var) => Node::VarAccess(*var),
                None => match self.ctx.get_fun(module, &name) {
                    Some(fun) => Node::FunAccess(fun),
                    None => {
                        self.diagnostic.emit(Diagnostic::error(format!("Unkown function or variable {}", self.display_path(&name)), file)
                            .with_span(ast.span)
                        );
                        return Err(())
                    }
                }
            },
            AstNode::FunCall(called, args) => {
                let args = args
                    .into_iter()
                    .map(|ast| self.resolve_ast(module, file, ast, scope, None))
                    .collect::<Result<Vec<_>, _>>()?;
                Node::Call {
                    fun_expr: Box::new(self.resolve_ast(module, file, *called, scope, None)?),
                    args,
                }
            },
            AstNode::Assignment {
                lhs,
                rhs
            } => Node::Assign { 
                dest: Box::new(self.resolve_ast(module, file, *lhs, scope, self.get_ast_ty(&scope, &rhs.node))?),
                src: Box::new(self.resolve_ast(
                        module, 
                        file, 
                        *rhs, 
                        scope,
                        self.get_ast_ty(&scope, &lhs.node)
                    )?)
            },
            AstNode::BinExpr(lhs, op, rhs) => Node::Bin(
                Box::new(self.resolve_ast(module, file, *lhs, scope, self.get_ast_ty(&scope, &rhs.node))?),
                op,
                Box::new(self.resolve_ast(module, file, *rhs, scope, self.get_ast_ty(&scope, &lhs.node))?)
            ),
            AstNode::UnaryExpr(op, rhs) => Node::Unary(
                op,
                Box::new(self.resolve_ast(module, file, *rhs, scope, type_hint)?)
            ),
            AstNode::CastExpr(ty, expr) => {
                let type_id = self.resolve_type(ast.span, file, &ty, module)?;
                Node::Cast(type_id, Box::new(self.resolve_ast(module, file, *expr, scope, None)?))
            },
            AstNode::IfExpr(if_expr) => Node::If(Box::new(self.resolve_if_ast(module, file, scope, if_expr)?)),
            AstNode::MemberAccess(expr, member) => Node::FieldAccess(
                Box::new(self.resolve_ast(module, file, *expr, scope, None)?),
                member
            ),
            AstNode::PhiExpr(phi) => Node::Phi(Box::new(self.resolve_ast(module, file, *phi, scope, type_hint)?)),
            AstNode::Return(returned) => Node::Return(Box::new(self.resolve_ast(module, file, *returned, scope, type_hint)?)),
            AstNode::NumberLiteral(n) => Node::NumberLiteral(n),
            AstNode::StringLiteral(s) => Node::StringLiteral(s),
            AstNode::BooleanLiteral(b) => Node::BooleanLiteral(b),
            AstNode::TupleLiteral(exprs) => {
                let exprs = self.resolve_ast_body(module, file, scope, exprs)?;
                Node::TupleLiteral(exprs)
            },
            AstNode::ArrayLiteral(exprs) => {
                let exprs = self.resolve_ast_body(module, file, scope, exprs)?;
                Node::ArrayLiteral(exprs)
            },
            AstNode::VarDeclaration {
                name,
                ty,
                mutable
            } => {
                let typeid = match ty.map(|ty| self.resolve_type(ast.span, file, &ty, module)) {
                    Some(id) => id?,
                    None => match type_hint {
                        Some(id) => id,
                        None => {
                            self.diagnostic.emit(Diagnostic::error(
                                    format!("No given or inferred type for variable '{}'", name), 
                                    file
                                )
                                .with_span(ast.span)
                            );
                            return Err(())
                        }
                    }
                };

                let var_id = self.ctx.new_var(typeid);
                scope.define(name, var_id);
                Node::VarDec {
                    id: var_id,
                    ty: typeid,
                    mutable,
                }
            },
            _ => unimplemented!("AST conversion for {:?} not implemented", ast.node)
        })
    }
    
    /// Lower an if AST and its else-if / else clauses
    fn resolve_if_ast(&mut self, module: ModuleId, file: FileId, scope: &mut ScopeMap<Symbol, VarId>, if_expr: IfExpr) -> SemanticResult<IfNode> {
        let cond = self.resolve_ast(module, file, *if_expr.cond, scope, Some(self.ctx.bool_id))?;
        let body = self.resolve_ast_body(module, file, scope, if_expr.body)?;
        let else_node = match if_expr.else_expr {
            Some(ElseExpr::Else(body)) => 
                Some(Box::new(ElseNode::Else(self.resolve_ast_body(module, file, scope, body)?))),
            Some(ElseExpr::ElseIf(if_expr)) => 
                Some(Box::new(ElseNode::ElseIf(self.resolve_if_ast(module, file, scope, *if_expr)?))),
            None => None
        };
        Ok(IfNode {
            cond,
            body,
            else_node
        })
    }
    
    /// Lower each AST statement in a body
    fn resolve_ast_body(&mut self, module: ModuleId, file: FileId, scope: &mut ScopeMap<Symbol, VarId>, body: Vec<Ast>) -> SemanticResult<Vec<Node>> {
        body
            .into_iter()
            .map(|expr| self.resolve_ast(module, file, expr, scope, None))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Resolve all previously forward declared types with definitions
    fn resolve_types(&mut self, (parsed, id): (&ParsedModule, ModuleId)) -> SemanticResult<()> {
        for def in parsed.defs.values() {
            match &def.data {
                DefData::StructDef {
                    name,
                    fields
                } => {
                    let struct_id = *self.ctx.modules[id].typedefs.get(name).expect("Forward declarations created for all types");
                    let struct_ty = TypeData::Struct {
                        fields: fields
                            .iter()
                            .map(|(name, ty)| match self.resolve_type(def.span, parsed.file, ty, id) {
                                Ok(ty) => Ok((*name, ty)),
                                Err(e) => Err(e)
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    };
                    self.ctx.types[struct_id].data = struct_ty;
                },
                DefData::EnumDef {
                    name,
                    variants
                } => {
                    let enum_id = *self
                        .ctx
                        .modules[id]
                        .typedefs
                        .get(name)
                        .expect("Forward declarations already declared");
                    let enum_ty = TypeData::Enum {
                        variants: variants
                            .iter()
                            .map(|variant| self.resolve_type(def.span, parsed.file, variant, id))
                            .collect::<Result<Vec<_>, _>>()?
                    };
                    self.ctx.types[enum_id].data = enum_ty;
                },
                DefData::AliasDef {
                    name,
                    aliased
                } => {
                    let alias_id = *self
                        .ctx
                        .modules[id]
                        .typedefs
                        .get(name)
                        .expect("Forward declarations already declared");
                    let alias_ty = TypeData::Alias(self.resolve_type(def.span, parsed.file, aliased, id)?);
                    self.ctx.types[alias_id].data = alias_ty;
                }
                _ => ()
            }
        }
        Ok(())
    }
        
    /// Attempt to infer the type of an AST node 
    fn get_ast_ty(&self, module: ModuleId, scope: &ScopeMap<Symbol, VarId>, ast: &AstNode) -> Option<TypeId> {
        match ast {
            AstNode::PhiExpr(phi) => self.get_ast_ty(scope, &phi.node),
            AstNode::NumberLiteral(n) => if let Some(annotation) = n.annotation() {
                match annotation {
                    NumberLiteralAnnotation::U8 => Some(self.ctx.u_ids[IntegerWidth::Eight as u8 as usize]),
                    NumberLiteralAnnotation::U16 => Some(self.ctx.u_ids[IntegerWidth::Sixteen as u8 as usize]),
                    NumberLiteralAnnotation::U32 => Some(self.ctx.u_ids[IntegerWidth::ThirtyTwo as u8 as usize]),
                    NumberLiteralAnnotation::U64 => Some(self.ctx.u_ids[IntegerWidth::SixtyFour as u8 as usize]),
                    
                    NumberLiteralAnnotation::I8 => Some(self.ctx.i_ids[IntegerWidth::Eight as u8 as usize]),
                    NumberLiteralAnnotation::I16 => Some(self.ctx.i_ids[IntegerWidth::Sixteen as u8 as usize]),
                    NumberLiteralAnnotation::I32 => Some(self.ctx.i_ids[IntegerWidth::ThirtyTwo as u8 as usize]),
                    NumberLiteralAnnotation::I64 => Some(self.ctx.i_ids[IntegerWidth::SixtyFour as u8 as usize]),

                    NumberLiteralAnnotation::F32 => Some(self.ctx.f32_id),
                    NumberLiteralAnnotation::F64 => Some(self.ctx.f64_id),
                }
            } else {
                match n {
                    NumberLiteral::Integer(..) => Some(self.ctx.i_ids[IntegerWidth::ThirtyTwo as u8 as usize]),
                    NumberLiteral::Float(..) => Some(self.ctx.f64_id),
                }
            },
            AstNode::StringLiteral(_) => {
                let id = self.ctx.new_type();
                let ptr_ty = TypeData::Pointer(self.ctx.u_ids[IntegerWidth::Eight as u8 as usize]);
                self.ctx.types[id].data = ptr_ty;
                Some(id)
            },
            AstNode::Access(path) => match scope.get(&path.last()) {
                Some(var_id) => Some(self.ctx.vars[*var_id].ty),
                None => match self.ctx.get_fun(module, path) {
                    Some(fun) => {
                        let fun_ty = self.ctx.new_type();
                        self.ctx.types[fun_ty].data = self.ctx.funs[fun].fun_type();
                        Some(fun_ty)
                    },
                    None => None,
                }
            },
            AstNode::BooleanLiteral(_) => Some(self.ctx.bool_id),
        }
    }

    /// Walk a module and any submodules 
    fn walk_module_with<R>(
        &mut self, 
        module: &ParsedModule, 
        f: &mut dyn FnMut(&mut Self, (&ParsedModule, ModuleId)) -> SemanticResult<R>
    ) -> SemanticResult<R>{
        let id = self.forward_modules[&module.name];
        for submodule in module.children.values() {
           self.walk_module_with(submodule, f)?; 
        }
        f(self, (module, id))
    }

    /// Walk all modules using the specified function
    fn walk_modules_with<R>(
        &mut self,
        modules: &[ParsedModule],
        f: &mut dyn FnMut(&mut Self, (&ParsedModule, ModuleId)) -> SemanticResult<R>
    ) -> SemanticResult<R> {
        for module in modules.iter().skip(1) {
            self.walk_module_with(module, f)?;
        }
        self.walk_module_with(&modules[0], f)
    }
    
    /// Resolve a type in the context of the given module
    fn resolve_type(&mut self, span: Span, file_id: FileId, ty: &UnresolvedType, module: ModuleId) -> SemanticResult<TypeId> {
        Ok(match ty {
            UnresolvedType::Integer { width, signed } => match signed {
                true => self.ctx.i_ids[*width as u8 as usize],
                false => self.ctx.u_ids[*width as u8 as usize],
            },
            UnresolvedType::Bool => self.ctx.bool_id,
            UnresolvedType::Unit => self.ctx.unit_id,
            UnresolvedType::Fun(unresolved_fun_ty) => {
                let fun_ty_id = self.ctx.new_type();
                let return_ty = self.resolve_type(span, file_id, &unresolved_fun_ty.return_ty, module)?;
                let args = unresolved_fun_ty.arg_tys.iter()
                        .map(|ty| self.resolve_type(span, file_id, ty, module))
                        .collect::<Result<Vec<_>, _>>()?;

                let fun_ty = &mut self.ctx.types[fun_ty_id];
                fun_ty.data = TypeData::Fun {
                    return_ty, 
                    args,
                };
                fun_ty_id
            },
            UnresolvedType::Float { doublewide } => if *doublewide { self.ctx.f64_id } else { self.ctx.f32_id },
            UnresolvedType::Array { elements, len } => {
                let array_type_id = self.ctx.new_type();
                let elements = self.resolve_type(span, file_id, elements, module)?;
                let array_ty = &mut self.ctx.types[array_type_id];
                array_ty.data = TypeData::Array {
                    elements,                   
                    len: *len,
                };
                array_type_id
            },
            UnresolvedType::Tuple { elements } => {
                let tuple_type_id = self.ctx.new_type();
                let elements =  elements.iter()
                        .map(|ty| self.resolve_type(span, file_id, ty, module))
                        .collect::<Result<Vec<_>, _>>()?;

                let tuple_type = &mut self.ctx.types[tuple_type_id];
                tuple_type.data = TypeData::Tuple { 
                    elements,
                };
                tuple_type_id
            },
            UnresolvedType::Pointer(to) => {
                let pointer_ty_id = self.ctx.new_type();
                let pointee_ty = self.resolve_type(span, file_id, &*to, module)?;
                let pointer_ty = &mut self.ctx.types[pointer_ty_id];
                pointer_ty.data = TypeData::Pointer(pointee_ty);
                pointer_ty_id
            },
            UnresolvedType::UserDefined { name } => {
                match self.ctx.get_type(module, name) {
                    Some(id) => id,
                    None => {
                        self.diagnostic.emit(Diagnostic::error(
                                format!("Unknown type name '{}'", self.display_path(name)),
                                file_id
                            )
                            .with_span(span)
                        );
                        return Err(())
                    }
                }
            }
        })
    }

    fn display_path(&self, path: &SymbolPath) -> String {
        let mut display = String::new();
        for (i, part) in path.iter().enumerate() {
            display.push_str(&part);
            if i != path.len() - 1 {
                display.push(':');
            }
        }
        display
    }

}
