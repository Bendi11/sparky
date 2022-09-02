//! Module containing definitions for structures containing all state needed to lower a parsed
//! abstract syntax tree to spark IR instructions

use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};
use hashbrown::HashMap;

use crate::{
    arena::{Arena, Index},
    ast::{
        DefData, FunFlags, ParsedModule, PathIter, SymbolPath, UnresolvedFunType,
        UnresolvedType, FunDef, Stmt, Expr,
    },
    util::{
        files::FileId,
        loc::Span,
    },
    Symbol,
};

use super::{
    types::{FunType, IrFloatType, IrIntegerType, IrStructField, IrStructType, IrType},
    BBId, FunId, IrContext, IrFun, TypeId, VarId, GlobalId, IrGlobal, IrBB, IrTerminator, value::{IrExpr, IrExprKind, IrLiteral}, IrBody, IrStmt, IrStmtKind,
};

pub mod ast;
pub mod op;

/// Structure containing all needed state to lower parsed ASTs into spark's IR, performing type
/// checking and resolution
pub struct IrLowerer<'ctx> {
    /// IR context containing all definitions
    ctx: &'ctx mut IrContext,
    /// The root intermediate module, to be populated with definition data during lowering
    root_module: IntermediateModuleId,
    /// All intermediate modules
    modules: Arena<IntermediateModule>,
    /// Stack representing the current scope
    scope_stack: Vec<ScopePlate>,
    /// Function for setting up global values
    global_setup_fun: FunId,
    /// Function for checking an expression's validity
    tmp_fun: FunId,
    /// Current basic block to generate code in
    bb: Option<BBId>,
}


/// Represents a type of scope that we are currently in, used to represent the nested
/// scope structure of programs with ifs, loops, etc.
pub struct ScopePlate {
    /// Variables defined in this scope
    vars: HashMap<Symbol, VarId>,
    /// Stack allocation to store the phi or return value of the block in
    return_var: Option<VarId>,
    /// Block to exit to after this one is done or a break / phi / return statement is encountered
    after_bb: BBId,
}

/// Index into the `modules` field of an [IrLowerer]
pub type IntermediateModuleId = Index<IntermediateModule>;

/// Enum that points to a [TypeId] or [FunId], used by the [IntermediateModule]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntermediateDefId {
    /// ID of a defined type
    Type(TypeId, FileId, Span),
    /// Function definition or declaration
    Fun(FunId, FileId, Span),
    /// Child module
    Module(IntermediateModuleId),
    /// Global value
    Global(GlobalId, FileId, Span),
}

/// Data only used by the [IrLowerer] in order to save what symbols are defined in each
/// [ParsedModule](crate::ast::ParsedModule)
pub struct IntermediateModule {
    /// Map of defined symbols to their IDs in the [IrContext]
    pub defs: HashMap<Symbol, IntermediateDefId>,
    /// Name of this module
    pub name: Symbol,
}

impl<'ctx> IrLowerer<'ctx> {
    /// Create a new IR lowerer that writes to the given IR context
    pub fn new(ctx: &'ctx mut IrContext, name: Symbol) -> Self {
        let mut modules = Arena::new();
        let root_module = modules.insert(IntermediateModule::new(name));
        
        let setup_ty = FunType {
            return_ty: IrContext::UNIT,
            params: vec![]
        };
        
        let entry = IrBB {
            stmts: vec![],
            terminator: IrTerminator::Return(IrExpr {
                span: Span::from(0..0),
                kind: IrExprKind::Lit(IrLiteral::Unit),
                ty: IrContext::UNIT,
            }),
        };
        let setup = IrFun {
            name: Symbol::from("__global_setup"),
            file: unsafe { FileId::from_raw(0) },
            span: Span::from(0..0),
            ty: setup_ty.clone(),
            ty_id: ctx.types.insert(IrType::Fun(setup_ty.clone())),
            body: None,
            flags: FunFlags::empty(),
        };
        
        let tmp = IrFun {
            name: Symbol::from("__tmp"),
            file: unsafe { FileId::from_raw(0) },
            span: Span::from(0..0),
            ty: setup_ty.clone(),
            ty_id: ctx.types.insert(IrType::Fun(setup_ty)),
            body: None,
            flags: FunFlags::empty(),
        };

        let global_setup_fun = ctx.funs.insert(setup);
        ctx.funs[global_setup_fun].body = Some(IrBody {
            parent: global_setup_fun,
            entry: ctx.bbs.insert(entry.clone()),
            args: vec![]
        });  

        let tmp_fun = ctx.funs.insert(tmp);
        ctx.funs[tmp_fun].body = Some(IrBody {
            parent: tmp_fun,
            entry: ctx.bbs.insert(entry),
            args: vec![]
        });

        Self {
            ctx,
            root_module,
            modules,
            global_setup_fun,
            scope_stack: Vec::new(),
            tmp_fun,
            bb: None,
        }
    }

    /// Lower a parsed module to IR
    pub fn lower(&mut self, root: &ParsedModule) -> Result<(), Diagnostic<FileId>> {
        self.populate_forward_modules_impl(self.root_module, root)?;
        self.populate_forward_types_impl(self.root_module, root)?;
        self.populate_imported_forward(self.root_module, root)?;
        self.populate_global_forwards_impl(self.root_module, root)?;
        self.populate_defs_impl(self.root_module, root)?;
        self.populate_global_defs_impl(self.root_module, root)?;
        self.populate_fn_bodies_impl(self.root_module, root)?;

        Ok(())
    }

    /// Get the basic block that code is being generated in
    pub fn bb(&self) -> BBId {
        self.bb
            .expect("ICE: IR lowerer is not currently in a basic block")
    }

    /// Get the basic block that code is being generated in
    pub fn bb_mut(&mut self) -> &mut BBId {
        self.bb
            .as_mut()
            .expect("ICE: IR lowerer is not currently in a basic block")
    }
   
    fn populate_forward_modules_impl(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        for child_parsed in parsed.children.iter() {
            let child_module = self
                .modules
                .insert(IntermediateModule::new(child_parsed.name));
            self.modules[module].defs.insert(
                child_parsed.name.clone(),
                IntermediateDefId::Module(child_module),
            );
            self.modules[child_module].defs.insert(
                Symbol::from("up"),
                IntermediateDefId::Module(module)
            );
            self.modules[child_module].defs.insert(
                Symbol::from("root"),
                IntermediateDefId::Module(self.root_module),
            );
            self.populate_forward_modules_impl(child_module, child_parsed)?;
        }

        //Create forward references for imported modules
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::ImportDef { name } => match self.resolve_path(module, name) {
                    Some(id) => {
                        self.modules[module].defs.insert(name.last(), id);
                    }
                    _ => (),
                },
                _ => (),
            }
        }


        Ok(())
    }

    /// Get forward references to all declared types and modules
    fn populate_forward_types_impl(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::AliasDef { name, aliased } => {
                    let ty = self.ctx.types.insert_nointern(IrType::Invalid);

                    let id = IntermediateDefId::Type(ty, def.file, def.span);
                    self.ensure_no_double(module, def.file, def.span, id, *name)?;
                    self.modules[module]
                        .defs
                        .insert(name.clone(), id);

                }
                _ => (),
            }
        }

        for child_parsed in parsed.children.iter() {
            if let IntermediateDefId::Module(child_module) = self.modules[module].defs[&child_parsed.name] {
                self.populate_forward_types_impl(child_module, child_parsed)?;
            } else { panic!() }
        }


        Ok(())
    }
    
    /// Generiate forward references for all imported symbols
    fn populate_imported_forward(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        //Create forward references for imported types
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::ImportDef { name } => match self.resolve_path(module, name) {
                    Some(id) => {
                        self.ensure_no_double(module, def.file, def.span, id, name.last())?;
                        self.modules[module].defs.insert(name.last(), id);
                    },
                    _ => (),
                },
                _ => (),
            }
        }

        for child in parsed.children.iter() {
            if let IntermediateDefId::Module(module_id) = self.modules[module].defs[&child.name] {
                self.populate_imported_forward(module_id, child)?;
            }
        }

        Ok(())
    }

    /// Lower the bodies of all functions to IR
    fn populate_fn_bodies_impl(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::FunDef(FunDef { proto, body, .. }) => {
                    let def_id = self.modules[module].defs[&proto.name];
                    if let IntermediateDefId::Fun(fun, ..) = def_id {
                        self.lower_body(module, def.file, fun, body)?;
                    } else {
                        panic!("Internal compiler error: definition id for symbol {} should be a function, but isn't", proto.name);
                    }
                }
                _ => (),
            }
        }

        for child_parsed in parsed.children.iter() {
            let child_module = match self.modules[module].defs.get(&child_parsed.name).unwrap() {
                IntermediateDefId::Module(module) => *module,
                _ => unreachable!(),
            };
            self.populate_fn_bodies_impl(child_module, child_parsed)?;
        }

        Ok(())
    }
   
    fn populate_global_forwards_impl(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::Global { name, comptime, val, ty } => {
                    let global = IrGlobal {
                        ty: IrContext::INVALID,
                        name: name.last(),
                    };

                    let global_id = self.ctx.globals.insert(global);
                    
                    let id = IntermediateDefId::Global(global_id, def.file, def.span);
                    self
                        .modules[module]
                        .defs
                        .insert(name.last(), id);
                    
                    if name.len() == 1 {
                        self.ensure_no_double(module, def.file, def.span, id, name.last().clone())?;
                    }
                },
                _ => (),
            }
        }

        for child_parsed in parsed.children.iter() {
            let child_module = match self.modules[module].defs.get(&child_parsed.name).unwrap() {
                IntermediateDefId::Module(module) => *module,
                _ => unreachable!(),
            };
            self.populate_global_forwards_impl(child_module, child_parsed)?;
        }

        Ok(())

    }
   
    /// Populate the definitions of all defined global variables
    fn populate_global_defs_impl(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        self.bb = Some(self.ctx[self.global_setup_fun].body.as_ref().unwrap().entry);
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::Global { name, comptime: _, val, ty, .. } => {
                    let glob = if let IntermediateDefId::Global(glob, ..) = *self.modules[module].defs.get(&name.last()).unwrap_or_else(|| panic!("ICE: cannot find global named {}", name)) {
                        glob
                    } else {
                        unreachable!()
                    };
                   

                    let ty = match val {
                        Some(expr) => {
                            self.bb = Some(self.ctx[self.global_setup_fun].body.as_ref().unwrap().entry);
                            let expr = self.lower_expr(module, def.file, self.global_setup_fun, expr)?;
                            self.ctx[self.bb.unwrap()].stmts.push(IrStmt {
                                span: Span::from(0..0),
                                kind: IrStmtKind::Write {
                                    ptr: IrExpr {
                                        span: Span::from(0..0),
                                        ty: expr.ty,
                                        kind: IrExprKind::Global(glob),
                                    },
                                    val: expr.clone(),
                                }
                            });


                            expr.ty                        },
                        None => match ty {
                            Some(ty) => self.resolve_type(ty, module, def.file, def.span)?,
                            None => return Err(Diagnostic::error()
                                .with_message(format!("Global with no declared type or assigned value"))
                                .with_labels(vec![
                                    Label::primary(def.file, def.span)
                                ])
                            )
                        }
                    };

                    self.ctx.globals[glob].ty = ty;
                }
                _ => (),
            }
        }

        for child_parsed in parsed.children.iter() {
            let child_module = match self.modules[module].defs.get(&child_parsed.name).unwrap() {
                IntermediateDefId::Module(module) => *module,
                _ => unreachable!(),
            };
            self.populate_global_defs_impl(child_module, child_parsed)?;
        }

        Ok(())
    }

    /// Populate all type definitions and function declaratations
    fn populate_defs_impl(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::AliasDef { name, aliased } => {
                    let ty = *self.modules[module].defs.get(name).unwrap_or_else(|| panic!("ICE: Cannot find definition named {}", name));
                    match ty {
                        IntermediateDefId::Type(ty, ..) => {
                            let resolved = self.resolve_type(aliased, module, def.file, def.span)?;
                            *self.ctx.types.get_mut(ty) = IrType::Alias {
                                name: name.clone(),
                                ty: resolved,
                            };
                        }
                        _ => unreachable!("{}", IntermediateDefFormatter { lower: self, id: ty }),
                    }
                }
                DefData::FunDec(proto) | DefData::FunDef(FunDef { proto, .. }) => {
                    let fun_ty = self.resolve_fn_type(&proto.ty, module, def.file, def.span)?;
                    let fun = IrFun {
                        file: def.file,
                        span: def.span,
                        name: proto.name.clone(),
                        ty_id: self.ctx.types.insert(IrType::Fun(fun_ty.clone())),
                        ty: fun_ty,
                        body: None,
                        flags: proto.flags,
                    };

                    if fun.flags.contains(FunFlags::EXTERN) {
                        for other in self.ctx.funs.iter() {
                            if other.flags.contains(FunFlags::EXTERN) && other.name == fun.name {
                                return Err(Diagnostic::error()
                                    .with_message(format!("Two external functions declared with the same name '{}'", fun.name))
                                    .with_labels(vec![
                                        Label::primary(other.file, other.span)
                                            .with_message("First external function appears here"),
                                        Label::secondary(fun.file, fun.span)
                                            .with_message("Second external function appears here")
                                    ])
                                    .with_notes(vec!["Functions marked as external will appear in the final object file with their original name".to_owned()])
                                );
                            }
                        }
                    }

                    let fun = self.ctx.funs.insert(fun);
                    let id = IntermediateDefId::Fun(fun, def.file, def.span);
                    self.modules[module]
                        .defs
                        .insert(proto.name.clone(), id);
                    self.ensure_no_double(module, def.file, def.span, id, proto.name)?;
                }
                _ => (),
            }
        }

        for child_parsed in parsed.children.iter() {
            let child_module = match self.modules[module].defs.get(&child_parsed.name).unwrap() {
                IntermediateDefId::Module(module) => *module,
                _ => unreachable!(),
            };
            self.populate_defs_impl(child_module, child_parsed)?;
        }

        //Resolve all imports, including function imports
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::ImportDef { name } => match self.resolve_path(module, name) {
                    Some(id) => {
                        self.ensure_no_double(module, def.file, def.span, id, name.last())?;
                        self.modules[module].defs.insert(name.last(), id);
                    }
                    None => {
                        return Err(Diagnostic::error()
                            .with_message(format!("Imported symbol {} not found", name))
                            .with_labels(vec![Label::new(LabelStyle::Primary, def.file, def.span)
                                .with_message("Import declaration here")])
                            .with_notes(vec![format!("In module {}", self.modules[module].name)]))
                    }
                },
                _ => (),
            }
        }

        Ok(())
    }

    /// Resolve a parsed type into a concrete type id
    fn resolve_type(
        &mut self,
        ty: &UnresolvedType,
        module: IntermediateModuleId,
        //parsed: &ParsedModule,
        file: FileId,
        span: Span,
    ) -> Result<TypeId, Diagnostic<FileId>> {
        Ok(match ty {
            UnresolvedType::Integer { width, signed } => IrContext::itype(*signed, *width),
            UnresolvedType::Float { doublewide } => self.ctx.types.insert(
                IrType::Float(IrFloatType {
                    doublewide: *doublewide,
                })
                .into(),
            ),
            UnresolvedType::Char => IrContext::CHAR,
            UnresolvedType::Pointer(ptr) => {
                let ty = self.resolve_type(ptr, module, file, span)?;
                self.ctx.types.insert(IrType::Ptr(ty))
            }
            UnresolvedType::Array { elements, len } => {
                let element = self.resolve_type(elements, module, file, span)?;
                self.ctx.types.insert(IrType::Array(element, *len))
            }
            UnresolvedType::Unit => IrContext::UNIT,
            UnresolvedType::Bool => IrContext::BOOL,
            UnresolvedType::Enum { variants } => {
                let variants = variants
                    .iter()
                    .map(|variant| self.resolve_type(variant, module, file, span))
                    .collect::<Result<Vec<_>, _>>()?;
                self.ctx.types.insert(IrType::Sum(variants).into())
            }
            UnresolvedType::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(
                        |(field, name)| match self.resolve_type(field, module, file, span) {
                            Ok(field) => Ok(IrStructField {
                                name: *name,
                                ty: field,
                            }),
                            Err(e) => Err(e),
                        },
                    )
                    .collect::<Result<Vec<_>, _>>()?;
                self.ctx
                    .types
                    .insert(IrType::Struct(IrStructType { fields }).into())
            }
            UnresolvedType::UserDefined { name } => match self.resolve_path(module, name) {
                Some(IntermediateDefId::Type(ty, ..)) => ty,
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Type {} not found in module {}",
                                name, self.modules[module].name
                            ))
                            .with_labels(vec![Label::new(LabelStyle::Primary, file, span)]))
                },
            },
            UnresolvedType::Fun(ty) => {
                let fn_ty = self.resolve_fn_type(ty, module, file, span)?;
                self.ctx.types.insert(IrType::Fun(fn_ty))
            }
        })
    }

    /// Resolve a function type, split into another function to be used when generating forward
    /// references for function declarations
    fn resolve_fn_type(
        &mut self,
        ty: &UnresolvedFunType,
        module: IntermediateModuleId,
        file: FileId,
        span: Span,
    ) -> Result<FunType, Diagnostic<FileId>> {
        let return_ty = self.resolve_type(&ty.return_ty, module, file, span)?;
        let params = ty
            .arg_tys
            .iter()
            .map(
                |(ty, name)| match self.resolve_type(ty, module, file, span) {
                    Ok(ty) => Ok((ty, name.clone())),
                    Err(e) => Err(e),
                },
            )
            .collect::<Result<Vec<_>, _>>()?;

        Ok(FunType { return_ty, params })
    }
    
    /// Ensure that the given definition with name is not defined twice in the module
    fn ensure_no_double(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        span: Span,
        id: IntermediateDefId,
        def: Symbol,
    ) -> Result<(), Diagnostic<FileId>> {
        if def == self.modules[module].name {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "Definition with name {} conflicts with module name {}",
                    def,
                    self.modules[module].name,
                ))
                .with_labels(vec![
                    Label::primary(file, span)
                        .with_message(format!(
                            "{} defined here",
                            def,
                        ))
                ])
            )
        }
        for (name, other) in self.modules[module].defs.iter() {
            if *other != id && *name == def {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "{} collides with previously-defined {}",
                        def,
                        IntermediateDefFormatter { lower: self, id: *other },
                    ))
                    .with_labels(vec![
                        Label::primary(file, span)
                            .with_message(format!("{} is defined here", def)),
                        match *other {
                            IntermediateDefId::Type(_, file, span)
                            | IntermediateDefId::Fun(_, file, span)
                            | IntermediateDefId::Global(_, file, span) => Label::secondary(file, span),
                            IntermediateDefId::Module(_) => Label::secondary(file, span),
                        }.with_message(format!(
                            "Conflicts with {}",
                            IntermediateDefFormatter { lower: self, id: *other }
                        ))
                    ])
                ) 
            }
        }

        Ok(())
    }

    pub fn def_name(&self, def: IntermediateDefId) -> Symbol {
        match def {
            IntermediateDefId::Type(ty, ..) => Symbol::from(&self.ctx.typename(ty).to_string()),
            IntermediateDefId::Fun(fun, ..) => self.ctx[fun].name,
            IntermediateDefId::Module(m) => self.modules[m].name,
            IntermediateDefId::Global(g, ..) => self.ctx[g].name,
        }
    }

    /// Resolve the path in the context of the given intermediate module
    #[inline]
    fn resolve_path(
        &self,
        module: IntermediateModuleId,
        path: &SymbolPath,
    ) -> Option<IntermediateDefId> {
        self.resolve_path_impl(module, path.iter())
    }

    /// Attempt to resolve the given path in this module
    fn resolve_path_impl(
        &self,
        module: IntermediateModuleId,
        mut path: PathIter,
    ) -> Option<IntermediateDefId> {
        let next = path.next().unwrap();
        let next = self.modules[module].defs.get(&next);
        match path.len() {
            0 => next.copied(),
            _ => match next {
                Some(IntermediateDefId::Module(other)) => self.resolve_path_impl(*other, path),
                _ => None,
            },
        }
    }
}

impl IntermediateModule {
    /// Create a new empty `IntermediateModule` from a name symbol
    pub fn new(name: Symbol) -> Self {
        Self {
            defs: HashMap::new(),
            name,
        }
    }
}

pub struct IntermediateDefFormatter<'lower, 'ctx> {
    lower: &'lower IrLowerer<'ctx>,
    id: IntermediateDefId,
}

impl<'lower, 'ctx> std::fmt::Display for IntermediateDefFormatter<'lower, 'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.id {
            IntermediateDefId::Type(ty, ..) => write!(f, "type {}", self.lower.ctx.typename(ty)),
            IntermediateDefId::Fun(fun, ..) => write!(f, "function {}", self.lower.ctx[fun].name),
            IntermediateDefId::Module(m) => write!(f, "module {}", self.lower.modules[m].name),
            IntermediateDefId::Global(g, ..) => write!(f, "global {}", self.lower.ctx[g].name),
        }
    }
}
