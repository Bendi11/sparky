//! Module containing definitions for structures containing all state needed to lower a parsed
//! abstract syntax tree to spark IR instructions

use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};
use hashbrown::HashMap;

use crate::{
    arena::{Arena, Index},
    ast::{
        DefData, FunProto, ParsedModule, PathIter, SymbolPath, UnresolvedFunType, UnresolvedType,
    },
    util::{
        files::{FileId, Files},
        loc::Span,
    },
    Symbol,
};

use super::{
    types::{
        array::IrArrayType, float::IrFloatType, fun::IrFunType, integer::IrIntegerType,
        structure::IrStructType, sum::IrSumType, IrType,
    },
    BBId, FunId, IrContext, IrFun, TypeId, VarId,
};

pub mod ast;

/// Structure containing all needed state to lower parsed ASTs into spark's IR, performing type
/// checking and resolution
pub struct IrLowerer<'files, 'ctx> {
    /// Arena containing all files indexed by ID
    files: &'files Files,
    /// IR context containing all definitions
    ctx: &'ctx mut IrContext,
    /// The root intermediate module, to be populated with definition data during lowering
    root_module: IntermediateModuleId,
    /// All intermediate modules
    modules: Arena<IntermediateModule>,
    /// Stack representing the current scope
    scope_stack: Vec<ScopePlate>,
    /// Current function we are lowering
    current_fun: Option<FunId>,
}

/// Represents a type of scope that we are currently in, used to represent the nested
/// scope structure of programs with ifs, loops, etc.
pub struct ScopePlate {
    /// Variables defined in this scope
    vars: HashMap<Symbol, VarId>,
    /// Stack allocation to store the phi or return value of the block in
    return_var: Option<VarId>,
    /// Block to exit to after this one is done or a break / phi statement is encountered
    after_bb: BBId,
}

/// Index into the `modules` field of an [IrLowerer]
pub type IntermediateModuleId = Index<IntermediateModule>;

/// Enum that points to a [TypeId] or [FunId], used by the [IntermediateModule]
#[derive(Clone, Copy, Debug)]
pub enum IntermediateDefId {
    /// ID of a defined type
    Type(TypeId),
    /// Function definition or declaration
    Fun(FunId),
    /// Child module
    Module(IntermediateModuleId),
}

/// Data only used by the [IrLowerer] in order to save what symbols are defined in each
/// [ParsedModule](crate::ast::ParsedModule)
pub struct IntermediateModule {
    /// Map of defined symbols to their IDs in the [IrContext]
    pub defs: HashMap<Symbol, IntermediateDefId>,
    /// Name of this module
    pub name: Symbol,
}

impl<'files, 'ctx> IrLowerer<'files, 'ctx> {
    /// Create a new IR lowerer that writes to the given IR context
    pub fn new(files: &'files Files, ctx: &'ctx mut IrContext, name: Symbol) -> Self {
        let mut modules = Arena::new();
        let root_module = modules.insert(IntermediateModule::new(name));

        Self {
            files,
            ctx,
            root_module,
            modules,
            scope_stack: Vec::new(),
            current_fun: None,
        }
    }

    /// Lower a parsed module to IR
    pub fn lower(&mut self, root: &ParsedModule) -> Result<(), Diagnostic<FileId>> {
        self.populate_forward_types_impl(self.root_module, root)?;
        self.populate_defs_impl(self.root_module, root)?;

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
                DefData::AliasDef { name, aliased: _ } => {
                    let ty = self.ctx.types.insert_nointern(IrType::Invalid);
                    self.modules[module]
                        .defs
                        .insert(name.clone(), IntermediateDefId::Type(ty));
                }
                _ => (),
            }
        }

        for child_parsed in parsed.children.iter() {
            let child_module = self
                .modules
                .insert(IntermediateModule::new(child_parsed.name));
            self.populate_forward_types_impl(child_module, child_parsed)?;
            self.modules[module].defs.insert(
                child_parsed.name.clone(),
                IntermediateDefId::Module(child_module),
            );
        }

        //Create forward references for imported types
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

    /// Populate all type definitions and function declaratations
    fn populate_defs_impl(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
    ) -> Result<(), Diagnostic<FileId>> {
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::AliasDef { name, aliased } => {
                    let ty = *self.modules[module].defs.get(name).unwrap();
                    match ty {
                        IntermediateDefId::Type(ty) => {
                            let resolved =
                                self.resolve_type(aliased, module, parsed, def.file, def.span)?;
                            *self.ctx.types.get_mut(ty) = IrType::Alias {
                                name: name.clone(),
                                ty: resolved,
                            }
                            .into();
                        }
                        _ => unreachable!(),
                    }
                }
                DefData::FunDec(proto) | DefData::FunDef(proto, _) => {
                    let fun = IrFun {
                        file: def.file,
                        span: def.span,
                        name: proto.name.clone(),
                        ty: self.resolve_fn_type(&proto.ty, module, parsed, def.file, def.span)?,
                        body: None,
                        flags: proto.flags,
                    };
                    let fun = self.ctx.funs.insert(fun);
                    self.modules[module]
                        .defs
                        .insert(proto.name.clone(), IntermediateDefId::Fun(fun));
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
        parsed: &ParsedModule,
        file: FileId,
        span: Span,
    ) -> Result<TypeId, Diagnostic<FileId>> {
        Ok(match ty {
            UnresolvedType::Integer { width, signed } => self.ctx.types.insert(
                IrIntegerType {
                    width: *width,
                    signed: *signed,
                }
                .into(),
            ),
            UnresolvedType::Float { doublewide } => self.ctx.types.insert(
                IrFloatType {
                    doublewide: *doublewide,
                }
                .into(),
            ),
            UnresolvedType::Pointer(ptr) => {
                let ty = self.resolve_type(ptr, module, parsed, file, span)?;
                self.ctx.types.insert(IrType::Ptr(ty))
            }
            UnresolvedType::Array { elements, len } => {
                let element = self.resolve_type(elements, module, parsed, file, span)?;
                self.ctx
                    .types
                    .insert(IrArrayType { element, len: *len }.into())
            }
            UnresolvedType::Unit => IrContext::UNIT,
            UnresolvedType::Bool => IrContext::BOOL,
            UnresolvedType::Enum { variants } => {
                let variants = variants
                    .iter()
                    .map(|variant| self.resolve_type(variant, module, parsed, file, span))
                    .collect::<Result<Vec<_>, _>>()?;
                self.ctx.types.insert(IrSumType { variants }.into())
            }
            UnresolvedType::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(|(field, name)| {
                        match self.resolve_type(field, module, parsed, file, span) {
                            Ok(field) => Ok((field, name.clone())),
                            Err(e) => Err(e),
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.ctx.types.insert(IrStructType { fields }.into())
            }
            UnresolvedType::UserDefined { name } => match self.resolve_path(module, name) {
                Some(IntermediateDefId::Type(ty)) => ty,
                _ => {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Type {} not found in module {}",
                            name, self.modules[module].name
                        ))
                        .with_labels(vec![Label::new(LabelStyle::Primary, file, span)]))
                }
            },
            UnresolvedType::Fun(ty) => {
                let fn_ty = self.resolve_fn_type(ty, module, parsed, file, span)?;
                self.ctx.types.insert(fn_ty.into())
            }
        })
    }

    /// Resolve a function type, split into another function to be used when generating forward
    /// references for function declarations
    fn resolve_fn_type(
        &mut self,
        ty: &UnresolvedFunType,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
        file: FileId,
        span: Span,
    ) -> Result<IrFunType, Diagnostic<FileId>> {
        let return_ty = self.resolve_type(&ty.return_ty, module, parsed, file, span)?;
        let args = ty
            .arg_tys
            .iter()
            .map(
                |(ty, name)| match self.resolve_type(ty, module, parsed, file, span) {
                    Ok(ty) => Ok((ty, name.clone())),
                    Err(e) => Err(e),
                },
            )
            .collect::<Result<Vec<_>, _>>()?;

        Ok(IrFunType { return_ty, args })
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
