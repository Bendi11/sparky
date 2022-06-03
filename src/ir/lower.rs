//! Module containing definitions for structures containing all state needed to lower a parsed
//! abstract syntax tree to spark IR instructions

use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};
use hashbrown::HashMap;

use crate::{util::{files::{Files, FileId}, loc::Span}, Symbol, ast::{ParsedModule, DefData, UnresolvedType, SymbolPath, PathIter}, arena::{Index, Arena}};

use super::{IrContext, TypeId, FunId, types::{IrType, integer::IrIntegerType, float::IrFloatType, array::IrArrayType, sum::IrSumType, structure::IrStructType, fun::IrFunType}};

pub mod scopemap;

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
}

/// Index into the `modules` field of an [IrLowerer]
pub type IntermediateModuleId = Index<IntermediateModule>;

/// Enum that points to a [TypeId] or [FunId], used by the [IntermediateModule]
#[derive(Clone, Copy, Debug,)]
enum IntermediateDefId {
    /// ID of a defined type
    Type(TypeId),
    /// Function definition or declaration
    Fun(FunId),
    /// Child module
    Module(IntermediateModuleId),
}

/// Data only used by the [IrLowerer] in order to save what symbols are defined in each
/// [ParsedModule](crate::ast::ParsedModule)
struct IntermediateModule {
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
        }
    }
    
    /// Get forward references to all declared types and modules
    fn populate_forward_types_impl(&mut self, module: IntermediateModuleId, parsed: &ParsedModule) -> Result<(), Diagnostic<FileId>> {
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::AliasDef { name, aliased: _ } => {
                    let ty = self.ctx.types.insert_nointern(IrType::Alias { name: name.clone(), ty: IrContext::INVALID });
                    self.modules[module].defs.insert(name.clone(), IntermediateDefId::Type(ty));
                },
                _ => (),
            }
        }

        for child_parsed in parsed.children.iter() {
            let child_module = self.modules.insert(IntermediateModule::new(child_parsed.name));
            self.populate_forward_types_impl(child_module, child_parsed)?;
            self.modules[module].defs.insert(child_parsed.name.clone(), IntermediateDefId::Module(child_module));
        }
    
        //Create forward references for imported types
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::ImportDef { name } => match self.resolve_path(module, name) {
                    Some(id) => { self.modules[module].defs.insert(name.last(), id); },
                    _ => (),
                },
                _ => (),
            }
        }

        Ok(())
    }
    
    /// Populate all type definitions and function declaratations
    fn populate_defs_impl(&mut self, module: IntermediateModuleId, parsed: &ParsedModule) -> Result<(), Diagnostic<FileId>> {
        for def in parsed.defs.iter() {
            match &def.data {
                DefData::AliasDef { name, aliased } => {
                    let ty = *self.modules[module].defs.get(name).unwrap();
                    match ty {
                        IntermediateDefId::Type(ty) => {
                            let resolved = self.resolve_type(aliased, module, parsed, def.file, def.span)?;
                            *self.ctx.types.get_mut(ty) = IrType::Alias { name: name.clone(), ty: resolved }.into();
                        },
                        _ => unreachable!()
                    } 
                },
                DefData::FunDec(proto) | DefData::FunDef(proto, _) => {

                },
                _ => (),
            }
        }

        Ok(())
    }
    
    /// Resolve a parsed type into a concrete type id
    fn resolve_type(&mut self, ty: &UnresolvedType, module: IntermediateModuleId, parsed: &ParsedModule, file: FileId, span: Span) -> Result<TypeId, Diagnostic<FileId>> {
        Ok(match ty {
            UnresolvedType::Integer { width, signed } => self.ctx.types.insert(IrIntegerType { width: *width, signed: *signed }.into()),
            UnresolvedType::Float { doublewide } => self.ctx.types.insert(IrFloatType { doublewide: *doublewide }.into()),
            UnresolvedType::Pointer(ptr) => {
                let ty = self.resolve_type(ptr, module, parsed, file, span)?;
                self.ctx.types.insert(IrType::Ptr(ty))
            },
            UnresolvedType::Array { elements, len } => {
                let element = self.resolve_type(elements, module, parsed, file, span)?;
                self.ctx.types.insert(IrArrayType { element, len: *len }.into())
            },
            UnresolvedType::Unit => IrContext::UNIT,
            UnresolvedType::Bool => IrContext::BOOL,
            UnresolvedType::Enum { variants } => {
                let variants = variants.iter().map(|variant| 
                    self.resolve_type(variant, module, parsed, file, span)
                ).collect::<Result<Vec<_>, _>>()?;
                self.ctx.types.insert(IrSumType { variants }.into())
            },
            UnresolvedType::Struct { fields } => {
                let fields = fields.iter().map(|(field, name)| match self.resolve_type(field, module, parsed, file, span) {
                    Ok(field) => Ok((field, name.clone())),
                    Err(e) => Err(e)
                }).collect::<Result<Vec<_>, _>>()?;
                self.ctx.types.insert(IrStructType { fields }.into())
            },
            UnresolvedType::UserDefined { name } => match self.resolve_path(module, name) {
                Some(IntermediateDefId::Type(ty)) => ty,
                _ => return Err(Diagnostic::error()
                    .with_message(format!("Type {} not found in module {}", name, self.modules[module].name))
                    .with_labels(vec![
                        Label::new(LabelStyle::Primary, file, span)
                    ])
                )
            },
            UnresolvedType::Fun(ty) => {
                let return_ty = self.resolve_type(&ty.return_ty, module, parsed, file, span)?;
                let args = ty.arg_tys.iter().map(|ty| 
                    self.resolve_type(ty, module, parsed, file, span)
                ).collect::<Result<Vec<_>, _>>()?;

                let fn_ty = IrFunType {
                    return_ty,
                    args,
                };

                self.ctx.types.insert(fn_ty.into())
            }
        })
    }
    
    /// Resolve the path in the context of the given intermediate module
    #[inline]
    fn resolve_path(&self, module: IntermediateModuleId, path: &SymbolPath) -> Option<IntermediateDefId> {
        self.resolve_path_impl(module, path.iter())
    }

    /// Attempt to resolve the given path in this module
    fn resolve_path_impl(&self, module: IntermediateModuleId, mut path: PathIter) -> Option<IntermediateDefId> {
        let next = path.next().unwrap();
        let next = self.modules[module].defs.get(&next);
        match path.len() {
            1 => next.copied(),
            _ => match next {
                Some(IntermediateDefId::Module(other)) => self.resolve_path_impl(*other, path),
                _ => None
            }
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
