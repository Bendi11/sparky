use crate::{ast::{ParsedModule, DefData}, util::{files::{Files, FileId}, loc::Span}};
use super::*;


/// A structure responsible for lowering a parsed AST into
/// intermediate representation, resolving all unknown symbols
#[derive(Debug)]
pub struct AstLowerer<'ctx> {
    ctx: &'ctx mut IRContext,
    modules: Vec<ParsedModule>,
    files: Files,
}

pub type SemanticResult<T> = Result<T, SemanticError>;

impl<'ctx> AstLowerer<'ctx> {
    /// Create a new lowerer from an ir context
    pub fn new(ctx: &'ctx mut IRContext, files: Files, modules: Vec<ParsedModule>) -> Self {
        Self {
            ctx,
            modules,
            files,
        }
    }
    
    /// Generate forward definitions of all modules in the IR context and return a map of module
    /// names to more module IDs
    pub fn forward_modules(&self) -> SemanticResult<HashMap<Symbol, ModuleId>> {
        fn forward_modules_for(ctx: &mut IRContext, list: &HashMap<Symbol, ParsedModule>) -> SemanticResult<HashMap<Symbol, ModuleId>> {
            let mut submods = HashMap::new();
            for submod in list.values() {
                let irmod_id = ctx.new_module(submod.name);
                let mut irmod = ctx.modules[irmod_id];
                irmod.children = forward_modules_for(ctx, &submod.children)?;
                submods.insert(submod.name, irmod_id);
            }
            Ok(submods)
        }
        let mut modules = HashMap::new();
        for module in self.modules.iter() {
            let mut irmod_id = self.ctx.new_module(module.name);
            let mut irmod = self.ctx.modules[irmod_id];
            irmod.children = forward_modules_for(&mut self.ctx, &module.children)?;
            modules.insert(module.name, irmod_id);
        }
        Ok(modules)
    }
    
    /// Create Type entries for all type definitions in all modules
    pub fn forward_types(&mut self) -> Result<HashMap<Symbol, TypeId>, SemanticError> {
        let modules = self.forward_modules()?;

        let mut forward_refs = HashMap::new();
        
        for module in self.modules {
            for (defname, def) in module.defs.iter() {
                match def.data {
                    DefData::StructDef { name, fields: _ }
                    | DefData::EnumDef { name, variants: _ }
                    | DefData::AliasDef { name, aliased: _ } => {
                        let type_id = self.ctx.new_type();
                        let mut type_ = self.ctx.types[type_id];
                        type_.data = TypeData::Invalid;
                        forward_refs.insert(name, type_id);
                    },
                    _ => (),
                }
            }
        }

    }

}

/// An error found in the AST lowerer
pub struct SemanticError {
    /// The error message of this error
    pub msg: String,
    /// The highlighted span of this error
    pub spans: Vec<Span>,
    /// The this error was in
    pub in_file: FileId,
}

impl SemanticError {
    pub fn new(msg: impl ToString, file: FileId) -> Self {
        Self {
            msg,
            in_file: file,
            spans: vec![]
        }
    } 

    pub fn with_span(mut self, span: Span) -> Self {
        self.spans.push(span);
        self
    }
}
