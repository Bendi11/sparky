use string_interner::StringInterner;

use crate::{ast::{ParsedModule, DefData, Def}, util::{files::{Files, FileId}, loc::Span}};
use super::*;


/// A structure responsible for lowering a parsed AST into
/// intermediate representation, resolving all unknown symbols
#[derive(Debug)]
pub struct AstLowerer<'ctx, 'sym> {
    ctx: &'ctx mut IRContext,
    symbols: &'sym StringInterner,
    modules: Vec<ParsedModule>,
    /// A map of module names to their IDs in the context
    forward_modules: HashMap<Symbol, ModuleId>,

    files: Files,
}

pub type SemanticResult<T> = Result<T, SemanticError>;

impl<'ctx, 'sym> AstLowerer<'ctx, 'sym> {
    /// Create a new lowerer from an ir context
    pub fn new(ctx: &'ctx mut IRContext, symbols: &'sym StringInterner, files: Files, modules: Vec<ParsedModule>) -> SemanticResult<Self> {
        let mut this = Self {
            ctx,
            symbols,
            modules,
            files,
            forward_modules: HashMap::new()
        };
        this.forward_modules = this.forward_modules()?;
        Ok(this)
    }
    
    /// Generate forward definitions of all modules in the IR context and return a map of module
    /// names to more module IDs
    pub fn forward_modules(&mut self) -> SemanticResult<HashMap<Symbol, ModuleId>> {
        fn forward_modules_for(ctx: &mut IRContext, list: &HashMap<Symbol, ParsedModule>) -> SemanticResult<HashMap<Symbol, ModuleId>> {
            let mut submods = HashMap::new();
            for submod in list.values() {
                let irmod_id = ctx.new_module(submod.name);
                ctx.modules[irmod_id].children = forward_modules_for(ctx, &submod.children)?;
                submods.insert(submod.name, irmod_id);
            }
            Ok(submods)
        }
        let mut modules = HashMap::new();
        for module in self.modules.iter() {
            let irmod_id = self.ctx.new_module(module.name);
            let children = forward_modules_for(&mut self.ctx, &module.children)?;
            self.ctx.modules[irmod_id].children = children;

            modules.insert(module.name, irmod_id);
        }
        Ok(modules)
    }
    
    /// Create Type entries for all type definitions in a module
    fn forward_types(&mut self, (parsed, id): (&ParsedModule, ModuleId)) -> SemanticResult<()> {
        //Keep a map of recorded definitions to check for duplicate names
        let mut remembered_defs: HashMap::<Symbol, Def>  = HashMap::new();
        
        for (defname, def) in parsed.defs.iter() {
            match def.data {
                DefData::StructDef { name, fields: _ }
                | DefData::EnumDef { name, variants: _ }
                | DefData::AliasDef { name, aliased: _ } => {
                    let type_id = self.ctx.new_type();
                    let type_ = &mut self.ctx.types[type_id];
                    type_.data = TypeData::Invalid;
                    
                    //Ensure that two type definitions don't have colliding names
                    if let Some(ref other_def) = remembered_defs.get(&name) {
                        return Err(SemanticError::new(format!("In module {}: two type definitions with the same name", self.symbol(parsed.name)), parsed.file)
                            .with_span(def.span)
                            .with_span(other_def.span)
                        )
                    }
                    remembered_defs.insert(name, def.clone());

                    self.ctx.modules[id].typedefs.insert(name, type_id);
                },
                _ => (),
            }
        }
        Ok(())

    }
    
    /// Walk a module and any submodules 
    fn walk_module_with<R, F: FnMut(&mut Self, (&ParsedModule, ModuleId)) -> SemanticResult<R>>(&mut self, module: &ParsedModule, mut f: F) -> SemanticResult<R> {
        let id = self.forward_modules[&module.name];
        for submodule in module.children.values() {
           self.walk_module_with(submodule, &mut f); 
        }
        f(self, (module, id))
    }

    fn symbol(&self, sym: Symbol) -> &str {
        self.symbols.resolve(sym).unwrap()
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
            msg: msg.to_string(),
            in_file: file,
            spans: vec![]
        }
    } 

    pub fn with_span(mut self, span: Span) -> Self {
        self.spans.push(span);
        self
    }
}
