use crate::{ast::ParsedModule, util::files::Files};
use super::*;


/// A structure responsible for lowering a parsed AST into
/// intermediate representation, resolving all unknown symbols
#[derive(Debug)]
pub struct AstLowerer<'ctx> {
    ctx: &'ctx mut IRContext,
    files: Files,
}

impl<'ctx> AstLowerer<'ctx> {
    /// Create a new lowerer from an ir context
    pub fn new(ctx: &'ctx mut IRContext, files: Files) -> Self {
        Self {
            ctx,
            files
        }
    }
    
    /// Lower a parsed module to IR
    pub fn lower_module(&mut self, module: ParsedModule) -> Result<ModuleId, SemanticError> {
        let mut ir_module = Module::new(module.name);

        Ok(self.ctx.modules.insert(ir_module))
    }
}

/// An error found in the AST lowerer
pub struct SemanticError {

}
