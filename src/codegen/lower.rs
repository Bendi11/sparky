use crate::{error::DiagnosticManager, util::files::Files};

use super::ir::SparkCtx;

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
}
