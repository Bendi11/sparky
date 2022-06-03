use codespan_reporting::diagnostic::Diagnostic;

use crate::{util::files::FileId, ir::{FunId, IrStmt}, ast::{Stmt, StmtNode}};

use super::{IrLowerer, IntermediateModuleId};


impl<'files, 'ctx> IrLowerer<'files, 'ctx> {
    /// Lower a function's body to IR statements and basic blocks
    fn lower_body(&mut self, module: IntermediateModuleId, file: FileId, fun: FunId, smts: &[Stmt]) -> Result<(), Diagnostic<FileId>> {
         

        Ok(())
    }
    
    /// Lower a single statement to IR
    fn lower_stmt(&mut self, module: IntermediateModuleId, file: FileId, stmt: &Stmt) -> Result<IrStmt, Diagnostic<FileId>> {
        match &stmt.node {
           
            _ => unimplemented!()
        }
    }
}
