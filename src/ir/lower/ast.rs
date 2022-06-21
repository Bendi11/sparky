use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::{Expr, ExprNode, ParsedModule, Stmt, StmtNode, Literal, NumberLiteral, NumberLiteralAnnotation, IntegerWidth},
    ir::{BBId, FunId, IrBB, IrFun, IrStmt, IrVar, VarId, types::IrType},
    util::files::FileId,
    Symbol,
};

use super::{IntermediateModuleId, IrLowerer, ScopePlate};

impl<'files, 'ctx> IrLowerer<'files, 'ctx> {
    /// Lower a function's body to IR statements and basic blocks
    fn lower_body(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        smts: &[Stmt],
    ) -> Result<(), Diagnostic<FileId>> {
        Ok(())
    }

    /// Lookup a declared variable in the current scope stack
    fn lookup_var(&self, var: &Symbol) -> Option<VarId> {
        for plate in self.scope_stack.iter().rev() {
            match plate.vars.get(var) {
                Some(var) => return Some(*var),
                None => continue,
            }
        }

        None
    }

    /// Get a mutable reference to the current scope plate
    fn current_scope_mut(&mut self) -> &mut ScopePlate {
        self.scope_stack
            .last_mut()
            .expect("Internal compiler error: scope stack is empty")
    }

    /// Get an immutable reference to the current scope plate
    fn current_scope(&self) -> &ScopePlate {
        self.scope_stack
            .last()
            .expect("Internal compiler error: scope stack is empty")
    }

    /// Get an immutable reference to the currently lowered function
    fn current_fun(&self) -> &IrFun {
        &self.ctx.funs[self
            .current_fun
            .expect("Internal compiler error: current function is none")]
    }

    /// Get a mutable reference to the currently lowered function
    fn current_fun_mut(&mut self) -> &mut IrFun {
        &mut self.ctx.funs[self
            .current_fun
            .expect("Internal compiler error: current function is none")]
    }
}
