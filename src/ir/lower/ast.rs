use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::{Expr, ExprNode, ParsedModule, Stmt, StmtNode},
    ir::{value::IrIntegerValue, BBId, FunId, IrBB, IrFun, IrStmt, IrVar, VarId},
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

    /// Lower a single statement to IR
    fn lower_stmt(
        &mut self,
        module: IntermediateModuleId,
        parsed: &ParsedModule,
        stmt: &Stmt,
        bb: BBId,
    ) -> Result<(), Diagnostic<FileId>> {
        match &stmt.node {
            StmtNode::Let(let_stmt) => {
                if let ExprNode::Access(ref var) = let_stmt.let_expr.node {
                    //Create a new variable if none exists
                    if var.len() == 1 && !self.lookup_var(&var.last()).is_none() {
                        let name = var.last();
                        let ty = match &let_stmt.ty {
                            Some(ty) => self.resolve_type(
                                ty,
                                module,
                                parsed,
                                self.current_fun().file,
                                stmt.span,
                            )?,
                            None => match &let_stmt.assigned {
                                Some(assigned) => unimplemented!(),
                                None => {
                                    return Err(Diagnostic::error()
                                        .with_message(format!(
                                            "Failed to infer the type of variable {}",
                                            name
                                        ))
                                        .with_labels(vec![Label::primary(
                                            self.current_fun().file,
                                            stmt.span,
                                        )
                                        .with_message("Variable declaration encountered here")]))
                                }
                            },
                        };

                        let var = self
                            .current_fun_mut()
                            .body
                            .as_mut()
                            .unwrap()
                            .vars
                            .insert(IrVar { name, ty });
                        self.current_scope_mut().vars.insert(name.clone(), var);
                        self.ctx.bbs[bb].stmts.push(IrStmt::VarLive(var));
                    }
                }
            }
            _ => unimplemented!(),
        }

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
