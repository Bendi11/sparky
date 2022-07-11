use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::{Expr, ExprNode, ParsedModule, Stmt, StmtNode, Literal, NumberLiteral, NumberLiteralAnnotation, IntegerWidth},
    ir::{BBId, FunId, IrBB, IrFun, IrStmtKind, IrVar, VarId, types::IrType, value::{IrExprKind, IrExpr}, IrTerminator},
    util::files::FileId,
    Symbol,
};

use super::{IntermediateModuleId, IrLowerer, ScopePlate, IntermediateDefId};

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
    
    /// Lower a single statement to IR instructions
    fn lower_stmt(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        stmt: &Stmt,
        bb: BBId,
    ) -> Result<(), Diagnostic<FileId>> {
        match &stmt.node {
            StmtNode::Return(val) => match self.scope_stack.first_mut() {
                Some(top) => match top.return_var {
                    Some(var) => {
                        self.ctx[bb].terminator = IrTerminator::Return(self.lower_expr(module, file, fun, val, bb)?);
                    },
                    None => return Err(Diagnostic::error()
                        .with_message("Return statement while current function does not return a value")
                        .with_labels(vec![Label::primary(file, stmt.span)])
                    )
                },
                None => return Err(Diagnostic::error()
                    .with_message("Return statement encountered while not in function".to_owned())
                    .with_labels(vec![
                        Label::primary(file, stmt.span)
                    ])
                )
            },
            StmtNode::Phi(val) => {
                self
                    .ctx[bb]
                    .stmts
                    .push(IrStmt {
                        span: stmt.span,
                        kind: IrStmtKind::Store { var, val: self.lower_expr(module, file, fun, val, bb)? }
                    });
                self.ctx[bb].terminator = IrTerminator::Jmp(self.current_scope().after_bb);

            }
            _ => unimplemented!()
        } 
        Ok(())
    }
    
    /// Lower a single AST expression to intermediate representation
    fn lower_expr(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        expr: &Expr,
        bb: IrBB,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        Ok(IrExpr {
            span: expr.span, 
            kind: match &expr.node {
                ExprNode::Access(pat) => match self.resolve_path(module, pat) {
                    Some(IntermediateDefId::Fun(fun_id)) => IrExprKind::Fun(fun_id),
                    _ => match self.lookup_var(&pat.last()) {
                        Some(var) if pat.len() == 1 => IrExprKind::Var(var),
                        _ => return Err(
                            Diagnostic::error()
                                .with_message(format!("No variable or function found for name {}", pat))
                                .with_labels(vec![
                                    Label::primary(file, expr.span)
                                        .with_message("Unknown identifier appears here")
                                ])
                        )
                    }
                },
                _ => unimplemented!()
            }
        })
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
