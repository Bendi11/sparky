use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::{Expr, ExprNode, ParsedModule, Stmt, StmtNode, Literal, NumberLiteral, NumberLiteralAnnotation, IntegerWidth},
    ir::{BBId, FunId, IrBB, IrFun, IrStmtKind, IrVar, VarId, types::IrType, value::{IrExprKind, IrExpr}, IrTerminator, IrStmt},
    util::files::FileId,
    Symbol, parse::token::Op,
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
            StmtNode::Return(val) => match self.lowest_scope().return_var {
                Some(var) => {
                    self.ctx[bb].terminator = IrTerminator::Return(self.lower_expr(module, file, fun, val, bb)?);
                },
                None => return Err(Diagnostic::error()
                    .with_message("Return statement while current function does not return a value")
                    .with_labels(vec![Label::primary(file, stmt.span)])
                )
            },
            StmtNode::Phi(val) => {
                let return_var = match self
                    .current_scope()
                    .return_var {
                        Some(ret) => ret,
                        None => return Err(
                            Diagnostic::error()
                                .with_message("Phi statement in a block that is not an expression".to_owned())
                                .with_labels(vec![
                                    Label::primary(file, stmt.span)
                                        .with_message("Phi statement appears here")
                                ])
                        )
                    };
                let return_val = self.lower_expr(module, file, fun, val, bb)?;
                self
                    .ctx[bb]
                    .stmts
                    .push(IrStmt {
                        span: stmt.span,
                        kind: IrStmtKind::Store {
                            var: return_var,
                            val: return_val
                        }
                    });
                self.ctx[bb].terminator = IrTerminator::Jmp(self.current_scope().after_bb);

            },
            StmtNode::Let(let_stmt) => {
                let assigned = self.lower_expr(module, file, fun, &let_stmt.assigned, bb)?;
                let ptr = match &let_stmt.let_expr.node {
                    ExprNode::Access(name) => {
                        let (ty, var) = match self.lookup_var(&name.last()) {
                            Some(var) => (self.ctx[var].ty, var),
                            None => {
                                let ty = let_stmt
                                    .ty
                                    .as_ref()
                                    .map(|ty| self.resolve_type(ty, module, file, let_stmt.let_expr.span))
                                    .unwrap_or(Ok(assigned.ty))?;

                                let var = IrVar {
                                    ty,
                                    name: name.last(),
                                };

                                let var_id = self.ctx.vars.insert(var);
                                self.ctx[bb].stmts.push(IrStmt {
                                    span: let_stmt.let_expr.span,
                                    kind: IrStmtKind::VarLive(var_id),
                                });

                                (ty, var_id)
                            }
                        };

                        IrExpr {
                            span: let_stmt.let_expr.span,
                            ty: self.ctx.types.insert(IrType::Ptr(ty)),
                            kind: IrExprKind::Unary(Op::AND, Box::new(IrExpr {
                                    span: let_stmt.let_expr.span,
                                    ty,
                                    kind: IrExprKind::Var(var),
                                })
                            )
                        }
                    },
                    _ => {
                        let let_expr = self.lower_expr(module, file, fun, &let_stmt.let_expr, bb)?;
                        IrExpr {
                            span: let_stmt.let_expr.span,
                            ty: self.ctx.types.insert(IrType::Ptr(let_expr.ty)),
                            kind: IrExprKind::Unary(Op::AND, Box::new(let_expr))
                        }
                    }
                };
                
                self.ctx[bb].stmts.push(IrStmt {
                    span: (let_stmt.let_expr.span.from..let_stmt.assigned.span.to).into(),
                    kind: IrStmtKind::Write { ptr, val: assigned }
                });
            }
            _ => unimplemented!()
        } 
        Ok(())
    }
    
    /// Lower a single AST expression to intermediate representation
    pub fn lower_expr(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        expr: &Expr,
        bb: BBId,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        Ok(match &expr.node {
            ExprNode::Access(pat) => match self.resolve_path(module, pat) {
                Some(IntermediateDefId::Fun(fun_id)) => IrExpr {
                    kind: IrExprKind::Fun(fun_id),
                    ty: self.ctx[fun_id].ty_id,
                    span: expr.span,
                },
                _ => match self.lookup_var(&pat.last()) {
                    Some(var) if pat.len() == 1 => IrExpr {
                        kind: IrExprKind::Var(var),
                        ty: self.ctx[var].ty,
                        span: expr.span,
                    },
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
            ExprNode::Member(object, name) => {
                let object = self.lower_expr(module, file, fun, object, bb)?;
                match &self.ctx[object.ty] {
                    IrType::Struct(s_ty) => {
                        for (idx, field) in s_ty.fields.iter().enumerate() {
                            if field.name == *name {
                                return Ok(IrExpr {
                                    kind: IrExprKind::Member(Box::new(object), idx),
                                    ty: field.ty,
                                    span: expr.span,
                                })
                            }
                        }
                        return Err(Diagnostic::error()
                            .with_message(format!("Field {} not found for structure type {}", name, self.ctx.typename(object.ty)))
                            .with_labels(vec![
                                Label::primary(file, expr.span)
                                    .with_message("Structure field access occurs here")
                            ])
                        )
                    },
                    _ => return Err(Diagnostic::error()
                        .with_message(format!("Attempting to access field {} of expression of non-structure type {}", name, self.ctx.typename(object.ty)))
                        .with_labels(vec![
                            Label::primary(file, expr.span)
                                .with_message("Field access occurs here")
                        ])
                    )
                }
            },
            ExprNode::Call(fun_ast, args) => {
                let fun_ir = self.lower_expr(module, file, fun, fun_ast, bb)?;
                match self.ctx[fun_ir.ty].clone() {
                    IrType::Fun(fun_ty) => {
                        let args = args
                            .iter()
                            .map(|arg| self.lower_expr(module, file, fun, arg, bb))
                            .collect::<Result<Vec<IrExpr>, _>>()?;
                        
                        if args.len() != fun_ty.params.len() {
                            return Err(Diagnostic::error()
                                .with_message(format!("Expected {} arguments when calling function, found {}", fun_ty.params.len(), args.len()))
                                .with_labels(vec![
                                    Label::primary(file, expr.span)
                                        .with_message("Call expression occurs here")
                                ])
                            )
                        }

                        for (idx, (param, arg)) in fun_ty.params.iter().zip(args.iter()).enumerate() {
                            if param.0 != arg.ty {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "Argument {}: expected parameter type {} but argument of type {} was passed",
                                        idx,
                                        self.ctx.typename(param.0),
                                        self.ctx.typename(arg.ty))
                                    )
                                    .with_labels(vec![
                                        Label::primary(file, arg.span)
                                            .with_message("Argument passed here"),
                                        Label::secondary(file, expr.span)
                                            .with_message("Call expression occurs here")
                                    ])
                                )
                            }
                        }

                        IrExpr {
                            kind: IrExprKind::Call(Box::new(fun_ir), args),
                            ty: fun_ty.return_ty,
                            span: expr.span,
                        }
                    },
                    _ => return Err(Diagnostic::error()
                        .with_message(format!("Attempting to call expression of non-function type {}", self.ctx.typename(fun_ir.ty)))
                        .with_labels(vec![
                            Label::primary(file, expr.span)
                                .with_message("Call expression occurs here")
                        ])
                    )
                }
            },
            ExprNode::Unary(op, expr) => return self.lower_unary(module, file, fun, *op, &expr, bb),
            ExprNode::Bin(lhs, op, rhs) => return self.lower_bin(module, file, fun, &lhs, *op, &rhs, bb),
            _ => unimplemented!()
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

    fn lowest_scope(&self) -> &ScopePlate {
        self
            .scope_stack
            .first()
            .expect("Internal compiler error: scope stack is empty")
    }

    fn lowest_scope_mut(&mut self) -> &mut ScopePlate {
        self
            .scope_stack
            .first_mut()
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
