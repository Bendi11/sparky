use codespan_reporting::diagnostic::{Diagnostic, Label};
use hashbrown::HashMap;

use crate::{
    ast::{
        ElseExpr, Expr, ExprNode, If, IntegerWidth, Literal, Match, NumberLiteral,
        NumberLiteralAnnotation, Stmt, StmtNode, DefData,
    },
    ir::{
        types::{FunType, IrFloatType, IrIntegerType, IrStructField, IrStructType, IrType},
        value::{IrExpr, IrExprKind, IrLiteral}, FunId, IrBB, IrBody, IrContext, IrStmt, IrStmtKind, IrTerminator, IrVar,
        VarId,
    },
    util::{files::FileId, loc::Span},
    Symbol,
};

use super::{IntermediateDefId, IntermediateModuleId, IrLowerer, ScopePlate};

impl<'ctx> IrLowerer<'ctx> {
    /// Lower a function's body to IR statements and basic blocks
    pub(super) fn lower_body(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        stmts: &[Stmt],
    ) -> Result<(), Diagnostic<FileId>> {
        let entry = self.ctx.bbs.insert(IrBB {
            stmts: vec![],
            terminator: IrTerminator::Invalid,
        });
        self.bb = Some(entry);
        let return_var = match self.ctx[self.ctx[fun].ty.return_ty] {
            IrType::Unit => None,
            _ => {
                let return_var = self.ctx.vars.insert(IrVar {
                    ty: self.ctx[fun].ty.return_ty,
                    name: Symbol::new(format!("@return_var#{}", self.ctx[fun].name)),
                });
                let span = self.ctx[fun].span;
                self.ctx[entry].stmts.push(IrStmt {
                    span,
                    kind: IrStmtKind::VarLive(return_var),
                });

                Some(return_var)
            }
        };

        self.scope_stack.push(ScopePlate {
            vars: HashMap::default(),
            return_var,
            after_bb: entry,
        });

        let params = self.ctx[fun].ty.params.clone();
        let mut param_vars = vec![];
        for (ty, name) in params {
            if let Some(name) = name {
                let param_var = self.ctx.vars.insert(IrVar {
                    ty,
                    name: name.clone(),
                });
                param_vars.push(Some(param_var));
                self.lowest_scope_mut().vars.insert(name.clone(), param_var);
            } else {
                param_vars.push(None);
            }
        }

        self.ctx[fun].body = Some(IrBody { entry, parent: fun, args: param_vars });

        for stmt in stmts {
            self.lower_stmt(module, file, fun, stmt)?;
        }

        match (self.ctx.unwrap_alias(self.ctx[fun].ty.return_ty), &self.ctx[entry].terminator) {
            (ty, IrTerminator::Invalid) if ty == IrContext::UNIT => self.ctx[entry].terminator = IrTerminator::Return(IrExpr {
                span: self.ctx[fun].span,
                ty: IrContext::UNIT,
                kind: IrExprKind::Lit(IrLiteral::Unit),
            }),
            (ty, IrTerminator::Invalid) => return Err(Diagnostic::error()
                .with_message(format!(
                    "Function {} must return a value of type {} but no return statement terminates the function",
                    self.ctx[fun].name,
                    self.ctx.typename(ty),
                ))
                .with_labels(vec![
                    Label::primary(file, self.ctx[fun].span)
                ])
            ),
            _ => (),
        }

        self.scope_stack.pop();

        Ok(())
    }

    /// Lower a single statement to IR instructions
    pub(super) fn lower_stmt(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        stmt: &Stmt,
    ) -> Result<(), Diagnostic<FileId>> {
        match &stmt.node {
            StmtNode::Loop(block) => { self.lower_loop(module, file, fun, stmt.span, &block)?; },
            StmtNode::Return(val) => match (
                self.lower_expr(module, file, fun, val)?,
                self.lowest_scope().return_var,
            ) {
                (val, Some(_)) => {
                    let current = self.bb();
                    self.ctx[current].terminator = IrTerminator::Return(val);
                }
                (val, None) if self.ctx.unwrap_alias(val.ty) == IrContext::UNIT => {
                    let current = self.bb();
                    self.ctx[current].terminator = IrTerminator::Return(val);
                }
                (_, None) => {
                    return Err(Diagnostic::error()
                        .with_message(
                            "Return statement while current function does not return a value",
                        )
                        .with_labels(vec![Label::primary(file, stmt.span)]))
                }
            },
            StmtNode::Phi(val) => {
                let return_var = match self.current_scope().return_var {
                    Some(ret) => ret,
                    None => {
                        return Err(Diagnostic::error()
                            .with_message(
                                "Phi statement in a block that is not an expression".to_owned(),
                            )
                            .with_labels(vec![Label::primary(file, stmt.span)
                                .with_message("Phi statement appears here")]))
                    }
                };

                let return_val = self.lower_expr(module, file, fun, val)?;

                if self.ctx[self.ctx[return_var].ty] == IrType::Invalid {
                    self.ctx[return_var].ty = return_val.ty
                }

                if self.ctx[return_var].ty != return_val.ty {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Phi statement returns expression of type {}, but type {} was expected",
                            self.ctx.typename(return_val.ty),
                            self.ctx.typename(self.ctx[return_var].ty),
                        ))
                        .with_labels(vec![Label::primary(file, val.span).with_message(format!(
                            "Value of type {} returned here",
                            self.ctx.typename(return_val.ty),
                        ))]));
                }
                let current = self.bb();
                self.ctx[current].stmts.push(IrStmt {
                    span: stmt.span,
                    kind: IrStmtKind::Store {
                        var: return_var,
                        val: return_val,
                    },
                });
                self.ctx[current].terminator = IrTerminator::Jmp(self.current_scope().after_bb);
                *self.bb_mut() = self.current_scope().after_bb;
            }
            StmtNode::Let(let_stmt) => match let_stmt.assigned.as_ref() {
                None => {
                    let expr = self.lower_expr(module, file, fun, &let_stmt.let_expr)?;
                    let current = self.bb();
                    self.ctx[current].stmts.push(IrStmt {
                        span: expr.span,
                        kind: IrStmtKind::Exec(expr),
                    })
                }
                Some(assigned) => {
                    let assigned = self.lower_expr(module, file, fun, &assigned)?;
                    let (ty, ptr) = match &let_stmt.let_expr.node {
                        ExprNode::Access(name) => {
                            let (ty, var) = match self.lookup_var(&name.last()) {
                                Some(var) => (self.ctx[var].ty, var),
                                None => {
                                    let ty = let_stmt
                                        .ty
                                        .as_ref()
                                        .map(|ty| {
                                            self.resolve_type(
                                                ty,
                                                module,
                                                file,
                                                let_stmt.let_expr.span,
                                            )
                                        })
                                        .unwrap_or(Ok(assigned.ty))?;

                                    let var = IrVar {
                                        ty,
                                        name: name.last(),
                                    };

                                    let var_id = self.ctx.vars.insert(var);
                                    self.current_scope_mut().vars.insert(name.last(), var_id);
                                    let current = self.bb();
                                    self.ctx[current].stmts.push(IrStmt {
                                        span: let_stmt.let_expr.span,
                                        kind: IrStmtKind::VarLive(var_id),
                                    });

                                    (ty, var_id)
                                }
                            };

                            (
                                ty,
                                IrExpr {
                                    span: let_stmt.let_expr.span,
                                    ty,
                                    kind: IrExprKind::Var(var),
                                },
                            )
                        }
                        _ => {
                            let let_expr =
                                self.lower_expr(module, file, fun, &let_stmt.let_expr)?;
                            (assigned.ty, let_expr)
                        }
                    };

                    if ty != assigned.ty {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Assigning a value of type {} to a value of incompatible type {}",
                                self.ctx.typename(assigned.ty),
                                self.ctx.typename(ty)
                            ))
                            .with_labels(vec![
                                Label::primary(file, assigned.span).with_message(format!(
                                    "Assigned value of type {} appears here",
                                    self.ctx.typename(assigned.ty)
                                )),
                                Label::secondary(file, ptr.span).with_message(format!(
                                    "Assignee of type {} appears here",
                                    self.ctx.typename(ty)
                                )),
                            ]));
                    }
                    let current = self.bb();
                    self.ctx[current].stmts.push(IrStmt {
                        span: (let_stmt.let_expr.span.from..assigned.span.to).into(),
                        kind: IrStmtKind::Write { ptr, val: assigned },
                    });
                }
            },
            StmtNode::Call(ident, args) => {
                let def = self.resolve_path(module, ident);
                match def {
                    Some(IntermediateDefId::Fun(fun_id)) => {
                        let fun_ty = self.ctx[fun_id].ty.clone();
                        let args = args
                            .iter()
                            .map(|arg| self.lower_expr(module, file, fun, arg))
                            .collect::<Result<Vec<_>, _>>()?;

                        self.typecheck_fun(file, stmt.span, &fun_ty, &args)?;
                        let current = self.bb();
                        self.ctx[current].stmts.push(IrStmt {
                            span: stmt.span,
                            kind: IrStmtKind::Call { fun: fun_id, args },
                        })
                    }
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "No function found in the current scope for path {}",
                                ident
                            ))
                            .with_labels(vec![Label::primary(file, stmt.span)]))
                    }
                }
            }
            StmtNode::If(expr) => return self.lower_if(module, file, fun, expr).map(|_| ()),
            StmtNode::Block(b) => {
                let old_bb = self.bb();
                let new_bb = self.ctx.bb();
                let after_bb = self.ctx.bb();
                self.scope_stack.push(ScopePlate {
                    vars: HashMap::new(),
                    return_var: None,
                    after_bb,
                });
                *self.bb_mut() = new_bb;
                self.lower_block(module, file, fun, &b)?;
                self.scope_stack.pop();
                self.ctx[old_bb].terminator = IrTerminator::Jmp(new_bb);
           }
            StmtNode::Match(match_stmt) => {
                self.lower_match(module, file, fun, match_stmt, stmt.span)?;
            }
            StmtNode::Break => {
                let current = self.bb();
                self.ctx[current].terminator = IrTerminator::Jmp(self.current_scope().after_bb);
                *self.bb_mut() = self.current_scope().after_bb;
            }
            StmtNode::Continue => {
                let current = self.bb();
                self.ctx[current].terminator = IrTerminator::Jmp(self.bb());
            }
        }
        Ok(())
    }

    /// Lower a single AST expression to intermediate representation
    pub(super) fn lower_expr(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        expr: &Expr,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        Ok(match &expr.node {
            ExprNode::Access(pat) => match self.resolve_path(module, pat) {
                Some(IntermediateDefId::Fun(fun_id)) => {
                    IrExpr {
                        kind: IrExprKind::Fun(fun_id),
                        ty: self.ctx[fun_id].ty_id,
                        span: expr.span,
                    }
                },
                Some(IntermediateDefId::Global(g)) => {
                    IrExpr {
                        span: expr.span,
                        ty: self.ctx[g].ty,
                        kind: IrExprKind::Global(g)
                    }
                },
                _ => match self.lookup_var(&pat.last()) {
                    Some(var) if pat.len() == 1 => IrExpr {
                        kind: IrExprKind::Var(var),
                        ty: self.ctx[var].ty,
                        span: expr.span,
                    },
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!("No variable or function found for name {}", pat))
                            .with_labels(vec![Label::primary(file, expr.span)
                                .with_message("Unknown identifier appears here")]))
                    }
                },
            },
            ExprNode::Member(object, name) => {
                let object = self.lower_expr(module, file, fun, object)?;
                let object_ty = self.ctx.unwrap_alias(object.ty);
                match &self.ctx[object_ty] {
                    IrType::Struct(s_ty) => {
                        for (idx, field) in s_ty.fields.iter().enumerate() {
                            if field.name == *name {
                                return Ok(IrExpr {
                                    kind: IrExprKind::Member(Box::new(object), idx),
                                    ty: field.ty,
                                    span: expr.span,
                                });
                            }
                        }
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Field {} not found for structure type {}",
                                name,
                                self.ctx.typename(object.ty)
                            ))
                            .with_labels(vec![Label::primary(file, expr.span)
                                .with_message("Structure field access occurs here")]));
                    }
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                            "Attempting to access field {} of expression of non-structure type {}",
                            name,
                            self.ctx.typename(object.ty)
                        ))
                            .with_labels(vec![Label::primary(file, expr.span)
                                .with_message("Field access occurs here")]))
                    }
                }
            }
            ExprNode::Call(fun_ast, args) => {
                let fun_ir = self.lower_expr(module, file, fun, fun_ast)?;
                match self.ctx[self.ctx.unwrap_alias(fun_ir.ty)].clone() {
                    IrType::Fun(fun_ty) => {
                        let args = args
                            .iter()
                            .map(|arg| self.lower_expr(module, file, fun, arg))
                            .collect::<Result<Vec<IrExpr>, _>>()?;
                        self.typecheck_fun(file, expr.span, &fun_ty, &args)?;

                        IrExpr {
                            kind: IrExprKind::Call(Box::new(fun_ir), args),
                            ty: fun_ty.return_ty,
                            span: expr.span,
                        }
                    }
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Attempting to call expression of non-function pointer type {}",
                                self.ctx.typename(fun_ir.ty)
                            ))
                            .with_labels(vec![Label::primary(file, expr.span)
                                .with_message("Call expression occurs here")]))
                    }
                }
            }
            ExprNode::If(expr) => return self.lower_if(module, file, fun, expr),
            ExprNode::Loop(stmts) => return self.lower_loop(module, file, fun, expr.span, &stmts),
            ExprNode::Match(match_expr) => {
                return self.lower_match(module, file, fun, match_expr, expr.span)
            }
            ExprNode::Unary(op, expr) => return self.lower_unary(module, file, fun, *op, &expr),
            ExprNode::Bin(lhs, op, rhs) => {
                return self.lower_bin(module, file, fun, &lhs, *op, &rhs)
            }
            ExprNode::Cast(ty, expr) => {
                let ty = self.resolve_type(ty, module, file, expr.span)?;
                return self.lower_cast(module, file, fun, expr, ty);
            }
            ExprNode::Index(obj, idx) => {
                let obj = self.lower_expr(module, file, fun, obj)?;
                let obj_ty = self.ctx.unwrap_alias(obj.ty);
                let elem_ty = match self.ctx[obj_ty] {
                    IrType::Array(elem, _) => elem,
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Cannot index an expression of non-array type {}",
                                self.ctx.typename(obj.ty),
                            ))
                            .with_labels(vec![Label::primary(file, obj.span)
                                .with_message("Index expression appears here")]))
                    }
                };

                let idx = self.lower_expr(module, file, fun, idx)?;

                let idx_ty = self.ctx.unwrap_alias(idx.ty);
                if !matches!(&self.ctx[idx_ty], IrType::Integer(_)) {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "Cannot index an expression of array type {} with a value of non-integer type {}",
                            self.ctx.typename(obj.ty),
                            self.ctx.typename(idx.ty),
                        ))
                        .with_labels(vec![
                            Label::primary(file, idx.span)
                                .with_message(format!("Index of type {} appears here", self.ctx.typename(idx.ty))),
                            Label::secondary(file, obj.span)
                                .with_message(format!("Array expression of type {} appears here", self.ctx.typename(obj.ty))),
                        ])
                    );
                }

                IrExpr {
                    span: expr.span,
                    ty: elem_ty,
                    kind: IrExprKind::Index(Box::new(obj), Box::new(idx)),
                }
            }
            ExprNode::Literal(lit) => match lit {
                Literal::String(s) => IrExpr {
                    span: expr.span,
                    ty: self.ctx.types.insert(IrType::Ptr(IrContext::U8)),
                    kind: IrExprKind::Lit(IrLiteral::String(s.clone())),
                },
                Literal::Bool(b) => IrExpr {
                    span: expr.span,
                    ty: IrContext::BOOL,
                    kind: IrExprKind::Lit(IrLiteral::Bool(*b)),
                },
                Literal::Char(c) => IrExpr {
                    span: expr.span,
                    ty: IrContext::CHAR,
                    kind: IrExprKind::Lit(IrLiteral::Char(*c)),
                },
                Literal::Unit => IrExpr {
                    span: expr.span,
                    ty: IrContext::UNIT,
                    kind: IrExprKind::Lit(IrLiteral::Unit),
                },
                Literal::Array(exprs) => {
                    let exprs = exprs
                        .iter()
                        .map(|expr| self.lower_expr(module, file, fun, expr))
                        .collect::<Result<Vec<_>, _>>()?;

                    let ty = if exprs.len() > 1 {
                        let ty = exprs.first().unwrap().ty;
                        for (i, elem) in exprs.iter().enumerate() {
                            if elem.ty != ty {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "Element {} of array literal has type {}, but array element type is {}", 
                                        i,
                                        self.ctx.typename(elem.ty),
                                        self.ctx.typename(ty),
                                    ))
                                    .with_labels(vec![
                                        Label::primary(file, elem.span)
                                            .with_message(format!(
                                                "Expression of type {} appears here",
                                                self.ctx.typename(elem.ty),
                                            )),
                                        Label::secondary(file, expr.span)
                                            .with_message(format!(
                                                "Array literal here has element type {}",
                                                self.ctx.typename(ty)
                                            ))
                                    ])
                                );
                            }
                        }
                        ty
                    } else {
                        IrContext::UNIT
                    };

                    IrExpr {
                        span: expr.span,
                        ty: self.ctx.types.insert(IrType::Array(ty, exprs.len() as u64)),
                        kind: IrExprKind::Lit(IrLiteral::Array(exprs)),
                    }
                }
                Literal::Struct { ty, fields } => {
                    let ty = ty
                        .as_ref()
                        .map(|ty| self.resolve_type(ty, module, file, expr.span))
                        .map_or(Ok(None), |ty| ty.map(Some))?;
                    let unwrapped = ty.map(|ty| self.ctx.unwrap_alias(ty));

                    let struct_ty = if let Some(unwrapped) = unwrapped {
                        match self.ctx[unwrapped] {
                            IrType::Struct(ref fields) => Some(fields.clone()),
                            _ => {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                    "Cannot create a structure literal of non-structure type {}",
                                    self.ctx.typename(unwrapped)
                                ))
                                    .with_labels(vec![Label::primary(file, expr.span)
                                        .with_message("Structure literal appears here")]))
                            }
                        }
                    } else {
                        None
                    };

                    let fields = fields
                        .iter()
                        .map(|(name, field)| {
                            if let Some(struct_ty) = &struct_ty {
                                if struct_ty.field_ty(name).is_none() {
                                    return Err(Diagnostic::error()
                                        .with_message(format!(
                                            "Structure literal assigns a value for field named {}, but structure type {} contains no such field",
                                            name,
                                            self.ctx.typename(ty.unwrap())
                                        ))
                                    )
                                }
                            }

                            Ok((*name, self.lower_expr(module, file, fun, field)? ))
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let lit_expr = IrExpr {
                        span: expr.span,
                        ty: self.ctx.types.insert(IrType::Struct(IrStructType {
                            fields: fields
                                .iter()
                                .map(|(name, expr)| IrStructField {
                                    name: *name,
                                    ty: expr.ty,
                                })
                                .collect(),
                        })),
                        kind: IrExprKind::Lit(IrLiteral::Struct(fields)),
                    };

                    match ty {
                        Some(ty) => IrExpr {
                            span: expr.span,
                            ty,
                            kind: IrExprKind::Cast(Box::new(lit_expr), ty),
                        },
                        None => lit_expr,
                    }
                }
                Literal::Number(num) => {
                    let (signed, ty) = match num.annotation().unwrap_or_else(|| {
                        if matches!(num, NumberLiteral::Float(..)) {
                            NumberLiteralAnnotation::F32
                        } else {
                            NumberLiteralAnnotation::I32
                        }
                    }) {
                        NumberLiteralAnnotation::I8 => (true, IrContext::I8),
                        NumberLiteralAnnotation::I16 => (true, IrContext::I16),
                        NumberLiteralAnnotation::I32 => (true, IrContext::I32),
                        NumberLiteralAnnotation::I64 => (true, IrContext::I64),

                        NumberLiteralAnnotation::U8 => (false, IrContext::U8),
                        NumberLiteralAnnotation::U16 => (false, IrContext::U16),
                        NumberLiteralAnnotation::U32 => (false, IrContext::U32),
                        NumberLiteralAnnotation::U64 => (false, IrContext::U64),

                        NumberLiteralAnnotation::F32 => (false, IrContext::F32),
                        NumberLiteralAnnotation::F64 => (false, IrContext::F64),

                        NumberLiteralAnnotation::Usz => (false, IrContext::USIZE),
                        NumberLiteralAnnotation::Isz => (false, IrContext::ISIZE),
                    };

                    let lit = match num {
                        NumberLiteral::Integer(num, _) => IrExpr {
                            span: expr.span,
                            ty: if signed {
                                IrContext::I64
                            } else {
                                IrContext::U64
                            },
                            kind: IrExprKind::Lit(IrLiteral::Integer(
                                *num,
                                IrIntegerType {
                                    width: IntegerWidth::SixtyFour,
                                    signed,
                                },
                            )),
                        },
                        NumberLiteral::Float(num, _) => IrExpr {
                            span: expr.span,
                            ty: IrContext::F64,
                            kind: IrExprKind::Lit(IrLiteral::Float(
                                *num,
                                IrFloatType { doublewide: true },
                            )),
                        },
                    };

                    IrExpr {
                        span: expr.span,
                        ty,
                        kind: IrExprKind::Cast(Box::new(lit), ty),
                    }
                }
            },
            ExprNode::Block(b) => {
                let old_bb = self.bb();
                let new_bb = self.ctx.bb();
                self.ctx[old_bb].terminator = IrTerminator::Jmp(new_bb);
                *self.bb_mut() = new_bb;

                let after_bb = self.ctx.bb();
                let phi_var = self.ctx.vars.insert(IrVar {
                    ty: IrContext::INVALID,
                    name: Symbol::new(format!("@phi_var#{}", new_bb)),
                });
                self.ctx[old_bb].stmts.push(IrStmt {
                    span: expr.span,
                    kind: IrStmtKind::VarLive(phi_var),
                });
                self.scope_stack.push(ScopePlate {
                    vars: HashMap::new(),
                    return_var: Some(phi_var),
                    after_bb,
                });
                self.lower_block(module, file, fun, &b)?;
                self.scope_stack.pop();
                IrExpr {
                    span: expr.span,
                    ty: self.ctx[phi_var].ty,
                    kind: IrExprKind::Var(phi_var),
                }
            }
        })
    }

    /// Lower an if statement to IR, including new basic blocks and jumps
    fn lower_if(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        expr: &If,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        let old_bb = self.bb();
        let if_cond = self.lower_expr(module, file, fun, &expr.cond)?;

        let if_body_bb = self.ctx.bb();
        let after_bb = self.ctx.bb();
        let phi_var = self.ctx.vars.insert(IrVar {
            ty: IrContext::INVALID,
            name: Symbol::new(format!("@phi_var#{}", if_body_bb)),
        });
        let bb = self.bb();
        self.ctx[bb].stmts.push(IrStmt {
            span: expr.cond.span,
            kind: IrStmtKind::VarLive(phi_var),
        });

        self.scope_stack.push(ScopePlate {
            vars: HashMap::new(),
            return_var: Some(phi_var),
            after_bb,
        });
        *self.bb_mut() = if_body_bb;
        self.lower_block(module, file, fun, &expr.body)?;
        self.scope_stack.pop();
        match &expr.else_expr {
            Some(ElseExpr::ElseIf(expr)) => {
                let else_bb = self.ctx.bb();
                self.scope_stack.push(ScopePlate {
                    vars: HashMap::new(),
                    return_var: Some(phi_var),
                    after_bb,
                });
                *self.bb_mut() = else_bb;
                self.lower_if(module, file, fun, &expr)?;
                self.scope_stack.pop();
                self.ctx[old_bb].terminator = IrTerminator::JmpIf {
                    condition: if_cond,
                    if_true: if_body_bb,
                    if_false: else_bb,
                };
            }
            Some(ElseExpr::Else(body)) => {
                let else_bb = self.ctx.bb();
                self.scope_stack.push(ScopePlate {
                    vars: HashMap::new(),
                    return_var: Some(phi_var),
                    after_bb,
                });
                *self.bb_mut() = else_bb;
                self.lower_block(module, file, fun, &body)?;
                self.scope_stack.pop();
                self.ctx[old_bb].terminator = IrTerminator::JmpIf {
                    condition: if_cond,
                    if_true: if_body_bb,
                    if_false: else_bb,
                };
            }
            None => {
                self.ctx[old_bb].terminator = IrTerminator::JmpIf {
                    condition: if_cond,
                    if_true: if_body_bb,
                    if_false: after_bb,
                };
            }
        }

        *self.bb_mut() = after_bb;

        Ok(IrExpr {
            span: expr.cond.span,
            ty: self.ctx[phi_var].ty,
            kind: IrExprKind::Var(phi_var),
        })
    }

    /// Lower a match expression, returning an IrExpr representing a load of the phi allocation
    fn lower_match(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        expr: &Match,
        span: Span,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        let old_bb = self.bb();
        let matched = self.lower_expr(module, file, fun, &expr.matched)?;
        let after_bb = self.ctx.bb();
        let phi_var = self.ctx.vars.insert(IrVar {
            ty: IrContext::INVALID,
            name: Symbol::new(format!("@phi_var#{}", old_bb)),
        });
        self.ctx[old_bb].stmts.push(IrStmt {
            span,
            kind: IrStmtKind::VarLive(phi_var),
        });

        self.scope_stack.push(ScopePlate {
            vars: HashMap::new(),
            return_var: Some(phi_var),
            after_bb,
        });
        let cases = expr
            .cases
            .iter()
            .map(|(ty, stmt)| {
                let ty = self.resolve_type(ty, module, file, span)?;
                if let IrType::Sum(variants) = &self.ctx[self.ctx.unwrap_alias(matched.ty)] {
                    if !variants.contains(&ty) {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "Match arm with type {} but sum type {} does not contain this variant",
                                self.ctx.typename(ty),
                                self.ctx.typename(matched.ty),
                            ))
                            .with_labels(vec![
                                Label::primary(file, stmt.span)
                                    .with_message("In this match arm"),
                                Label::primary(file, matched.span)
                                    .with_message(format!("Matched value of type {} appears here", self.ctx.typename(matched.ty)))
                            ])
                        )
                    }
                } else {
                    return Err(Diagnostic::error()
                        .with_message(format!("Cannot match an expression of type {}", self.ctx.typename(matched.ty)))
                        .with_labels(vec![
                            Label::primary(file, matched.span)
                                .with_message(format!("Expression of type {} appears here", self.ctx.typename(matched.ty))),
                            Label::secondary(file, span)
                                .with_message("Match expression appears here"),
                        ])
                    )
                }
                let arm_bb = self.ctx.bb();
                *self.bb_mut() = arm_bb;
                self.lower_stmt(module, file, fun, stmt)?;
                if matches!(self.ctx[arm_bb].terminator, IrTerminator::Invalid) {
                    self.ctx[arm_bb].terminator = IrTerminator::Jmp(after_bb);
                }
                Ok((ty, arm_bb))
            })
            .collect::<Result<Vec<_>, _>>()?;

        self.ctx[old_bb].terminator = IrTerminator::JmpMatch {
            variant: matched,
            discriminants: cases,
            default_jmp: after_bb,
        };
        self.scope_stack.pop();

        Ok(IrExpr {
            span,
            ty: self.ctx[phi_var].ty,
            kind: IrExprKind::Var(phi_var),
        })
    }
    
    /// Lower a loop statement or expression
    fn lower_loop(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        span: Span,
        stmts: &[Stmt],
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        let old_bb = self.bb();
        let phi_var = self.ctx.vars.insert(IrVar {
            ty: IrContext::INVALID,
            name: Symbol::new(format!("@phi_var#{}", old_bb)),
        });
        let bb = self.bb();
        self.ctx[bb].stmts.push(IrStmt {
            span,
            kind: IrStmtKind::VarLive(phi_var),
        });

        let loop_bb = self.ctx.bb();
        let after_bb = self.ctx.bb();

        self.scope_stack.push(ScopePlate {
            vars: HashMap::new(),
            return_var: Some(phi_var),
            after_bb,
        });
        *self.bb_mut() = loop_bb;

        for stmt in stmts {
            self.lower_stmt(module, file, fun, stmt)?;
        }

        if matches!(self.ctx[self.bb()].terminator, IrTerminator::Invalid) {
            let bb = self.bb();
            self.ctx[bb].terminator = IrTerminator::Jmp(loop_bb);
        }

        
        self.scope_stack.pop();
        self.ctx[old_bb].terminator = IrTerminator::Jmp(loop_bb);

        Ok(IrExpr {
            span,
            ty: self.ctx[phi_var].ty,
            kind: IrExprKind::Var(phi_var),
        })
    }

    fn lower_block(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        stmts: &[Stmt],
    ) -> Result<(), Diagnostic<FileId>> {
        for stmt in stmts.iter() {
            self.lower_stmt(module, file, fun, stmt)?;
        }

        if matches!(self.ctx[self.bb()].terminator, IrTerminator::Invalid) {
            let current = self.bb();
            self.ctx[current].terminator = IrTerminator::Jmp(self.current_scope().after_bb);
            *self.bb_mut() = self.current_scope().after_bb;
        }

        Ok(())
    }

    /// Ensure that the passed arguments to the given function are of the correct type
    fn typecheck_fun(
        &self,
        file: FileId,
        span: Span,
        fun_ty: &FunType,
        args: &[IrExpr],
    ) -> Result<(), Diagnostic<FileId>> {
        if args.len() != fun_ty.params.len() {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "Expected {} arguments when calling function, found {}",
                    fun_ty.params.len(),
                    args.len()
                ))
                .with_labels(vec![
                    Label::primary(file, span).with_message("Call expression occurs here")
                ]));
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
                        Label::secondary(file, span)
                            .with_message("Call expression occurs here")
                    ])
                );
            }
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

    fn lowest_scope(&self) -> &ScopePlate {
        self.scope_stack
            .first()
            .expect("Internal compiler error: scope stack is empty")
    }

    fn lowest_scope_mut(&mut self) -> &mut ScopePlate {
        self.scope_stack
            .first_mut()
            .expect("Internal compiler error: scope stack is empty")
    }
}
