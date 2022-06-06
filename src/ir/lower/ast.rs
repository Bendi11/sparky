use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::{Expr, ExprNode, ParsedModule, Stmt, StmtNode, Literal, NumberLiteral, NumberLiteralAnnotation, IntegerWidth},
    ir::{value::{IrIntegerValue, IrAnyValue, IrIntegerValueKind, IrFloatValue, IrFloatValueKind}, BBId, FunId, IrBB, IrFun, IrStmt, IrVar, VarId, types::{IrType, float::IrFloatType, integer::IrIntegerType}},
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
        stmt: &Stmt,
        bb: BBId,
    ) -> Result<(), Diagnostic<FileId>> {
        match &stmt.node {
            StmtNode::Let(let_stmt) => {
                let assigned = let_stmt
                    .assigned
                    .as_ref()
                    .map(|assigned| 
                        self.lower_expr(module, bb, assigned)
                    ).map_or(Ok(None), |v| v.map(Some))?;

                if let ExprNode::Access(ref var) = let_stmt.let_expr.node {
                    //Create a new variable if none exists
                    if var.len() == 1 && !self.lookup_var(&var.last()).is_none() {
                        let name = var.last();
                        let ty = match &let_stmt.ty {
                            Some(ty) => self.resolve_type(
                                ty,
                                module,
                                self.current_fun().file,
                                stmt.span,
                            )?,
                            None => match &assigned {
                                Some(assigned) => assigned.ty(self.ctx),
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
                            .ctx
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
        
    /// Lower an expression of any type
    fn lower_expr(
        &mut self,
        module: IntermediateModuleId,
        bb: BBId,
        expr: &Expr,
    ) -> Result<IrAnyValue, Diagnostic<FileId>> {
        Ok(match &expr.node {
            ExprNode::Cast(ty, rhs) => {
                let ty = self.resolve_type(ty, module, self.current_fun().file, expr.span)?;
                let rhs = self.lower_expr(module, bb, rhs)?;
                match &self.ctx.types[ty] {
                    IrType::Integer(casted) => match rhs {
                        IrAnyValue::Pointer(ptr) =>
                            IrAnyValue::Integer(IrIntegerValue {
                                loc: expr.span,
                                kind: IrIntegerValueKind::PtrCast(ptr, *casted),
                            }),
                        _ => unreachable!()
                    },
                    _ => return Err(Diagnostic::error()
                        //.with_message(format!("Cannot cast expression of type {} to {}", self.ctx.typename(ty), self.ctx.typename(self.ctx.typeof(rhs))))
                        .with_labels(vec![
                            Label::primary(self.current_fun().file, expr.span),
                        ])
                    )
                } 
            },
            ExprNode::Literal(literal) => match literal {
                Literal::Number(number) => match number {
                    NumberLiteral::Float(val, annot) => match annot {
                        Some(NumberLiteralAnnotation::F32) => IrAnyValue::Float(IrFloatValue {
                            span: expr.span,
                            kind: IrFloatValueKind::Literal(*val, IrFloatType { doublewide: false }),
                        }),
                        Some(NumberLiteralAnnotation::F64) => IrAnyValue::Float(IrFloatValue {
                            span: expr.span,
                            kind: IrFloatValueKind::Literal(*val, IrFloatType { doublewide: true }),
                        }),
                        Some(annot) => {
                            let literal = IrFloatValue { span: expr.span, kind: IrFloatValueKind::Literal(*val, IrFloatType { doublewide: true })};
                            IrAnyValue::Integer(
                                IrIntegerValue {
                                    loc: expr.span,
                                    kind: IrIntegerValueKind::FloatCast(literal, match annot {
                                        NumberLiteralAnnotation::U8 => IrIntegerType { signed: false, width: IntegerWidth::Eight },
                                        NumberLiteralAnnotation::U16 => IrIntegerType { signed: false, width: IntegerWidth::Sixteen },
                                        NumberLiteralAnnotation::U32 => IrIntegerType { signed: false, width: IntegerWidth::ThirtyTwo },
                                        NumberLiteralAnnotation::U64 => IrIntegerType { signed: false, width: IntegerWidth::SixtyFour },

                                        NumberLiteralAnnotation::I8 => IrIntegerType { signed: true, width: IntegerWidth::Eight },
                                        NumberLiteralAnnotation::I16 => IrIntegerType { signed: true, width: IntegerWidth::Sixteen },
                                        NumberLiteralAnnotation::I32 => IrIntegerType { signed: true, width: IntegerWidth::ThirtyTwo },
                                        NumberLiteralAnnotation::I64 => IrIntegerType { signed: true, width: IntegerWidth::SixtyFour },

                                        _ => unreachable!()
                                    }),
                                },
                            )
                        }
                    }
                }
            },
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
