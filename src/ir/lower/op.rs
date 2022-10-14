use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ast::Expr,
    ir::{
        types::IrTypeTemplate,
        value::{IrExpr, IrExprKind},
        FunId, IrContext, TypeId,
    },
    parse::token::Op,
    util::{files::FileId, loc::Span},
};

use super::{IntermediateModuleId, IrLowerer};

impl<'ctx> IrLowerer<'ctx> {
    /// Lower a binary expression to IR
    pub fn lower_bin(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        lhs: &Expr,
        op: Op,
        rhs: &Expr,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        let lhs = self.lower_expr(module, file, fun, lhs)?;
        let rhs = self.lower_expr(module, file, fun, rhs)?;

        let ty = match (&self.ctx[lhs.ty], op, &self.ctx[rhs.ty]) {
            (
                IrTypeTemplate::Bool,
                Op::LogicalAnd | Op::LogicalOr | Op::LogicalNot | Op::Eq,
                IrTypeTemplate::Bool,
            ) => IrContext::BOOL,
            (
                IrTypeTemplate::Integer(_),
                Op::Eq
                | Op::Greater
                | Op::GreaterEq
                | Op::Less
                | Op::LessEq
                | Op::Star
                | Op::Div
                | Op::Add
                | Op::Sub
                | Op::ShLeft
                | Op::ShRight,
                IrTypeTemplate::Integer(_),
            ) => lhs.ty,
            (
                IrTypeTemplate::Float(_),
                Op::Eq
                | Op::Greater
                | Op::GreaterEq
                | Op::Less
                | Op::LessEq
                | Op::Star
                | Op::Div
                | Op::Add
                | Op::Sub,
                IrTypeTemplate::Float(_),
            ) => lhs.ty,
            (IrTypeTemplate::Ptr(_), Op::ShRight | Op::ShLeft, IrTypeTemplate::Integer(_)) => lhs.ty,
            (IrTypeTemplate::Ptr(_), Op::Add | Op::Sub, IrTypeTemplate::Ptr(_) | IrTypeTemplate::Integer(_)) => lhs.ty,
            _ => {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "Cannot apply binary operator {} to operand types {} and {}",
                        op,
                        self.ctx.typename(lhs.ty),
                        self.ctx.typename(rhs.ty),
                    ))
                    .with_labels(vec![
                        Label::primary(file, Span::from(lhs.span.from..rhs.span.to)),
                        Label::secondary(file, lhs.span).with_message(format!(
                            "LHS of type {} appears here",
                            self.ctx.typename(lhs.ty)
                        )),
                        Label::secondary(file, rhs.span).with_message(format!(
                            "RHS of type {} appears here",
                            self.ctx.typename(rhs.ty)
                        )),
                    ]))
            }
        };

        Ok(IrExpr {
            span: (lhs.span.from..rhs.span.to).into(),
            ty,
            kind: IrExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
        })
    }

    /// Lower a unary expression to IR
    pub fn lower_unary(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        op: Op,
        expr: &Expr,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        let expr = self.lower_expr(module, file, fun, expr)?;

        let ty = match (op, self.ctx[expr.ty].clone()) {
            (Op::Star, IrTypeTemplate::Ptr(to)) => to,
            (Op::AND, _) => self.ctx.types.insert(IrTypeTemplate::Ptr(expr.ty)),
            (Op::Sub, IrTypeTemplate::Integer(_) | IrTypeTemplate::Float(_)) => expr.ty,
            (Op::NOT, IrTypeTemplate::Integer(_) | IrTypeTemplate::Ptr(_)) => expr.ty,
            _ => {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "Cannot apply unary operator {} to expression of type {}",
                        op,
                        self.ctx.typename(expr.ty),
                    ))
                    .with_labels(vec![Label::primary(file, expr.span)]))
            }
        };

        Ok(IrExpr {
            ty,
            span: expr.span,
            kind: IrExprKind::Unary(op, Box::new(expr)),
        })
    }

    /// Lower and typecheck a cast expression
    pub fn lower_cast(
        &mut self,
        module: IntermediateModuleId,
        file: FileId,
        fun: FunId,
        expr: &Expr,
        ty: TypeId,
    ) -> Result<IrExpr, Diagnostic<FileId>> {
        let expr = self.lower_expr(module, file, fun, expr)?;

        let uty = self.ctx.unwrap_alias(ty);
        let uexprty = self.ctx.unwrap_alias(expr.ty);
        match (&self.ctx[uexprty], &self.ctx[uty]) {
            (IrTypeTemplate::Float(_) | IrTypeTemplate::Integer(_), IrTypeTemplate::Integer(_) | IrTypeTemplate::Float(_)) => (),
            (IrTypeTemplate::Ptr(_) | IrTypeTemplate::Integer(_), IrTypeTemplate::Ptr(_) | IrTypeTemplate::Integer(_)) => (),
            (IrTypeTemplate::Ptr(_) | IrTypeTemplate::Fun(_), IrTypeTemplate::Ptr(_) | IrTypeTemplate::Fun(_)) => (),
            (IrTypeTemplate::Integer(_) | IrTypeTemplate::Char, IrTypeTemplate::Integer(_) | IrTypeTemplate::Char) => (),
            (IrTypeTemplate::Sum(s), _) if s.contains(&uty) => (),
            (_, IrTypeTemplate::Sum(s)) if s.contains(&expr.ty) => (),
            (from, to) if from == to => (),
            _ => {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "Cannot cast an expression of type {} to {}",
                        self.ctx.typename(expr.ty),
                        self.ctx.typename(uty),
                    ))
                    .with_labels(vec![Label::primary(file, expr.span)
                        .with_message("Cast expression appears here")]))
            }
        }

        Ok(IrExpr {
            span: expr.span,
            ty,
            kind: IrExprKind::Cast(Box::new(expr), ty),
        })
    }
}
