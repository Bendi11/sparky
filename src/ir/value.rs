use crate::{util::loc::Span, parse::token::Op};

use super::{VarId, types::IrType, IrContext, TypeId, FunId};

/// Structure containing an [IrExprKind] plus location data for error messages
#[derive(Clone, Debug,)]
pub struct IrExpr {
    /// Location in the source file of this expression
    pub span: Span,
    /// What kind of expression this is
    pub kind: IrExprKind,
}

/// Literal in the IR containing any user-created literal value
#[derive(Clone, Debug,)]
pub enum IrLiteral {
    
}

/// Enumeration containing all expressions that produce a value in the intermediate representation
#[derive(Clone, Debug)]
pub enum IrExprKind {
    /// Variable access with the ID of the variable
    Var(VarId), 
    /// A literal value 
    Lit(IrLiteral),
    /// Binary expression
    Binary(Box<IrExpr>, Op, Box<IrExpr>),
    /// Unary operator applied to a single operand
    Unary(Op, Box<IrExpr>),
    /// Calling an expression that must be function type
    Call(Box<IrExpr>, Vec<IrExpr>),
    /// Taking the address of a function
    Fun(FunId),
}
