use crate::{ast::BigInt, parse::token::Op, util::loc::Span, Symbol};

use super::{
    types::{IrFloatType, IrIntegerType},
    FunId, GlobalId, TypeId, VarId,
};

/// Structure containing an [IrExprKind] plus location data for error messages
#[derive(Clone, Debug)]
pub struct IrExpr {
    /// Location in the source file of this expression
    pub span: Span,
    /// What kind of expression this is
    pub kind: IrExprKind,
    /// What type the expression's value is
    pub ty: TypeId,
}

/// Literal in the IR containing any user-created literal value
#[derive(Clone, Debug)]
pub enum IrLiteral {
    Integer(BigInt, IrIntegerType),
    Float(f64, IrFloatType),
    Char(char),
    String(String),
    Bool(bool),
    Array(Vec<IrExpr>),
    Struct(Vec<(Symbol, IrExpr)>),
    Unit,
}

/// Enumeration containing all expressions that produce a value in the intermediate representation
#[derive(Clone, Debug)]
pub enum IrExprKind {
    /// Variable access with the ID of the variable
    Var(VarId),
    /// Global variable access
    Global(GlobalId),
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
    /// Accessing a field of an expression
    Member(Box<IrExpr>, usize),
    /// Casting an expression to another type
    Cast(Box<IrExpr>, TypeId),
    /// Indexing an array type with integer-valued index
    Index(Box<IrExpr>, Box<IrExpr>),
}
