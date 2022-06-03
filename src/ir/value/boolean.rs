use super::*;


/// Any expression that produces a boolean value
pub enum IrBoolValueKind {
    ///Comparing two integer values for equality
    IntegerEq(IrIntegerValue, IrIntegerValue),
    ///Comparing two floating point values for equality
    FloatEq(IrFloatValue, IrFloatValue),
    /// Comparing two other boolean values for equality
    BooleanEq(Box<IrBoolValue>, Box<IrBoolValue>),
}

/// An expression that produces a boolean value
pub struct IrBoolValue {
    /// Span in the parent function's file that this expression opccupies
    pub span: Span,
    pub kind: IrBoolValueKind,
}

