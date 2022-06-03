use super::*;

/// Any expression that produces a floating point value
pub enum IrFloatValueKind {
    /// Add two floating point values together
    Add(Box<IrFloatValue>, Box<IrFloatValue>),
    /// Subtract the right hand side from the left hand side operand
    Sub(Box<IrFloatValue>, Box<IrFloatValue>),
    /// Multiply two floating point values
    Mul(Box<IrFloatValue>, Box<IrFloatValue>),
    /// Divide the right hand side from the left hand side
    Div(Box<IrFloatValue>, Box<IrFloatValue>),

    /// Access a variable of floating point type
    Var(VarId),
    
    /// Integer to float cast
    IntCast()
}

/// An expression that produces a floating point value
pub struct IrFloatValue {
    /// Span in the source file of the parent function that this expression occupies
    pub span: Span,
    pub kind: IrFloatValueKind,
}

