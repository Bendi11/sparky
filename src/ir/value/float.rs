use super::*;

/// Any expression that produces a floating point value
pub enum IrFloatValueKind {
    
}

/// An expression that produces a floating point value
pub struct IrFloatValue {
    /// Span in the source file of the parent function that this expression occupies
    pub span: Span,
    pub kind: IrFloatValueKind,
}

