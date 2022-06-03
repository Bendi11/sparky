use super::*;


/// Any expression that produces a boolean value
pub enum IrBoolValueKind {
    ///Comparing two integer values for equality
    IntegerEq(IrIntegerValue, IrIntegerValue),
    ///Comparing two floating point values for equality
    FloatEq(IrFloatValue, IrFloatValue),
    /// Comparing two other boolean values for equality
    BooleanEq(Box<IrBoolValue>, Box<IrBoolValue>),
    
    /// Comparing two pointers for equality
    PtrEq(IrPointerValue, IrPointerValue),
    
    /// Integer less than comparison
    ILt(IrIntegerValue, IrIntegerValue),
    /// Integer less than or equal to comparison
    ILe(IrIntegerValue, IrIntegerValue),
    /// Integer greater than comparision
    IGt(IrIntegerValue, IrIntegerValue),
    /// Integer greater than or equal to comparision
    IGe(IrIntegerValue, IrIntegerValue),

    /// Float less than comparision
    FLt(IrFloatValue, IrFloatValue),
    /// Float less than or equal to comparison
    FLe(IrFloatValue, IrFloatValue),
    /// Float greater than comparison
    FGt(IrFloatValue, IrFloatValue),
    /// Float greater than or equal to comparison
    FGe(IrFloatValue, IrFloatValue),
    
    /// Accessing a boolean-valued variables
    Var(VarId),
}

/// An expression that produces a boolean value
pub struct IrBoolValue {
    /// Span in the parent function's file that this expression opccupies
    pub span: Span,
    pub kind: IrBoolValueKind,
}

