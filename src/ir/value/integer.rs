use crate::ir::types::integer::IrIntegerType;

use super::*;

/// An expression producing an integer value
pub enum IrIntegerValueKind {
    Add(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Sub(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Mul(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Div(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Mod(Box<IrIntegerValue>, Box<IrIntegerValue>),
    /// Pointer-to-int cast
    PtrCast(IrPointerValue, IrIntegerType),
}

/// [IrIntegerValueKind] with additional location data
pub struct IrIntegerValue {
    /// Span in the source code that contains this integer value
    pub loc: Span,
    /// The kind of integer-valued expression
    pub kind: IrIntegerValueKind,
}
