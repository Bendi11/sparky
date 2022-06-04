pub mod boolean;
pub mod float;
pub mod integer;
pub mod pointer;

pub use boolean::*;
pub use float::*;
pub use integer::*;
pub use pointer::*;

use crate::util::loc::Span;

use super::VarId;

/// Expression with source code info and
pub struct IrAnyValue {
    /// Location of the value in code
    pub loc: Span,
    /// Data representing the rvalue
    pub kind: IrAnyRvalueKind,
}

/// An expression in the IR, can only be assigned to variables or used in some statements
pub enum IrAnyRvalueKind {
    /// Dereferencing a pointer value
    Deref(IrPointerValue),
    /// Accessing a declared variable's value
    Var(VarId),
    /// Any expression producing a pointer value
    Pointer(IrPointerValue),
    /// Any expression producing an integer value
    Integer(IrIntegerValue),
    /// Any expression producing a boolean value
    Boolean(IrBoolValue),
}
