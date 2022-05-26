use super::*;

/// Expression with source code info and 
pub struct IrRvalue {
    /// Location of the value in code
    pub loc: Loc,
    /// Data representing the rvalue
    pub kind: IrRvalueKind
}

/// A binary operation with explicit types
pub enum BinaryOp {

}

/// An expression that produces a pointer
pub enum IrPointerValueKind {
    /// Taking the address of a stack-allocated value
    AddrOf(VarId),
}

/// An expression producing a pointer value with additional span information
pub struct IrPointerValue {
    /// Location in the source code of this expression
    pub loc: Span,
    /// Kind of expression that produces a pointer
    pub kind: IrPointerValueKind,
}

/// An expression producing an integer value
pub enum IrIntegerValueKind {
    Add(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Sub(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Mul(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Div(Box<IrIntegerValue>, Box<IrIntegerValue>),
    Mod(Box<IrIntegerValue>, Box<IrIntegerValue>),
    /// Pointer-to-int cast
    PtrCast(IrPointerValue),
}

/// [IrIntegerValueKind] with additional location data
pub struct IrIntegerValue {
    /// Span in the source code that contains this integer value
    pub loc: Span,
    /// The kind of integer-valued expression
    pub kind: IrPointerValueKind,
}

/// An expression in the IR, can only be assigned to variables or used in some statements
pub enum IrRvalueKind {
    /// Binary operation on two values
    BinaryOp(BinaryOp),
    /// Taking the ddress of another value
    AddrOf(Box<IrValue>),
    /// Dereferencing a pointer value
    Deref(Box<IrValue>)
}

