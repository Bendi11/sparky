use super::*;

/// An expression that produces a pointer
pub enum IrPointerValueKind {
    /// Taking the address of a stack-allocated value
    AddrOf(VarId),
    
    /// Accessing pointer-valued variable
    Var(VarId),
}

/// An expression producing a pointer value with additional span information
pub struct IrPointerValue {
    /// Location in the source code of this expression
    pub loc: Span,
    /// Kind of expression that produces a pointer
    pub kind: IrPointerValueKind,
}
