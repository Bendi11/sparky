use crate::ir::TypeId;

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

impl IrPointerValueKind {
    /// Get the type that this pointer value points to
    pub fn ty(&self, ctx: &mut IrContext) -> TypeId {
        match self {
            Self::AddrOf(var) => ctx.types.insert(IrType::Ptr(ctx[*var].ty)),
            Self::Var(var) => ctx[*var].ty,
        }
    }
}
