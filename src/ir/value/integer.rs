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
    FloatCast(Box<IrFloatValue>, IrIntegerType),
}

/// [IrIntegerValueKind] with additional location data
pub struct IrIntegerValue {
    /// Span in the source code that contains this integer value
    pub loc: Span,
    /// The kind of integer-valued expression
    pub kind: IrIntegerValueKind,
}

impl IrIntegerValueKind {
    pub fn ty(&self, ctx: &mut IrContext) -> TypeId {
        match self {
            Self::Add(v, _) |
            Self::Sub(v, _) |
            Self::Mul(v, _) |
            Self::Div(v, _) |
            Self::Mod(v, _) => v.kind.ty(ctx),

            Self::PtrCast(_, ty) | Self::FloatCast(_, ty) => ctx.types.insert((*ty).into()),
        }
    }
}
