pub mod boolean;
pub mod float;
pub mod integer;
pub mod pointer;

pub use boolean::*;
pub use float::*;
pub use integer::*;
pub use pointer::*;

use crate::util::loc::Span;

use super::{VarId, types::IrType, IrContext, TypeId};

/// An expression in the IR, can only be assigned to variables or used in some statements
pub enum IrAnyValue {
    /// Any expression producing a pointer value
    Pointer(IrPointerValue),
    /// Any expression producing an integer value
    Integer(IrIntegerValue),
    /// Any expression producing a boolean value
    Boolean(IrBoolValue),
}

impl IrAnyValue {
    /// Get the type of this expression
    pub fn ty(&self, ctx: &mut IrContext) -> TypeId {
        match self {
            Self::Pointer(v) => v.kind.ty(ctx),
            Self::Integer(v) => v.kind.ty(ctx),
            Self::Boolean(_) => IrContext::BOOL,
        } 
    }
}
