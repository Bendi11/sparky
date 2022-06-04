use crate::{ir::TypeId, Symbol};

use super::IrType;

/// Type of a function
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct IrFunType {
    /// Return type of the function
    pub return_ty: TypeId,
    /// Argument types and order of the function
    pub args: Vec<(TypeId, Option<Symbol>)>,
}

impl From<IrFunType> for IrType {
    fn from(ty: IrFunType) -> Self {
        Self::Fun(ty)
    }
}
