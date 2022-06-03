use crate::ir::TypeId;

use super::IrType;


/// A 'tagged union' type that contains a discriminant value and can be one of many types
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IrSumType {
    pub variants: Vec<TypeId>,
}

impl From<IrSumType> for IrType {
    fn from(ty: IrSumType) -> Self {
        Self::Sum(ty)
    }
}
