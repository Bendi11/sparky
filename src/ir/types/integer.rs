use crate::ast::IntegerWidth;

use super::IrType;

/// Integer type containing signedness and bit width information
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IrIntegerType {
    /// If this integer type is signed
    pub signed: bool,
    /// Number of bits that this integer type contains
    pub width: IntegerWidth,
}

impl From<IrIntegerType> for IrType {
    fn from(ty: IrIntegerType) -> Self {
        Self::Integer(ty)
    }
}
