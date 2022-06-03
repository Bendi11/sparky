use crate::ir::TypeId;

use super::IrType;


/// Array type with a compile-time known length and element type
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IrArrayType {
    /// Type of the array elements
    pub element: TypeId,
    /// Constant size of the array
    pub len: u64,
}

impl From<IrArrayType> for IrType {
    fn from(ty: IrArrayType) -> Self {
        Self::Array(ty)
    }
}
