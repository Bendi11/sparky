use crate::{ir::TypeId, Symbol};

use super::IrType;

/// An unnamed structure type mapping string field names to fields of a specific type,
/// preserving field ordering information
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IrStructType {
    /// A map of all field names to their type IDs,
    /// preserves field order data
    pub fields: Vec<(TypeId, Symbol)>,
}

impl From<IrStructType> for IrType {
    fn from(ty: IrStructType) -> Self {
        Self::Struct(ty)
    }
}
