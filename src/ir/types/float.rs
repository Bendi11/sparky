use super::IrType;

/// Floating point type containing the width of the IEEE 754 value
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IrFloatType {
    /// If this is 32 or 64 bits wide
    pub doublewide: bool,
}

impl From<IrFloatType> for IrType {
    fn from(ty: IrFloatType) -> Self {
        Self::Float(ty)
    }
}
