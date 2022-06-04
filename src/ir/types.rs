use crate::Symbol;

use self::{
    array::IrArrayType, float::IrFloatType, fun::IrFunType, integer::IrIntegerType,
    structure::IrStructType, sum::IrSumType,
};

use super::TypeId;

pub mod array;
pub mod float;
pub mod fun;
pub mod integer;
pub mod structure;
pub mod sum;

/// Data for an [IRType] that contains the actual type data
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum IrType {
    /// An integer type with width and signedness
    Integer(IrIntegerType),
    /// A 32 or 64 bit floating point type
    Float(IrFloatType),
    /// Unnamed structure type with fields
    Struct(IrStructType),
    /// Sum type that can be many different types
    Sum(IrSumType),
    /// Boolean true or false type
    Bool,
    /// Unit type with a single value
    Unit,
    /// User-defined alias type
    Alias {
        /// Name of the aliased type
        name: Symbol,
        /// Aliased type
        ty: TypeId,
    },
    /// Array with compile-time known length and element type
    Array(IrArrayType),
    /// Pointer to a type
    Ptr(TypeId),
    /// Function type
    Fun(IrFunType),
    /// Never used except by the IR lowerer
    Invalid,
}
