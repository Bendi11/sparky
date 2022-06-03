use crate::Symbol;

use self::{integer::IrIntegerType, float::IrFloatType, structure::IrStructType, sum::IrSumType, fun::IrFunType, array::IrArrayType};

use super::TypeId;

pub mod float;
pub mod fun;
pub mod integer;
pub mod structure;
pub mod sum;
pub mod array;

/// Data for an [IRType] that contains the actual type data
#[derive(Clone, Hash, PartialEq, Eq)]
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
