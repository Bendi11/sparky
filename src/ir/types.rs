use crate::{Symbol, ast::IntegerWidth};

use super::TypeId;

/// The signature of a function with argument and return types
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FunType {
    /// Type that the IR function must return
    pub return_ty: TypeId, 
    /// Arguments types with optional names
    pub params: Vec<(TypeId, Option<Symbol>)>,
}

/// Data for an [IRType] that contains the actual type data
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum IrType {
    /// An integer type with width and signedness
    Integer {
        signed: bool,
        width: IntegerWidth,
    },
    /// A 32 or 64 bit floating point type
    Float { 
        doublewide: bool,
    },
    /// Unnamed structure type with fields
    Struct(Vec<(TypeId, Symbol)>),
    /// Sum type that can be many different types
    Sum(Vec<TypeId>),
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
    Array(TypeId, u64),
    /// Pointer to a type
    Ptr(TypeId),
    /// Function type
    Fun(FunType),
    /// Never used except by the IR lowerer
    Invalid,
}
