use crate::{ast::IntegerWidth, Symbol, arena::Index, util::loc::Span};

use super::{TypeId, IrFunRef, generic::GenericArgs};

/// The signature of a function with argument and return types
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FunType {
    /// Type that the IR function must return
    pub return_ty: TypeId,
    /// Arguments types with optional names
    pub params: Vec<(TypeId, Option<Symbol>)>,
}

/// Structure representing signed and unsigned integer types of varying bit width
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrIntegerType {
    pub width: IntegerWidth,
    pub signed: bool,
}

/// Structure representing either a 32 bit or 64 bit float type
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct IrFloatType {
    pub doublewide: bool,
}

/// Structure contained in an [IrStructType] representing a single field of s structure type
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct IrStructField {
    pub ty: TypeId,
    pub name: Symbol,
}

/// Structure representing an anonymous structure type with fields
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct IrStructType {
    pub fields: Vec<IrStructField>,
}

/// A reference to an IrType with generic arguments
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct IrTypeRef {
    pub ty: Index<IrType>,
    pub args: GenericArgs,
}

/// Data for an [IRType] that contains the actual type data
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum IrType {
    /// An integer type with width and signedness
    Integer(IrIntegerType),
    /// A 32 or 64 bit floating point type
    Float(IrFloatType),
    /// A 4-byte UTF-32 character type
    Char,
    /// Unnamed structure type with fields
    Struct(IrStructType),
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

impl IrStructType {
    /// Get the field of this structure type by the given name
    pub fn field_ty(&self, name: &Symbol) -> Option<TypeId> {
        self.fields.iter().find_map(|field| {
            if field.name == *name {
                Some(field.ty)
            } else {
                None
            }
        })
    }

    /// Get the index of the given field name
    pub fn field_idx(&self, name: &Symbol) -> Option<usize> {
        self.fields
            .iter()
            .enumerate()
            .find_map(|(idx, field)| if field.name == *name { Some(idx) } else { None })
    }
}
