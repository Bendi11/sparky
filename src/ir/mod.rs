//! Module containing definitions for structures representing type-lowered Intermediate
//! Representation created from an Abstract Syntax Tree

use crate::{ast::IntegerWidth, arena::{Interner, Index}};

/// An IR context containing arenas with all type definitons, function declarations / definitions,
/// and modules
pub struct IRContext {
    /// A container with all defined types 
    types: Interner<IRType>,
}

/// ID referencing an [IRType] in an [IRContext]
pub type TypeId = Index<IRType>;

/// Data for an [IRType] that contains the actual type data
pub enum IRType {
    /// An integer type with width and signedness
    Integer {
        /// If this integer is a signed type
        signed: bool,
        /// Width of the integer type
        width: IntegerWidth,
    },
    /// A 32 or 64 bit floating point type
    Float {
        /// If this is a 64-bit wide float
        doublewide: bool,
    },
    /// Unnamed structure type with fields 
    Struct {
        /// Field types and names, in the order they were declared
        fields: Vec<(TypeId, String)>,
    },
    /// Sum type that can be many different types
    Sum {
        /// The possible variants of this sum type
        variants: Vec<TypeId>,
    },
    /// Boolean true or false type
    Bool,
    /// Unit type with a single value
    Unit,
    /// User-defined alias type
    Alias {
        /// Name of the aliased type
        name: String,
        /// Aliased type
        ty: TypeId,
    }
}

impl IRContext {
    pub const I8: TypeId = unsafe { TypeId::from_raw(0) };
    pub const I16: TypeId = unsafe { TypeId::from_raw(1) };
    pub const I32: TypeId = unsafe { TypeId::from_raw(2) };
    pub const I64: TypeId = unsafe { TypeId::from_raw(3) };
    pub const U8: TypeId = unsafe { TypeId::from_raw(4) };
    pub const U16: TypeID = unsafe { TypeId::from_raw(5) };
    pub const U32: TypeId = unsafe { TypeId::from_raw(6) };
    pub const U64: TypeId = unsafe { TypeId::from_raw(7) };

    pub const BOOL: TypeId = unsafe { TypeId::from_raw(8) };
    pub const UNIT: TypeId = unsafe { TypeId::from_raw(9) };

    pub const F32: TypeId = unsafe { TypeId::from_raw(10) };
    pub const F64: TypeId = unsafe { TypeId::from_raw(11) };

    /// Create a new `IRContext` with primitive types defined
    pub fn new() -> Self {
        let mut types = Interner::new();

        types.insert(IRType::Integer { signed: true, width: IntegerWidth::Eight });    
        types.insert(IRType::Integer { signed: true, width: IntegerWidth::Sixteen });
        types.insert(IRType::Integer { signed: true, width: IntegerWidth::ThirtyTwo });
        types.insert(IRType::Integer { signed: true, width: IntegerWidth::SixtyFour });
        
        types.insert(IRType::Integer { signed: false, width: IntegerWidth::Eight });    
        types.insert(IRType::Integer { signed: false, width: IntegerWidth::Sixteen });
        types.insert(IRType::Integer { signed: false, width: IntegerWidth::ThirtyTwo });
        types.insert(IRType::Integer { signed: false, width: IntegerWidth::SixtyFour });
        
        types.insert(IRType::Bool);
        types.insert(IRType::Unit);
    
        types.insert(IRType::Float { doublewide: false });
        types.insert(IRType::Float { doublewide: true });

        Self {
            types, 
        }
    }
    
    /// Get the [TypeId] of an integer type with the given width and signededness
    pub const fn itype(signed: bool, width: IntegerWidth) -> TypeId {
        match (signed, width) {
            (true, IntegerWidth::Eight) => Self::I8,
            (true, IntegerWidth::Sixteen) => Self::I16,
            (true, IntegerWidth::ThirtyTwo) => Self::I32,
            (true, IntegerWidth::SixtyFour) => Self::I64,
            
            (false, IntegerWidth::Eight) => Self::U8,
            (false, IntegerWidth::Sixteen) => Self::U16,
            (false, IntegerWidth::ThirtyTwo) => Self::U32,
            (false, IntegerWidth::SixtyFour) => Self::U64,
        }
    }
}
