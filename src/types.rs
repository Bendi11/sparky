use hashbrown::HashMap;
use std::fmt;


/// The `Struct` struct contains all information about a struct type in Spark
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Struct {
    /// The name of the struct type
    pub name: String,

    /// The contained data in the struct
    pub fields: HashMap<String, Type>,
}

/// The `Type` enum enumerates all possible types for expressions like integer, pointer, struct, and union types
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// A primitive integer type with bit width and sign 
    Integer {
        signed: bool,
        width: u8,
    },

    /// A user - defined struct type
    Struct(Struct),

    /// Pointer to a type
    Ptr(Box<Type>),
}

impl Type {
    /// Generate a type from a string, this function is only meant to be used in a lexer where it can not possibly fail
    pub fn int_ty(s: String) -> Self {
        Self::Integer {
            signed: &s[0..1] == "i",
            width: s[1..].parse().unwrap(),
        }
    }

    /// Generate a pointer type that points to a type of Self
    #[inline]
    pub fn ptr_type(&self) -> Self {
        Self::Ptr(Box::new(self.clone()))
    }

    /// Get the size of this type in bytes
    pub fn size(&self) -> usize {
        match self {
            Self::Integer{signed: _, width} => (width / 8) as usize,
            Self::Struct(s) => s.fields.iter().fold(0, |acc, (_, ty)| acc + ty.size()),
            Self::Ptr(_) => 8,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer{width, signed} => write!(f, "{}{}", match signed {
                true => 'i',
                false => 'u',
            }, width),
            Self::Ptr(ty) => write!(f, "pointer to {}", ty),
            Self::Struct(s) => write!(f, "Struct {}: {{\n{:#?}\n}}", s.name, s.fields)
        }
    }
}
