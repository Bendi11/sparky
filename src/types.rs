use hashbrown::HashMap;
use std::fmt;


/// The `Container` struct contains all information about a struct / union type in Spark
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Container {
    /// The name of the struct / union type
    pub name: String,

    /// The contained data in the struct / union
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
    Struct(Container),

    /// A user - defined union type
    Union(Container),

    /// An unknown struct type with name only
    UnknownStruct(String),
    
    /// An unknown union type with name only
    UnknownUnion(String),

    /// An unknown union or struct type
    Unknown(String),

    /// No type
    Void,

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
            Self::Union(s) => s.fields.iter().map(|(_, ty)| ty.size()).max().unwrap_or(0),
            Self::UnknownStruct(_) => panic!("Unknown struct"),
            Self::UnknownUnion(_)  => panic!("Unknown union"),
            Self::Unknown(name) => panic!("Unknown type {}", name),
            Self::Void => panic!("Void type"),
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
            Self::Struct(s) => write!(f, "Struct {}: {{\n{:#?}\n}}", s.name, s.fields),
            Self::Union(s) => write!(f, "Union {}: {{\n{:#?}\n}}", s.name, s.fields),
            Self::UnknownStruct(s) => write!(f, "Unknown struct {}", s),
            Self::UnknownUnion(s) => write!(f, "Unknown union {}", s),
            Self::Unknown(name) => write!(f, "Unknown type {}", name),
            Self::Void => write!(f, "void"),
        }
    }
}
