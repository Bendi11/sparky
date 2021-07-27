use std::{fmt, ops::Deref};

use crate::code::ns::Path;

/// The `Container` struct contains all information about a struct / union type in Spark
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Container {
    /// The name of the struct / union type
    pub name: String,

    /// The contained data in the struct / union or `None` if the type is opaque
    pub fields: Option<Vec<(String, Type)>>,
}

/// The `Type` enum enumerates all possible types for expressions like integer, pointer, struct, and union types
#[derive(Clone, Debug, Eq)]
pub enum Type {
    /// A primitive integer type with bit width and sign
    Integer { signed: bool, width: u8 },

    /// A user - defined struct type
    Struct(Container),

    /// A user - defined union type
    Union(Container),

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
        if s == "bool" {
            Self::Integer {
                signed: false,
                width: 1,
            }
        } else {
            Self::Integer {
                signed: &s[0..1] == "i",
                width: s[1..].parse().unwrap(),
            }
        }
    }

    /// Generate a pointer type that points to a type of Self
    #[inline]
    pub fn ptr_type(&self) -> Self {
        Self::Ptr(Box::new(self.clone()))
    }

    /// Get the contained type if this is a pointer type
    pub fn deref_type(&self) -> Option<Self> {
        match self {
            Self::Ptr(t) => Some(t.deref().clone()),
            _ => None,
        }
    }

    /// Get the size of this type in bytes
    pub fn size(&self) -> usize {
        match self {
            Self::Integer { signed: _, width } => (width / 8) as usize,
            Self::Struct(s) => s
                .fields
                .as_ref()
                .unwrap()
                .iter()
                .fold(0, |acc, (_, ty)| acc + ty.size()),
            Self::Ptr(_) => 8,
            Self::Union(s) => s
                .fields
                .as_ref()
                .unwrap()
                .iter()
                .map(|(_, ty)| ty.size())
                .max()
                .unwrap_or(0),
            Self::Unknown(name) => panic!("Unknown type {}", name),
            Self::Void => panic!("Void type"),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer{signed, width}, Self::Integer{signed: osigned, width: owidth}) 
                if (signed == osigned) && (width == owidth) => true,
            (Self::Void, Self::Void) => true,
            (Self::Ptr(ty), Self::Ptr(oty)) => ty.eq(oty),

            (Self::Union(c), Self::Union(oc)) 
                if c == oc => true,
            (Self::Struct(c), Self::Struct(oc))
                if c == oc => true,
            
            //Named the same thing is considered equal
            (Self::Unknown(name), Self::Struct(oc))
                if <Path as std::str::FromStr>::from_str(name).unwrap().last().unwrap() == <Path as std::str::FromStr>::from_str(&oc.name).unwrap().last().unwrap() => true,
            (Self::Struct(c), Self::Unknown(name))
                if <Path as std::str::FromStr>::from_str(name).unwrap().last().unwrap() == <Path as std::str::FromStr>::from_str(&c.name).unwrap().last().unwrap() => true,
            (Self::Unknown(name), Self::Union(oc))
                if <Path as std::str::FromStr>::from_str(name).unwrap().last().unwrap() == <Path as std::str::FromStr>::from_str(&oc.name).unwrap().last().unwrap() => true,
            (Self::Union(c), Self::Unknown(name))
                if <Path as std::str::FromStr>::from_str(name).unwrap().last().unwrap() == <Path as std::str::FromStr>::from_str(&c.name).unwrap().last().unwrap() => true,
            (Self::Unknown(name), Self::Unknown(oname))
                if <Path as std::str::FromStr>::from_str(name).unwrap().last().unwrap() == <Path as std::str::FromStr>::from_str(oname).unwrap().last().unwrap() => true,

            (_, _) => false
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer { width, signed } => write!(
                f,
                "{}{}",
                match signed {
                    true => 'i',
                    false => 'u',
                },
                width
            ),
            Self::Ptr(ty) => write!(f, "pointer to {}", ty),
            Self::Struct(s) => write!(f, "Struct {}: {{\n{:#?}\n}}", s.name, s.fields),
            Self::Union(s) => write!(f, "Union {}: {{\n{:#?}\n}}", s.name, s.fields),
            Self::Unknown(name) => write!(f, "Unknown type {}", name),
            Self::Void => write!(f, "void"),
        }
    }
}
