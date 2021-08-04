use std::{fmt, ops::Deref};

/// The `Container` struct contains all information about a struct / union type in Spark
#[derive(Clone, Debug, Eq)]
pub struct Container {
    /// The name of the struct / union type
    pub name: String,

    /// The contained data in the struct / union or `None` if the type is opaque
    pub fields: Option<Vec<(String, Type)>>,

    /// The ID of this type
    pub typeid: usize,
}

/// The `FunType` struct contains all information about a function type like return type,
/// and argument types (if any)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunType {
    /// Return type of the function
    pub ret: Type,
    /// Argument types of the function
    pub args: Vec<Type>,
}

impl fmt::Display for Container {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} ({})", self.name, self.typeid)?;
        if let Some(ref fields) = self.fields {
            writeln!(f, "{{")?;
            for (name, ty) in fields {
                writeln!(f, "    {} {},\n", ty, name)?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

/// The `Type` enum enumerates all possible types for expressions like integer, pointer, struct, and union types
#[derive(Clone, Debug, PartialEq, Eq)]
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

    /// An array type
    Array(Box<Type>, usize),

    /// A function pointer type
    FunPtr(Box<FunType>),
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
            Self::Array(ty, len) => ty.size() * len,
            Self::FunPtr(_) => 8,
        }
    }
}

impl PartialEq for Container {
    fn eq(&self, other: &Self) -> bool {
        self.typeid == other.typeid
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
            Self::Struct(s) => write!(f, "{}", s),
            Self::Union(s) => write!(f, "{}", s),
            Self::Unknown(name) => write!(f, "Unknown type {}", name),
            Self::Void => write!(f, "void"),
            Self::Array(ty, len) => write!(f, "{} [{}]", ty, len),
            Self::FunPtr(fun) => write!(f, "fun({}) {}", fun.args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "), fun.ret)
        }
    }
}
