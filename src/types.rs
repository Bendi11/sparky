use std::{convert::TryFrom, fmt};

/// The `Type` enumeration represents all possible types like integer and user-defined struct types
#[derive(Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum Type {
    /// The `Int` variant represents one primitive integer type with the amount of bits in the type like 32 for a 32 bit integer
    Int {
        /// The number of bits that this integer takes
        width: u8,
        /// Wether the integer type is signed or not
        signed: bool,
    },

    /// The `Ptr` variant represents a pointer to a type
    Ptr(Box<Type>),
}

impl TryFrom<&str> for Type {
    type Error = &'static str;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.len() < 2 {
            return Err("Integer type string is not long enough, need at least two characters");
        }

        let signed = match value.chars().next().unwrap() {
            'i' => true,
            'u' => false,
            _ => {
                return Err(
                    "First character of integer type must be i or u to specify signededness",
                )
            }
        };
        let width = value[1..]
            .parse::<u8>()
            .map_err(|_| "Number typename must specify a bit width after sign!")?;
        match width {
            8 | 16 | 32 | 64 => (),
            _ => return Err("Accepted bit widths for integer types are: 8, 16, 32, and 64!"),
        };

        Ok(Self::Int { width, signed })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int { width, signed } => write!(
                f,
                "{} integer {} bits wide",
                match signed {
                    true => "signed",
                    false => "unsigned",
                },
                width
            ),
            Self::Ptr(ptr) => write!(f, "Pointer to {}", ptr),
        }
    }
}

impl Type {
    /// Create a new signed integer type
    pub fn isigned(width: u8) -> Self {
        Self::Int {
            width,
            signed: true,
        }
    }
}

/// The `StackLayout` struct describes what types are on the stack and the number of stack plates.
/// It is used to do static type checking of the stack
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct StackLayout {
    /// The stack layout, with the top of the stack at the last index
    lay: Vec<Type>,
}

impl StackLayout {
    /// Push one type to the stack
    #[inline]
    pub fn push(&mut self, ty: Type) {
        self.lay.push(ty);
    }

    /// Pop one type from the top of the stack
    #[inline]
    pub fn pop(&mut self) {
        self.lay.pop();
    }

    /// Create a new empty type stack
    pub const fn new() -> Self {
        Self { lay: Vec::new() }
    }

    pub fn with(mut self, ty: Type) -> Self {
        self.push(ty);
        self
    }

    /// If the two stacks have a different size, then make sure that the smaller one matches our top nodes
    pub fn top_match(&self, other: &Self) -> bool {
        // Check if any of our stack nodes don't match starting from the top to the bottom
        self.lay
            .iter()
            .zip(other.lay.iter())
            .rev()
            .all(|(me, them)| me == them)
    }
}

impl fmt::Display for StackLayout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for ty in self.lay.iter() {
            writeln!(f, "{}", ty)?;
        }
        Ok(())
    }
}

/// The `Val` enum holds every type of value for the `Value` struct
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Val {
    /// A number
    Num(i64),
    /// A raw string literal
    String(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Value {
    /// The type of this value
    pub ty: Type,
    /// The values that are being pused to the stack
    pub val: Val,
}

impl Value {
    /// Cast our bytes to an integer
    pub fn to_int(&self) -> i64 {
        match (&self.ty, &self.val) {
            (
                &Type::Int {
                    width: _,
                    signed: _,
                },
                &Val::Num(n),
            ) => n,
            _ => panic!("Not an integer type!"),
        }
    }

    /// Get this value as a pointer to an address with no type info
    pub fn to_ptr(&self) -> u64 {
        match (&self.ty, &self.val) {
            (&Type::Ptr(_), &Val::Num(n)) => n as u64,
            _ => panic!("Not an integer type!"),
        }
    }

    /// Return a number value
    pub fn num(n: i64, ty: Type) -> Self {
        Self {
            ty,
            val: Val::Num(n),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::Int {
                width: _,
                signed: _,
            } => write!(f, "Type: {} {}", self.ty, self.to_int()),
            Type::Ptr(_) => write!(f, "{:#x}, {}", self.to_ptr(), self.ty,),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn stack_eq() {
        let eq = StackLayout::new().with(Type::isigned(8)).top_match(
            &StackLayout::new()
                .with(Type::isigned(100))
                .with(Type::isigned(8)),
        );
        assert_eq!(
            eq, true,
            "Comparisons between differently sized type stacks are broken"
        );
    }
}
