use std::fmt;

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
