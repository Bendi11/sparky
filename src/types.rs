use std::fmt;
use inkwell::types::AnyTypeEnum;

/// The `StackLayout` struct describes what types are on the stack and the number of stack plates.
/// It is used to do static type checking of the stack
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct StackLayout<'a> {
    /// The stack layout, with the top of the stack at the last index
    pub lay: Vec<AnyTypeEnum<'a>>,
}

impl<'a> StackLayout<'a> {
    pub fn len(&self) -> usize {
        self.lay.len()
    }

    /// Push one type to the stack
    #[inline]
    pub fn push(&mut self, ty: AnyTypeEnum<'a>) {
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

    pub fn with(mut self, ty: AnyTypeEnum<'a>) -> Self {
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

impl<'a> fmt::Display for StackLayout<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for ty in self.lay.iter() {
            writeln!(f, "{:?}", ty)?;
        }
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use inkwell::{context::Context, types::AnyType};

    use super::*;

    #[test]
    pub fn stack_eq() {
        let ctx = Context::create();

        let eq = StackLayout::new().with(ctx.i32_type().as_any_type_enum()).top_match(
            &StackLayout::new()
                .with(ctx.i16_type().as_any_type_enum())
                .with(ctx.i32_type().as_any_type_enum()),
        );
        assert_eq!(
            eq, true,
            "Comparisons between differently sized type stacks are broken"
        );
    }
}
