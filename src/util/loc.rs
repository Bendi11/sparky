use std::{cmp::Ordering, num::NonZeroU16};


/// A type holding one location in source text
#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord)]
pub struct Loc {
    line: NonZeroU16,
    col: u16
}

impl PartialOrd for Loc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match self.line.cmp(&other.line) {
            Ordering::Equal => self.col.cmp(&other.col),
            other => other
        })
    }
}

/// A span representing a section of the input text over two locations
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub from: Loc,
    pub to: Loc,
}