use std::{cmp::Ordering, num::NonZeroU16};


/// A type holding one location in source text
#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord)]
pub struct Loc {
    pub line: NonZeroU16,
    pub col: u16
}

impl Loc {
    /// Create a new location from a line and column number
    pub fn new(line: NonZeroU16, col: u16) -> Self {
        Self {
            line, 
            col
        }
    }

    /// Create a new location from raw u16 line count, panics if the line count is 0
    pub fn checked_new(line: u16, col: u16) -> Self {
        Self {
            line: NonZeroU16::new(line).expect("Attempting to create a new location with line number of 0"),
            col,
        }
    }
}

impl From<(u16, u16)> for Loc {
    fn from((line, col): (u16, u16)) -> Self {
        Self::checked_new(line, col)
    }
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

impl Span {
    /// Create a new span from one location to another
    pub fn new(from: Loc, to: Loc) -> Self {
        assert_eq!(from <= to, true, "Attempting to create a new span with from location higher than to location");
        Self {
            from,
            to
        }
    }

    /// Create a new span from a single location in a source file
    pub fn single(loc: Loc) -> Self {
        Self {
            from: loc,
            to: loc
        }
    }
}

impl From<Loc> for Span {
    fn from(loc: Loc) -> Self {
        Self::single(loc)
    }
}

impl From<(Loc, Loc)> for Span {
    fn from((from, to): (Loc, Loc)) -> Self {
        Self::new(from, to)
    }
}