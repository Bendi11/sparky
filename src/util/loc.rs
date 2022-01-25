use std::{cmp::Ordering, num::NonZeroU16, fmt, io::Write};

use super::files::CompiledFile;

/// A span representing a section of the input text over two locations
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    /// The location that the span goes from
    pub from: usize,
    /// The location that the span goes to
    pub to: usize,
}

impl Span {
    /// Create a new span from one location to another
    pub fn new(from: usize, to: usize) -> Self {
        assert_eq!(
            from <= to,
            true,
            "Attempting to create a new span with from location higher than to location"
        );
        Self { from, to }
    }

    /// Create a new span from a single location in a source file
    pub fn single(loc: usize) -> Self {
        Self { from: loc, to: loc }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.from, self.to)
    }
}

impl From<usize> for Span {
    fn from(loc: usize) -> Self {
        Self::single(loc)
    }
}

impl From<(usize, usize)> for Span {
    fn from((from, to): (usize, usize)) -> Self {
        Self::new(from, to)
    }
}
impl From<std::ops::Range<usize>> for Span {
    fn from(loc: std::ops::Range<usize>) -> Self {
        Self::new(loc.start, loc.end)
    }
}
impl Into<std::ops::Range<usize>> for Span {
    fn into(self) -> std::ops::Range<usize> {
        self.from..self.to
    }
}
