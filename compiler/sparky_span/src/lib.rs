use std::ops::{Index, Range};

mod files;
mod line;

pub use files::{Files, CompilerFile};

/// A span from a source file
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    begin: u32,
    end: u32,
}

impl Span {
    /// Create a new span from a start and end index into a string- `begin` must be less than
    /// `end`, function will panic if this invariant is not satisfied
    pub const fn new(begin: u32, end: u32) -> Self {
        assert!(
            begin <= end,
            "Attempted to create a Span with a begin index greater than the end index"
        );
        Self { begin, end }
    }

    /// Get immutable access to the first index in this span
    pub const fn begin(&self) -> u32 {
        self.begin
    }
    /// Get immutable access to the end index in this span
    pub const fn end(&self) -> u32 {
        self.end
    }

    /// Create a new span from a start index and a length value
    pub const fn from_start_len(begin: u32, len: u32) -> Self {
        Self {
            begin,
            end: begin + len,
        }
    }

    /// Check if the given offset is contained in this span
    pub const fn contains(&self, offset: u32) -> bool {
        self.begin <= offset && self.end > offset
    }
}

impl From<Span> for Range<u32> {
    fn from(value: Span) -> Self {
        Self { start: value.begin, end: value.end }
    }
}
impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        Self { start: value.begin as usize, end: value.end as usize }
    }
}

impl Index<Span> for str {
    type Output = str;
    fn index(&self, index: Span) -> &Self::Output {
        &self[(index.begin() as usize)..(index.end() as usize)]
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {})", self.begin, self.end) 
    }
}