use std::{cmp::Ordering, num::NonZeroU16, fmt, io::Write};

use super::files::CompiledFile;

/// A type holding one location in source text
#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord)]
pub struct Loc {
    /// The line of code of this location, cannot be 0 because line numbers start at 1
    pub line: NonZeroU16,
    pub col: u16,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Loc {
    /// Create a new location from a line and column number
    pub fn new(line: NonZeroU16, col: u16) -> Self {
        Self { line, col }
    }

    /// Create a new location from raw u16 line count, panics if the line count is 0
    pub fn checked_new(line: u16, col: u16) -> Self {
        Self {
            line: NonZeroU16::new(line)
                .expect("Attempting to create a new location with line number of 0"),
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
            other => other,
        })
    }
}

/// A span representing a section of the input text over two locations
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    /// The location that the span goes from
    pub from: Loc,
    /// The location that the span goes to
    pub to: Loc,
}

impl Span {
    /// Create a new span from one location to another
    pub fn new(from: Loc, to: Loc) -> Self {
        assert_eq!(
            from <= to,
            true,
            "Attempting to create a new span with from location higher than to location"
        );
        Self { from, to }
    }

    /// Display this token data using a source file
    pub fn display(&self, file: &CompiledFile) -> std::io::Result<()> {
        let start_line = *file
            .lines
            .get(self.from.line.get() as usize - 1)
            .expect("Invalid span line when displaying token");
        let startpos = start_line + self.from.col as usize;

        let end_line = *file
            .lines
            .get(self.to.line.get() as usize - 1)
            .expect("Invalid span line when displaying token");
        let endpos = end_line + self.to.col as usize;

        let mut stdout = std::io::stderr();
        let mut line = self.from.line.get();
        let mut buf = [0u8; 4];

        stdout.write_fmt(format_args!("{}: ", line))?;
        for character in file.text[startpos..=endpos].chars() {
            character.encode_utf8(&mut buf);
            stdout.write(&buf[..character.len_utf8()])?;

            if character == '\n' {
                line += 1;
                stdout.write_fmt(format_args!("{}: ", line))?;
            }
        }

        stdout.flush()
    }

    /// Create a new span from a single location in a source file
    pub fn single(loc: Loc) -> Self {
        Self { from: loc, to: loc }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.from, self.to)
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
