mod files;
mod line;

/// A span from a source file
#[derive(Clone, Copy)]
pub struct Span {
    pub begin: u32,
    pub end: u32
}
