use std::{path::Path, fs::File};

use crate::{line::LineOffsets, Span};

/// An enumeration over all types of files that may be parsed by the compiler
#[derive(Debug)]
pub(crate) enum OpenFile {
    Handle(OpenFileHandle),
    Memory(MemoryFile),
}

/// A read-only file that can seek to arbitrary bytes, instead of storing the full file in memory
/// as in the [MemoryFile]
#[derive(Debug)]
pub(crate) struct OpenFileHandle {
    file: File,
    lines: LineOffsets,
}

/// A read-only string stored in memory instead of on the file system
#[derive(Debug)]
pub(crate) struct MemoryFile {
    txt: Box<str>,
    linemap: LineOffsets,
}

/// All errors that can occur when reading from an open file
#[derive(Debug, thiserror::Error)]
pub enum FileError {
    #[error("Internal I/O error: {0}")]
    IO(#[from] std::io::Error)
}

impl MemoryFile {
    /// Open a file from the given path, reading all line indices and returning an instance of
    /// `Self` containing the file's text
    pub fn open<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let txt = std::fs::read_to_string(path)?.into_boxed_str();
        let linemap = LineOffsets::read(txt.char_indices());
        Ok(Self { txt, linemap })
    }
}

impl OpenFileHandle {
    pub fn open<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let file = File::open(path)?;
        
    }
}

impl OpenFile {
    pub fn read_span(&self, span: Span) {

    }
}
