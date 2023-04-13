use std::{path::Path, ops};

use sparky_arena::Arena;

use crate::Span;

use self::file::CompilerFile;

mod file;

sparky_arena::new_arena_key! {
    /// Key used to access file data from a [Files] structure
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct FileId;
}

/// A collection of text documents that can be read from files or created in memory for testing or
/// macros
#[derive(Debug)]
pub struct Files {
    arena: Arena<CompilerFile, FileId>,
}

/// All errors that can occur when reading from an open file
#[derive(Debug, thiserror::Error)]
pub enum FileError {
    #[error("Internal I/O error: {0}")]
    IO(#[from] std::io::Error),
    #[error("Span {span} was out of range for file {filename}")]
    SpanOutOfRange {
        span: Span,
        filename: String
    }
}


impl Files {
    /// Create a new empty [Files] collection
    pub const fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }
    
    /// Read all text from the file at the given path, doing any necessary preprocessing like
    /// storing line offsets
    pub fn read(&mut self, path: impl AsRef<Path>) -> Result<FileId, FileError> {
        let file = CompilerFile::open(path)?;
        Ok(self.arena.insert(file))
    }
    
    /// Attempt to retrieve a file by ID, `panic`ing if the ID is invalid
    #[inline]
    pub fn get(&self, id: FileId) -> &CompilerFile { self.arena.get(id) }
}

impl CompilerFile {
    }

impl ops::Index<FileId> for Files {
    type Output = CompilerFile;
    #[inline]
    fn index(&self, index: FileId) -> &Self::Output { self.get(index) }
}
