use std::path::Path;

use sparky_arena::Arena;

use self::file::{OpenFile, MemoryFile};

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
    arena: Arena<OpenFile, FileId>,
}

impl Files {
    /// Create a new empty [Files] collection
    pub const fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    pub fn open_in_memory(&mut self, path: impl AsRef<Path>) -> std::io::Result<FileId> {
        let file = OpenFile::Memory(MemoryFile::open(path)?);
        Ok(self.arena.insert(file))
    }
}
