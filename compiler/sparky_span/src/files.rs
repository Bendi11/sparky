use std::path::Path;

use sparky_arena::Arena;

use crate::line::LineOffsets;

sparky_arena::new_arena_key!{
    /// Key used to access file data from a [Files] structure
    pub struct FileId;
}

/// A collection of text documents that can be read from files or created in memory for testing or
/// macros
#[derive(Clone, Debug)]
pub struct Files {
    arena: Arena<OpenFile>,
}

/// An enumeration over all types of files that may be parsed by the compiler
#[derive(Clone, Debug)]
pub enum OpenFile {
    Handle(OpenFileHandle),
    Memory(MemoryFile),
}

/// A read-only file that can seek to arbitrary bytes, instead of storing the full file in memory
/// as in the [MemoryFile]
#[derive(Clone, Debug)]
struct OpenFileHandle {
    
}

/// A read-only string stored in memory instead of on the file system
#[derive(Clone, Debug)]
struct MemoryFile {
    txt: Box<str>,
    linemap: LineOffsets,
}

impl MemoryFile {
    /// Open a file from the given path, reading all line indices and returning an instance of
    /// `Self` containing the file's text
    pub fn open<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let txt = std::fs::read_to_string(path)?.into_boxed_str();
        let linemap = LineOffsets::read(txt.char_indices());
        Ok(Self {
            txt,
            linemap,
        })
    }
}
