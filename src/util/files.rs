//! Module containing the [Files] structure that holds an arena of [CompiledFile] structures

use std::path::PathBuf;

/// A structure containing all data from a compiled spark source file needed by the compiler
/// for location information
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompiledFile {
    pub path: PathBuf,
    /// Vector mapping line numbers - 1 to offsets in source text
    pub lines: Vec<usize>,
}

/// Container holding the data of all files being compiled by sparkc
#[derive(Clone, Debug, Default)]
pub struct Files {
    /// A dynamic array of all files that are being compiled
    files: Vec<CompiledFile>,
}

/// An identifier for a certain [CompiledFile] in a [Files] structure
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FileId(u16);

impl Files {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new file info structure and return the ID of the inserted file
    pub fn add(&mut self, data: CompiledFile) -> FileId {
        self.files.push(data);
        FileId((self.files.len() - 1) as u16)
    }

    /// Get a reference to the file information for the given ID, panics if the ID is invalid
    pub fn get(&self, id: FileId) -> &CompiledFile {
        self.files.get(id.0 as usize).expect("Attempting to get file from Files using invalid ID")
    }
}

