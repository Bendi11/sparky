//! Module containing the [Files] structure that holds an arena of [CompiledFile] structures

use std::{fs::File, io::{self, Read}, path::{Path, PathBuf}};

/// A structure containing all data from a compiled spark source file needed by the compiler
/// for location information
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompiledFile {
    pub path: PathBuf,
    /// Vector mapping line numbers - 1 to offsets in source text
    pub lines: Vec<usize>,
    /// The text read from the file
    pub text: String,
}

impl CompiledFile {
    /// Open a file from the path and create a new `CompiledFile` holding its data
    pub fn open<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let mut file = File::open(&path)?;
        let mut source = String::with_capacity(10_000);
        file.read_to_string(&mut source)?;
        let mut lines = vec![0];
        lines.extend(source.char_indices().filter_map(|(idx, c)| if c == '\n' { Some(idx) } else { None }));

        Ok(Self {
            path: path.as_ref().to_path_buf(),
            lines,
            text: source
        })
    }

    /// Create a new CompiledFile from an in-memory string, used for debugging and testing mostly
    pub fn in_memory(text: String) -> Self {
        Self {
            path: PathBuf::new(),
            lines: {
                let mut lines = vec![0];
                lines.extend(text.char_indices().filter_map(|(idx, c)| if c == '\n' { Some(idx) } else { None }));
                lines
            },
            text
        }
    }
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

