//! Module containing the [Files] structure that holds an arena of [CompiledFile] structures

use std::{
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
};

use crate::arena::{Arena, Index};

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
        let lines = codespan_reporting::files::line_starts(&source).collect();

        Ok(Self {
            path: path.as_ref().to_path_buf(),
            lines,
            text: source,
        })
    }

    /// Create a new CompiledFile from an in-memory string, used for debugging and testing mostly
    pub fn in_memory(text: String) -> Self {
        Self {
            path: PathBuf::new(),
            lines: {
                let mut lines = vec![0];
                lines.extend(text.char_indices().filter_map(|(idx, c)| {
                    if c == '\n' {
                        Some(idx)
                    } else {
                        None
                    }
                }));
                lines
            },
            text,
        }
    }
}

/// Container holding the data of all files being compiled by sparkc
#[derive(Clone, Debug, Default)]
pub struct Files {
    /// A dynamic array of all files that are being compiled
    files: Arena<CompiledFile>,
}

/// An identifier for a certain [CompiledFile] in a [Files] structure
pub type FileId = Index<CompiledFile>;

impl Files {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new file info structure and return the ID of the inserted file
    pub fn add(&mut self, data: CompiledFile) -> FileId {
        self.files.insert(data)
    }

    /// Get a reference to the file information for the given ID, panics if the ID is invalid
    pub fn get(&self, id: FileId) -> &CompiledFile {
        self.files.get(id)
    }
}

impl<'a> codespan_reporting::files::Files<'a> for Files {
    type FileId = FileId;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok(self.get(id).path.to_string_lossy().into_owned())
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        Ok(self.get(id).text.as_str())
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        let file = self.get(id);

        if byte_index >= file.text.len() {
            return Ok(file.lines.len());
        }

        let mut lowest_line = 0;
        for (line, line_idx) in file.lines.iter().enumerate() {
            if *line_idx <= byte_index {
                lowest_line = line;
            } else {
                break;
            }
        }

        Ok(lowest_line)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        let file = self.get(id);

        let line = file.lines[line_index];
        if line == file.lines.len() - 1 {
            Ok(line..file.text.len())
        } else {
            let next_line = file.lines[line_index + 1];
            Ok(line..next_line)
        }
    }
}
