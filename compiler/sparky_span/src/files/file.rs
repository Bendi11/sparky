use std::{path::Path, fs::File, io::{BufReader, BufRead}, ops::Range};

use crate::{line::LineOffsets, Span};

use super::FileError;

/// A read-only string read from a file that exists on the disk
#[derive(Debug)]
pub struct CompilerFile {
    /// Read-only text of the file
    pub(super) txt: Box<str>,
    /// User-facing name of the file; can be a path or filename
    pub(super) name: String,
    linemap: LineOffsets,
}

impl CompilerFile {
    /// Open a file from the given path, reading all line indices and returning an instance of
    /// `Self` containing the file's text
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, FileError> {
        let name = path.as_ref().to_string_lossy().into_owned();
        let txt = std::fs::read_to_string(path)?.into_boxed_str();
        let linemap = LineOffsets::read(txt.char_indices());
        Ok(Self { txt, name, linemap })
    }

    /// Read a span of text from this file, returns an error if reading failed or the span was out
    /// of range
    pub fn read_span(&self, span: Span) -> Result<&str, FileError> {
        self
            .txt
            .get(Range::<usize>::from(span))
            .ok_or(FileError::SpanOutOfRange { span, filename: self.name.clone() })
    }

}
