//! Module defining error structures and error handlers for displaying 
//! error / warn messages as they occur

use crate::util::{files::{FileId, Files}, loc::Span};

/// A semantic error or warning emitted by the compiler during the process of 
/// AST lowering
#[derive(Clone, Debug)]
pub struct Diagnostic {
    /// A list of spans to display with this error 
    pub spans: Vec<Span>,
    /// An error message to display for the diagnostic
    pub message: String,
    /// The file that this diagnostic occurred in
    pub file: FileId,
    /// If this is information, warning, or error message
    pub lvl: DiagnosticLevel,
}

impl Diagnostic {
    /// Create a new diagnostic from a level, 
    pub fn error(msg: impl ToString, file: FileId) -> Self {
        Self {
            spans: vec![],
            message: msg.to_string(),
            file,
            lvl: DiagnosticLevel::Error
        }
    }

    pub fn warn(msg: impl ToString, file: FileId) -> Self {
        Self {
            spans: vec![],
            message: msg.to_string(),
            file,
            lvl: DiagnosticLevel::Warn
        }
    }

    pub fn info(msg: impl ToString, file: FileId) -> Self {
        Self {
            spans: vec![],
            message: msg.to_string(),
            file,
            lvl: DiagnosticLevel::Info
        }
    }
    
    /// Return a new diagnostic with the given span added
    pub fn with_span(mut self, span: impl Into<Span>) -> Self {
        self.spans.push(span.into());
        self
    }
}

/// The level of severity a diagnostic was emitted at
#[derive(Clone, Debug)]
pub enum DiagnosticLevel {
    Info,
    Warn,
    Error,
}

/// A structure that handles emitted diagnostics from the compiler, 
/// respecting command line options for verbosity
#[derive(Clone, Debug)]
pub struct DiagnosticManager<'files> {
    /// A collection of compiled files
    files: &'files Files,
}

impl<'files> DiagnosticManager<'files> {
    /// Create a new diagnostic manager using a reference to all
    /// currently compiled files
    pub fn new(files: &'files Files) -> Self {
        Self {
            files
        }
    }
    
    /// Emit a diagnostic to the console 
    pub fn emit(&self, diag: Diagnostic) {
    
    }
}
