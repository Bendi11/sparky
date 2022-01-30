//! Module defining error structures and error handlers for displaying
//! error / warn messages as they occur

use codespan_reporting::{
    diagnostic::Diagnostic,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Chars, DisplayStyle, Styles,
    },
};

use crate::util::files::{FileId, Files};
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
        Self { files }
    }

    /// Emit a diagnostic to the console
    pub fn emit(&mut self, diag: Diagnostic<FileId>) {
        codespan_reporting::term::emit(
            &mut StandardStream::stderr(ColorChoice::Auto),
            &codespan_reporting::term::Config {
                display_style: DisplayStyle::Rich,
                tab_width: 2,
                styles: Styles::default(),
                chars: Chars::box_drawing(),
                ..Default::default()
            },
            self.files,
            &diag,
        ).expect("Failed to write compiler output to stderr");
    }
}
