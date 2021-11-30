use std::iter::Peekable;

use thiserror::Error;

use crate::util::loc::Span;

use self::lex::Lexer;

pub mod lex;
pub mod token;

/// A structure consuming a token stream from a lexer and transforming it to an Abstract Syntax Tree
#[derive(Clone, Debug)]
pub struct Parser<'src> {
    /// The token stream to consume tokens from 
    toks: Peekable<Lexer<'src>>,
    /// The current parse trace used for error and debug backtraces
    trace: Vec<&'static str>,
}

pub type ParseResult<T> = Result<T, ParseError>;

impl<'src> Parser<'src> {
    /// Create a new `Parser` from the given source string
    pub fn new(src: &'src str) -> Self {
        Self {
            toks: Lexer::new(src).peekable(),
            trace: vec![]
        }
    }

    /// Parse a top-level declaration from the token stream
    fn parse_decl(&mut self) -> ParseResult<Ast> {

    }

}

/// Structure containing parse error backtrace information and a [ParseErrorKind] with more specific error
/// data
#[derive(Clone, Debug)]
pub struct ParseError {
    /// The code span to highlight as the error location
    pub highlighted_span: Option<Span>,
    /// A backtrace of what the parser believes it was parsing
    pub backtrace: Vec<&'static str>,
    /// More specific error data
    pub error: ParseErrorKind,
}

/// Enumeration containing all possible parser errors 
#[derive(Clone, Debug, Error)]
pub enum ParseErrorKind {
    #[error("Unexpected Token {}")]
    UnexpectedToken {

    }
}

pub struct 