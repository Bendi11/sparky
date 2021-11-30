use std::{iter::Peekable, fmt};

use arrayvec::ArrayVec;

use crate::{util::loc::Span, ast::Ast};

use self::{lex::Lexer, token::{TokenData, BracketType, Token}};

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

pub type ParseResult<'src, T> = Result<T, ParseError<'src>>;

impl<'src> Parser<'src> {
    /// Create a new `Parser` from the given source string
    pub fn new(src: &'src str) -> Self {
        Self {
            toks: Lexer::new(src).peekable(),
            trace: vec![]
        }
    }

    /// Parse a top-level declaration from the token stream
    fn parse_decl(&mut self) -> ParseResult<'src, Ast> {

    }

}

/// Structure containing parse error backtrace information and a [ParseErrorKind] with more specific error
/// data
#[derive(Clone, Debug)]
pub struct ParseError<'src> {
    /// The code span to highlight as the error location
    pub highlighted_span: Option<Span>,
    /// A backtrace of what the parser believes it was parsing
    pub backtrace: Vec<&'static str>,
    /// More specific error data
    pub error: ParseErrorKind<'src>,
}

/// Enumeration containing all possible parser errors 
#[derive(Clone, Debug)]
pub enum ParseErrorKind<'src> {
    /// An unexpected token was found in the input file
    UnexpectedToken {
        /// The token that was consumed
        found: Token<'src>,
        /// A list of tokens that were expected to come
        expecting: ExpectingOneOf,
    }
}

/// Wrapper over an [ArrayVec] that holds expected token data for the [UnexpectedToken](ParseErrorKind::UnexpectedToken) error
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpectingOneOf(ArrayVec<TokenData<'static>, 24>);

impl fmt::Display for ExpectingOneOf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for expecting in self.0.iter() {
            match expecting {
                TokenData::Arrow => write!(f, "'->'"),
                TokenData::Assign => write!(f, "':='"),
                TokenData::Char(_) => write!(f, "character literal"),
                TokenData::Colon => write!(f, "':'"),
                TokenData::Comma => write!(f, "','"),
                TokenData::Period => write!(f, "'.'"),
                TokenData::Op(op) => op.fmt(f),
                TokenData::Ident(_) => write!(f, "identifier"),
                TokenData::Number(_) => write!(f, "number literal"),
                TokenData::String(_) => write!(f, "string literal"),
                TokenData::OpenBracket(ty) => match ty {
                    BracketType::Brace => write!(f, "'{{'"),
                    BracketType::Parenthese => write!(f, "'('"),
                    BracketType::Bracket => write!(f, "'['"),
                },
                TokenData::CloseBracket(ty) => match ty {
                    BracketType::Brace => write!(f, "'}}'"),
                    BracketType::Parenthese => write!(f, "')'"),
                    BracketType::Bracket => write!(f, "']'"),
                },
                TokenData::Period => write!(f, "'.'"),
            }?;
        }

        Ok(())
    }
}