use std::{iter::Peekable, fmt};

use smallvec::SmallVec;
use string_interner::StringInterner;

use crate::{util::loc::Span, ast::Ast};

use self::{lex::Lexer, token::{TokenData, BracketType, Token}};

pub mod lex;
pub mod token;
mod eof;

/// A structure consuming a token stream from a lexer and transforming it to an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser<'int, 'src> {
    /// The token stream to consume tokens from 
    toks: Peekable<Lexer<'src>>,
    /// The current parse trace used for error and debug backtraces
    trace: SmallVec<[&'static str ; 24]>,
    /// The string interner used for building an AST with Symbols instead of allocating strings
    interner: &'int mut StringInterner,
}

pub type ParseResult<'src, T> = Result<T, ParseError<'src>>;

impl<'int, 'src> Parser<'int, 'src> {
    /// Create a new `Parser` from the given source string
    pub fn new(src: &'src str, interner: &'int mut StringInterner) -> Self {
        Self {
            toks: Lexer::new(src).peekable(),
            trace: SmallVec::new(),
            interner
        }
    }
    
    /// Consume the next token from the token stream or an [error](ParseErrorKind::UnexpectedEOF) if there are no more tokens to be lexed
    fn next_tok(&mut self, expecting: &'static [TokenData<'static>]) -> ParseResult<'src, Token<'src>> {
        self.toks.next()
            .ok_or_else(|| ParseError {
                highlighted_span: None,
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedEOF {
                    expecting: ExpectingOneOf(expecting)
                }
            })
    }

    /// Consume the next token and expect it to be an identifier
    fn expect_next_ident(&mut self, expected: &'static str) -> ParseResult<'src, &'src str> {
        let next = self.next_tok(&[TokenData::Ident(expected)])?;
        if let TokenData::Ident(name) = next.data {
            Ok(name)
        } else {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(&[TokenData::Ident(expected)])
                }
            })
        }

    }

    /// Parse a top-level declaration from the token stream
    fn parse_decl(&mut self) -> ParseResult<'src, Ast> {
        const EXPECTING_NEXT: &[TokenData<'static>] = &[
            TokenData::Ident("fun"),
            TokenData::Ident("type"),
            TokenData::Ident("const")
        ];

        let next = self.next_tok(EXPECTING_NEXT)?;
        match next.data {
            TokenData::Ident("fun") => {
                self.trace.push("function declaration");
                let name = self.expect_next_ident("function name")?;
            }
        }
    }

}

/// Structure containing parse error backtrace information and a [ParseErrorKind] with more specific error
/// data
#[derive(Clone, Debug)]
pub struct ParseError<'src> {
    /// The code span to highlight as the error location
    pub highlighted_span: Option<Span>,
    /// A backtrace of what the parser believes it was parsing
    pub backtrace: SmallVec<[&'static str ; 24]>,
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
    },
    /// The source ended unexpectedly
    UnexpectedEOF {
        expecting: ExpectingOneOf,
    }
}

/// Wrapper over an [ArrayVec] that holds expected token data for the [UnexpectedToken](ParseErrorKind::UnexpectedToken) error
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpectingOneOf(&'static [TokenData<'static>]);

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
                TokenData::Ident(ident) => write!(f, "identifier: {}", ident),
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
            }?;
        }

        Ok(())
    }
}