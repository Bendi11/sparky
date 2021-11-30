use std::iter::Peekable;

use self::lex::Lexer;

pub mod lex;
pub mod token;

/// A structure consuming a token stream from a lexer and transforming it to an Abstract Syntax Tree
#[derive(Clone, Debug)]
pub struct Parser<'src> {
    /// The token stream to consume tokens from 
    toks: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    /// Create a new `Parser` from the given source string
    pub fn new(src: &'src str) -> Self {
        Self {
            toks: Lexer::new(src).peekable()
        }
    }

    /// Parse a top-level declaration from the token stream
    fn parse_decl(&mut self)

}