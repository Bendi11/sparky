use std::iter::Peekable;

use crate::{Type, lex::Token};
use thiserror::Error;

/// The `ParseRes<T>` type is a wrapper over the standard libraries Result type with [ParseErr] always set as the 
/// error variant type
pub type ParseRes<T> = Result<T, ParseErr>;

/// The `Parser` struct takes lexed tokens from a [Lexer](crate::lex::Lexer) and parses it into a completed [Ast](crate::ast::Ast)
pub struct Parser<L: Iterator<Item = Token>> {
    /// Any type producing tokens as an iterator
    toks: Peekable<L>,
}

impl<L: Iterator<Item = Token>> Parser<L> {
    /// Create a new `Parser` from any type that can produces [Token]s as an iterator
    pub fn new(lexer: impl IntoIterator<Item = Token>) -> Self {
        Self {
            toks: lexer.into_iter().peekable()
        }
    }

    /// Parse a typename from the input stream, assumes that there is either an int type or an identifier 
    /// ready to be consumed from the lexer
    pub fn parse_typename(&mut self) -> ParseRes<Type> {
        let mut ty= match self.toks.next() {
            //This is a struct, union, or typedef'd type
            Token(_, TokenType::Ident(ident)) => {
                Type::Unknown(ident);   
            },
            Token(_, TokenType::IntType(ty)) => ty,
        };
        while match self.toks.peek() {
            Some(Token(_, TokenType::Key(Key::Ptr))) => {
                self.toks.next(); //Consume the pointer keyword
                ty = ty.ptr_type(); //Apply one level of pointer type
                true
            },
            _ => return Ok(ty), //Return the type once we are no longer parsing pointer types
        } {}
    }
}

/// The `ParseErr` enum enumerates every possible error that can happen when parsing in the [Parser] struct
#[derive(Error, Debug)]
pub enum ParseErr {

}