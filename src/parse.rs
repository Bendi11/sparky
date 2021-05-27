//! The `parse` module provides the [Parser](struct@Parser) struct, used to parse an iterator of tokens into an
//! abstract syntax tree
//!
//! # Grammar
//! The language currently features a relatively simple imperative grammar similar to C.
//!
//! ### Grammar Description
//! ```notrust, ebnf
//! program = {declaration} ;
//!
//! declaration = function declaration
//!             | global declaration ;
//!
//! function declaration = {function attribute} , "fun" , type , identifier , "(" , { type , identifier , "," } , ")" ,  ";" | body ;
//! function attribute = "ext" (*If this keyword is used, the function must not have a body given*)
//!                     | "inline" ;
//!
//! identifier = ?Any alphanumeric character? ;
//! type = "i" , digit , [digit] (*Signed integer type*)
//!      | "u" , digit , [digit] (*Unsigned integer type*)
//!      | "col" , identifier (*Collection type, this is user defined*) ;
//!
//! global declaration = "glob" , {var attribute} , type , ident , ["=" , expr] , ";" ;
//! var attribute = "ext" (*If this is given, no initializer should be given*)
//!               | "const" ;
//!
//! primary expr = literal | variable access | fun call ;
//!
//! literal = """ , ?any character except "? , """ ;
//!
//! fun call = identifier , "(" , {expr , ","} ")" ;
//! variable access = identifier ;
//!
//! expr = ( primary expr | binary expr | "(" , expr , ")" ) , ";" ;
//! binary expr = primary expr , operator , {expr} ;
//! operator = "+" | "-" | "*" | "/" | "%" | "^" | "&" | "&&" | "||" | "|" | "=" | "==" ;
//!
//! body = "{" , { expression } , "}";
//! ```

use std::fmt;
use std::iter::Peekable;

use crate::{
    ast::Ast,
    lex::{Token, TokenType},
};
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;

/// The `Parser` struct contains a reference the the llvm compilation [Context](inkwell::context::Context), used for creating types that LLVM
/// can understand
pub struct Parser<'ctx, I: Iterator<Item = Token>> {
    /// The reference to the compilation context
    ctx: &'ctx Context,

    /// The token iterator that has been lexed from the input reader
    toks: Peekable<I>,
}

impl<'ctx, I: Iterator<Item = Token>> Parser<'ctx, I> {
    /// Create a new Parser from raw parts
    pub fn new(ctx: &'ctx Context, toks: I) -> Self {
        Self {
            ctx,
            toks: toks.peekable(),
        }
    }

    /// Parse a complete expression from the token stream
    pub fn parse_expr(&mut self) -> Result<Ast<'ctx>, ParseErr> {
        let primary = self.parse_primary()?; //Get the primary expression
                                             //Get the next token, this must be either a semicolon or a binary expression operator
        match self.toks.next().eof()? {
            //This is a binary expression
            Token(_, TokenType::Op(op)) => {
                return Ok(Ast::Binary {
                    lhs: Box::new(primary),
                    rhs: Box::new(self.parse_expr()?),
                    op,
                })
            }
            _ => Ok(primary),
        }
    }

    /// Parse a complete semicolon terminated expression
    pub fn parse_complete(&mut self) -> Result<Ast<'ctx>, ParseErr> {
        let expr = self.parse_expr()?;
        match self.toks.next().eof()? {
            Token(_, TokenType::Semicolon) => Ok(expr),
            Token(line, _) => Err(ParseErr::Syntax(
                line,
                "Expected semicolon to terminate complete expression".to_owned(),
            )),
        }
    }

    /// Parse a primary expression from the input token stream
    pub fn parse_primary(&mut self) -> Result<Ast<'ctx>, ParseErr> {
        //Peek the next token
        match self.toks.next().eof()? {
            //This is a number literal
            Token(line, TokenType::NumLiteral(num)) => {
                //Check if there is an explicit type after this literal
                if let Some(ty) = self.parse_type() {
                    if !ty.is_int_type() {
                        return Err(ParseErr::Syntax(
                            line,
                            "Expected integer type after integer literal".to_owned(),
                        ));
                    }

                    return Ok(Ast::Literal(
                        ty.into_int_type()
                            .const_int_from_string(
                                num.as_str(),
                                inkwell::types::StringRadix::Decimal,
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                    ));
                    //Get a constant int for the specified type
                }
                //Return the default i32 type
                Ok(Ast::Literal(
                    self.ctx
                        .i32_type()
                        .const_int_from_string(num.as_str(), inkwell::types::StringRadix::Decimal)
                        .unwrap()
                        .as_basic_value_enum(),
                ))
            }

            Token(line, tok) => Err(ParseErr::Syntax(line, format!("Unexpected token {}", tok))),
        }
    }

    /// Attempt to parse a type or None if the type name is invalid
    fn parse_type(&mut self) -> Option<BasicTypeEnum<'ctx>> {
        match self.toks.peek() {
            // This is an integer type
            Some(Token(_, TokenType::Ident(s))) if s.starts_with('i') || s.starts_with('u') => {
                if s.len() < 2 {
                    return None;
                }
                let width = s[1..].parse::<u32>().ok()?; //Strip the prefix from the int type
                self.toks.next(); //Consume the typename token
                Some(self.ctx.custom_width_int_type(width).as_basic_type_enum())
                //Return the type constructed from the width
            }
            _ => None,
        }
    }
}

/// The `ParseErr` enum is an error type that represents all possible errors that can occur when parsing a token stream
#[derive(Debug)]
pub enum ParseErr {
    /// An error occurred while lexing (this is very uncommon)
    LexErr(String),

    /// Unexpected end-of-file in the input token stream, this doesn't have line information becuase it will
    /// always occur on the last line (hence; END of file)
    UnexpectedEOF,

    /// There was a syntax error on a line
    Syntax(usize, String),
}

/// The `NoEof` trait is used to ensure that if EOF is encountered, the appropriate error is thrown
pub trait NoEof: Sized {
    type Output;
    fn eof(self) -> Result<Self::Output, ParseErr>;
}

impl NoEof for Option<Token> {
    type Output = Token;
    fn eof(self) -> Result<Self::Output, ParseErr> {
        self.ok_or(ParseErr::UnexpectedEOF)
    }
}

impl<'a> NoEof for Option<&'a Token> {
    type Output = &'a Token;
    fn eof(self) -> Result<Self::Output, ParseErr> {
        self.ok_or(ParseErr::UnexpectedEOF)
    }
}

impl std::error::Error for ParseErr {}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LexErr(msg) => write!(f, "Error lexing: {}", msg),
            Self::UnexpectedEOF => write!(f, "Unexpected end of file!"),
            Self::Syntax(line, msg) => write!(f, "Syntax error on line #{}: {}", line, msg),
        }
    }
}
