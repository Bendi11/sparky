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

use crate::ast::Body;
use crate::ast::FnProto;
use crate::{
    ast::{Ast, FnAttrs},
    lex::{Token, TokenType},
};
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use std::convert::TryFrom;

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

    /// Parse a complete program from the token stream
    pub fn parse(&mut self) -> Result<Body<'ctx>, ParseErr> { 
        let mut program = Body::new(); //Get a list of all AST nodes

        while let Some(_) = self.toks.peek() {
            program.push( self.parse_complete()? );
        }

        Ok(program)
    }

    /// Parse a complete expression from the token stream
    pub fn parse_expr(&mut self) -> Result<Ast<'ctx>, ParseErr> {
        let primary = self.parse_primary()?; //Get the primary expression
                                             //Get the next token, this must be either a semicolon or a binary expression operator

        if let Token(_, TokenType::Op(op)) = self.toks.peek().eof()? {
            let op = op.clone();
            return Ok(Ast::Binary {
                lhs: Box::new(primary),
                rhs: Box::new(self.parse_expr()?),
                op,
            })
        }
        Ok(primary)
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

    /// Parse a function prototype from the token stream, with no extra body attempted to parse.
    /// Assumes that the `fn` keyword has already been consumed
    fn parse_proto(&mut self) -> Result<FnProto<'ctx>, ParseErr> {
        let mut attrs = FnAttrs::empty(); //An empty set of function attributes

        //Get all attributes from the function
        while match self.toks.peek().eof()? {
            Token(line, TokenType::Key(key)) => match FnAttrs::try_from(*key) {
                Ok(attr) => {
                    self.toks.next(); //Consume the attribute token
                    attrs.insert(attr);
                    true
                }
                _ => {
                    return Err(ParseErr::Syntax(
                        *line,
                        format!("Expected a function attribute, got {}", key),
                    ))
                }
            },
            _ => false,
        } {}

        //Get the function name
        let name = match self.toks.next().eof()? {
            Token(_, TokenType::Ident(name)) => name,
            Token(line, tok) => {
                return Err(ParseErr::Syntax(
                    line,
                    format!(
                        "Expected a function name in function prototype, got {}",
                        tok
                    ),
                ))
            }
        };

        let open = self.toks.next().eof()?; //Get the opening argument types list brace
        if !open.is(TokenType::LeftBrace('(')) {
            return Err(ParseErr::Syntax(
                open.0,
                format!("Expected an opening brace, but got {}", open.1),
            ));
        }

        let mut types = Vec::new(); //Create a vec to hold all argument types
        let mut names = Vec::new(); //Create a vec to hold all argument names
        loop {
            //Parse a typename from the tokens
            types.push(self.parse_type().ok_or_else(|| {
                ParseErr::Syntax(
                    open.0,
                    format!("Expected a valid typename in argument list!"),
                )
            })?);
            //Get either an identifier or a comma after the name
            match self.toks.next().eof()? {
                Token(_, TokenType::Comma) => {
                    names.push("".to_owned()); //Push an empty argument name
                                               //Check if there is an ending brace
                    if self.toks.peek().eof()?.is(TokenType::RightBrace(')')) {
                        self.toks.next(); //Consume the closing brace
                        break;
                    }
                }
                //This is an argument name
                Token(_, TokenType::Ident(ref name)) => {
                    names.push(name.clone());
                    match self.toks.next().eof()? {
                        Token(_, TokenType::Comma) => continue,
                        Token(_, TokenType::RightBrace(')')) => break,
                        Token(line, tok) => return Err(ParseErr::Syntax(line, format!("Expected either a closing brace or a comma after argument name {}, got {}", name, tok)))
                    };
                }
                Token(_, TokenType::RightBrace(')')) => break,
                Token(line, tok) => {
                    return Err(ParseErr::Syntax(
                        line,
                        format!(
                        "Expected closing brace, comma, or argument name after typename, got {}",
                        tok
                    ),
                    ))
                }
            }
        }

        let ret_type = self.parse_type().ok_or_else(|| {
            ParseErr::Syntax(
                open.0,
                format!("Expected a return type after function prototype {}", name),
            )
        })?;

        //Return the function prototype
        Ok(FnProto {
            name,
            attrs,
            arg_names: names,
            args: types,
            ret: ret_type,
        })
    }

    /// Parse a function declaration, if the ext attribute is set in the prototype then no body is parsed, and if the prototype ends with a semicolon
    /// no body is parsed. This assumes that the `fun` keyword is already consumed
    fn parse_fn(&mut self) -> Result<Ast<'ctx>, ParseErr> {
        let proto = self.parse_proto()?; //Get the function prototype
                                         //No body needed, return just the declaration
        if proto.attrs.contains(FnAttrs::EXTERN) {
            return Ok(Ast::FnProto(proto));
        } else {
            match self.toks.peek().eof()? {
                Token(_, TokenType::Semicolon) => return Ok(Ast::FnProto(proto)),
                Token(_, TokenType::LeftBrace('{')) => {
                    return Ok(Ast::Fn(proto, self.parse_body()?))
                }
                Token(line, tok) => {
                    return Err(ParseErr::Syntax(
                        *line,
                        format!(
                        "Expected a semicolon or function body after function prototype {}, got {}",
                        proto.name, tok
                    ),
                    ))
                }
            }
        }
    }

    /// Parse a function call, this assumes that the function name has already been consumed
    fn parse_call(&mut self, name: String) -> Result<Ast<'ctx>, ParseErr> {
        let _ = self.toks.next(); //Consume the opening brace token

        let mut args = Body::new(); //Create a list of argument expressions

        //Loop and take all expressions from the arguments
        loop {
            args.push(self.parse_expr()?); //Parse an argument expression
                                           //Either there is a comma delimiter or a closing brace
            match self.toks.next().eof()? {
                Token(_, TokenType::Comma) => (),
                Token(_, TokenType::RightBrace(')')) => break,
                Token(line, tok) => {
                    return Err(ParseErr::Syntax(
                        line,
                        format!("Unexpected token {}, expecting comma or closing brace", tok),
                    ))
                }
            }
        }

        //Return the function call
        Ok(Ast::Call { name, args })
    }

    /// Parse a function, if statement, or other body that is enclosed in curly braces. This assumes that the opening left brace has not been consumed yet
    fn parse_body(&mut self) -> Result<Body<'ctx>, ParseErr> {
        let _ = self.toks.next(); //Consume the left brace character from the token stream

        let mut body = Body::new(); //Create a new body to hold the
                                    //Parse expressions until the closing brace
        while !self.toks.peek().eof()?.is(TokenType::RightBrace('}')) {
            body.push(self.parse_complete()?); //Parse a complete expression
        }
        self.toks.next(); //Consume the closing brace token

        Ok(body)
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

            //This is either a variable access or function call
            Token(_, TokenType::Ident(ident)) => {
                //Check if it is a function call or a variable access
                match self.toks.peek().eof()? {
                    //This is a call because of the braces
                    Token(_, TokenType::LeftBrace('(')) => return self.parse_call(ident),
                    //Otherwise, this is a variable access
                    _ => Ok(Ast::VarAccess(ident)),
                }
            },
            Token(_, TokenType::Key(crate::lex::Key::Fun)) => return self.parse_fn(),

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

#[cfg(test)]
mod tests {
    use std::io::BufReader;

    use crate::lex::Lexer;

    use super::*;

    #[test]
    pub fn parse_correct() {
        let ctx = Context::create();

        const SRC: &[u8] = b"
        fun testing(i32 a) i32 {
        
        };
        ";

        let mut reader = BufReader::new(SRC);
        let mut parser: Parser<'_, Lexer<_>> = Parser::new(&ctx, Lexer::from_reader(&mut reader));

        assert_eq!(parser.parse().unwrap(), vec![
            Ast::Fn(FnProto{
                name: "testing".to_owned(),
                attrs: FnAttrs::empty(),
                arg_names: vec!["a".to_owned()],
                args: vec![ctx.custom_width_int_type(32).as_basic_type_enum()],
                ret: ctx.custom_width_int_type(32).as_basic_type_enum(),
            }, vec![])
        ], "Parsing a function definition and prototype fails")
    }
}