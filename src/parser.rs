use std::iter::Peekable;

use crate::{Type, ast::{Ast, Attributes, FunProto}, lex::{Key, Op, Token, TokenType}, types::Container};
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
    pub fn new(lexer: L) -> Self {
        Self {
            toks: lexer.peekable()
        }
    }

    /// Parse a program full of declarations and defintions
    pub fn parse(mut self) -> ParseRes<Vec<Ast>> {
        let mut body = Vec::new();
        loop {
            match self.toks.peek() {
                Some(_) => body.push(self.parse_decl()?),
                None => break
            }
        }
        Ok(body)
    }

    /// Parse a single declaration, the highest expression possible
    fn parse_decl(&mut self) -> ParseRes<Ast> {
        match self.toks.peek().eof()? {
            Token(_, TokenType::Key(Key::Fun)) => self.parse_fun(),

            Token(_, TokenType::Key(Key::Struct)) => {
                self.toks.next(); //Consume the struct keyword
                let name = self.expect_next_ident()?;
                match self.toks.peek().eof()? {
                    Token(_, TokenType::LeftBrace('{')) => {
                        let body = self.parse_struct_def_body()?;
                        Ok(Ast::StructDec(Container {
                            name,
                            fields: Some(body),
                        }))
                    },
                    _ => Ok(Ast::StructDec(Container {
                        name, 
                        fields: None
                    }))
                }
            },

            Token(line, other) => Err(ParseErr::UnexpectedToken(*line, other.clone(), vec![
                TokenType::Key(Key::Fun),
                TokenType::Key(Key::Struct),
            ]))
        }
    }

    /// Parse a comma separated list of typenames to identifiers for struct or union bodies
    fn parse_struct_def_body(&mut self) -> ParseRes<Vec<(String, Type)>> {
        self.expect_next(TokenType::LeftBrace('{'))?;
        let mut body = Vec::new();
        loop {
            let ty = self.parse_typename()?;
            let name = self.expect_next_ident()?;
            body.push((name, ty));

            match self.toks.peek().eof()? {
                Token(_, TokenType::Comma) => {
                    self.toks.next();
                    continue
                }
                Token(_, TokenType::RightBrace('}')) => {
                    self.toks.next();
                    break Ok(body)
                },
                Token(line, other) => break Err(ParseErr::UnexpectedToken(*line, other.clone(), vec![
                    TokenType::Comma,
                    TokenType::RightBrace('}')
                ]))
            }
        }
    }

    /// Parse a typename from the input stream, assumes that there is either an int type or an identifier 
    /// ready to be consumed from the lexer
    fn parse_typename(&mut self) -> ParseRes<Type> {
        let mut ty= match self.toks.next().eof()? {
            //This is a struct, union, or typedef'd type
            Token(_, TokenType::Ident(ident)) => {
                Type::Unknown(ident)
            },
            Token(_, TokenType::IntType(ty)) => ty,
            Token(_, TokenType::Key(Key::Void)) => Type::Void,
            Token(line, tok) => return Err(ParseErr::UnexpectedToken(line, tok, vec![TokenType::Ident(String::new()), TokenType::Key(Key::Void)]))
        };
        while match self.toks.peek() {
            Some(Token(_, TokenType::Key(Key::Ptr))) => {
                self.toks.next(); //Consume the pointer keyword
                ty = ty.ptr_type(); //Apply one level of pointer type
                true
            },
            _ => false, //Return the type once we are no longer parsing pointer types
        } {}
        Ok(ty)
    }
     
    /// Parse a variable declaration and optional assignment, expects the keyword `let`to be the next token consumed
    fn parse_var_dec(&mut self) -> ParseRes<Ast> {
        self.expect_next(TokenType::Key(Key::Let))?; //Expect the next token to be the let keyword
        let ty = self.parse_typename()?; //Get the type of this variable
        let attrs = self.parse_attrs(); //Get attributes, if any
        let name = self.expect_next_ident()?; 

        let decl = Ast::VarDecl{
            name,
            ty,
            attrs
        };

        //Check if assignment is present
        match self.toks.peek().eof()? {
            Token(_, TokenType::Op(Op::Assign)) => {
                self.toks.next(); //Consume the assignment operator
                let assigned = self.parse_expr()?; //Get the assigned value
                Ok(Ast::Bin(Box::new(decl), Op::Assign, Box::new(assigned)))
            },
            _ => Ok(decl)
        }
    }

    /// Parse a top level expression like variable declarations, if and while statements, etc.
    fn parse_top(&mut self) -> ParseRes<Ast> {
        match self.toks.peek().eof()? {
            Token(line, TokenType::Key(key)) => match key {
                Key::Let => self.parse_var_dec(),
                Key::If  => {
                    self.toks.next();
                    let cond = self.parse_expr()?; //Parse the conditional expression
                    let if_body = self.parse_body()?; //Parse the if statement body

                    let else_body = match self.toks.peek().eof()? {
                        Token(_, TokenType::Key(Key::Else)) => {
                            self.toks.next(); //Consume the else keyword
                            Some(self.parse_body()?)
                        },
                        _ => None,
                    };

                    Ok(Ast::If{
                        cond: Box::new(cond),
                        true_block: if_body,
                        else_block: else_body,
                    })
                },
                Key::While => {
                    self.toks.next(); //Consume the while keyword
                    let cond = self.parse_expr()?; 
                    let body = self.parse_body()?;
                    Ok(Ast::While{
                        cond: Box::new(cond),
                        block: body
                    }) 
                },
                Key::Ret => {
                    self.toks.next();
                    let val = match self.toks.peek().eof()? {
                        Token(_, TokenType::Semicolon) => {
                            None 
                        },
                        _ => Some(self.parse_expr()?),
                    };
                    Ok(Ast::Ret(Box::new(val)))
                }
                other => Err(ParseErr::UnexpectedToken(*line, TokenType::Key(other.clone()), vec![
                    TokenType::Key(Key::If),
                    TokenType::Key(Key::Let),
                    TokenType::Ident(String::new())
                ]))

            },

            //Variable assignment or function calls can be top level expressions
            Token(_, TokenType::Ident(name)) => {
                let name = name.clone();
                match self.toks.peek().eof()? {
                    Token(_, TokenType::LeftBrace('(')) => self.parse_funcall(name.clone()),
                    _ => {
                        self.expect_next(TokenType::Op(Op::Assign))?; //Expect the assignment operator
                        let assigned = self.parse_expr()?; //Get the assigned value
                        Ok(Ast::Bin(Box::new(Ast::VarAccess(name)), Op::Assign, Box::new(assigned)))
                    },
                }
            },

            Token(line, unexpected) => Err(ParseErr::UnexpectedToken(*line, unexpected.clone(), vec![
                TokenType::Ident(String::new()),
                TokenType::Key(Key::If),
                TokenType::Key(Key::Let)
            ]))

        }
    }

    /// Parse a function, if statement, while statement body, assuming that a semiclon separates expressions.
    /// Expects the next token to be an opening curly brace
    fn parse_body(&mut self) -> ParseRes<Vec<Ast>> {
        self.expect_next(TokenType::LeftBrace('{'))?;
        let mut body = Vec::new();

        loop {
            match self.toks.peek().eof()? {
                Token(_, TokenType::RightBrace('}')) => {
                    self.toks.next();
                    break Ok(body)
                },
                _ => {
                    body.push(self.parse_top()?);
                    self.expect_next(TokenType::Semicolon)?
                }
            }
        }
    }

    /// Parse a number literal or bool literal from the token stream
    fn parse_numliteral(&mut self) -> ParseRes<Ast> {
        //Get the number string
        let num = match self.toks.next().eof()? {
            Token(_, TokenType::NumLiteral(num)) => num,
            Token(_, TokenType::Key(Key::True)) => return Ok(Ast::NumLiteral(Type::Integer{signed: false, width: 1}, "1".to_owned())),
            Token(_, TokenType::Key(Key::False)) => return Ok(Ast::NumLiteral(Type::Integer{signed: false, width: 1}, "0".to_owned())),
            Token(line, tok) => return Err(ParseErr::UnexpectedToken(line, tok, vec![TokenType::NumLiteral(String::new()), TokenType::Key(Key::True), TokenType::Key(Key::False)])),
        };
        match self.toks.peek().eof()? {
            Token(_, TokenType::IntType(ty)) => Ok(Ast::NumLiteral(ty.clone(), num)),
            _ => Ok(Ast::NumLiteral(Type::Integer{signed: true, width: 32}, num))
        }
    }

    /// Parse a function call from the input tokens
    fn parse_funcall(&mut self, name: String) -> ParseRes<Ast> {
        self.expect_next(TokenType::LeftBrace('('))?; //Consume the opening brace
        let mut args = Vec::new(); 
        loop {
            match self.toks.peek().eof()? {
                Token(_, TokenType::RightBrace(')')) => {
                    self.toks.next();
                    break
                },
                Token(_, TokenType::Comma) => {
                    self.toks.next();
                }
                _ => {
                    let expr = self.parse_expr()?; //Parse the argument
                    args.push(expr); 
                },
            }
        }

        Ok(Ast::FunCall(name, args))
    }

    /// Parse a prefix expression like variable access or function calls
    fn parse_prefix(&mut self) -> ParseRes<Ast> {
        match self.toks.next().eof()? {
            Token(_, TokenType::LeftBrace(')')) => {
                let expr = self.parse_expr()?; 
                self.expect_next(TokenType::RightBrace(')'))?;
                Ok(expr)
            },
            Token(_, TokenType::Ident(name)) => {
                match self.toks.peek().eof()? {
                    Token(_, TokenType::LeftBrace('(')) => self.parse_funcall(name.clone()),
                    _ => Ok(Ast::VarAccess(name.clone())),
                }
            },
            _ => unreachable!(),
        }
    }
    
    /// Parse an expression from the input stream
    fn parse_expr(&mut self) -> ParseRes<Ast> {
        let lhs = match self.toks.peek().eof()? { 
            Token(_, TokenType::Ident(_)) | Token(_, TokenType::LeftBrace('('))=> {
                let expr = self.parse_prefix()?; //Parse the prefix expression
                match self.toks.peek().eof()? {
                    //This is a member item access
                    Token(_, TokenType::Dot) => {
                        self.toks.next(); //Consume the token
                        let val = self.expect_next_ident()?; //Get the identifier from the next token
                        Ok(Ast::MemberAccess(Box::new(expr), val))
                    },
                    _ => Ok(expr)
                } 
            },

            //Cast expression
            Token(_, TokenType::LeftBrace('{')) => {
                self.toks.next(); //Consume the opening curly brace token
                let ty = self.parse_typename()?; //Parse a typename 
                self.expect_next(TokenType::RightBrace('}'))?; //Consume the closing curly brace
                let casted = self.parse_expr()?; //Get the casted expression
                Ok(Ast::Cast(Box::new(casted), ty)) 
            },

            //Unary expression
            Token(_, TokenType::Op(op)) => {
                let op = op.clone();
                self.toks.next(); //Consume the operator
                let expr = self.parse_expr()?; //Parse the expression that the unary operator is being applied to
                Ok(Ast::Unary(op, Box::new(expr)))
            }

            Token(_, TokenType::StrLiteral(string)) => {
                let string = string.clone();
                self.toks.next(); //Consume the string literal
                Ok(Ast::StrLiteral(string))
            }
            
            Token(_, TokenType::NumLiteral(_)) 
            | Token(_, TokenType::Key(Key::True)) 
            | Token(_, TokenType::Key(Key::False)) => self.parse_numliteral(), 


            Token(line, unexpected) => Err(ParseErr::UnexpectedToken(*line, unexpected.clone(), 
            vec![
                TokenType::NumLiteral(String::new()),
                TokenType::Key(Key::True),
                TokenType::Key(Key::False),
                TokenType::Ident(String::new()),
                TokenType::Op(Op::Plus),
                TokenType::LeftBrace('('),
            ]))
        };

        //Check for binary expressions
        match self.toks.peek().eof()? {
            Token(_, TokenType::Op(op)) => {
                let op = op.clone();
                self.toks.next(); //Consume the operator
                let rhs = self.parse_expr()?; //Parse the right hand side of the expression
                Ok(Ast::Bin(Box::new(lhs?), op, Box::new(rhs)))
            },
            _ => lhs, //Return LHS if there is no operator
        }
    }

    /// Parse a function prototype from the input tokens, assumes that the `fun` keyword is the next token to be consumed
    fn parse_fun_proto(&mut self) -> ParseRes<FunProto> {
        self.toks.next(); //Consume the fun keyword
        let attrs = self.parse_attrs(); 
        let name = match self.toks.next().eof()? {
            Token(_, TokenType::Ident(name)) => name,
            Token(line, tok) => return Err(ParseErr::UnexpectedToken(line, tok, vec![TokenType::Ident(String::new())]))
        };
        self.expect_next(TokenType::LeftBrace('('))?; //Expect an opening brace

        let mut args = Vec::new(); //Create a new vec to hold argument names and types
        loop {
            //If this is the closing brace
            if self.toks.peek().eof()?.is(TokenType::RightBrace(')')) {
                self.toks.next();
                break
            }

            let ty = self.parse_typename()?; //Parse a typename of an argument
            match self.toks.next().eof()? {
                Token(_, TokenType::Ident(ident)) => {
                    args.push((ty, Some(ident))); //Add the argument
                    continue
                }, 
                Token(_, TokenType::Comma) => {
                    args.push((ty, None));
                },
                Token(_, TokenType::RightBrace(')')) => {
                    args.push((ty, None));
                    break
                },
                Token(line, tok) => return Err(ParseErr::UnexpectedToken(line, tok, vec![TokenType::Ident(String::new()), TokenType::Comma, TokenType::RightBrace(')')]))
            };
        }

        let ret = self.parse_typename()?; //Parse the return type of the function
        Ok(FunProto {
            name,
            attrs,
            args,
            ret
        })
    }

    /// Parse a function prototype or defintion
    fn parse_fun(&mut self) -> ParseRes<Ast> {
        let proto = self.parse_fun_proto()?; 
        match self.toks.peek().eof()? {
            Token(_, TokenType::LeftBrace('{')) => {
                let body = self.parse_body()?; 
                Ok(Ast::FunDef(proto, body))
            },
            _ => Ok(Ast::FunProto(proto)),
        }
    }

    /// Expect the next token to be an identifier and return `Ok` with the identifier string if it is
    fn expect_next_ident(&mut self) -> ParseRes<String> {
        let next = self.toks.next().eof()?;
        match next {
            Token(_, TokenType::Ident(ident)) => Ok(ident),
            _ => Err(ParseErr::UnexpectedToken(next.0, next.1, vec![TokenType::Ident(String::new())]))
        }
    }

    /// Expect the next token to be a certain type, or return an `Err`
    fn expect_next(&mut self, tok: TokenType) -> ParseRes<()> {
        let next = self.toks.next().eof()?;
        match next.is(tok.clone()) {
            true => Ok(()),
            false => Err(ParseErr::UnexpectedToken(next.0, next.1, vec![tok]))
        }
    }

    /// Parse attributes if there are any
    fn parse_attrs(&mut self) -> Attributes {
        let mut attrs = Attributes::empty();
        while match self.toks.peek() {
            Some(Token(_, TokenType::Key(key))) => match key {
                Key::Const => {
                    self.toks.next();
                    attrs.insert(Attributes::CONST);
                    true
                },
                Key::Static => {
                    self.toks.next();
                    attrs.insert(Attributes::STATIC);
                    true
                },
                _ => false
                
            }
            _ => false
        } {}
        attrs
    }
}

/// The `ParseErr` enum enumerates every possible error that can happen when parsing in the [Parser] struct
#[derive(Error, Debug)]
pub enum ParseErr {
    #[error("Unexpected End-Of-File")]
    UnexpectedEOF,

    #[error("Unexpected token {} on line {}, expecting one of {:?}", .0, .1, .2)]
    UnexpectedToken(usize, TokenType, Vec<TokenType>),
}

trait NoEof: Sized {
    type Item;
    fn eof(self) -> ParseRes<Self::Item>;

}

impl NoEof for Option<Token> {
    type Item = Token;
    fn eof(self) -> ParseRes<Self::Item> {
        self.ok_or(ParseErr::UnexpectedEOF)
    }
}
impl<'a> NoEof for Option<&'a Token> {
    type Item = &'a Token;
    fn eof(self) -> ParseRes<Self::Item> {
        self.ok_or(ParseErr::UnexpectedEOF)
    }
}