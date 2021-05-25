//! ## Grammar
//! Currently, the language features a relatively simple grammar very similar to Forth or Factor.
//! One major difference are **attributes**, fields of words, types, etc. that
//! are resolved at compile time and serve to select, for example, a specific generic type of a word call.
//! ```notrust
//! :w[ret: ui[w: 8]]
//! ```
//! or implied by the context:
//! ```notrust
//! :w[ui[8]]
//! ```
//!
//! ```notrust
//! <program> := <decl>*
//! <decl> := <word-decl>
//!        |= <coldecl>
//!
//! <coldecl> := ':' + 'col' + <ident> + '{' + (<type> + <ident> + ',')*  '}'
//! <word-decl> := ':' + 'w' + <ident> + <stack-decl> + <stack-decl> + (';' | <fnbody>)
//!
//! <stack-decl> := '(' + (<type> + ','?)* + ')'
//!
//! <body> := '{' + <expr>* + '}'
//! <attr> := '[' + (<ident> + (':' + <type> | <num> | <ident>)? + ',')*  + ']'
//!
//! <expr> := <value>* <word>
//!
//! <word> := <ident> + <attr>?
//!  
//! <literal> := '"' + <letter>* + '"' ; String literal pointer pushed to the stack
//!           |= <number>* + <int-type>? ; Number literal pushed to the stack
//!           |= '[' + (<expr> + ','?) + ',' ]
//!
//!
//! //If no attribute is present, default integer width is 32
//! <int-type> := 'si' + <attr>?
//!            |= 'ui' + <attr>?
//!
//! <type> := <int-type> | 'col' + <attr>  | 'ptr' + <attr>
//! ```
//!
//! An example program:
//!
//! ```notrust
//! :w main (si, ptr[ptr[ui[8]]]) -> (si) {
//!     //Print the argument given to the executable
//!     2 eq ? { 1 + deref puts 0} {drop "Enter an argument to print!" print -1}
//! }
//! ```

use std::convert::TryFrom;
use std::error::Error;
use std::fmt::Display;

use hashbrown::HashMap;
use std::iter::Peekable;

use crate::ast::Body;
use crate::ast::WordDecl;
use crate::ast::WordProto;
use crate::types::StackLayout;
use crate::types::Type;
use crate::types::Value;
use crate::{
    ast::{Ast, Decl},
    lex::{Token, TokenType},
};

/// The `Attr` can struct holds a value given to an attribute and additional attributes for the value
#[derive(Clone, PartialEq, Debug, Eq)]
pub struct Attr {
    pub val: String, 
    pub attrs: HashMap<String, Attr>,
}

impl Attr {
    /// Create a new unnamed attributes
    pub fn new() -> Self {
        Self {
            val: String::new(),
            attrs: HashMap::new()
        }
    }

    /// Insert an attribute into this attribute's attributes
    pub fn insert(&mut self, into: String, item: Attr) {
        self.attrs.insert(into, item);
    }

    pub fn get(&self, key: &str) -> Option<&Attr> {
        self.attrs.get(key)
    }
}


/// A struct representing an error that occurred when parsing
#[derive(Debug)]
pub enum ParseErr {
    /// An error occurred when lexing the input text
    LexErr {
        /// The line that this error occurred on
        line: usize,
        /// The error message
        msg: String,
    },
    /// There were no tokens left but we still expected tokens to finish parsing
    UnexpectedEOF,
    /// Unexpected token when parsing
    UnexpectedTok {
        /// The unexpected token
        token: Token,

        /// The tokens that we were expecting
        expecting: Vec<TokenType>,
    },
}

/// A trait that throws a `ParseErr::UnexpectedEOF` if an Option is None
trait NoEof: Sized {
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

    fn eof(self) -> Result<&'a Token, ParseErr> {
        self.ok_or(ParseErr::UnexpectedEOF)
    }
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexErr { line, msg } => write!(f, "Line #{}: Lexer error: {}", line, msg),
            Self::UnexpectedEOF => write!(f, "Unexpected end of file"),
            Self::UnexpectedTok { token, expecting } => write!(
                f,
                "Line #{}, unexpected token {}, expected one of: {:#?}",
                token.line, token, expecting
            ),
        }
    }
}

impl Error for ParseErr {}

/// The `Parser` struct converts a token stream into an abstract syntax tree representing the program. It takes a token stream from
/// the [Lexer](struct@crate::lex::Lexer) struct.
pub struct Parser<T: Iterator<Item = Token>> {
    /// The peekable token stream that we will parse
    pub tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Parse the token stream into a list of abstract syntax tree nodes
    pub fn parse(mut self) -> Result<Body, ParseErr> {
        let mut exprs = Vec::new();
        while self.tokens.peek().is_some() {
            exprs.push(self.parse_expr()?);
        }
        Ok(Body(exprs))
    }

    /// Parse a word prototype, assumed that the `w` keyword is already consumed from the token stream
    fn parse_proto(&mut self) -> Result<WordProto, ParseErr> {
        let attrs = self.parse_attr()?; //Parse all attributes of the function
                                       //Get the name of the function
        let name = match self.tokens.next().eof()? {
            Token {
                token: TokenType::Word(name),
                line: _,
            } => name,
            tok => {
                return Err(ParseErr::UnexpectedTok {
                    expecting: vec![TokenType::Word("".to_owned())],
                    token: tok,
                })
            }
        };

        let input = self.parse_stack_layout()?; //Parse the input stack layout
        let output = self.parse_stack_layout()?; //Parse the output stack layout
        Ok(WordProto {
            name,
            attrs,
            input,
            output,
        })
    }

    /// Parse a stack layout, see the module level [docs](mod@crate::parse) for more information about the grammar
    fn parse_stack_layout(&mut self) -> Result<StackLayout, ParseErr> {
        //Make sure that we get the opening brace
        if !self.tokens.peek().eof()?.is(TokenType::LeftBrace('(')) {
            return Err(ParseErr::UnexpectedTok {
                token: self.tokens.next().unwrap(),
                expecting: vec![TokenType::LeftBrace('(')],
            });
        }
        self.tokens.next(); //Consume the opening brace token

        let mut lay = StackLayout::new(); //Create a new stack layout

        //Parse typenames until the closing brace is encountered
        while !self.tokens.peek().eof()?.is(TokenType::RightBrace(')')) {
            if let TokenType::Word(word) = self.tokens.next().eof()?.token {
                let attrs = self.parse_attr()?; //Parse the attributes of the type
                lay.push(self.parse_typename(word, &attrs)?); //Add a type to the stack layout
            }
            

            if self.tokens.peek().eof()?.is(TokenType::Comma) {
                self.tokens.next(); //Consume the comma
            }
        }
        self.tokens.next(); //Consume the token

        Ok(lay) //Return the parsed stack layout
    }

    /// Parse attributes of a function or type and return them in a hashmap of strings to string values
    /// If the first parameter has no name given, then the hashmap entry at `""` will be filled with the attribute value.
    /// This function doesn't assume that the left square bracket has already been consumed and will consume it
    fn parse_attr(&mut self) -> Result<HashMap<String, Attr>, ParseErr> {
        if !matches!(
            self.tokens.peek(),
            Some(&Token {
                line: _,
                token: TokenType::LeftBrace('[')
            })
        ) {
            return Ok(HashMap::new()); //Return a hashmap with no entries because the attributes are empty
        }
        self.tokens.next(); //Consume the opening brace

        let mut attrs = HashMap::new();

        loop {
            //Check the next token in the stream
            let next = self.tokens.next().eof()?;
            //Check the next token to see if we should stop parsing attributes
            match next.token {
                TokenType::Comma => (),              //Consume the comma character
                TokenType::RightBrace(']') => break, //Consume the end space and stop parsing

                //This is either an attribute name or an attribute value with an implicit name
                TokenType::Word(ref ident) => match self.tokens.peek().eof()?.token {
                    //This is a attribute name because the semicolon is there
                    TokenType::Semicolon => {
                        self.tokens.next(); //Consume the semicolon token
                        let val = self.tokens.next().eof()?; //Get the value being assigned to the attribute name
                                                             //Make sure that the value is valid and get the value being assigned
                        let val = match val {
                            Token {
                                line: _,
                                token: TokenType::Word(id),
                            } => id,
                            unexpected => {
                                return Err(ParseErr::UnexpectedTok {
                                    expecting: vec![TokenType::Word("".to_owned())],
                                    token: unexpected,
                                })
                            }
                        };

                        attrs.insert(ident.clone(), Attr { val, attrs: self.parse_attr()?}); //Insert the name and value pair into the attributes
                    }
                    _ => {
                        attrs.insert("".to_owned(), Attr {val: ident.clone(), attrs: self.parse_attr()?}); //Insert the value with implicit name
                    }
                },
                _ => {
                    return Err(ParseErr::UnexpectedTok {
                        expecting: vec![
                            TokenType::Comma,
                            TokenType::RightBrace(']'),
                            TokenType::Word("".to_owned()),
                        ],
                        token: next,
                    })
                }
            }
        }

        Ok(attrs) //Return the attributes map
    }

    /// Parse the body of a word, if statement, etc.
    fn parse_body(&mut self) -> Result<Body, ParseErr> {
        //Ensure that we get the opening brace
        if !self.tokens.peek().eof()?.is(TokenType::LeftBrace('{')) {
            return Err(ParseErr::UnexpectedTok {
                token: self.tokens.next().unwrap(),
                expecting: vec![TokenType::LeftBrace('{')],
            });
        }
        self.tokens.next(); //Consume the opening brace
        let mut body = Body(Vec::new());
        //Iterate parsing expressions until we get a closing brace
        while !self.tokens.peek().eof()?.is(TokenType::RightBrace('}')) {
            body.0.push(self.parse_expr()?);
        }
        self.tokens.next(); //Consume the closing brace
        Ok(body)
    }

    /// Parse an expression from the token stream
    pub fn parse_expr(&mut self) -> Result<Ast, ParseErr> {
        match self.tokens.next().eof()? {
            Token {
                line: _,
                token: TokenType::Semicolon,
            } => match self.tokens.next().eof()?.token {
                //Word prototype, parse it
                TokenType::Word(word) => match word.as_ref() {
                    "w" => self.parse_proto().map(|proto| {
                        Ast::Decl(Decl::Word(WordDecl {
                            proto,
                            body: self.parse_body().unwrap(),
                        }))
                    }),
                    _ => unimplemented!("NOTHING BUT FUN"),
                },
                _ => panic!("FUCK"),
            },
            //A word call
            Token {
                line: _,
                token: TokenType::Word(word),
            } => Ok(Ast::Word {
                path: word,
                attrs: self.parse_attr()?,
            }),
            //Push a number to the stack
            Token {
                line: _,
                token: TokenType::NumLiteral(num),
            } => {
                let attrs = self.parse_attr()?; //Parse the attributes of the number for a type
                                                //There is an explicit type given
                if let Some(ty) = attrs.get("type") {
                    Ok(Ast::Literal {
                        val: Value::num(num, Type::try_from(ty.val.as_str()).unwrap()),
                    })
                } else {
                    Ok(Ast::Literal {
                        val: Value::num(
                            num,
                            Type::Int {
                                width: 32,
                                signed: true,
                            },
                        ),
                    })
                }
            }
            what => Err(ParseErr::UnexpectedTok {
                expecting: vec![TokenType::Word("_".to_owned()), TokenType::NumLiteral(0), TokenType::StrLiteral("_".to_owned())],
                token: what,
            }),
        }
    }


    /// Parse a type name from one or more tokens
    fn parse_typename(&mut self, word: String, attrs: &HashMap<String, Attr>) -> Result<Type, ParseErr> {
        //Try to parse an integer type from the string
        if let Ok(ty) = Type::try_from(word.as_str()) {
            return Ok(ty)
        }
        match word.as_str() {
            "ptr" => {
                match attrs.get("") {
                    Some(Attr { val, attrs }) => Ok( Type::Ptr(Box::new(self.parse_typename(val.clone(), attrs)?)) ),
                    _ => panic!(),
                }
            },
            _ => panic!("")
        }
    }
}
