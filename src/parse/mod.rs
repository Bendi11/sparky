use std::{iter::Peekable, fmt};

use smallvec::SmallVec;
use string_interner::{StringInterner, symbol::SymbolU32 as Symbol};

use crate::{util::loc::Span, ast::{Ast, UnresolvedType, IntegerWidth, FunProto, AstNode, FunFlags, IfExpr, ElseExpr}, parse::token::Op};

use self::{lex::Lexer, token::{TokenData, BracketType, Token}};

pub mod lex;
pub mod token;

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
    /// All tokens expected to begin when parsing an expression
    const EXPECTED_FOR_EXPRESSION: &'static [TokenData<'static>] = &[
        
    ];

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

    /// Peek the next token from the token stream or an [error](ParseErrorKind::UnexpectedEOF) if there are no more tokens to be lexed
    fn peek_tok(&mut self, expecting: &'static [TokenData<'static>]) -> ParseResult<'src, &Token<'src>> {
        let Self {
            toks,
            trace,
            ..
        } = self;

        toks.peek()
            .ok_or_else(|| ParseError {
                highlighted_span: None,
                backtrace: trace.clone(),
                error: ParseErrorKind::UnexpectedEOF {
                    expecting: ExpectingOneOf(expecting)
                }
            })
    }

    /// Consume the next token and expect it to be an identifier
    fn expect_next_ident(&mut self, expected: &'static [TokenData<'static>]) -> ParseResult<'src, &'src str> {
        let next = self.next_tok(expected)?;
        if let TokenData::Ident(name) = next.data {
            Ok(name)
        } else {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(expected)
                }
            })
        }
    }

    /// Consume the next token and expect it to be the given type of token
    fn expect_next(&mut self, expecting: &'static [TokenData<'static>]) -> ParseResult<'src, ()> {
        let next = self.next_tok(expecting)?;
        if !expecting.contains(&next.data) {
            Ok(())
        } else {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(expecting)
                }
            })
        }
    }

    /// Generate a [Symbol] for the given string using the string interner contained in `self`
    /// 
    /// Encapsulated as a function to allow for easier refactoring later
    #[inline]
    fn symbol(&mut self, for_str: &'src str) -> Symbol {
        self.interner.get_or_intern(for_str)
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
                let name = self.expect_next_ident(&[TokenData::Ident("function name")])?;

                const ARGS_EXPECTING: &[TokenData<'static>] = &[
                    TokenData::Ident("argument typename"),
                    TokenData::Arrow,
                    TokenData::OpenBracket(BracketType::Curly)
                ];

                let mut args = Vec::new();

                loop {
                    let peeked = self.peek_tok(ARGS_EXPECTING)?;
                    match peeked.data {
                        TokenData::Ident(_) => {
                            self.trace.push("function argument typename");
                            let arg_type = self.parse_typename()?;
                            self.trace.pop();

                            self.trace.push("function argument name");
                            let arg_name = self.expect_next_ident(&[TokenData::Ident("function argument name")])?;
                            self.trace.pop();

                            args.push((self.symbol(arg_name), arg_type));

                            const EXPECTING_AFTER_ARG: &[TokenData<'static>] = &[
                                TokenData::OpenBracket(BracketType::Curly), 
                                TokenData::Comma, 
                                TokenData::Arrow
                            ];

                            let after_arg = self.peek_tok(EXPECTING_AFTER_ARG)?;
                            if let TokenData::Comma = after_arg.data {
                                self.next_tok(EXPECTING_AFTER_ARG)?;
                            }
                        },
                        _ => break
                    }
                }

                const EXPECTING_AFTER_ARGS: &[TokenData<'static>] = &[
                    TokenData::OpenBracket(BracketType::Curly), 
                    TokenData::Arrow
                ];

                let after_args = self.peek_tok(EXPECTING_AFTER_ARGS).map(|tok| tok.data.clone());
                let return_ty = if let Ok(TokenData::Arrow) = after_args {
                    self.next_tok(EXPECTING_AFTER_ARGS)?;
                    self.trace.push("function return typename");
                    let return_ty = self.parse_typename()?;
                    self.trace.pop();
                    return_ty
                } else {
                    UnresolvedType::Unit
                };

                let proto = FunProto {
                    name: self.symbol(name),
                    args,
                    return_ty,
                    flags: FunFlags::empty()
                };

                if let Ok(TokenData::OpenBracket(BracketType::Curly)) = after_args {
                    self.trace.push("function body");
                    let body = self.parse_body()?;
                    self.trace.pop();

                    Ok(Ast {
                        span: next.span,
                        node: AstNode::FunDef(proto, body)
                    })
                } else {
                    Ok(Ast {
                        span: next.span,
                        node: AstNode::FunDecl(proto)
                    })
                }
            },
            _ => Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(&[TokenData::Number("fixed-point number")])
                }
            })
        }
    }

    /// Parse a curly brace enclosed AST body
    fn parse_body(&mut self) -> ParseResult<'src, Vec<Ast>> {
        self.expect_next(&[TokenData::OpenBracket(BracketType::Curly)])?;

        Ok(vec![])
    }

    fn parse_stmt(&mut self) -> ParseResult<'src, Ast> {
        const EXPECTING_FOR_STMT: &[TokenData<'static>] = &[

        ];

        let peeked = self.peek_tok(EXPECTING_FOR_STMT)?;

        match peeked.data {
            TokenData::Ident("let") | TokenData::Ident("mut") => {
                const EXPECTING_AFTER_LET: &[TokenData<'static>] = &[
                    TokenData::Ident("variable name"), 
                    TokenData::OpenBracket(BracketType::Smooth)
                ];
    
                self.toks.next();
                let mutable = peeked.data == TokenData::Ident("mut");
                self.trace.push("variable declaration");
    
                let next = self.next_tok(EXPECTING_AFTER_LET)?;
    
                let mut var_type = None;
                let name = match next.data {
                    TokenData::Ident(name) => self.symbol(name),
                    TokenData::OpenBracket(BracketType::Smooth) => {
                        let typename = self.parse_typename()?;
                        self.expect_next(&[TokenData::CloseBracket(BracketType::Smooth)])?;
    
                        var_type = Some(typename);
    
                        let name = self.expect_next_ident(&[TokenData::Ident("variable name")])?;
                        self.symbol(name)
                    },
                    _ => return Err(ParseError {
                        highlighted_span: Some(next.span),
                        backtrace: self.trace.clone(),
                        error: ParseErrorKind::UnexpectedToken {
                            found: next,
                            expecting: ExpectingOneOf(EXPECTING_AFTER_LET)
                        }
                    })
                };
    
                self.trace.pop();
    
                Ok(Ast {
                    span: (peeked.span.from, next.span.to).into(),
                    node: AstNode::VarDeclaration {
                        name,
                        ty: var_type,
                        mutable
                    }
                })
            },
            _ => Err(ParseError {
                highlighted_span: Some(peeked.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: peeked.clone(),
                    expecting: ExpectingOneOf(EXPECTING_FOR_STMT)
                }
            })
        }
        
    }

    /// Parse a full expression from the token stream
    fn parse_expr(&mut self) -> ParseResult<'src, Ast> {
        let peeked = self.peek_tok(Self::EXPECTED_FOR_EXPRESSION)?;

        let expr = match peeked.data {
            TokenData::Ident("if") => {
                let if_expr = self.parse_if()?;
                Ast {
                    span: peeked.span,
                    node: AstNode::IfExpr(if_expr)
                }
            },

            TokenData::
            
        };

        self.parse_expr_rhs(expr)
    }

    /// Parse the right hand side of an expression if there is one
    fn parse_expr_rhs(&mut self, lhs: Ast) -> ParseResult<'src, Ast> {
        let peeked = self.toks.peek();
        if let Some(peeked) = peeked {
            match peeked.data {
                TokenData::Op(operator) => {
                    self.toks.next();
                    
                    let rhs = self.parse_expr()?;
                    Ok(Ast {
                        span: (lhs.span.from, rhs.span.to).into(),
                        node: AstNode::BinExpr(Box::new(lhs), operator, Box::new(rhs))
                    })
                },
                _ => Ok(lhs)
            }
        } else {
            Ok(lhs)
        }
    }

    /// Parse an if statement
    fn parse_if(&mut self) -> ParseResult<'src, IfExpr> {
        self.expect_next(&[TokenData::Ident("if")])?;
        self.trace.push("if condition");
        let cond = self.parse_expr()?;
        self.trace.pop();

        self.trace.push("if body");
        let body = self.parse_body()?;
        self.trace.pop();

        let peek = self.toks.peek();
        if let Some(TokenData::Ident("else")) = peek.map(|tok| &tok.data) {
            self.toks.next();

            let after_else = self.peek_tok(&[TokenData::OpenBracket(BracketType::Curly)])?;
            match after_else.data {
                TokenData::OpenBracket(BracketType::Curly) => {
                    self.trace.push("else body");
                    let else_body = self.parse_body()?;
                    self.trace.pop();

                    Ok(IfExpr {
                        cond: Box::new(cond),
                        body,
                        else_expr: Some(ElseExpr::Else(else_body))
                    })
                },
                _ => Ok(IfExpr {
                    cond: Box::new(cond),
                    body,
                    else_expr: Some(ElseExpr::ElseIf(Box::new(self.parse_if())))
                })
            }
        } else {
            Ok(IfExpr {
                cond: Box::new(cond),
                body,
                else_expr: None
            })
        }
    }

    /// Parse a prefix expression from the token stream
    fn parse_prefix_expr(&mut self) -> ParseResult<'src, Ast> {
        const EXPECTING_NEXT: &[TokenData<'static>] = &[
            TokenData::Ident("variable or function name"),
            TokenData::OpenBracket(BracketType::Smooth)
        ];
        
        let next = self.next_tok(EXPECTING_NEXT)?;
        let member_of = match next.data {
            TokenData::Ident(name) => {
                let peeked = self.toks.peek();
                if let Some(TokenData::OpenBracket(BracketType::Smooth)) = peeked.map(|tok| &tok.data) {
                    self.next_tok(EXPECTING_NEXT)?; //Consume the opening bracket

                    let mut args = vec![];

                    loop {
                        let next_in_args = self.peek_tok(Self::EXPECTED_FOR_EXPRESSION)?;
                        match next_in_args.data {
                            TokenData::Comma => {
                                self.next_tok(&[TokenData::Comma])?;
                            },
                            TokenData::CloseBracket(BracketType::Smooth) => {
                                self.next_tok(&[TokenData::CloseBracket(BracketType::Smooth)])?;
                                break
                            },
                            _ => {
                                self.trace.push("function call argument");
                                args.push(self.parse_expr()?);
                                self.trace.pop();
                            }
                        }
                    }

                    Ast {
                        span: next.span,
                        node: AstNode::FunCall(self.symbol(name), args)
                    }

                } else {
                    Ast {
                        span: next.span,
                        node: AstNode::VarAccess(self.symbol(name))
                    }
                }
            },
            TokenData::OpenBracket(BracketType::Smooth) => {
                self.trace.push("expression in parentheses");
                let expr = self.parse_expr()?;
                self.expect_next(&[TokenData::CloseBracket(BracketType::Smooth)])?;
                self.trace.pop();
                expr
            },
            _ => return Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(EXPECTING_NEXT)
                }
            })
        };
        
        //Parse any indexing or member access expressions
        self.parse_access(member_of)
    }

    /// Recursive function to parse member accesses with the '.' token,
    /// and indexing with the [] array indexing method
    fn parse_access(&mut self, accessing: Ast) -> ParseResult<'src, Ast> {
        const ACCESS_EXPECTING: &[TokenData<'static>] = &[
            TokenData::Period,
            TokenData::OpenBracket(BracketType::Square)
        ];

        let peeked = self.peek_tok(ACCESS_EXPECTING)?.clone();
        match peeked.data {
            TokenData::Period => {
                self.toks.next(); //Eat the period character
                self.trace.push("member access expression");
                let item = self.expect_next_ident(&[TokenData::Ident("member name")])?;
                self.trace.pop();

                let symbol = self.symbol(item);
                self.parse_access(Ast {
                    span: (accessing.span.from, peeked.span.to).into(),
                    node: AstNode::MemberAccess(Box::new(accessing), symbol)
                })
            },
            TokenData::OpenBracket(BracketType::Square) => {
                self.toks.next();
                self.trace.push("index expression");
                let index = self.parse_expr()?;
                

                self.expect_next(&[TokenData::CloseBracket(BracketType::Square)])?;
                self.trace.pop();

                self.parse_access(Ast {
                    span: (accessing.span.from, peeked.span.to).into(),
                    node: AstNode::Index {
                        object: Box::new(accessing),
                        index: Box::new(index)
                    }
                })
            }

            _ => Ok(accessing)
        }
    }

    /// Attempt to parse a typename from the token stream
    fn parse_typename(&mut self) -> ParseResult<'src, UnresolvedType> {
        const EXPECTING_NEXT: &[TokenData<'static>] = &[
            TokenData::Ident("type name"), 
            TokenData::OpenBracket(BracketType::Smooth),
        ];

        const EXPECTING_INTEGER: &[TokenData<'static>] = &[
            TokenData::Ident("i8"), TokenData::Ident("i16"), TokenData::Ident("i32"), TokenData::Ident("i64"),
            TokenData::Ident("u8"), TokenData::Ident("u16"), TokenData::Ident("u32"), TokenData::Ident("u64"),
        ];

        let next = self.next_tok(EXPECTING_NEXT)?;

        match next.data {
            TokenData::Ident(name) => match &name[0..1] {
                "i" | "u" => {
                    let signed = &name[0..1] == "i";

                    match &name[1..] {
                        "8" => Ok(UnresolvedType::Integer { signed, width: IntegerWidth::Eight }),
                        "16" => Ok(UnresolvedType::Integer { signed, width: IntegerWidth::Sixteen }),
                        "32" => Ok(UnresolvedType::Integer { signed, width: IntegerWidth::ThirtyTwo }),
                        "64" => Ok(UnresolvedType::Integer { signed, width: IntegerWidth::SixtyFour }),
                        _ => Err(ParseError {
                            highlighted_span: Some(next.span),
                            backtrace: self.trace.clone(),
                            error: ParseErrorKind::UnexpectedToken {
                                found: next,
                                expecting: ExpectingOneOf(EXPECTING_INTEGER)
                            }
                        })
                    }
                },
                "f" => match &name[1..] {
                    "32" => Ok(UnresolvedType::Float { doublewide: false }),
                    "64" => Ok(UnresolvedType::Float { doublewide: true }),
                    _ => Err(ParseError {
                        highlighted_span: Some(next.span),
                        backtrace: self.trace.clone(),
                        error: ParseErrorKind::UnexpectedToken {
                            found: next,
                            expecting: ExpectingOneOf(&[TokenData::Ident("f32"), TokenData::Ident("f64")])
                        }
                    })
                },
                _ => Ok(UnresolvedType::UserDefined { name: self.symbol(name), generic_args: SmallVec::new() })
            },
            TokenData::OpenBracket(BracketType::Square) => {
                self.trace.push("array type length");
                let len = self.parse_fixed_number()?;
                self.trace.pop();

                let closing = self.next_tok(&[TokenData::CloseBracket(BracketType::Square)])?;
                if let TokenData::CloseBracket(BracketType::Square) = closing.data {
                    self.trace.push("array item typename");
                    let item_type = self.parse_typename()?;
                    self.trace.pop();

                    Ok(UnresolvedType::Array {
                        elements: Box::new(item_type),
                        len
                    })
                } else {
                    Err(ParseError {
                        highlighted_span: Some(closing.span),
                        backtrace: self.trace.clone(),
                        error: ParseErrorKind::UnexpectedToken {
                            found: closing,
                            expecting: ExpectingOneOf(&[TokenData::CloseBracket(BracketType::Square)])
                        }
                    })
                }
            },
            TokenData::OpenBracket(BracketType::Smooth) => {
                self.trace.push("unit type");
                self.expect_next(&[TokenData::CloseBracket(BracketType::Smooth)])?;
                self.trace.pop();

                Ok(UnresolvedType::Unit)
            },
            TokenData::Op(Op::Star) => {
                self.trace.push("pointer type");
                let pointed_to = self.parse_typename()?;
                self.trace.pop();

                Ok(UnresolvedType::Pointer(Box::new(pointed_to)))
            },
            _ => Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(EXPECTING_NEXT)
                }
            })
        }
    }

    /// Parse a fixed-point number literal from the token stream
    fn parse_fixed_number(&mut self) -> ParseResult<'src, u64> {
        let next = self.next_tok(&[TokenData::Number("fixed-point number")])?;
        if let TokenData::Number(num) = next.data {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(&[TokenData::Number("fixed-point number")])
                }
            })
        } else {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(&[TokenData::Number("fixed-point number")])
                }
            })
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
                TokenData::Number(num) => write!(f, "number literal: {}", num),
                TokenData::String(string) => write!(f, "string literal: {}", string),
                TokenData::OpenBracket(ty) => match ty {
                    BracketType::Curly => write!(f, "'{{'"),
                    BracketType::Smooth => write!(f, "'('"),
                    BracketType::Square => write!(f, "'['"),
                },
                TokenData::CloseBracket(ty) => match ty {
                    BracketType::Curly => write!(f, "'}}'"),
                    BracketType::Smooth => write!(f, "')'"),
                    BracketType::Square => write!(f, "']'"),
                },
            }?;
        }

        Ok(())
    }
}
