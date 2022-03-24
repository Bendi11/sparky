use std::{borrow::Cow, fmt};

use crate::{
    ast::{BigInt, Literal},
    Symbol,
};
use smallvec::SmallVec;

use crate::{
    ast::{
        Ast, AstNode, Def, DefData, ElseExpr, FunFlags, FunProto, IfExpr, IntegerWidth,
        NumberLiteral, NumberLiteralAnnotation, ParsedModule, SymbolPath, UnresolvedFunType,
        UnresolvedType,
    },
    parse::token::Op,
    util::{files::FileId, loc::Span},
};

use self::{
    lex::Lexer,
    token::{BracketType, Token, TokenData},
};

pub mod lex;
pub mod token;

/// A structure consuming a token stream from a lexer and transforming it to an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser<'src> {
    /// The token stream to consume tokens from
    toks: Lexer<'src>,
    /// The current parse trace used for error and debug backtraces
    trace: SmallVec<[Cow<'static, str>; 24]>,
}

pub type ParseResult<'src, T> = Result<T, ParseError<'src>>;

impl<'src> Parser<'src> {
    /// All tokens expected to begin when parsing an expression
    const EXPECTED_FOR_EXPRESSION: &'static [TokenData<'static>] = &[
        TokenData::Ident("if"),
        TokenData::Ident("true"),
        TokenData::Ident("false"),
        TokenData::Dollar,
        TokenData::Op(Op::Star),
        TokenData::Op(Op::AND),
        TokenData::String("string literal"),
        TokenData::Number("number literal"),
        TokenData::OpenBracket(BracketType::Smooth),
        TokenData::OpenBracket(BracketType::Square),
        TokenData::OpenBracket(BracketType::Curly),
    ];

    /// Parse the input source code into a full AST
    pub fn parse(&mut self, name: Symbol, file: FileId) -> ParseResult<'src, ParsedModule> {
        let mut module = ParsedModule::new(name);

        self.parse_to(&mut module, file)?;

        Ok(module)
    }

    /// Set the parsed text
    pub fn set_text(&mut self, src: &'src str) {
        self.toks = Lexer::new(src);
    }

    /// Parse and add items to a module
    pub fn parse_to(&mut self, to: &mut ParsedModule, file: FileId) -> ParseResult<'src, ()> {
        while self.toks.peek().is_some() {
            let def = self.parse_decl(file)?;
            to.defs.insert(def.data.name(), def);
        }

        Ok(())
    }

    /// Create a new `Parser` from the given source string
    pub fn new(src: &'src str) -> Self {
        Self {
            toks: Lexer::new(src),
            trace: SmallVec::new(),
        }
    }

    /// Consume the next token from the token stream or an [error](ParseErrorKind::UnexpectedEOF) if there are no more tokens to be lexed
    fn next_tok(
        &mut self,
        expecting: &'static [TokenData<'static>],
    ) -> ParseResult<'src, Token<'src>> {
        self.toks.next().ok_or_else(|| ParseError {
            highlighted_span: None,
            backtrace: self.trace.clone(),
            error: ParseErrorKind::UnexpectedEOF {
                expecting: ExpectingOneOf(expecting),
            },
        })
    }

    /// Peek the next token from the token stream or an [error](ParseErrorKind::UnexpectedEOF) if there are no more tokens to be lexed
    fn peek_tok(
        &mut self,
        expecting: &'static [TokenData<'static>],
    ) -> ParseResult<'src, &Token<'src>> {
        let Self { toks, trace, .. } = self;

        toks.peek().ok_or_else(|| ParseError {
            highlighted_span: None,
            backtrace: trace.clone(),
            error: ParseErrorKind::UnexpectedEOF {
                expecting: ExpectingOneOf(expecting),
            },
        })
    }

    /// Consume the next token and expect it to be an identifier
    fn expect_next_ident(
        &mut self,
        expected: &'static [TokenData<'static>],
    ) -> ParseResult<'src, &'src str> {
        let next = self.next_tok(expected)?;
        if let TokenData::Ident(name) = next.data {
            Ok(name)
        } else {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(expected),
                },
            })
        }
    }

    /// Consume the next path from the input tokens, requiring at least one identifier
    fn expect_next_path(
        &mut self,
        expected: &'static [TokenData<'static>],
    ) -> ParseResult<'src, SymbolPath> {
        let first = self.expect_next_ident(expected)?;
        let first = self.symbol(first);
        self.expect_next_path_with(expected, first)
    }

    /// Get the next path in the token stream using a first identifier
    fn expect_next_path_with(
        &mut self,
        expected: &'static [TokenData<'static>],
        first: Symbol,
    ) -> ParseResult<'src, SymbolPath> {
        let mut parts = vec![first];
        while let Some(TokenData::Colon) = self.toks.peek().map(|tok| &tok.data) {
            if let Some(TokenData::Ident(_)) = self.toks.peek2().map(|tok| &tok.data) {
                self.toks.next(); //Consume the colon character
                let part = self.expect_next_ident(expected)?;
                parts.push(self.symbol(part));
            } else {
                break;
            }
        }

        Ok(if parts.len() == 1 {
            SymbolPath::new(parts[0])
        } else {
            SymbolPath::new_parts(&parts)
        })
    }

    /// Consume the next token and expect it to be the given type of token
    fn expect_next(&mut self, expecting: &'static [TokenData<'static>]) -> ParseResult<'src, ()> {
        let next = self.next_tok(expecting)?;
        if expecting.contains(&next.data) {
            Ok(())
        } else {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(expecting),
                },
            })
        }
    }

    /// Generate a [Symbol] for the given string using the string interner contained in `self`
    ///
    /// Encapsulated as a function to allow for easier refactoring later
    #[inline]
    fn symbol(&mut self, for_str: &'src str) -> Symbol {
        Symbol::from(for_str)
    }

    /// Parse a top-level declaration from the token stream
    fn parse_decl(&mut self, file: FileId) -> ParseResult<'src, Def> {
        const EXPECTING_NEXT: &[TokenData<'static>] = &[
            TokenData::Ident("fun"),
            TokenData::Ident("type"),
            TokenData::Ident("const"),
            TokenData::Ident("imp"),
        ];

        let next = self.next_tok(EXPECTING_NEXT)?;
        match next.data {
            TokenData::Ident("imp") => {
                self.trace.push("import statement".into());
                let imported = self.expect_next_path(&[TokenData::Ident("imported module")])?;
                self.trace.pop();

                Ok(Def {
                    file,
                    span: next.span,
                    data: DefData::ImportDef { name: imported },
                })
            }
            TokenData::Ident("fun") => {
                let (name, flags) =
                    match self.expect_next_ident(&[TokenData::Ident("function name")])? {
                        "ext" => (
                            self.expect_next_ident(&[TokenData::Ident("function name")])?,
                            FunFlags::EXTERN,
                        ),
                        other => (other, FunFlags::empty()),
                    };

                self.trace
                    .push(format!("function declaration '{}'", name).into());

                const ARGS_EXPECTING: &[TokenData<'static>] = &[
                    TokenData::Ident("argument typename"),
                    TokenData::Arrow,
                    TokenData::OpenBracket(BracketType::Curly),
                ];

                self.expect_next(&[TokenData::OpenBracket(BracketType::Smooth)])?;

                let mut args = Vec::new();

                loop {
                    let peeked = self.peek_tok(ARGS_EXPECTING)?;
                    match peeked.data {
                        TokenData::CloseBracket(BracketType::Smooth) => {
                            self.toks.next();
                            break;
                        }
                        _ => {
                            self.trace.push("function argument typename".into());
                            let arg_type = self.parse_typename()?;
                            self.trace.pop();

                            self.trace.push("function argument name".into());
                            let arg_name = self
                                .expect_next_ident(&[TokenData::Ident("function argument name")])?;
                            self.trace.pop();

                            args.push((self.symbol(arg_name), arg_type));

                            const EXPECTING_AFTER_ARG: &[TokenData<'static>] = &[
                                TokenData::OpenBracket(BracketType::Curly),
                                TokenData::Comma,
                                TokenData::Arrow,
                            ];

                            let after_arg = self.peek_tok(EXPECTING_AFTER_ARG)?;
                            if let TokenData::Comma = after_arg.data {
                                self.next_tok(EXPECTING_AFTER_ARG)?;
                            }
                        }
                    }
                }

                const EXPECTING_AFTER_ARGS: &[TokenData<'static>] =
                    &[TokenData::OpenBracket(BracketType::Curly), TokenData::Arrow];

                let after_args = self
                    .peek_tok(EXPECTING_AFTER_ARGS)
                    .map(|tok| tok.data.clone());
                let return_ty = if let Ok(TokenData::Arrow) = after_args {
                    self.next_tok(EXPECTING_AFTER_ARGS)?;
                    self.trace.push("function return typename".into());
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
                    flags,
                };

                self.trace.pop();

                if let Ok(TokenData::OpenBracket(BracketType::Curly)) =
                    self.peek_tok(EXPECTING_AFTER_ARGS).map(|a| a.data.clone())
                {
                    self.trace.push("function body".into());
                    let body = self.parse_body()?;
                    self.trace.pop();

                    Ok(Def {
                        file,
                        span: body.1,
                        data: DefData::FunDef(proto, body.0),
                    })
                } else {
                    Ok(Def {
                        file,
                        span: next.span,
                        data: DefData::FunDec(proto),
                    })
                }
            }
            TokenData::Ident("type") => {
                let name = self.expect_next_ident(&[TokenData::Ident("type name")])?;
                self.trace
                    .push(format!("type definition '{}'", name).into());

                self.expect_next(&[TokenData::Assign])?;
                let aliased = self.parse_typename()?;

                self.trace.pop();
                Ok(Def {
                    span: next.span,
                    data: DefData::AliasDef {
                        name: self.symbol(name),
                        aliased,
                    },
                    file,
                })
            }
            _ => Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(EXPECTING_NEXT),
                },
            }),
        }
    }

    /// Parse a curly brace enclosed AST body
    fn parse_body(&mut self) -> ParseResult<'src, (Vec<Ast>, Span)> {
        const EXPECTING_FOR_BODY: &[TokenData<'static>] =
            &[TokenData::OpenBracket(BracketType::Curly)];

        let tok = self.next_tok(EXPECTING_FOR_BODY)?;
        let start_loc = if let TokenData::OpenBracket(BracketType::Curly) = tok.data {
            tok.span.from
        } else {
            return Err(ParseError {
                highlighted_span: Some(tok.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    expecting: ExpectingOneOf(EXPECTING_FOR_BODY),
                    found: tok,
                },
            });
        };

        let mut body = vec![];

        let end_loc = loop {
            if self
                .peek_tok(&[TokenData::CloseBracket(BracketType::Curly)])?
                .data
                == TokenData::CloseBracket(BracketType::Curly)
            {
                let next = self.toks.next().unwrap().span.to;
                break next;
            }
            body.push(self.parse_stmt()?);
        };
        Ok((body, Span::new(start_loc, end_loc)))
    }
    
    /// Parse a statement from the token stream
    fn parse_stmt(&mut self) -> ParseResult<'src, Ast> {
        const EXPECTING_FOR_STMT: &[TokenData<'static>] = &[
            TokenData::Ident("if"),
            TokenData::Ident("let"),
            TokenData::Ident("mut"),
            TokenData::Ident("phi"),
            TokenData::Ident("match"),
            TokenData::Ident("return"),
            TokenData::Ident("break"),
            TokenData::Ident("continue"),
            TokenData::Ident("variable / function name"),
            TokenData::OpenBracket(BracketType::Smooth),
        ];

        let peeked = self.peek_tok(EXPECTING_FOR_STMT)?.clone();

        let stmt = match peeked.data {
            TokenData::Ident("break") => {
                self.toks.next();
                Ok(Ast {
                    span: peeked.span,
                    node: AstNode::Break,
                })
            }
            TokenData::Ident("continue") => {
                self.toks.next();
                Ok(Ast {
                    span: peeked.span,
                    node: AstNode::Continue,
                })
            }
            TokenData::Ident("if") => {
                let if_stmt = self.parse_if()?;
                Ok(Ast {
                    span: peeked.span,
                    node: AstNode::IfExpr(if_stmt),
                })
            }
            TokenData::Ident("match") => self.parse_match(),
            TokenData::Ident("let") | TokenData::Ident("mut") => {
                const EXPECTING_AFTER_LET: &[TokenData<'static>] = &[
                    TokenData::Ident("variable name"),
                    TokenData::OpenBracket(BracketType::Smooth),
                ];

                self.toks.next();
                let mutable = peeked.data == TokenData::Ident("mut");
                self.trace.push("variable declaration".into());

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
                    }
                    _ => {
                        return Err(ParseError {
                            highlighted_span: Some(next.span),
                            backtrace: self.trace.clone(),
                            error: ParseErrorKind::UnexpectedToken {
                                found: next,
                                expecting: ExpectingOneOf(EXPECTING_AFTER_LET),
                            },
                        })
                    }
                };

                self.trace.pop();

                Ok(Ast {
                    span: next.span,
                    node: AstNode::VarDeclaration {
                        name,
                        ty: var_type,
                        mutable,
                    },
                })
            }
            TokenData::Ident("phi") => {
                self.toks.next();
                self.trace.push("phi statement".into());
                let phi_expr = self.parse_expr()?;
                self.trace.pop();
                Ok(Ast {
                    span: (peeked.span.from, phi_expr.span.to).into(),
                    node: AstNode::PhiExpr(Box::new(phi_expr)),
                })
            }
            TokenData::Ident("return") => {
                self.toks.next();
                self.trace.push("return statement".into());
                //Attempt to parse a return expression
                let returned = self.parse_expr()?;

                self.trace.pop();
                Ok(Ast {
                    span: peeked.span,
                    node: AstNode::Return(Box::new(returned)),
                })
            }
            //Parse an assignment expression
            TokenData::Ident(_)
            | TokenData::OpenBracket(BracketType::Curly)
            | TokenData::OpenBracket(BracketType::Smooth) => self.parse_prefix_expr(),
            _ => Err(ParseError {
                highlighted_span: Some(peeked.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: peeked.clone(),
                    expecting: ExpectingOneOf(EXPECTING_FOR_STMT),
                },
            }),
        }?;

        if let Some(TokenData::Assign) = self.toks.peek().map(|tok| &tok.data) {
            self.trace.push("assignment statement".into());
            self.toks.next();
            let assigned = self.parse_expr()?;
            self.trace.pop();
            Ok(Ast {
                span: (stmt.span.from, assigned.span.to).into(),
                node: AstNode::Assignment {
                    lhs: Box::new(stmt),
                    rhs: Box::new(assigned),
                },
            })
        } else {
            Ok(stmt)
        }
    }

    /// Parse a full expression from the token stream
    fn parse_expr(&mut self) -> ParseResult<'src, Ast> {
        let peeked = self.peek_tok(Self::EXPECTED_FOR_EXPRESSION)?.clone();

        let expr = match &peeked.data {
            TokenData::Ident("if") => {
                let if_expr = self.parse_if()?;
                Ast {
                    span: peeked.span,
                    node: AstNode::IfExpr(if_expr),
                }
            }
            TokenData::Ident("match") => self.parse_match()?,
            TokenData::Ident("true") => {
                self.toks.next();
                Ast {
                    span: peeked.span,
                    node: AstNode::Literal(Literal::Bool(true)),
                }
            }
            TokenData::Ident("false") => {
                self.toks.next();
                Ast {
                    span: peeked.span,
                    node: AstNode::Literal(Literal::Bool(false)),
                }
            }
            TokenData::Dollar => {
                self.toks.next();
                self.trace.push("cast expression typename".into());
                let casted_to = self.parse_typename()?;
                self.trace.pop();
                self.trace.push("cast expression".into());
                let expr = self.parse_expr()?;
                self.trace.pop();
                Ast {
                    span: (peeked.span.from, expr.span.to).into(),
                    node: AstNode::CastExpr(casted_to, Box::new(expr)),
                }
            }

            TokenData::Op(unaryop) => {
                self.toks.next();
                self.trace.push("unary operation".into());
                let rhs = self.parse_expr()?;
                self.trace.pop();

                Ast {
                    span: (peeked.span.from, rhs.span.to).into(),
                    node: AstNode::UnaryExpr(*unaryop, Box::new(rhs)),
                }
            }
            TokenData::OpenBracket(BracketType::Square) => {
                self.trace.push("array literal".into());
                self.toks.next();

                let elements = if let Some(TokenData::CloseBracket(BracketType::Square)) =
                    self.toks.peek().map(|tok| &tok.data)
                {
                    vec![]
                } else {
                    const EXPECTING_FOR_ARRAY: &[TokenData<'static>] = &[
                        TokenData::CloseBracket(BracketType::Square),
                        TokenData::Comma,
                    ];
                    let mut elements = vec![];

                    loop {
                        let element = self.parse_expr()?;
                        elements.push(element);

                        let next = self.next_tok(EXPECTING_FOR_ARRAY)?;
                        match next.data {
                            TokenData::Comma => continue,
                            TokenData::CloseBracket(BracketType::Square) => break elements,
                            _ => {
                                return Err(ParseError {
                                    highlighted_span: Some(
                                        (peeked.span.from, elements.last().unwrap().span.to).into(),
                                    ),
                                    backtrace: self.trace.clone(),
                                    error: ParseErrorKind::UnexpectedToken {
                                        found: next,
                                        expecting: ExpectingOneOf(EXPECTING_FOR_ARRAY),
                                    },
                                })
                            }
                        }
                    }
                };
                self.trace.pop();

                Ast {
                    span: if let Some(last) = elements.last() {
                        (peeked.span.from, last.span.to).into()
                    } else {
                        peeked.span
                    },
                    node: AstNode::Literal(Literal::Array(elements)),
                }
            }
            TokenData::String(_data) => Ast {
                span: peeked.span,
                node: AstNode::Literal(Literal::String(self.parse_string_literal()?)),
            },

            TokenData::Number(_) => {
                self.trace.push("number literal".into());
                let num = self.parse_numliteral()?;
                self.trace.pop();
                Ast {
                    span: peeked.span,
                    node: AstNode::Literal(Literal::Number(num)),
                }
            },
            TokenData::Pound => {
                const EXPECTING_AFTER_POUND: &[TokenData<'static>] = &[
                    TokenData::Ident("typename"), TokenData::OpenBracket(BracketType::Curly)
                ];
                const EXPECTING_AFTER_BRACE: &[TokenData<'static>] = &[
                    TokenData::Ident("field name"), TokenData::CloseBracket(BracketType::Curly)
                ];

                let start_loc = peeked.span.from;

                self.trace.push("struct literal".into());
                self.toks.next();
                let after = self.peek_tok(EXPECTING_AFTER_POUND)?;
                let typename = match after.data {
                    TokenData::OpenBracket(BracketType::Curly) => None,
                    _ => Some(self.parse_typename()?),
                };

                self.expect_next(&[TokenData::OpenBracket(BracketType::Curly)])?;
                let mut fields = vec![];
                let end_loc = loop {
                    let next = self.next_tok(EXPECTING_AFTER_BRACE)?;
                    match &next.data {
                        TokenData::CloseBracket(BracketType::Curly) => {
                            break next.span.to
                        },
                        TokenData::Ident(name) => {
                            let name = self.symbol(name);
                            self.expect_next(&[TokenData::Assign])?;
                            let expr = self.parse_expr()?;
                            if let TokenData::Comma = self.peek_tok(&[
                                TokenData::CloseBracket(BracketType::Curly),
                                TokenData::Comma
                            ])?.data {
                                    self.toks.next();
                                }
                            fields.push((name, expr));
                        },
                        _ => return Err(ParseError {
                            highlighted_span: Some(next.span),
                            backtrace: self.trace.clone(),
                            error: ParseErrorKind::UnexpectedToken {
                                found: next,
                                expecting: ExpectingOneOf(EXPECTING_AFTER_BRACE)
                            }
                        })
                    }
                };

                self.trace.pop();

                Ast {
                    span: (start_loc, end_loc).into(),
                    node: AstNode::Literal(Literal::Struct {
                        ty: typename,
                        fields,
                    })
                }
            },
            TokenData::OpenBracket(BracketType::Smooth)
            | TokenData::Ident(_)
            | TokenData::OpenBracket(BracketType::Curly) => self.parse_prefix_expr()?,
            _ => {
                return Err(ParseError {
                    highlighted_span: Some(peeked.span),
                    backtrace: self.trace.clone(),
                    error: ParseErrorKind::UnexpectedToken {
                        found: peeked,
                        expecting: ExpectingOneOf(Self::EXPECTED_FOR_EXPRESSION),
                    },
                })
            }
        };

        self.parse_expr_rhs(expr)
    }

    /// Parse a single string literal, inserting escaped characters
    fn parse_string_literal(&mut self) -> ParseResult<'src, String> {
        let next_tok = self.next_tok(&[TokenData::String("string literal")])?;
        let (src, span) = if let Token {
            span,
            data: TokenData::String(src),
        } = next_tok
        {
            (src, span)
        } else {
            return Err(ParseError {
                highlighted_span: Some(next_tok.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next_tok,
                    expecting: ExpectingOneOf(&[TokenData::String("string literal")]),
                },
            });
        };

        let mut unescaped = String::with_capacity(src.len());
        let mut escaped_chars = src.chars();
        loop {
            let next = match escaped_chars.next() {
                Some(c) => c,
                None => break,
            };

            match next {
                '\\' => {
                    let after_backslash = match escaped_chars.next() {
                        Some(c) => c,
                        None => {
                            return Err(ParseError {
                                highlighted_span: Some(span),
                                backtrace: self.trace.clone(),
                                error: ParseErrorKind::ExpectingEscapeSeq { literal: src },
                            })
                        }
                    };

                    match after_backslash {
                        '\\' => unescaped.push('\\'),
                        'n' => unescaped.push('\n'),
                        't' => unescaped.push('\t'),
                        'r' => unescaped.push('\r'),
                        other => {
                            return Err(ParseError {
                                highlighted_span: Some(next_tok.span),
                                backtrace: self.trace.clone(),
                                error: ParseErrorKind::UnknownEscapeSeq {
                                    escaped: other,
                                    literal: src,
                                },
                            })
                        }
                    }
                }
                _ => unescaped.push(next),
            }
        }

        Ok(unescaped)
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
                        node: AstNode::BinExpr(Box::new(lhs), operator, Box::new(rhs)),
                    })
                }
                _ => Ok(lhs),
            }
        } else {
            Ok(lhs)
        }
    }

    /// Parse a match expression from the token stream
    fn parse_match(&mut self) -> ParseResult<'src, Ast> {
        self.expect_next_ident(&[TokenData::Ident("match")])?;
        let matched = self.parse_expr()?;
        let start_span = matched.span.from;

        self.expect_next(&[TokenData::OpenBracket(BracketType::Curly)])?;
        let mut cases = vec![];
        let end_span = loop {
            let next = self.peek_tok(&[
                TokenData::CloseBracket(BracketType::Curly),
                TokenData::Ident("typename"),
            ])?;
            match next.data {
                TokenData::CloseBracket(BracketType::Curly) => {
                    let tok = self.toks.next().unwrap();
                    break tok.span.to;
                }
                _ => {
                    let ty = self.parse_typename()?;
                    self.expect_next(&[TokenData::Arrow])?;
                    let stmt = self.parse_stmt()?;
                    cases.push((ty, stmt));
                }
            }
        };

        Ok(Ast {
            span: (start_span, end_span).into(),
            node: AstNode::Match {
                matched: Box::new(matched),
                cases,
            },
        })
    }

    /// Parse an if statement
    fn parse_if(&mut self) -> ParseResult<'src, IfExpr<UnresolvedType>> {
        self.expect_next(&[TokenData::Ident("if")])?;
        self.trace.push("if condition".into());
        let cond = self.parse_expr()?;
        self.trace.pop();

        self.trace.push("if body".into());
        let body = self.parse_body()?;
        self.trace.pop();

        let peek = self.toks.peek();
        if let Some(TokenData::Ident("else")) = peek.map(|tok| &tok.data) {
            self.toks.next();

            let after_else = self.peek_tok(&[TokenData::OpenBracket(BracketType::Curly)])?;
            match after_else.data {
                TokenData::OpenBracket(BracketType::Curly) => {
                    self.trace.push("else body".into());
                    let else_body = self.parse_body()?;
                    self.trace.pop();

                    Ok(IfExpr {
                        cond: Box::new(cond),
                        body: body.0,
                        else_expr: Some(ElseExpr::Else(else_body.0)),
                    })
                }
                _ => Ok(IfExpr {
                    cond: Box::new(cond),
                    body: body.0,
                    else_expr: Some(ElseExpr::ElseIf(Box::new(self.parse_if()?))),
                }),
            }
        } else {
            Ok(IfExpr {
                cond: Box::new(cond),
                body: body.0,
                else_expr: None,
            })
        }
    }

    /// Parse a prefix expression from the token stream
    fn parse_prefix_expr(&mut self) -> ParseResult<'src, Ast> {
        const EXPECTING_NEXT: &[TokenData<'static>] = &[
            TokenData::Ident("variable or function name"),
            TokenData::OpenBracket(BracketType::Smooth),
            TokenData::OpenBracket(BracketType::Curly),
        ];

        let next = self.peek_tok(EXPECTING_NEXT)?.clone();
        let member_of = match next.data {
            TokenData::Ident(_) => {
                self.trace.push("variable or function name".into());
                let name = self.expect_next_path(EXPECTING_NEXT)?;
                Ast {
                    span: next.span,
                    node: AstNode::Access(name),
                }
            }
            TokenData::OpenBracket(BracketType::Curly) => {
                self.trace.push("block expression".into());
                let block = self.parse_body()?;

                Ast {
                    span: block.1,
                    node: AstNode::Block(block.0),
                }
            }
            TokenData::OpenBracket(BracketType::Smooth) => {
                self.toks.next(); //Consume the opening bracket
                self.trace.push("expression in parentheses".into());
                if let Some(TokenData::CloseBracket(BracketType::Smooth)) =
                    self.toks.peek().map(|tok| &tok.data)
                {
                    let close = self.toks.next().unwrap();
                    return Ok(Ast {
                        span: close.span,
                        node: AstNode::Literal(Literal::Unit),
                    });
                }

                let expr = self.parse_expr()?;
                self.expect_next(&[TokenData::CloseBracket(BracketType::Smooth)])?;

                expr
            }
            _ => {
                return Err(ParseError {
                    highlighted_span: Some(next.span),
                    backtrace: self.trace.clone(),
                    error: ParseErrorKind::UnexpectedToken {
                        found: next,
                        expecting: ExpectingOneOf(EXPECTING_NEXT),
                    },
                })
            }
        };

        //Parse any indexing or member access expressions
        let access = self.parse_access(member_of)?;
        self.trace.pop();
        Ok(access)
    }

    /// Recursive function to parse member accesses with the '.' token,
    /// and indexing with the [] array indexing method
    fn parse_access(&mut self, accessing: Ast) -> ParseResult<'src, Ast> {
        const ACCESS_EXPECTING: &[TokenData<'static>] = &[
            TokenData::Period,
            TokenData::OpenBracket(BracketType::Square),
            TokenData::Colon,
        ];

        let peeked = self.peek_tok(ACCESS_EXPECTING)?.clone();
        match peeked.data {
            TokenData::Period => {
                const EXPECTING_AFTER_PERIOD: &[TokenData<'static>] = &[
                    TokenData::Ident("structure field name"),
                    TokenData::OpenBracket(BracketType::Smooth)
                ];

                self.toks.next(); //Eat the period character
                self.trace.push("member access or function call".into());
                let next = self.next_tok(EXPECTING_AFTER_PERIOD)?;
                match next.data {
                    TokenData::OpenBracket(BracketType::Smooth) => {
                        let mut args = vec![];

                        loop {
                            let next_in_args = self.peek_tok(Self::EXPECTED_FOR_EXPRESSION)?;
                            match next_in_args.data {
                                TokenData::Comma => {
                                    self.next_tok(&[TokenData::Comma])?;
                                }
                                TokenData::CloseBracket(BracketType::Smooth) => {
                                    self.next_tok(&[TokenData::CloseBracket(BracketType::Smooth)])?;
                                    break;
                                }
                                _ => {
                                    self.trace.push("function call argument".into());
                                    args.push(self.parse_expr()?);
                                    self.trace.pop();
                                }
                            }
                        }

                        self.trace.pop();

                        Ok(Ast {
                            span: if let Some(last) = args.last() {
                                (peeked.span.from, last.span.to).into()
                            } else {
                                peeked.span
                            },
                            node: AstNode::FunCall(Box::new(accessing), args),
                        })
                    },
                    TokenData::Ident(item) => {
                        self.trace.pop();

                        let symbol = self.symbol(item);
                        self.parse_access(Ast {
                            span: (accessing.span.from, peeked.span.to).into(),
                            node: AstNode::MemberAccess(Box::new(accessing), symbol),
                        })
                    },
                    _ => return Err(ParseError {
                        highlighted_span: Some(next.span),
                        backtrace: self.trace.clone(),
                        error: ParseErrorKind::UnexpectedToken {
                            found: next,
                            expecting: ExpectingOneOf(EXPECTING_AFTER_PERIOD)
                        }
                    })
                }
            }
            TokenData::OpenBracket(BracketType::Square) => {
                self.toks.next();
                self.trace.push("index expression".into());
                let index = self.parse_expr()?;

                self.expect_next(&[TokenData::CloseBracket(BracketType::Square)])?;
                self.trace.pop();

                self.parse_access(Ast {
                    span: (accessing.span.from, peeked.span.to).into(),
                    node: AstNode::Index {
                        object: Box::new(accessing),
                        index: Box::new(index),
                    },
                })
            }
            _ => Ok(accessing),
        }
    }

    /// Parse a full typename from the input stream
    fn parse_typename(&mut self) -> ParseResult<'src, UnresolvedType> {
        let first = self.parse_first_typename()?;
        match self.toks.peek().map(|tok| &tok.data) {
            Some(TokenData::Op(Op::OR)) => {
                let mut variants = vec![first];

                while let Some(TokenData::Op(Op::OR)) = self.toks.peek().map(|tok| &tok.data) {
                    self.toks.next();

                    self.trace.push("enum variant typename".into());
                    let variant_type = self.parse_first_typename()?;
                    self.trace.pop();

                    variants.push(variant_type);
                }

                Ok(UnresolvedType::Enum { variants })
            }
            _ => Ok(first),
        }
    }

    /// Attempt to parse a typename from the token stream
    fn parse_first_typename(&mut self) -> ParseResult<'src, UnresolvedType> {
        const EXPECTING_NEXT: &[TokenData<'static>] = &[
            TokenData::Ident("type name"),
            TokenData::OpenBracket(BracketType::Smooth),
            TokenData::OpenBracket(BracketType::Square),
            TokenData::Op(Op::Star),
        ];

        const EXPECTING_INTEGER: &[TokenData<'static>] = &[
            TokenData::Ident("i8"),
            TokenData::Ident("i16"),
            TokenData::Ident("i32"),
            TokenData::Ident("i64"),
            TokenData::Ident("u8"),
            TokenData::Ident("u16"),
            TokenData::Ident("u32"),
            TokenData::Ident("u64"),
        ];

        let next = self.next_tok(EXPECTING_NEXT)?;

        match next.data {
            TokenData::Ident(name) => match &name[0..1] {
                "i" | "u" => {
                    let signed = &name[0..1] == "i";

                    match &name[1..] {
                        "8" => Ok(UnresolvedType::Integer {
                            signed,
                            width: IntegerWidth::Eight,
                        }),
                        "16" => Ok(UnresolvedType::Integer {
                            signed,
                            width: IntegerWidth::Sixteen,
                        }),
                        "32" => Ok(UnresolvedType::Integer {
                            signed,
                            width: IntegerWidth::ThirtyTwo,
                        }),
                        "64" => Ok(UnresolvedType::Integer {
                            signed,
                            width: IntegerWidth::SixtyFour,
                        }),
                        _ => Err(ParseError {
                            highlighted_span: Some(next.span),
                            backtrace: self.trace.clone(),
                            error: ParseErrorKind::UnexpectedToken {
                                found: next,
                                expecting: ExpectingOneOf(EXPECTING_INTEGER),
                            },
                        }),
                    }
                }
                "f" => match &name[1..] {
                    "32" => Ok(UnresolvedType::Float { doublewide: false }),
                    "64" => Ok(UnresolvedType::Float { doublewide: true }),
                    "un" => {
                        self.trace.push("function typename".into());

                        self.expect_next(&[TokenData::OpenBracket(BracketType::Smooth)])?;
                        let arg_tys = if let Some(TokenData::CloseBracket(BracketType::Smooth)) =
                            self.toks.peek().map(|tok| &tok.data)
                        {
                            self.toks.next(); //Consume the closing brace
                            vec![]
                        } else {
                            let mut args = vec![];
                            loop {
                                args.push(self.parse_typename()?);
                                let next = self.next_tok(&[
                                    TokenData::Comma,
                                    TokenData::CloseBracket(BracketType::Smooth),
                                ])?;
                                match next.data {
                                    TokenData::CloseBracket(BracketType::Smooth) => break args,
                                    TokenData::Comma => continue,
                                    _ => {
                                        return Err(ParseError {
                                            highlighted_span: Some(next.span),
                                            backtrace: self.trace.clone(),
                                            error: ParseErrorKind::UnexpectedToken {
                                                found: next,
                                                expecting: ExpectingOneOf(&[
                                                    TokenData::Comma,
                                                    TokenData::CloseBracket(BracketType::Smooth),
                                                ]),
                                            },
                                        })
                                    }
                                }
                            }
                        };

                        self.expect_next(&[TokenData::Arrow])?;

                        let return_ty = self.parse_typename()?;

                        self.trace.pop();
                        Ok(UnresolvedType::Fun(Box::new(UnresolvedFunType {
                            return_ty,
                            arg_tys,
                        })))
                    }
                    _ => Err(ParseError {
                        highlighted_span: Some(next.span),
                        backtrace: self.trace.clone(),
                        error: ParseErrorKind::UnexpectedToken {
                            found: next,
                            expecting: ExpectingOneOf(&[
                                TokenData::Ident("f32"),
                                TokenData::Ident("f64"),
                            ]),
                        },
                    }),
                },
                "b" if name == "bool" => Ok(UnresolvedType::Bool),
                _ => {
                    self.trace.push("user-defined typename".into());
                    let name = self.symbol(name);
                    let name = self.expect_next_path_with(
                        &[TokenData::Ident("typename path part")],
                        name,
                    )?;
                     
                    let ty = UnresolvedType::UserDefined {
                        name,
                    };
                    self.trace.pop();
                    Ok(ty)
                }
            },
            TokenData::OpenBracket(BracketType::Square) => {
                self.trace.push("array type length".into());
                let len = match self.parse_numliteral()? {
                    NumberLiteral::Integer(bigint, _) => bigint.val,
                    NumberLiteral::Float(floating, _) => floating as u64,
                };

                self.trace.pop();

                let closing = self.next_tok(&[TokenData::CloseBracket(BracketType::Square)])?;
                if let TokenData::CloseBracket(BracketType::Square) = closing.data {
                    self.trace.push("array item typename".into());
                    let item_type = self.parse_typename()?;
                    self.trace.pop();

                    Ok(UnresolvedType::Array {
                        elements: Box::new(item_type),
                        len,
                    })
                } else {
                    Err(ParseError {
                        highlighted_span: Some(closing.span),
                        backtrace: self.trace.clone(),
                        error: ParseErrorKind::UnexpectedToken {
                            found: closing,
                            expecting: ExpectingOneOf(&[TokenData::CloseBracket(
                                BracketType::Square,
                            )]),
                        },
                    })
                }
            }
            TokenData::OpenBracket(BracketType::Curly) => {
                const EXPECTING_FOR_STRUCT: &[TokenData<'static>] = &[
                    TokenData::Ident("field type"),
                    TokenData::CloseBracket(BracketType::Curly),
                    TokenData::OpenBracket(BracketType::Square),
                    TokenData::OpenBracket(BracketType::Smooth),
                    TokenData::Op(Op::Star),
                ];

                self.trace.push("structure typename".into());

                let mut fields = vec![];

                loop {
                    const EXPECTING_AFTER_FIELD: &[TokenData<'static>] = &[
                        TokenData::Comma,
                        TokenData::CloseBracket(BracketType::Curly),
                    ];

                    if let TokenData::CloseBracket(BracketType::Curly) =
                        self.peek_tok(EXPECTING_FOR_STRUCT)?.data
                    {
                        self.toks.next();
                        break;
                    }

                    self.trace.push("struct type field".into());
                    let field_typename = self.parse_typename()?;

                    let field_name =
                        self.expect_next_ident(&[TokenData::Ident("struct field name")])?;
                    self.trace.pop();
                    fields.push((field_typename, self.symbol(field_name)));

                    let next = self.next_tok(EXPECTING_AFTER_FIELD)?;

                    match next.data {
                        TokenData::Comma => (),
                        TokenData::CloseBracket(BracketType::Curly) => break,
                        _ => {
                            return Err(ParseError {
                                highlighted_span: Some(next.span),
                                backtrace: self.trace.clone(),
                                error: ParseErrorKind::UnexpectedToken {
                                    found: next,
                                    expecting: ExpectingOneOf(EXPECTING_AFTER_FIELD),
                                },
                            })
                        }
                    }
                }

                self.trace.pop();

                Ok(UnresolvedType::Struct { fields })
            }
            TokenData::OpenBracket(BracketType::Smooth) => {
                self.trace.push("Type in parentheses".into());
                let peeked = self
                    .peek_tok(&[
                        TokenData::CloseBracket(BracketType::Smooth),
                        TokenData::Ident("typename in parentheses"),
                    ])?
                    .clone();
                let ty = match peeked.data {
                    TokenData::CloseBracket(BracketType::Smooth) => {
                        self.toks.next();
                        self.trace.pop();
                        UnresolvedType::Unit
                    }
                    _ => {
                        let ty = self.parse_typename()?;
                        self.expect_next(&[TokenData::CloseBracket(BracketType::Smooth)])?;
                        ty
                    }
                };

                Ok(ty)
            }
            TokenData::Op(Op::Star) => {
                self.trace.push("pointer type".into());
                let pointed_to = self.parse_typename()?;
                self.trace.pop();

                Ok(UnresolvedType::Pointer(Box::new(pointed_to)))
            }
            _ => Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(EXPECTING_NEXT),
                },
            }),
        }
    }

    /// Parse a number literal from the token stream
    fn parse_numliteral(&mut self) -> ParseResult<'src, NumberLiteral> {
        const EXPECTED_FOR_NUMLITERAL: &[TokenData<'static>] =
            &[TokenData::Number("Number Literal")];
        let next = self.next_tok(EXPECTED_FOR_NUMLITERAL)?;
        if let TokenData::Number(num_str) = next.data {
            let (base, ignore_start) = if num_str.len() > 2 {
                match &num_str[0..2] {
                    "0x" => (16, true),
                    "0b" => (2, true),
                    "0o" => (8, true),
                    _ => (10, false),
                }
            } else {
                (10, false)
            };
            let number = &num_str[if ignore_start { 2 } else { 0 }..];

            let annotation =
                if let Some(TokenData::Ident(ident)) = self.toks.peek().map(|t| &t.data) {
                    match *ident {
                        "u8" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::U8)
                        }
                        "u16" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::U16)
                        }
                        "u32" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::U32)
                        }
                        "u64" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::U64)
                        }

                        "i8" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::I8)
                        }
                        "i16" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::I16)
                        }
                        "i32" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::I32)
                        }
                        "i64" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::I64)
                        }

                        "f32" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::F32)
                        }
                        "f64" => {
                            self.toks.next();
                            Some(NumberLiteralAnnotation::F64)
                        }

                        _ => None,
                    }
                } else {
                    None
                };

            Ok(match u64::from_str_radix(number, base) {
                Ok(val) => NumberLiteral::Integer(BigInt { val, sign: false }, annotation),
                Err(_) => match number.parse::<f64>() {
                    Ok(val) => NumberLiteral::Float(val, annotation),
                    Err(_) => {
                        return Err(ParseError {
                            highlighted_span: Some(next.span),
                            backtrace: self.trace.clone(),
                            error: ParseErrorKind::NumberParse { number: num_str },
                        })
                    }
                },
            })
        } else {
            Err(ParseError {
                highlighted_span: Some(next.span),
                backtrace: self.trace.clone(),
                error: ParseErrorKind::UnexpectedToken {
                    found: next,
                    expecting: ExpectingOneOf(EXPECTED_FOR_NUMLITERAL),
                },
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
    pub backtrace: SmallVec<[Cow<'static, str>; 24]>,
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
    UnexpectedEOF { expecting: ExpectingOneOf },
    /// Failed to parse a number literal
    NumberParse { number: &'src str },
    /// An unknown escape sequence was encountered in a string literal
    UnknownEscapeSeq { escaped: char, literal: &'src str },
    /// A backslash character was encountered with no escaped character
    ExpectingEscapeSeq {
        /// The string that an escape sequence was found in
        literal: &'src str,
    },
}

impl fmt::Display for ParseErrorKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { found, expecting } => writeln!(
                f,
                "Unexpected token {}, expecting {}",
                found.data, expecting
            ),
            Self::UnexpectedEOF { expecting } => {
                writeln!(f, "Unexpected EOF, expecting {}", expecting)
            }
            Self::NumberParse { number } => {
                writeln!(f, "Failed to parse numeric literal {}", number)
            }
            Self::UnknownEscapeSeq { escaped, literal } => writeln!(
                f,
                "Unknown escape sequence '\\{}' in string literal \"{}\"",
                escaped, literal
            ),
            Self::ExpectingEscapeSeq { literal } => {
                writeln!(f, "Expecting an escape sequence in \"{}\"", literal)
            }
        }
    }
}

/// Wrapper over an [ArrayVec] that holds expected token data for the [UnexpectedToken](ParseErrorKind::UnexpectedToken) error
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpectingOneOf(&'static [TokenData<'static>]);

impl fmt::Display for ExpectingOneOf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for expecting in self.0.iter() {
            if !first {
                write!(f, ", ")?;
            }
            first = false;

            expecting.fmt(f)?;

        }

        Ok(())
    }
}
