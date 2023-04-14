use std::{str::CharIndices, iter::Peekable};

use sparky_span::Span;

use crate::tok::{Token, TokenKind, OperatorKind, BraceKind, NumLitPrefix};


/// Lexer that produces a stream of tokens from a file
#[derive(Clone, Debug)]
pub(crate) struct Lexer<'src> {
    src: Peekable<CharIndices<'src>>,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer consuming characters from the given source string
    pub fn new(src: CharIndices<'src>) -> Self {
        Self {
            src: src.peekable(),
        }
    }
    
    /// Lex a single token from the character stream
    pub fn tok(&mut self) -> Option<Token> {
        while self.src.peek()?.1.is_whitespace() {
            self.src.next();
        }
        
        let (start, first) = self.src.next()?;
        let peek = self.src.peek().copied();

        let tok = move |kind| Token {
            span: Span::new(start as u32, (start + 1) as u32),
            kind,
        };

        let mut tok2 = |kind| {
            let (pos, _) = self.src.next().unwrap();
            Token {
                span: Span::new(start as u32, (pos + 1) as u32),
                kind,
            }
        };

        Some(match (first, peek) {
            (c, _) if c.is_ascii_alphabetic() || c == '_' => self.identifier((start, first)),
            ('>', Some((_, '='))) => tok2(TokenKind::Op(OperatorKind::GreaterEq)),
            ('<', Some((_, '='))) => tok2(TokenKind::Op(OperatorKind::LessEq)),
            ('=', Some((_, '='))) => tok2(TokenKind::Op(OperatorKind::Equal)),
            ('&', Some((_, '&'))) => tok2(TokenKind::Op(OperatorKind::LogicalAnd)),
            ('|', Some((_, '|'))) => tok2(TokenKind::Op(OperatorKind::LogicalOr)),
            ('>', Some((_, '>'))) => tok2(TokenKind::Op(OperatorKind::ShRight)),
            ('<', Some((_, '<'))) => tok2(TokenKind::Op(OperatorKind::ShLeft)),

            ('0', Some((_, 'b'))) => self.numerals(Some(NumLitPrefix::Bin), start), 
            ('0', Some((_, 'x'))) => self.numerals(Some(NumLitPrefix::Hex), start),
            ('0', Some((_, 'o'))) => self.numerals(Some(NumLitPrefix::Oct), start),
            (c, _) if c.is_numeric() => self.numerals(None, start),

            ('-', Some((_, '>'))) => tok2(TokenKind::Arrow),

            ('>', _) => tok(TokenKind::Op(OperatorKind::GreaterThan)),
            ('<', _) => tok(TokenKind::Op(OperatorKind::LessThan)),
            ('&', _) => tok(TokenKind::Op(OperatorKind::BitAnd)),
            ('|', _) => tok(TokenKind::Op(OperatorKind::BitOr)),
            ('^', _) => tok(TokenKind::Op(OperatorKind::BitXor)),
            ('~', _) => tok(TokenKind::Op(OperatorKind::BitNot)),
            ('!', _) => tok(TokenKind::Op(OperatorKind::LogicalNot)),
            ('*', _) => tok(TokenKind::Op(OperatorKind::Mul)),
            ('%', _) => tok(TokenKind::Op(OperatorKind::Mod)),
            ('+', _) => tok(TokenKind::Op(OperatorKind::Add)),
            ('-', _) => tok(TokenKind::Op(OperatorKind::Sub)),

            ('.', _) => tok(TokenKind::Dot),
            (',', _) => tok(TokenKind::Comma),
            ('=', _) => tok(TokenKind::Assign),
            (';', _) => tok(TokenKind::Semicolon),

            ('{', _) => tok(TokenKind::OpenBrace(BraceKind::Curly)),
            ('(', _) => tok(TokenKind::OpenBrace(BraceKind::Smooth)),
            ('[', _) => tok(TokenKind::OpenBrace(BraceKind::Square)),
            ('}', _) => tok(TokenKind::CloseBrace(BraceKind::Curly)),
            (']', _) => tok(TokenKind::CloseBrace(BraceKind::Square)),
            (')', _) => tok(TokenKind::CloseBrace(BraceKind::Smooth)),

            _ => unimplemented!("Do something with unexpected lexer characters"),
        })
    }
    
    /// Get a number literal with optional prefix
    fn numerals(&mut self, prefix: Option<NumLitPrefix>, start: usize) -> Token {
        let radix = prefix.map(|p| p.radix()).unwrap_or(10);
        
        let start_digits = self.src.peek().map(|(i,_)| *i).unwrap_or(start);
        let mut end = start;

        while self.src.peek().map(move |(_,c)| c.is_digit(radix)).unwrap_or(false) {
            end = self.src.next().unwrap().0;
        }

        Token {
            span: Span::new(start as u32, start_digits as u32),
            kind: TokenKind::Number {
                prefix,
                numerals: Span::new(start_digits as u32, end as u32 + 1)
            }
        }
    }

    fn identifier(&mut self, first: (usize, char)) -> Token {
        let mut last = first.0;
        while match self.src.peek() {
            Some((idx, c)) if c.is_ascii_alphanumeric() || *c == '_' => {
                last = *idx;
                true
            },
            _ => false,
        } {
            self.src.next();
        }

        let span = Span::new(first.0 as u32, last as u32);

        Token {
            span,
            kind: TokenKind::Ident(span)
        }
    }
} 
