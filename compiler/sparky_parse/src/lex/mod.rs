use std::{str::CharIndices, iter::Peekable};

use sparky_span::Span;

use crate::tok::{Token, TokenKind, OperatorKind};


/// Lexer that produces a stream of tokens from a file
#[derive(Clone, Debug)]
pub struct Lexer<'src> {
    src: Peekable<CharIndices<'src>>,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer consuming characters from the given source string
    pub fn new(src: CharIndices<'src>) -> Self {
        Self {
            src: src.peekable(),
        }
    }

    pub fn tok(&mut self) -> Option<Token> {
        while self.src.peek()?.1.is_whitespace() {
            self.src.next();
        }
        
        let (start, first) = self.src.next()?;
        let peek = self.src.peek();

        let mut op = |tokkind, end: usize| Token {
            span: Span::new(start as u32, end as u32),
            kind: TokenKind::Op(tokkind)
        };

        Some(match (first, peek) {
            (c, _) if c.is_ascii_alphabetic() || c == '_' => self.identifier((start, first)),
            ('>', Some((end, '='))) => op(OperatorKind::GreaterEq, *end),
            ('<', Some((end, '='))) => op(OperatorKind::LessEq, *end),
            ('=', Some((end, '='))) => op(OperatorKind::Equal, *end),
            ('&', Some((end, '&'))) => op(OperatorKind::LogicalAnd, *end),
            ('|', Some((end, '|'))) => op(OperatorKind::LogicalOr, *end),
            ('>', Some((end, '>'))) => op(OperatorKind::ShRight, *end),
            ('<', Some((end, '<'))) => op(OperatorKind::ShLeft, *end),
        })
    }

    fn identifier(&mut self, first: (usize, char)) -> Token {
        let mut last = first.0;
        while match self.src.peek() {
            Some((idx, c)) if c.is_ascii_alphanumeric() || *c == '_' => true,
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
