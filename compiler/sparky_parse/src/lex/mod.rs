use std::{str::CharIndices, iter::Peekable};

use sparky_span::Span;

use crate::tok::{Token, TokenKind, OperatorKind, BraceKind, NumLitPrefix};


/// Lexer that produces a stream of tokens from a file
#[derive(Clone, Debug)]
pub(crate) struct Lexer<'src> {
    src: Peekable<CharIndices<'src>>,
    current_idx: usize,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer consuming characters from the given source string
    pub fn new(src: CharIndices<'src>) -> Self {
        Self {
            src: src.peekable(),
            current_idx: 0,
        }
    }
    
    /// Consume a character from the input stream
    fn nextchar(&mut self) -> Option<(usize, char)> {
        match self.src.next() {
            Some(v) => {
                self.current_idx += 1;
                Some(v)
            },
            None => None,
        }
    }
    
    #[inline]
    fn peekchar(&mut self) -> Option<(usize, char)> { self.src.peek().copied() }
    
    /// Lex a single token from the character stream
    pub fn tok(&mut self) -> Option<Token> {
        self.take_while(|(_, c)| c.is_whitespace()); 
        
        let (start, first) = self.nextchar()?;
        let peek = self.peekchar();

        let tok = move |kind| Token {
            span: Span::new(start as u32, (start + 1) as u32),
            kind,
        };

        let mut tok2 = |kind| {
            let (pos, _) = self.nextchar().unwrap();
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

            ('0', Some((_, 'b'))) => { self.next(); self.numerals(Some(NumLitPrefix::Bin), start) }, 
            ('0', Some((_, 'x'))) => { self.next(); self.numerals(Some(NumLitPrefix::Hex), start) },
            ('0', Some((_, 'o'))) => { self.next(); self.numerals(Some(NumLitPrefix::Oct), start) },
            (c, _) if c.is_numeric() => { self.next(); self.numerals(None, start) },

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

            (c @ '"', _) => self.string((start, c)),
            ('\'', _) => {
                let ch = self.escaped_char();
                let end = if self.peekchar().map(|(_, c)| c == '\'').unwrap_or(false) {
                    self.nextchar();
                    self.current_idx
                } else {
                    self.current_idx
                };

                Token {
                    span: Span::new(start as u32, end as u32),
                    kind: TokenKind::CharLiteral(ch),
                }
            }

            _ => unimplemented!("Do something with unexpected lexer characters"),
        })
    }
    
    /// Consume characters from the stream while the given condition is satisfied, returning the
    /// span that matching characters occupy
    fn take_while<F: Fn((usize, char)) -> bool>(&mut self, f: F) -> Span {
        let start = self.current_idx;
        while self.peekchar().map(&f).unwrap_or(false) {
            self.nextchar();
        }
        
        Span::new(start as u32, self.current_idx as u32)
    }
    
    /// Consume a string with escaped characters from the character stream
    fn string(&mut self, quotes: (usize, char)) -> Token {
        let mut str_span = None;
        loop {
            match self.peekchar() {
                Some((_, '"')) => break,
                Some(_) => if let Some(span) = self.escaped_char() {
                    str_span = Some(
                        str_span.map_or(
                            span,
                            |s: Span| Span::new(s.begin(), span.end())
                        )
                    );
                } else {
                    break
                },
                None => break,
            }
        }

        Token {
            span: Span::new(quotes.0 as u32, self.current_idx as u32),
            kind: TokenKind::StringLiteral(str_span),
        }
    }

    /// Consume a single character, or an escape sequence beginning with a '\' character
    fn escaped_char(&mut self) -> Option<Span> {
        let (start, next) = self.nextchar()?;
        let end = if next == '\\' {
            self.nextchar().map(|(i,_)| i).unwrap_or(start)
        } else {
            start
        };

        Some(Span::new(start as u32, end as u32))
    }

    /// Get a number literal with optional prefix
    fn numerals(&mut self, prefix: Option<NumLitPrefix>, start: usize) -> Token {
        let radix = prefix
            .map(|p| p.radix())
            .unwrap_or(10);

        let start_digits = self.current_idx;
        let numerals = self.take_while(move |(_, c)| c.is_digit(radix));

        Token {
            span: Span::new(start as u32, numerals.end() as u32),
            kind: TokenKind::Number {
                prefix,
                numerals,
            }
        }
    }
    
    /// Consume identifier characters from the stream
    fn identifier(&mut self, first: (usize, char)) -> Token {
        let last = self.take_while(|(_, c)| c.is_ascii_alphanumeric() || c == '_').end();
        let span = Span::new(first.0 as u32, last as u32);

        Token {
            span,
            kind: TokenKind::Ident(span)
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> { self.tok() }
}

#[cfg(test)]
mod test {
    use super::*;

    const SRC: &str = 
r#"
ident_1 0x10

 { | }
"#;
    
    #[test]
    fn test_lexer() {
        let lexer = Lexer::new(SRC.char_indices());
        let toks = lexer.collect::<Vec<_>>();
       
        assert_eq!(&SRC[toks[0].span], "ident_1");
        assert!(matches!(toks[0].kind, TokenKind::Ident(_)), "First token must be an identifier");

        assert_eq!(&SRC[toks[1].span], "0x10");
        assert!(
            matches!(
                toks[1].kind,
                TokenKind::Number { prefix: Some(NumLitPrefix::Hex), numerals: _ }
            ),
            "Second token must be a number literal with hex prefix"
        );
        
        assert_eq!(&SRC[toks[2].span], "{");
        assert!(matches!(toks[2].kind, TokenKind::OpenBrace(BraceKind::Curly)));
        
        assert_eq!(&SRC[toks[3].span], "|");
        assert!(matches!(toks[3].kind, TokenKind::Op(OperatorKind::BitOr)));
        
        assert_eq!(&SRC[toks[4].span], "}");
        assert!(matches!(toks[4].kind, TokenKind::CloseBrace(BraceKind::Curly)));
    }
}
