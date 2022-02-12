use std::{iter::Peekable, str::CharIndices};

use crate::util::loc::Span;

use super::token::{BracketType, Op, Token, TokenData};

/// Lexer responsible for tokenizing an input string to be parsed
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    /// A reference to the original source string
    src: &'src str,
    /// An iterator over the UTF-8 codepoints and their indices in the source string
    chars: Peekable<CharIndices<'src>>,
    /// The next token to be returned when `next` or `peek` is called
    current: Option<Token<'src>>,
    /// The token after `current`, returned with `peek2`
    peek2: Option<Token<'src>>,
}

impl<'src> Lexer<'src> {
    /// Create a new `Lexer` to tokenize the given source string
    pub fn new(src: &'src str) -> Self {
        let mut this = Self {
            chars: src.char_indices().peekable(),
            src,
            current: None,
            peek2: None,
        };
        this.current = this.token();
        this.peek2 = this.token();
        this
    }

    /// Consume one character from the character iterator if one exists,
    /// incrementing line numbers if the character is a newline
    fn next_char(&mut self) -> Option<(usize, char)> {
        let next = self.chars.next();
        next
    }

    /// Lex a new token if present from the source text
    fn token(&mut self) -> Option<Token<'src>> {
        //Skip whitespace
        while self
            .chars
            .peek()
            .map(|(_, w)| w.is_whitespace())
            .unwrap_or(false)
        {
            self.next_char()?;
        }

        let (startpos, next) = self.next_char()?;
        let start_loc = Span::single(startpos);

        Some(match next {
            '+' => Token::new(start_loc, TokenData::Op(Op::Add)),

            '*' => Token::new(start_loc, TokenData::Op(Op::Star)),
            '/' => Token::new(start_loc, TokenData::Op(Op::Div)),
            '%' => Token::new(start_loc, TokenData::Op(Op::Mod)),
            '!' => Token::new(start_loc, TokenData::Op(Op::LogicalNot)),
            '~' => Token::new(start_loc, TokenData::Op(Op::NOT)),
            '^' => Token::new(start_loc, TokenData::Op(Op::XOR)),
            '=' => Token::new(start_loc, TokenData::Assign),
            '$' => Token::new(start_loc, TokenData::Dollar),

            '.' => Token::new(start_loc, TokenData::Period),
            ',' => Token::new(start_loc, TokenData::Comma),

            // Multi or single character tokens
            '&' | '|' | '>' | '<' | ':' | '-' => {
                let peek = self.chars.peek().map(|(_, peek)| *peek);
                match (next, peek) {
                    ('>', Some('=')) => {
                        self.next_char();
                        Token::new(startpos..startpos + 1, TokenData::Op(Op::GreaterEq))
                    }
                    ('<', Some('=')) => {
                        self.next_char();
                        Token::new(startpos..startpos + 1, TokenData::Op(Op::LessEq))
                    }
                    ('&', Some('&')) => {
                        self.next_char();
                        Token::new(startpos..startpos + 1, TokenData::Op(Op::LogicalAnd))
                    }
                    ('|', Some('|')) => {
                        self.next_char();
                        Token::new(startpos..startpos + 1, TokenData::Op(Op::LogicalOr))
                    }

                    ('-', Some('>')) => {
                        self.next_char();
                        Token::new(startpos..startpos + 1, TokenData::Arrow)
                    }

                    ('=', Some('=')) => {
                        self.next_char();
                        Token::new(startpos..startpos + 1, TokenData::Op(Op::Eq))
                    }
                    (':', _) => Token::new(start_loc, TokenData::Colon),

                    ('<', Some('<')) => {
                        self.next_char();
                        Token::new(start_loc, TokenData::Op(Op::ShLeft))
                    }
                    ('>', Some('>')) => {
                        self.next_char();
                        Token::new(start_loc, TokenData::Op(Op::ShRight))
                    }

                    ('&', _) => Token::new(start_loc, TokenData::Op(Op::AND)),
                    ('|', _) => Token::new(start_loc, TokenData::Op(Op::OR)),
                    ('<', _) => Token::new(start_loc, TokenData::Op(Op::Less)),
                    ('>', _) => Token::new(start_loc, TokenData::Op(Op::Greater)),
                    ('-', _) => Token::new(start_loc, TokenData::Op(Op::Sub)),

                    (next, peek) => unreachable!(
                        "Not possible, checked all options of next, next is {}, peek is {:?}",
                        next, peek
                    ),
                }
            }

            '{' | '(' | '[' => Token::new(
                start_loc,
                TokenData::OpenBracket(match next {
                    '{' => BracketType::Curly,
                    '(' => BracketType::Smooth,
                    '[' => BracketType::Square,
                    _ => unreachable!(),
                }),
            ),
            '}' | ')' | ']' => Token::new(
                start_loc,
                TokenData::CloseBracket(match next {
                    '}' => BracketType::Curly,
                    ')' => BracketType::Smooth,
                    ']' => BracketType::Square,
                    _ => unreachable!(),
                }),
            ),

            //Character literal
            '\'' => {
                let (firstpos, first) = self.next_char()?;
                if first == '\\' {
                    self.next_char()?; //Consume the escaped character
                }

                if let (end, '\'') = self.next_char()? {
                    Token::new(startpos..end, TokenData::Char(&self.src[firstpos..end - 1]))
                } else {
                    return None;
                }
            }

            //String literal
            '"' => {
                let endpos = loop {
                    match self.next_char()? {
                        (_, '\\') => {
                            self.next_char()?;
                        }
                        (endpos, '"') => break endpos,
                        _ => (),
                    }
                };
                Token::new(
                    startpos..endpos,
                    TokenData::String(&self.src[startpos + 1..endpos]),
                )
            }

            n if n.is_digit(10) => {
                let digit = n.to_digit(10).unwrap();
                let radix = if digit == 0 {
                    match self.chars.peek() {
                        Some((_, 'x')) => {
                            self.next_char();
                            16
                        }
                        Some((_, 'o')) => {
                            self.next_char();
                            8
                        }
                        Some((_, 'b')) => {
                            self.next_char();
                            2
                        }
                        Some((_, n)) if n.is_digit(10) => 10,
                        _ => {
                            return Some(Token::new(
                                start_loc,
                                TokenData::Number(&self.src[startpos..startpos + 1]),
                            ))
                        }
                    }
                } else {
                    10
                };

                let mut endpos = startpos;

                loop {
                    match self.chars.peek() {
                        Some((_, digit)) if digit.is_digit(radix) || *digit == '.' => {
                            self.next_char();
                        }
                        Some((_, 'e')) => {
                            self.next_char();
                            self.next_char(); //Skip + / -
                            while match self.chars.peek() {
                                Some((_, exp)) if exp.is_digit(10) => true,
                                _ => false,
                            } {
                                self.next_char();
                            }
                        }
                        Some((endnum, _)) => {
                            endpos = *endnum;
                            break;
                        }
                        None => break,
                    }
                }

                Token::new(
                    startpos..endpos,
                    TokenData::Number(&self.src[startpos..endpos]),
                )
            }

            other if other.is_ascii_alphabetic() || other == '_' => {
                let mut endpos = startpos;
                loop {
                    if let Some((peeked_pos, peeked)) = self.chars.peek() {
                        if peeked.is_ascii_alphanumeric() || *peeked == '_' {
                            endpos = *peeked_pos;
                            self.next_char();
                            continue;
                        }
                    }
                    break;
                }

                Token::new(
                    startpos..endpos,
                    TokenData::Ident(&self.src[startpos..=endpos]),
                )
            }

            _ => self.token()?,
        })
    }

    /// Peek the current token
    pub fn peek(&self) -> Option<&Token<'src>> {
        self.current.as_ref()
    }

    /// Peek the token two spots ahead
    pub fn peek2(&self) -> Option<&Token<'src>> {
        self.peek2.as_ref()
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        use std::mem::replace;
        if self.current.is_none() {
            return None;
        }

        let next = if self.peek2.is_some() {
            let tok = self.token();
            let next = replace(&mut self.current, replace(&mut self.peek2, tok));
            next?
        } else {
            self.current.take()?
        };

        Some(next)
    }
}
