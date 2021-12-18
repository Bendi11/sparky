use std::{iter::Peekable, str::CharIndices};

use crate::util::loc::Loc;

use super::token::{BracketType, Op, Token, TokenData};

/// Lexer responsible for tokenizing an input string to be parsed
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    /// A reference to the original source string
    src: &'src str,
    /// An iterator over the UTF-8 codepoints and their indices in the source string
    chars: Peekable<CharIndices<'src>>,
    /// The current line of the lexer
    line: u16,
    /// Current column number of the lexer's cursor
    col: u16,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            chars: src.char_indices().peekable(),
            src,
            line: 1,
            col: 0,
        }
    }

    /// Consume one character from the character iterator if one exists,
    /// incrementing line numbers if the character is a newline
    fn next_char(&mut self) -> Option<(usize, char)> {
        let next = self.chars.next();
        self.col += 1;
        if let Some((_, '\n')) = next {
            self.line += 1;
            self.col = 0;
        }
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
        let start_loc = (self.line, self.col).into();
        Some(match next {
            '+' => Token::new(start_loc, TokenData::Op(Op::Add)),

            '*' => Token::new(start_loc, TokenData::Op(Op::Star)),
            '/' => Token::new(start_loc, TokenData::Op(Op::Div)),
            '%' => Token::new(start_loc, TokenData::Op(Op::Mod)),
            '!' => Token::new(start_loc, TokenData::Op(Op::LogicalNot)),
            '~' => Token::new(start_loc, TokenData::Op(Op::NOT)),
            '^' => Token::new(start_loc, TokenData::Op(Op::XOR)),
            '=' => Token::new(start_loc, TokenData::Op(Op::Eq)),
            '$' => Token::new(start_loc, TokenData::Dollar),

            '.' => Token::new(start_loc, TokenData::Period),
            ',' => Token::new(start_loc, TokenData::Comma),

            // Multi or single character tokens
            '&' | '|' | '>' | '<' | ':' | '-' => {
                let peek = self.chars.peek().map(|(_, peek)| *peek);
                match (next, peek) {
                    ('>', Some('=')) => {
                        self.next_char();
                        Token::new(
                            (start_loc, (self.line, self.col).into()),
                            TokenData::Op(Op::GreaterEq),
                        )
                    }
                    ('<', Some('=')) => {
                        self.next_char();
                        Token::new(
                            (start_loc, (self.line, self.col).into()),
                            TokenData::Op(Op::LessEq),
                        )
                    }
                    ('&', Some('&')) => {
                        self.next_char();
                        Token::new(
                            (start_loc, (self.line, self.col).into()),
                            TokenData::Op(Op::LogicalAnd),
                        )
                    }
                    ('|', Some('|')) => {
                        self.next_char();
                        Token::new(
                            (start_loc, (self.line, self.col).into()),
                            TokenData::Op(Op::LogicalOr),
                        )
                    }

                    ('-', Some('>')) => {
                        self.next_char();
                        Token::new((start_loc, (self.line, self.col).into()), TokenData::Arrow)
                    }

                    (':', Some('=')) => {
                        self.next_char();
                        Token::new((start_loc, (self.line, self.col).into()), TokenData::Assign)
                    }
                    (':', _) => Token::new(start_loc, TokenData::Colon),

                    ('<', Some('<')) => Token::new(start_loc, TokenData::Op(Op::ShLeft)),
                    ('>', Some('>')) => Token::new(start_loc, TokenData::Op(Op::ShRight)),

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
                    Token::new(
                        (start_loc, (self.line, self.col).into()),
                        TokenData::Char(&self.src[firstpos..end - 1]),
                    )
                } else {
                    panic!("")
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
                    (start_loc, (self.line, self.col).into()),
                    TokenData::String(&self.src[startpos + 1..endpos - 1]),
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
                        },
                        Some((_, 'e')) => {
                            self.next_char();
                            self.next_char(); //Skip + / -
                            while match self.chars.peek() {
                                Some((_, exp)) if exp.is_digit(10) => true,
                                _ => false
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
                    (start_loc, (self.line, self.col).into()),
                    TokenData::Number(&self.src[startpos..endpos]),
                )
            }

            other if other.is_ascii_alphabetic() || other == '_' || other == ':' => {
                let mut endpos = startpos;
                loop {
                    if let Some((peeked_pos, peeked)) = self.chars.peek() {
                        if peeked.is_ascii_alphanumeric() || *peeked == '_' || *peeked == ':' {
                            endpos = *peeked_pos;
                            self.next_char();
                            continue;
                        }
                    }
                    break;
                }

                Token::new(
                    (start_loc, (self.line, self.col).into()),
                    TokenData::Ident(&self.src[startpos..=endpos]),
                )
            }

            _ => self.token()?,
        })
    }

    /// Return the current position of the lexer in the source file
    pub fn pos(&self) -> Loc {
        (self.line, self.col).into()
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        self.token()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use crate::util::files::CompiledFile;

    use super::*;

    const TEST_SRC: &str = r#"

fun main i32 argc, []*char argv -> i32 {
    let name := argv[0b0011010102]
}

"#;

    #[test]
    pub fn test_lex() {
        let file = CompiledFile::in_memory(TEST_SRC.to_owned());
        let toks = Lexer::new(TEST_SRC);
        for tok in toks {
            std::io::stdout()
                .write_fmt(format_args!("TOKEN: {:?}\n", tok))
                .unwrap();
            tok.display(&file).unwrap();
            std::io::stdout()
                .write_fmt(format_args!("\n----------\n"))
                .unwrap();
        }
    }
}
