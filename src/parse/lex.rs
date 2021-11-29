use std::str::CharIndices;

use crate::util::files::FileId;


/// Lexer responsible for tokenizing an input string to be parsed
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: &'src str,
    chars: CharIndices<'src>,
    line: u32,
    col: u32,
    file: FileId,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str, file: FileId) -> Self {
        Self {
            chars: src.char_indices(),
            src,
            line: 1,
            col: 0,
            file
        }
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        let next = self.chars.next();
        self.col += 1;
        if let Some((_, '\n')) = next {
            self.line += 1;
            self.col = 0;
        }
        next
    }

    fn token(&mut self) -> Option<Token<'src>> {
        
    }
}