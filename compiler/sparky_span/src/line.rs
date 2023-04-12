
/// Structure mapping line numbers to character offsets in a string and vice versa
///
/// **NOTE**: Line numbers are 1 based, not 0
#[derive(Clone, Debug)]
pub struct LineOffsets {
    offsets: Box<[usize]>,
}

impl LineOffsets {
    /// Read line indices from an iterator over characters and indices like the
    /// [std::str::CharIndices] iterator
    pub fn read<I: Iterator<Item = (usize, char)>>(iter: I) -> Self {
        Self {
            offsets: iter
                .filter_map(|(idx, c)| (c == '\n').then_some(idx))
                .collect()
        }
    }
    
    /// Get the character offset of the given line number- offset is the index of the newline
    /// character
    #[inline]
    pub fn line_offset(&self, line: u32) -> Option<usize> {
        self.offsets.get(line.saturating_sub(1) as usize).copied()
    }
    
    /// Get the line number that the given character index is located on
    pub fn line_of_index(&self, idx: usize) -> u32 {
        self
            .offsets
            .iter()
            .enumerate()
            .find_map(move |(line, off)| 
                (*off >= idx)
                    .then_some((line as u32).saturating_sub(1) + 1)
            )
            .unwrap_or(self.offsets.len() as u32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_map() {
        
    }
}
