use crate::Span;

/// Structure mapping line numbers to character offsets in a string and vice versa
///
/// **NOTE**: Line numbers are 1 based, not 0
#[derive(Clone, Debug)]
pub struct LineOffsets {
    lines: Box<[Span]>,
}

impl LineOffsets {
    /// Read line indices from an iterator over characters and indices like the
    /// [std::str::CharIndices] iterator
    pub fn read<I: Iterator<Item = (usize, char)>>(iter: I) -> Self {
        #[repr(transparent)]
        struct LineSpanIter<I>(I);

        impl<I: Iterator<Item = (usize, char)>> Iterator for LineSpanIter<I> {
            type Item = Span;

            fn next(&mut self) -> Option<Self::Item> {
                let (start_idx, _) = self.0.next()?;
                let len = (&mut self.0).take_while(|(_, c)| *c != '\n').count();
                Some(Span::from_start_len(start_idx as u32, (len + 1) as u32))
            }
        }

        Self {
            lines: LineSpanIter(iter).collect(),
        }
    }

    /// Get the total number of lines
    pub const fn lines(&self) -> u32 {
        self.lines.len() as u32
    }

    /// Get the span a given line occupies, with a leading newline and no trailing newline
    pub fn try_line_span(&self, line: u32) -> Option<Span> {
        let line = line.saturating_sub(1);
        self.lines.get(line as usize).copied()
    }

    /// Get the span for a given line, `panic`ing if the line is greater than the number of lines
    /// in this collection
    pub fn line_span(&self, line: u32) -> Span {
        self.try_line_span(line)
            .expect("Attempt to get the span of a line that doesn't exist")
    }

    /// Get the line number that the given character index is located on
    pub fn line_of_index(&self, idx: u32) -> u32 {
        self.lines
            .iter()
            .enumerate()
            .find_map(move |(line, off)|
                off.contains(idx).then_some((line as u32).saturating_sub(1) + 1)
            )
            .unwrap_or(self.lines())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const L1: &str = "Line No.1";
    const L2: &str = "Line 2";
    const L3: &str = "Last line";

    #[test]
    fn test_line_map() {
        let text = format!("{}\n{}\n{}", L1, L2, L3);
        let lines = LineOffsets::read(text.char_indices());
        let first = lines.line_span(1);

        eprintln!("{:#?}", lines);

        assert_eq!(&text.as_str()[lines.line_span(1)], L1);
        assert_eq!(&text.as_str()[lines.line_span(2)], L2);
        assert_eq!(&text.as_str()[lines.line_span(3)], L3);
    }
}
