// SPDX-License-Identifier: MPL-2.0

//! The text is a representation of Fuyu source code.
//!
//! [`Text`] is optimized for quick look ups of line and column position based on a [`ByteIdx`].

/// An index into a [`Text`] aligned at the byte level.
///
/// Byte indices are used rather than `char` indices for performance.
pub type ByteIdx = usize;

/// A span in the source text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Span {
    /// Inclusive start index.
    pub start: ByteIdx,
    /// Exclusive end index.
    pub end: ByteIdx,
}

/// The text of an expression.
///
/// The text includes a data structure that is used to efficiently convert a byte index into the
/// text into a line and column number pair.
#[derive(Debug, Eq, PartialEq)]
pub struct Text {
    /// The source text.
    text: String,
    /// The line table for the source text.
    line_table: LineTable,
}

impl Text {
    /// Create a new `Text`.
    pub fn new(text: String) -> Self {
        let line_table = LineTable::new(&text);
        Self { text, line_table }
    }

    /// Access the underlying text.
    pub fn as_str(&self) -> &str {
        self.text.as_str()
    }

    /// Convert a index in the text into line and column numbers.
    pub fn line_col(&self, idx: ByteIdx) -> LineCol {
        // In release builds this assertion is not run, but there is no undefied behavior. Indices
        // that are out of range simply saturate and return the last line and column in the text.
        debug_assert!(
            idx < self.text.len(),
            "byte index {idx:?} is out of bounds for text of length {:?}",
            self.text.len()
        );
        let table = self.line_table.indices.as_slice(); // For easier access.
        let line = {
            // Perform a binary search to find the line. A special case is created for the open
            // interval on the upper end since a binary search along cannot find this line.
            let mut lo = 0;
            let mut hi = table.len() - 1;
            let mut md = hi / 2;
            if idx >= table[hi] {
                hi
            } else {
                loop {
                    // The binary search needs to find the inderval that the index belongs, not the
                    // index itself in the list. This makes things slightly more complex in the
                    // found case since it must satisfy a range, not an equality.
                    if table[md] <= idx && idx < table[md + 1] {
                        break md;
                    } else if table[lo] <= idx && idx < table[md] {
                        hi = md;
                    } else {
                        lo = md;
                    }
                    md = (lo + hi) / 2;
                }
            }
        };
        let col = self.text[table[line]..idx].chars().count();
        LineCol { line, col }
    }
}

/// An index used to determine line and column numbers for a source text.
///
/// The source text is indexed by a byte offset from the start for normal processing, however,
/// there are times where reporting information by line and column number is useful. The
/// `LineTable` stores indices to the first byte of each line. This structure allows for efficient
/// lookups of line and column numbers with a worst case complexity of _O(log(n) + m)_ where _n_ is
/// the number lines and _m_ is the length of the longest line.
#[derive(Debug, Eq, PartialEq)]
struct LineTable {
    /// The indices for the first character of each line of the source. This is guaranteed to be in
    /// increasing order.
    indices: Vec<ByteIdx>,
}

impl LineTable {
    /// Construct a [`LineTable`] from a source text.
    fn new(text: &str) -> Self {
        // This is guaranteed to produce a sorted line table so that a binary search can be used.
        let mut indices = vec![0]; // The first line always starts at 0.
        indices.extend(text.char_indices().filter_map(|(byte_offset, c)| match c {
            '\n' => Some(byte_offset + '\n'.len_utf8()),
            _ => None,
        }));
        LineTable { indices }
    }
}

/// A line and column position in a source [`Text`].
///
/// Both the line and column are 0-indexed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LineCol {
    /// The 0-indexed line.
    pub line: usize,
    /// The 0-indexed column.
    pub col: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_new() {
        let text = Text::new("x = 5\nx".into());
        assert_eq!(text.text, "x = 5\nx");
        assert_eq!(text.line_table, LineTable::new("x = 5\nx"))
    }

    #[test]
    fn text_as_str() {
        let text = Text::new("abc".into());
        assert_eq!(text.as_str(), "abc");
    }

    #[test]
    fn text_line_col() {
        // - U+000041 (A): Basic Latin/ASCII require 1 byte in UTF-8.
        // - U+0003B2 (B): Greek and Coptic require 2 bytes in UTF-8.
        // - U+001E8D (ẍ): Latin Extended Additional require 3 bytes in UTF-8.
        // - U+01D50D (𝔍): Mathematical Alphanumeric Symbols require 4 bytes in UTF-8.
        let text = Text::new("\n\u{000041}\r\n\u{0003B2}\u{001E8D}\n\u{01D50D}.".into());
        assert_eq!(text.line_col(0), LineCol { line: 0, col: 0 }); // \n
        assert_eq!(text.line_col(1), LineCol { line: 1, col: 0 }); // U+000041 (1 byte)
        assert_eq!(text.line_col(2), LineCol { line: 1, col: 1 }); // \r
        assert_eq!(text.line_col(3), LineCol { line: 1, col: 2 }); // \n
        assert_eq!(text.line_col(4), LineCol { line: 2, col: 0 }); // U+0003B2 (2 bytes)
        assert_eq!(text.line_col(6), LineCol { line: 2, col: 1 }); // U+001E8D (3 bytes)
        assert_eq!(text.line_col(9), LineCol { line: 2, col: 2 }); // \n
        assert_eq!(text.line_col(10), LineCol { line: 3, col: 0 }); // U+01D50D (4 bytes)
        assert_eq!(text.line_col(14), LineCol { line: 3, col: 1 }); // .
    }

    #[test]
    #[should_panic]
    fn text_line_col_bad_index() {
        // - U+000041 (A): Basic Latin/ASCII require 1 byte in UTF-8.
        // - U+0003B2 (B): Greek and Coptic require 2 bytes in UTF-8.
        // - U+001E8D (ẍ): Latin Extended Additional require 3 bytes in UTF-8.
        // - U+01D50D (𝔍): Mathematical Alphanumeric Symbols require 4 bytes in UTF-8.
        let text = Text::new("\n\u{000041}\r\n\u{0003B2}\u{001E8D}\n\u{01D50D}.".into());
        // 10 and 14 are valid byte indices into this source text. 11 is in the middle of a UTF-8
        // encoding of a codepoint, so it is not valid.
        text.line_col(11);
    }

    #[test]
    fn line_table_new() {
        // Empty text.
        let line_table = LineTable::new("");
        assert_eq!(line_table.indices, vec![0]);
        // Several empty lines.
        let line_table = LineTable::new("\n\n\n");
        assert_eq!(line_table.indices, vec![0, 1, 2, 3]);
        // Non-empty lines.
        let line_table = LineTable::new("a\nbc\ndef\n");
        assert_eq!(line_table.indices, vec![0, 2, 5, 9]);
        // No new line at the end.
        let line_table = LineTable::new("a\nbc\ndef");
        assert_eq!(line_table.indices, vec![0, 2, 5]);
        // With carriage returns.
        let line_table = LineTable::new("a\r\nbc\r\ndef\r\n");
        assert_eq!(line_table.indices, vec![0, 3, 7, 12]);
        // With Unicode.
        // - U+000041 (A): Basic Latin/ASCII require 1 byte in UTF-8.
        // - U+0003B2 (B): Greek and Coptic require 2 bytes in UTF-8.
        // - U+001E8D (ẍ): Latin Extended Additional require 3 bytes in UTF-8.
        // - U+01D50D (𝔍): Mathematical Alphanumeric Symbols require 4 bytes in UTF-8.
        let line_table = LineTable::new("\n\u{0041}\n\u{003B2}\n\u{001E8D}\n\u{01D50D}\n");
        assert_eq!(line_table.indices, vec![0, 1, 3, 6, 10, 15]);
    }
}
