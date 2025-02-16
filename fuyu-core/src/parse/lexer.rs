// SPDX-License-Identifier: MPL-2.0

use crate::parse::{ByteIdx, Text, Token};
use std::iter;
use std::str::Chars;

/// The Unicode byte order mark (BOM).
const BOM: char = '\u{feff}';

/// The lexer looks ahead by 3 tokens.
const LOOKAHEAD: usize = 3;

/// The `Lexer` produces an iterator of [`Spanned`] [`Token`]s from a source [`Text`].
#[derive(Debug)]
pub struct Lexer<'a> {
    /// The source text.
    text: &'a Text,
    /// An iterator over the characters in the source text.
    chars: Chars<'a>,
    /// The lexer works based on a window of [`LOOKAHEAD`] characters.
    ///
    /// The first character in the window (i.e., `window[0]`) is considered the next character in the source.
    window: [Option<char>; LOOKAHEAD],
    /// The current position of the iterator.
    idx: ByteIdx,
    /// The marked start index (inclusive) that tracks the start of a marked region (e.g., token in
    /// a lexer).
    ///
    /// It is guaranteed that `idx >= mark_idx`.
    mark_idx: ByteIdx,
}

/// The values that are produced by the [`Lexer`].
///
/// The `Ok(...)` variant represents a valid token. In order, the fields of the tuple are in the
/// inclusive start index, the token kind, and the exclusive end index.
///
/// The `Err(...)` variant represents a lexing error. In order, the fields of the tuple are in the
/// inclusive start index, the error kind, and the exclusive end index.
pub type Spanned = Result<(ByteIdx, Token, ByteIdx), (ByteIdx, LexicalError, ByteIdx)>;

/// The kind of an error that can be reported by the lexer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LexicalError {
    /// A character that cannot start a token.
    Char,

    /// A raw string where the opening `"` was not seen after the leading `#`.
    StringStart,

    /// A raw string where there are too many closing `"` or `#` at the end.
    StringEnd,

    /// A binary, octal, or hexadecimal literal with an illegally cased base marker.
    ///
    /// The base markers `0b`, `0o` and `0x` must be lowercase. Uppercase base markers (i.e, `0B`,
    /// `0O`, and `0X`) are illegal.
    BaseMarker,

    /// A incorrectly formed numeric literal.
    ///
    /// This reports incorrect `_` usage and illegal characters, such as disallowed letters, that
    /// appear in a numeric literal.
    Number,

    /// A string that is not properly closed.
    UnclosedString,

    /// An identifier with illegal characters.
    Ident,
}

/// Checks if a character is text-like and can end a text-like token.
///
/// A text-like token is an identifier, keyword, or number.
fn is_text_like(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// A shorthand to create a closure that accepts a single value and matches it against a pattern.
macro_rules! pat {
    ($p:pat) => {
        |value| matches!(value, $p)
    };
}

/// Shorthand for scanning a binary, octal, or hexadecimal integer. The lexer is an instance of
/// [`Lexer`] and the `pattern` is the characters to include in the literal. A [`Token`] of `kind`
/// is emitted. Note that the pattern should not include `_`.
macro_rules! scan_int {
    ($lexer:expr, $pattern:pat, $kind:expr) => {{
        $lexer.advance_by(2); // Consume base marker (e.g, `0b`, `0o`, `0x`).
        if !$lexer.advance_if(pat!($pattern)) {
            $lexer.advance_while(is_text_like);
            return $lexer.emit_error(LexicalError::Number);
        }
        $lexer.advance_while(pat!($pattern | '_'));
        if !$lexer.window[0].map_or(false, is_text_like) {
            $lexer.emit($kind)
        } else {
            $lexer.advance_while(is_text_like);
            $lexer.emit_error(LexicalError::Number)
        }
    }};
}

impl<'a> Lexer<'a> {
    /// Create a new lexer from a source text.
    pub fn new(text: &'a Text) -> Self {
        let mut lexer = Self {
            text,
            chars: text.as_str().chars(),
            window: [None; LOOKAHEAD],
            idx: 0,
            mark_idx: 0,
        };
        // Fill the window.
        for i in 0..LOOKAHEAD {
            lexer.window[i] = lexer.chars.next();
        }
        // The BOM is skipped so nothing else has to deal with it.
        lexer.advance_if(pat!(BOM));
        lexer.mark();
        lexer
    }

    /// Set the mark index to the current index.
    fn mark(&mut self) {
        self.mark_idx = self.idx;
    }

    /// Get the string from the mark index (inclusive) to the current index (exclusive).
    fn as_marked_str(&self) -> &str {
        &self.text.as_str()[self.mark_idx..self.idx]
    }

    /// Move the lexer ahead by one character.
    fn advance(&mut self) {
        // Update the position.
        if let Some(c) = self.window[0] {
            self.idx += c.len_utf8();
        }
        // Shift the next character into the window.
        self.window.rotate_left(1);
        self.window[LOOKAHEAD - 1] = self.chars.next();
    }

    /// Call [`Self::advance()`] `n` times.
    fn advance_by(&mut self, n: usize) {
        // There is nothing wrong with advancing zero times, however, it is likely a bug as it
        // would be a noop.
        debug_assert!(n > 0, "n must be non-zero");
        for _ in 0..n {
            self.advance();
        }
    }

    /// Perform [`Lexer::advance()`] if the next character satisfies a predicate.
    ///
    /// Returns `true` when the predicate is satisfied.
    fn advance_if<P>(&mut self, pred: P) -> bool
    where
        P: Fn(char) -> bool,
    {
        self.window[0]
            .is_some_and(pred)
            .then(|| self.advance())
            .is_some()
    }

    /// Advance while characters satisfy a predicate.
    ///
    /// This does not consume the character for which the predicate fails. The number of characters
    /// advanced is returned.
    fn advance_while<P>(&mut self, pred: P) -> usize
    where
        P: Fn(char) -> bool,
    {
        let mut count = 0;
        while self.advance_if(&pred) {
            count += 1;
        }
        count
    }

    /// Advance until a character satisfies a predicate.
    ///
    /// This does not consume the character for which the predicate is satisfied. The number of
    /// characters advanced is returned.
    fn advance_until<P>(&mut self, pred: P) -> usize
    where
        P: Fn(char) -> bool,
    {
        self.advance_while(|c| !pred(c))
    }

    /// Create a token that spans from the marked start index to the current index.
    fn emit(&mut self, kind: Token) -> Option<Spanned> {
        Some(Ok((self.mark_idx, kind, self.idx)))
    }

    /// Advance the lexer by `n` characters and then [`Self::emit()`] a token of the given `kind`.
    fn advance_by_and_emit(&mut self, n: usize, kind: Token) -> Option<Spanned> {
        self.advance_by(n);
        self.emit(kind)
    }

    /// Create an error that spans from the marked start index to the current index.
    fn emit_error(&mut self, error: LexicalError) -> Option<Spanned> {
        Some(Err((self.mark_idx, error, self.idx)))
    }

    /// Check if a [`Token::ShebangComment`] is allowed at the current location.
    fn shebang_comment_location(&self) -> bool {
        // Check if the current location is at the start of the text.
        if self.idx == 0 {
            return true;
        }
        // Check if the current location is at the second character in the text and the first
        // character was a BOM.
        self.idx == BOM.len_utf8() && self.text.as_str().starts_with('\u{feff}')
    }

    /// Produce the next token and advance the lexer.
    ///
    /// This returns `None` at the end of the source text, and once `None` is returned, it will
    /// always return `None` on subsequent calls.
    fn scan(&mut self) -> Option<Spanned> {
        self.advance_while(char::is_whitespace);
        self.mark();
        match self.window {
            //-------------------------------------------------------------------------------------
            // Comments.
            //-------------------------------------------------------------------------------------
            [Some('#'), Some('!'), ..] if self.shebang_comment_location() => {
                self.advance_until(pat!('\n'));
                self.emit(Token::ShebangComment)
            }
            [Some('/'), Some('/'), Some('/'), ..] => {
                self.advance_until(pat!('\n'));
                self.emit(Token::DocComment)
            }
            [Some('/'), Some('/'), ..] => {
                self.advance_until(pat!('\n'));
                self.emit(Token::LineComment)
            }
            //-------------------------------------------------------------------------------------
            // Operators and punctuation.
            //-------------------------------------------------------------------------------------
            [Some('!'), Some('='), ..] => self.advance_by_and_emit(2, Token::BangEq),
            [Some('!'), ..] => self.advance_by_and_emit(1, Token::Bang),
            [Some('%'), ..] => self.advance_by_and_emit(1, Token::Percent),
            [Some('&'), Some('&'), ..] => self.advance_by_and_emit(2, Token::AmpAmp),
            [Some('('), ..] => self.advance_by_and_emit(1, Token::LeftParen),
            [Some(')'), ..] => self.advance_by_and_emit(1, Token::RightParen),
            [Some('*'), Some('*'), ..] => self.advance_by_and_emit(2, Token::StarStar),
            [Some('*'), ..] => self.advance_by_and_emit(1, Token::Star),
            [Some('+'), ..] => self.advance_by_and_emit(1, Token::Plus),
            [Some(','), ..] => self.advance_by_and_emit(1, Token::Comma),
            [Some('-'), Some('>'), ..] => self.advance_by_and_emit(2, Token::MinusGt),
            [Some('-'), ..] => self.advance_by_and_emit(1, Token::Minus),
            [Some('.'), Some('.'), ..] => self.advance_by_and_emit(2, Token::DotDot),
            [Some('.'), ..] => self.advance_by_and_emit(1, Token::Dot),
            [Some('/'), ..] => self.advance_by_and_emit(1, Token::Slash),
            [Some(':'), Some(':'), ..] => self.advance_by_and_emit(2, Token::ColonColon),
            [Some(':'), ..] => self.advance_by_and_emit(1, Token::Colon),
            [Some(';'), ..] => self.advance_by_and_emit(1, Token::Semicolon),
            [Some('<'), Some('='), ..] => self.advance_by_and_emit(2, Token::LtEq),
            [Some('<'), ..] => self.advance_by_and_emit(2, Token::Lt),
            [Some('='), Some('='), ..] => self.advance_by_and_emit(2, Token::EqEq),
            [Some('='), Some('>'), ..] => self.advance_by_and_emit(2, Token::EqGt),
            [Some('='), ..] => self.advance_by_and_emit(1, Token::Eq),
            [Some('>'), Some('='), ..] => self.advance_by_and_emit(2, Token::GtEq),
            [Some('>'), ..] => self.advance_by_and_emit(1, Token::Gt),
            [Some('['), ..] => self.advance_by_and_emit(1, Token::LeftSquare),
            [Some(']'), ..] => self.advance_by_and_emit(1, Token::RightSquare),
            [Some('{'), ..] => self.advance_by_and_emit(1, Token::LeftBrace),
            [Some('|'), Some('|'), ..] => self.advance_by_and_emit(2, Token::PipePipe),
            [Some('|'), ..] => self.advance_by_and_emit(1, Token::Pipe),
            [Some('}'), ..] => self.advance_by_and_emit(1, Token::RightBrace),
            //-------------------------------------------------------------------------------------
            // Numbers.
            //-------------------------------------------------------------------------------------
            [Some('0'), Some('b'), ..] => scan_int!(self, '0'..='1', Token::BinInt),
            [Some('0'), Some('o'), ..] => scan_int!(self, '0'..='7', Token::OctInt),
            [Some('0'), Some('x'), ..] => {
                scan_int!(self, '0'..='9' | 'A'..='F' | 'a'..='f', Token::HexInt)
            }
            [Some('0'), Some('B' | 'O' | 'X'), ..] => {
                self.advance_while(is_text_like);
                self.emit_error(LexicalError::BaseMarker)
            }
            [Some('0'..='9'), ..] => {
                // Decimal number.
                // Assume the number is an integer until proven otherwise.
                let mut kind_or_error = Ok(Token::DecInt);
                // Decimal integer and float literals must match the regex:
                //
                // \d[_\d]*(\.\d[_\d]*)?(e[+-]?\d[_\d]*)?
                //
                // This is broken down into three phases:
                //
                // 1. \d[_\d]*
                // 2. (\.\d[_\d]*)?
                // 3. (e[+-]?\d[_\d]*)?
                //
                // Phase 1: \d[_\d]*
                // Note: The lexer is sitting on a \d so the advance_while() is guaranteed to advance at
                // least one character.
                self.advance_while(pat!('0'..='9' | '_'));
                // Phase 2: (\.\d[_\d]*)?
                if matches!(self.window, [Some('.'), Some('0'..='9'), ..]) {
                    kind_or_error = Ok(Token::Float);
                    self.advance(); // Consume '.'.
                    self.advance_while(pat!('0'..='9' | '_'));
                }
                // Phase 3: (e[+-]?\d[_\d]*)?
                if matches!(self.window, [Some('e'), ..]) {
                    kind_or_error = Ok(Token::Float);
                    self.advance(); // Consume `e`.
                    if self.advance_while(pat!('_')) > 0 {
                        kind_or_error = Err(LexicalError::Number);
                    }
                    self.advance_if(pat!('+' | '-'));
                    if self.advance_while(pat!('_')) > 0 {
                        kind_or_error = Err(LexicalError::Number);
                    }
                    self.advance_while(pat!('0'..='9' | '_'));
                }
                if self.window[0].is_some_and(is_text_like) {
                    kind_or_error = Err(LexicalError::Number);
                    self.advance_while(is_text_like);
                }
                match kind_or_error {
                    Ok(kind) => self.emit(kind),
                    Err(error) => self.emit_error(error),
                }
            }
            //-------------------------------------------------------------------------------------
            // Strings.
            //-------------------------------------------------------------------------------------
            [Some('"'), Some('"'), Some('"'), ..] => {
                self.advance_by(3); // Consume `"""`.
                self.scan_string(Token::BlockString, 3, 0, true)
            }
            [Some('"'), ..] => {
                self.advance(); // Consume `"`.
                self.scan_string(Token::String, 1, 0, true)
            }
            [Some('r'), Some('"'), ..] | [Some('r'), Some('#'), Some('#' | '"'), ..] => {
                self.advance(); // Consume `r`.
                let hashes = self.advance_while(pat!('#'));
                if !self.advance_if(pat!('"')) {
                    self.advance_until(char::is_whitespace); // Consume sensible rest of token.
                    return self.emit_error(LexicalError::StringStart);
                }
                self.scan_string(Token::RawString, 1, hashes, false)
            }
            //-------------------------------------------------------------------------------------
            // Identifiers and keywords.
            //-------------------------------------------------------------------------------------
            [Some('A'..='Z' | 'a'..='z' | '_'), ..] => {
                self.advance_while(|c| is_text_like(c) || c == '#');
                match self.as_marked_str() {
                    // Keywords.
                    "as" => self.emit(Token::KwAs),
                    "const" => self.emit(Token::KwConst),
                    "extern" => self.emit(Token::KwExtern),
                    "fn" => self.emit(Token::KwFn),
                    "if" => self.emit(Token::KwIf),
                    "immediate" => self.emit(Token::KwImmediate),
                    "import" => self.emit(Token::KwImport),
                    "let" => self.emit(Token::KwLet),
                    "match" => self.emit(Token::KwMatch),
                    "provide" => self.emit(Token::KwProvide),
                    "pub" => self.emit(Token::KwPub),
                    "return" => self.emit(Token::KwReturn),
                    "self" => self.emit(Token::KwSelf),
                    "transparent" => self.emit(Token::KwTransparent),
                    "type" => self.emit(Token::KwType),
                    "use" => self.emit(Token::KwUse),
                    "with" => self.emit(Token::KwWith),
                    // If not a keyword then this must be an identifier.
                    "_" => self.emit(Token::Underscore),
                    ident => {
                        let raw = ident.starts_with("r#");
                        let mut chars = ident.chars();
                        if raw {
                            // Skip `r#`.
                            chars.next();
                            chars.next();
                        }
                        let mut chars = chars.skip_while(pat!('_'));
                        macro_rules! classify {
                            ($pattern:pat, $regular:expr, $raw:expr) => {
                                if chars.all(pat!($pattern)) {
                                    self.emit(if raw { $raw } else { $regular })
                                } else {
                                    self.emit_error(LexicalError::Ident)
                                }
                            };
                        }
                        match chars.next() {
                            Some('a'..='z') => classify!('a'..='z' | '_' | '0'..='9',
                                Token::LowerIdent, Token::RawLowerIdent),
                            Some('A'..='Z') => classify!('A'..='Z' | 'a'..='z' | '_' | '0'..='9',
                                Token::UpperIdent, Token::RawUpperIdent),
                            _ => self.emit_error(LexicalError::Ident),
                        }
                    }
                }
            }
            //-------------------------------------------------------------------------------------
            // Other.
            //-------------------------------------------------------------------------------------
            // Illegal character to start a token.
            [Some(_), ..] => {
                self.advance();
                self.emit_error(LexicalError::Char)
            }
            // All done.
            [None, ..] => None,
        }
    }

    /// Scan a string-like literal.
    ///
    /// A token of type `kind` is emitted on success. The scanning is controlled by the number of
    /// `quotes` (`"`) that terminate the literal, the number of `hashes` (`#`) that terminate the
    /// literal, and whether the literal supports `escapes`.
    ///
    /// Backslashes (`/`) may or may not be allowed directly before string terminator depending on
    /// the string type. For example, `\"` cannot end [`Token::String`] but it can end a
    /// [`Token::RawString`].
    fn scan_string(
        &mut self,
        kind: Token,
        quotes: usize,
        hashes: usize,
        escapes: bool,
    ) -> Option<Spanned> {
        loop {
            if matches!(self.window, [None, ..]) {
                return self.emit_error(LexicalError::UnclosedString);
            }
            // Advance the lexer until it is sitting on a `"` that can start the closing delimiter.
            if escapes {
                // Advance to the next string-like terminator or escape.
                self.advance_until(pat!('"' | '\\'));
                match self.window {
                    [Some('"'), ..] => { /* Do nothing. */ }
                    [Some('\\'), ..] => {
                        self.advance_by(2); // Skip the escape (e.g., the sequence `\"`).
                        continue;
                    }
                    [None, ..] => continue, // End of input.
                    _ => unreachable!(),
                }
            } else {
                self.advance_until(pat!('"'));
            }
            // Currently sitting on a `"` that can start the terminator of a string-like.
            let seen_quotes = self.advance_while(pat!('"'));
            let seen_hashes = self.advance_while(pat!('#'));
            let enough_quotes = seen_quotes == quotes || (seen_quotes >= quotes && hashes > 0);
            return if enough_quotes && seen_hashes == hashes {
                self.emit(kind)
            } else if seen_quotes < quotes || seen_hashes < hashes {
                continue;
            } else {
                self.emit_error(LexicalError::StringEnd)
            };
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Spanned;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan()
    }
}

impl iter::FusedIterator for Lexer<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    /// Quickly create a new [`Lexer`].
    macro_rules! lexer {
        (mut $name:ident <- $source:expr) => {
            let _text = Text::new($source.into());
            let mut $name = Lexer::new(&_text);
        };
        ($name:ident <- $source:expr) => {
            let _text = Text::new($source.into());
            let $name = Lexer::new(&_text);
        };
    }

    /// Shorthand for asserting that a scan produces a value.
    macro_rules! scan {
        // Assert that the `source` string produces the specified [`Spanned`] `token`.
        ($source:expr, $spanned:expr) => {{
            lexer!(mut lexer <- $source);
            assert_eq!(Lexer::scan(&mut lexer), $spanned);
        }};
        // Assert that the entire `source` string is used as a given [`Token`] `kind`.
        ($source:expr, ok: $kind:expr) => {
            scan!($source, Some(Ok((0, $kind, $source.len()))));
        };
        // Assert that the entire `source` string is used as a given [`Token`] `kind`.
        ($source:expr, err: $error:expr) => {
            scan!($source, Some(Err((0, $error, $source.len()))));
        };
    }

    #[test]
    fn new() {
        lexer!(lexer <- "abc");
        assert_eq!(lexer.text.as_str(), "abc");
        assert_eq!(lexer.window, [Some('a'), Some('b'), Some('c')]);
        assert_eq!(lexer.idx, 0);
        assert_eq!(lexer.mark_idx, 0);
        assert_eq!(lexer.text.as_str(), "abc");
    }

    #[test]
    fn new_with_bom() {
        lexer!(lexer <- "\u{feff}abc");
        assert_eq!(lexer.text.as_str(), "\u{feff}abc");
        assert_eq!(lexer.window, [Some('a'), Some('b'), Some('c')]);
        assert_eq!(lexer.idx, BOM.len_utf8());
        assert_eq!(lexer.mark_idx, BOM.len_utf8());
        assert_eq!(lexer.text.as_str(), "\u{feff}abc");
    }

    #[test]
    fn mark() {
        lexer!(mut lexer <- "abcdef");
        assert_eq!(lexer.idx, 0);
        assert_eq!(lexer.mark_idx, 0);
        lexer.advance();
        assert_eq!(lexer.idx, 1);
        assert_eq!(lexer.mark_idx, 0);
        lexer.mark();
        assert_eq!(lexer.idx, 1);
        assert_eq!(lexer.mark_idx, 1);
    }

    #[test]
    fn as_marked_str() {
        lexer!(mut lexer <- "abcdef");
        assert_eq!(lexer.as_marked_str(), "");
        lexer.advance();
        assert_eq!(lexer.as_marked_str(), "a");
        lexer.mark();
        lexer.advance_by(3);
        assert_eq!(lexer.as_marked_str(), "bcd");
        lexer.mark();
        assert_eq!(lexer.as_marked_str(), "");
    }

    #[test]
    fn advance() {
        lexer!(mut lexer <- "abc");
        assert_eq!(lexer.idx, 0);
        assert_eq!(lexer.window, [Some('a'), Some('b'), Some('c')]);
        lexer.advance();
        assert_eq!(lexer.idx, 1);
        assert_eq!(lexer.window, [Some('b'), Some('c'), None]);
        lexer.advance();
        assert_eq!(lexer.idx, 2);
        assert_eq!(lexer.window, [Some('c'), None, None]);
        lexer.advance();
        assert_eq!(lexer.idx, 3);
        assert_eq!(lexer.window, [None, None, None]);
        lexer.advance();
        // Test idempotency.
        assert_eq!(lexer.idx, 3);
        assert_eq!(lexer.window, [None, None, None]);
    }

    #[test]
    fn advance_by() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_by(3);
        assert_eq!(lexer.idx, 3);
    }

    #[test]
    fn advance_unicode() {
        // - U+000041 (A): Basic Latin/ASCII require 1 byte in UTF-8.
        // - U+0003B2 (B): Greek and Coptic require 2 bytes in UTF-8.
        // - U+001E8D (ẍ): Latin Extended Additional require 3 bytes in UTF-8.
        // - U+01D50D (𝔍): Mathematical Alphanumeric Symbols require 4 bytes in UTF-8.
        lexer!(mut lexer <- "\u{0041}\u{003B2}\u{001E8D}\u{01D50D}");
        assert_eq!(lexer.idx, 0);
        lexer.advance();
        assert_eq!(lexer.idx, 1);
        lexer.advance();
        assert_eq!(lexer.idx, 3);
        lexer.advance();
        assert_eq!(lexer.idx, 6);
        lexer.advance();
        assert_eq!(lexer.idx, 10);
    }

    #[test]
    fn advance_if() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_if(|_| false);
        assert_eq!(lexer.idx, 0);
        lexer.advance_if(|_| true);
        assert_eq!(lexer.idx, 1);
    }

    #[test]
    fn advance_while() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_while(|_| false);
        assert_eq!(lexer.idx, 0);
        lexer.advance_while(pat!('a'..='c'));
        assert_eq!(lexer.idx, 3);
    }

    #[test]
    fn advance_until() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_until(|_| true);
        assert_eq!(lexer.idx, 0);
        lexer.advance_until(pat!('d'..='f'));
        assert_eq!(lexer.idx, 3);
    }

    #[test]
    fn emit() {
        lexer!(mut lexer <- "abc def");
        lexer.advance_by(3);
        let token = lexer.emit(Token::LowerIdent).unwrap().unwrap();
        assert_eq!(token, (0, Token::LowerIdent, 3));
    }

    #[test]
    fn advance_by_and_emit() {
        lexer!(mut lexer <- "abc def");
        let token = lexer
            .advance_by_and_emit(3, Token::LowerIdent)
            .unwrap()
            .unwrap();
        assert_eq!(token, (0, Token::LowerIdent, 3));
    }

    #[test]
    fn emit_error() {
        lexer!(mut lexer <- "aBc def");
        lexer.advance_by(3);
        let error = lexer.emit_error(LexicalError::Ident).unwrap().unwrap_err();
        assert_eq!(error, (0, LexicalError::Ident, 3));
    }

    #[test]
    fn shebang_comment_location() {
        lexer!(mut lexer <- "#!shebang");
        assert!(lexer.shebang_comment_location());
        lexer.advance();
        assert!(!lexer.shebang_comment_location());
    }

    #[test]
    fn shebang_comment_location_with_bom() {
        lexer!(mut lexer <- "\u{feff}#!shebang");
        assert!(lexer.shebang_comment_location());
        lexer.advance();
        assert!(!lexer.shebang_comment_location());
    }

    #[test]
    fn scan_shebang_comment() {
        // Valid.
        scan!("#!x", ok: Token::ShebangComment);
        scan!("#!x\na", Some(Ok((0, Token::ShebangComment, 3))));
        scan!(
            "\u{feff}#!x\na",
            Some(Ok((
                BOM.len_utf8(),
                Token::ShebangComment,
                BOM.len_utf8() + 3
            )))
        );
        // Invalid.
        scan!(" #!x\na", Some(Err((1, LexicalError::Char, 2))));
        scan!(
            "\u{feff} #!x\na",
            Some(Err((
                BOM.len_utf8() + 1,
                LexicalError::Char,
                BOM.len_utf8() + 2
            )))
        );
    }

    #[test]
    fn scan_doc_comment() {
        // Valid.
        scan!("///x", ok: Token::DocComment);
        scan!("///x\na", Some(Ok((0, Token::DocComment, 4))));
        scan!("///x///x", ok: Token::DocComment);
        scan!("///x//x", ok: Token::DocComment);
        scan!("///x/*x*/", ok: Token::DocComment);
    }

    #[test]
    fn scan_line_comment() {
        scan!("//x", ok: Token::LineComment);
        scan!("//x\n", Some(Ok((0, Token::LineComment, 3))));
        scan!("//x//x", ok: Token::LineComment);
        scan!("//x///x", ok: Token::LineComment);
        scan!( "//x/*x*/", ok: Token::LineComment);
    }

    #[test]
    fn scan_operators_and_punctuation() {
        scan!("{", ok: Token::LeftBrace);
        scan!("[", ok: Token::LeftSquare);
        scan!("(", ok: Token::LeftParen);
        scan!("}", ok: Token::RightBrace);
        scan!("]", ok: Token::RightSquare);
        scan!(")", ok: Token::RightParen);
        scan!("&&", ok: Token::AmpAmp);
        scan!("!", ok: Token::Bang);
        scan!("!=", ok: Token::BangEq);
        scan!(",", ok: Token::Comma);
        scan!(":", ok: Token::Colon);
        scan!("::", ok: Token::ColonColon);
        scan!(".", ok: Token::Dot);
        scan!("..", ok: Token::DotDot);
        scan!("=", ok: Token::Eq);
        scan!("==", ok: Token::EqEq);
        scan!("=>", ok: Token::EqGt);
        scan!(">", ok: Token::Gt);
        scan!(">=", ok: Token::GtEq);
        scan!("<", ok: Token::Lt);
        scan!("<=", ok: Token::LtEq);
        scan!("-", ok: Token::Minus);
        scan!("->", ok: Token::MinusGt);
        scan!("%", ok: Token::Percent);
        scan!("|", ok: Token::Pipe);
        scan!("||", ok: Token::PipePipe);
        scan!("+", ok: Token::Plus);
        scan!(";", ok: Token::Semicolon);
        scan!("/", ok: Token::Slash);
        scan!("*", ok: Token::Star);
        scan!("**", ok: Token::StarStar);
    }

    #[test]
    fn scan_binary_int() {
        // Valid.
        scan!("0b01", ok: Token::BinInt);
        scan!("0b0101_1100", ok: Token::BinInt);
        scan!("0b00__11", ok: Token::BinInt);
        scan!("0b0_", ok: Token::BinInt);
        scan!("0b0__", ok: Token::BinInt);
        // Errors.
        scan!("0B01", err: LexicalError::BaseMarker);
        scan!("0b_", err: LexicalError::Number);
        scan!("0b_0", err: LexicalError::Number);
        scan!("0b1010a", err: LexicalError::Number);
        scan!("0b10a10", err: LexicalError::Number);
        scan!("0b10210", err: LexicalError::Number);
        scan!("_0b1", err: LexicalError::Ident);
        scan!("0_b1", err: LexicalError::Number);
    }

    #[test]
    fn scan_octal_int() {
        // Valid.
        scan!("0o01234567", ok: Token::OctInt);
        scan!("0o123_456", ok: Token::OctInt);
        scan!("0o12__34", ok: Token::OctInt);
        scan!("0o1_", ok: Token::OctInt);
        scan!("0o1__", ok: Token::OctInt);
        // Errors.
        scan!("0O123", err: LexicalError::BaseMarker);
        scan!("0o_", err: LexicalError::Number);
        scan!("0o_0", err: LexicalError::Number);
        scan!("0o1234x", err: LexicalError::Number);
        scan!("0o12x34", err: LexicalError::Number);
        scan!("0o12834", err: LexicalError::Number);
        scan!("_0o1", err: LexicalError::Ident);
        scan!("0_o1", err: LexicalError::Number);
    }

    #[test]
    fn scan_hexadecimal_int() {
        // Valid.
        scan!("0x0123456789ABCDEFabcdef", ok: Token::HexInt);
        scan!("0x123_456", ok: Token::HexInt);
        scan!("0x12__34", ok: Token::HexInt);
        scan!("0x1_", ok: Token::HexInt);
        scan!("0x1__", ok: Token::HexInt);
        // Errors.
        scan!("0X123", err: LexicalError::BaseMarker);
        scan!("0x_", err: LexicalError::Number);
        scan!("0x_0", err: LexicalError::Number);
        scan!("0x1234z", err: LexicalError::Number);
        scan!("0x12z34", err: LexicalError::Number);
        scan!("0x12g34", err: LexicalError::Number);
        scan!("_0x1", err: LexicalError::Ident);
        scan!("0_x1", err: LexicalError::Number);
    }

    #[test]
    fn scan_decimal_int() {
        // Valid.
        scan!("0123456789", ok: Token::DecInt);
        scan!("5", ok: Token::DecInt);
        scan!("1_000_000", ok: Token::DecInt);
        scan!("1__23", ok: Token::DecInt);
        scan!("6_", ok: Token::DecInt);
        scan!("7__", ok: Token::DecInt);
        // Errors.
        scan!("_1", err: LexicalError::Ident);
        scan!("1a", err: LexicalError::Number);
    }

    #[test]
    fn scan_float() {
        // Valid with fractions.
        scan!("0.123456789", ok: Token::Float);
        scan!("0.123_456", ok: Token::Float);
        scan!("0.123__456_", ok: Token::Float);
        // Valid with exponents.
        scan!("1e5", ok: Token::Float);
        scan!("1e-5", ok: Token::Float);
        scan!("1e+5", ok: Token::Float);
        scan!("1_e5", ok: Token::Float);
        scan!("23_e3_5", ok: Token::Float);
        scan!("23_e-3_5", ok: Token::Float);
        scan!("23_e-3__5_", ok: Token::Float);
        // Valid with fractions and exponents.
        scan!("1__23.4_5__e-5_6_7", ok: Token::Float);
        // Invalid.
        scan!("1e_3", err: LexicalError::Number);
        scan!("1e-_3", err: LexicalError::Number);
        scan!("1e_+3", err: LexicalError::Number);
        scan!("1E3", err: LexicalError::Number);
        scan!("1.0a", err: LexicalError::Number);
        scan!("1.0e3b5", err: LexicalError::Number);
    }

    #[test]
    fn scan_block_string() {
        // Valid.
        scan!(r#""""""""#, ok: Token::BlockString);
        scan!(r#""""abc""""#, ok: Token::BlockString);
        scan!(r#"""" " """"#, ok: Token::BlockString);
        scan!(r#"""" "" """"#, ok: Token::BlockString);
        scan!(r#"""""" """"#, ok: Token::BlockString);
        scan!(r#"""" escaped quote \"""""#, ok: Token::BlockString);
        scan!(r#"""" escaped quote \""" """"#, ok: Token::BlockString);
        // Invalid.
        scan!(r#""""unclosed"#, err: LexicalError::UnclosedString);
        scan!(r#""""unclosed""#, err: LexicalError::UnclosedString);
        scan!(r#""""unclosed"""#, err: LexicalError::UnclosedString);
        scan!(r#""""unclosed\""""#, err: LexicalError::UnclosedString);
        scan!(r#""""extra closing"""""#, err: LexicalError::StringEnd);
    }

    #[test]
    fn scan_string() {
        // Valid.
        scan!(r#""""#, ok: Token::String);
        scan!(r#""abc""#, ok: Token::String);
        scan!(r#""ㄩ几丨匚ㄖᗪ乇""#, ok: Token::String);
        scan!(r#""A\nB\r\nC""#, ok: Token::String);
        scan!(r#""escaped \" quote""#, ok: Token::String);
        scan!(r#""escaped quote\"""#, ok: Token::String);
        scan!(r#""escaped slash\\""#, ok: Token::String);
        scan!("\"abc\ndef\"", ok: Token::String);
        // Invalid.
        scan!(r#""unclosed"#, err: LexicalError::UnclosedString);
        scan!(r#""unclosed\""#, err: LexicalError::UnclosedString);
        scan!(r#""extra closing"""#, err: LexicalError::StringEnd);
    }

    #[test]
    fn scan_raw_string() {
        // Valid.
        scan!(r###"r"""###, ok: Token::RawString);
        scan!(r###"r#""#"###, ok: Token::RawString);
        scan!(r###"r##""##"###, ok: Token::RawString);
        scan!(r###"r#""""""#"###, ok: Token::RawString);
        scan!(r###"r##""#"#"##"###, ok: Token::RawString);
        scan!(r###"r"\""###, ok: Token::RawString);
        scan!(r###"r"\\""###, ok: Token::RawString);
        scan!("r\"abc\ndef\"", ok: Token::RawString);
        // Invalid.
        scan!(r###"r""###, err: LexicalError::UnclosedString);
        scan!(r###"r#"""###, err: LexicalError::UnclosedString);
        scan!(r###"r##a"###, err: LexicalError::StringStart);
        scan!(r###"r#""##"###, err: LexicalError::StringEnd);
    }

    #[test]
    fn scan_keywords() {
        scan!("as", ok: Token::KwAs);
        scan!("const", ok: Token::KwConst);
        scan!("extern", ok: Token::KwExtern);
        scan!("fn", ok: Token::KwFn);
        scan!("if", ok: Token::KwIf);
        scan!("immediate", ok: Token::KwImmediate);
        scan!("import", ok: Token::KwImport);
        scan!("let", ok: Token::KwLet);
        scan!("match", ok: Token::KwMatch);
        scan!("provide", ok: Token::KwProvide);
        scan!("pub", ok: Token::KwPub);
        scan!("return", ok: Token::KwReturn);
        scan!("self", ok: Token::KwSelf);
        scan!("transparent", ok: Token::KwTransparent);
        scan!("type", ok: Token::KwType);
        scan!("use", ok: Token::KwUse);
        scan!("with", ok: Token::KwWith);
    }

    #[test]
    fn scan_underscore() {
        // Valid.
        scan!("_", ok: Token::Underscore);
        // Invalid.
        scan!("__", err: LexicalError::Ident);
        scan!("_1", err: LexicalError::Ident);
        scan!("_ä", err: LexicalError::Ident);
    }

    #[test]
    fn scan_lower_ident() {
        // Valid.
        scan!("x", ok: Token::LowerIdent);
        scan!("abcdefghijklmnopqrstuvwxyz", ok: Token::LowerIdent);
        scan!("z_0123456789", ok: Token::LowerIdent);
        scan!("__g_4", ok: Token::LowerIdent);
        // Invalid.
        scan!("xA", err: LexicalError::Ident);
        scan!("yöw", err: LexicalError::Ident);
        scan!("a#b", err: LexicalError::Ident);
    }

    #[test]
    fn scan_raw_lower_ident() {
        // Valid.
        scan!("r#x", ok: Token::RawLowerIdent);
        scan!("r#abcdefghijklmnopqrstuvwxyz", ok: Token::RawLowerIdent);
        scan!("r#z_0123456789", ok: Token::RawLowerIdent);
        scan!("r#__g_4", ok: Token::RawLowerIdent);
        scan!("r#match", ok: Token::RawLowerIdent);
        // Invalid.
        scan!("r#xA", err: LexicalError::Ident);
        scan!("r#yöw", err: LexicalError::Ident);
        scan!("r#a#b", err: LexicalError::Ident);
    }

    #[test]
    fn scan_upper_ident() {
        // Valid.
        scan!("U", ok: Token::UpperIdent);
        scan!("H_abcdefghijklmnopqrstuvwxyz", ok: Token::UpperIdent);
        scan!("ABCDEFGHIJKLMNOPQRSTUVWXYZ", ok: Token::UpperIdent);
        scan!("Q_0123456789", ok: Token::UpperIdent);
        scan!("__B_p", ok: Token::UpperIdent);
        // Invalid.
        scan!("BÉj", err: LexicalError::Ident);
    }

    #[test]
    fn scan_raw_upper_ident() {
        // Valid.
        scan!("r#U", ok: Token::RawUpperIdent);
        scan!("r#H_abcdefghijklmnopqrstuvwxyz", ok: Token::RawUpperIdent);
        scan!("r#ABCDEFGHIJKLMNOPQRSTUVWXYZ", ok: Token::RawUpperIdent);
        scan!("r#Q_0123456789", ok: Token::RawUpperIdent);
        scan!("r#__B_p", ok: Token::RawUpperIdent);
        // Invalid.
        scan!("r#BÉj", err: LexicalError::Ident);
        scan!("r#A#B", err: LexicalError::Ident);
    }

    #[test]
    fn scan_illegal_char() {
        // There are lots of illegal chars but only 2 are tested.
        scan!("$", err: LexicalError::Char);
        scan!("ä", err: LexicalError::Char);
    }

    #[test]
    fn scan_eof() {
        scan!("", None);
        scan!("\r\n \t", None);
    }

    #[test]
    fn iter() {
        // This does not test every token, only a handful are needed to test the iterator.
        lexer!(lexer <- "let x = 123;");
        assert_eq!(
            lexer.collect::<Vec<_>>(),
            vec![
                Ok((0, Token::KwLet, 3)),
                Ok((4, Token::LowerIdent, 5)),
                Ok((6, Token::Eq, 7)),
                Ok((8, Token::DecInt, 11)),
                Ok((11, Token::Semicolon, 12)),
            ]
        );
        lexer!(lexer <- "  let x = 123;\n");
        assert_eq!(
            lexer.collect::<Vec<_>>(),
            vec![
                Ok((2, Token::KwLet, 5)),
                Ok((6, Token::LowerIdent, 7)),
                Ok((8, Token::Eq, 9)),
                Ok((10, Token::DecInt, 13)),
                Ok((13, Token::Semicolon, 14)),
            ]
        );
        lexer!(lexer <- "let x=123;");
        assert_eq!(
            lexer.collect::<Vec<_>>(),
            vec![
                Ok((0, Token::KwLet, 3)),
                Ok((4, Token::LowerIdent, 5)),
                Ok((5, Token::Eq, 6)),
                Ok((6, Token::DecInt, 9)),
                Ok((9, Token::Semicolon, 10)),
            ]
        );
    }
}
