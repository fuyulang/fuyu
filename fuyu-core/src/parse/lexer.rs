// SPDX-License-Identifier: MPL-2.0

use crate::parse::{ByteIdx, Text, TextChars, Token};
use std::collections::VecDeque;
use std::iter::FusedIterator;
use std::num::NonZeroUsize;
use std::str::Chars;

/// The `Lexer` produces an iterator of [`Spanned`] [`Token`]s from a source [`Text`].
#[derive(Debug)]
pub struct Lexer<'a> {
    /// The source text.
    text: &'a Text,
    /// An iterator over the characters in the source text.
    text_chars: TextChars<'a>,
    /// The marked start index (inclusive) that tracks the start of a marked region (e.g., token in
    /// a lexer).
    ///
    /// It is guaranteed that `self.text_chars.idx() >= self.mark_idx`.
    mark_idx: ByteIdx,
    /// The indentations tracked by the lexer.
    ///
    /// The indentation is 1-indexed. When an indentation is `Some`, then it corresponds to a real
    /// indentation level, however, when an indentation is `None`, then it corresponds to `{` and
    /// `}` explicitly present in the source.
    indent_stack: Vec<Option<NonZeroUsize>>,
    /// Queued tokens to emit from the iterator the next time it is advanced.
    queued: VecDeque<Spanned>,
    /// Tracks whether a token was just emitted that should be followed by a `{`.
    next_must_be_brace: bool,
    /// The previous token that was emitted.
    prev_token: Option<Token>,
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
        if !$lexer.peek().map_or(false, is_text_like) {
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
            text_chars: TextChars::new(text),
            mark_idx: 0,
            indent_stack: vec![NonZeroUsize::new(1)],
            queued: VecDeque::new(),
            next_must_be_brace: false,
            prev_token: None,
        };
        // The index might not start at 0.
        lexer.mark();
        // Queue up the shebang comment here instead of the main loop since it can appear only at the start.
        if lexer.peek_pair() == (Some('#'), Some('!')) {
            lexer.advance_until(pat!('\n'));
            lexer.emit_queue(Token::ShebangComment);
        }
        lexer
    }

    /// Shorthand for accessing the index.
    fn idx(&self) -> usize {
        self.text_chars.idx()
    }

    /// Shorthand for accessing the column.
    fn column(&self) -> NonZeroUsize {
        self.text_chars.column()
    }

    /// Set the mark index to the current index.
    fn mark(&mut self) {
        self.mark_idx = self.idx();
    }

    /// Get the string from the mark index (inclusive) to the current index (exclusive).
    fn as_marked_str(&self) -> &str {
        &self.text.as_str()[self.mark_idx..self.idx()]
    }

    /// Peek the next character without advancing.
    fn peek(&mut self) -> Option<char> {
        self.text_chars.peek()
    }

    /// Peek the next two characters without advancing.
    fn peek_pair(&mut self) -> (Option<char>, Option<char>) {
        (self.text_chars.peek_nth(0), self.text_chars.peek_nth(1))
    }

    /// Peek the next three characters without advancing.
    fn peek_triplet(&mut self) -> (Option<char>, Option<char>, Option<char>) {
        (
            self.text_chars.peek_nth(0),
            self.text_chars.peek_nth(1),
            self.text_chars.peek_nth(2),
        )
    }

    /// Move the lexer ahead by one character.
    fn advance(&mut self) {
        self.text_chars.next();
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
        self.peek()
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
        self.prev_token = Some(kind.clone());
        Some(Ok((self.mark_idx, kind, self.idx())))
    }

    /// Create a token that spans from the marked start index to the current index and push it on
    /// to the queue.
    fn emit_queue(&mut self, kind: Token) {
        self.queued.push_back(Ok((self.mark_idx, kind, self.idx())));
    }

    /// Advance the lexer by `n` characters and then [`Self::emit()`] a token of the given `kind`.
    fn advance_by_and_emit(&mut self, n: usize, kind: Token) -> Option<Spanned> {
        self.advance_by(n);
        self.emit(kind)
    }

    /// Create an error that spans from the marked start index to the current index.
    fn emit_error(&mut self, error: LexicalError) -> Option<Spanned> {
        Some(Err((self.mark_idx, error, self.idx())))
    }

    /// Produce the next token and advance the lexer.
    ///
    /// This returns `None` at the end of the source text, and once `None` is returned, it will
    /// always return `None` on subsequent calls.
    fn scan(&mut self) -> Option<Spanned> {
        // Pop from the queue if there is anything ready.
        if !self.queued.is_empty() {
            return self.queued.pop_front();
        }
        // Skip the whitespace and maybe put tokens in the queue.
        self.scan_through_whitespace();
        if !self.queued.is_empty() {
            return self.queued.pop_front();
        }
        // At the end of the text, go through the indentation stack and emit `}` for all unclosed
        // indentation.
        if self.peek().is_none() && self.indent_stack.len() > 1 {
            if let Some(indent) = self.indent_stack.pop() {
                return self.emit(Token::RightBrace(indent));
            } else {
                todo!("TODO: There was an unclosed `{{` in the source.")
            }
            // TODO: Does this also need to emit a semicolon?
        }
        // Nothing was found so try to find the next token normally. The lexer is now sitting on a
        // character that can start a token.
        self.scan_next()
    }

    /// Scan for tokens in the following whitespace.
    ///
    /// This does not return any tokens, but may put some in the queue. This skips all whitespace.
    fn scan_through_whitespace(&mut self) {
        // Skip all whitespace and detect if a newline was seen.
        self.advance_while(|c| char::is_whitespace(c) && c != '\n');
        let saw_newline = self.advance_if(pat!('\n'));
        self.advance_while(char::is_whitespace);
        self.mark();
        // It is possible to emit a `;` and zero of more `}` based on indentation.
        if saw_newline {
            self.scan_through_whitespace_handle_newline();
        }
        // When a brace is expected, either confirm that it is there or insert one.
        self.scan_through_whitespace_infer_braces();
    }

    /// A helper for [`Self::scan_through_whitespace()`] that emits `;` and `{` tokens as required
    /// after a newline.
    ///
    /// This should be called after skipping whitespace if at least one newline was present in the
    /// skipped space.
    fn scan_through_whitespace_handle_newline(&mut self) {
        macro_rules! emit_semicolon {
            ($lexer:expr) => {
                // A semicolon is emitted when all of the following hold:
                //
                // - The top of the indent stack is an inferred `{`.
                // - The previous token was not a semicolon.
                // - At least one token has be emitted so far.
                if self.indent_stack.last().unwrap().is_some()
                    && self.prev_token != Some(Token::Semicolon)
                    && self.prev_token.is_some()
                {
                    self.emit_queue(Token::Semicolon);
                }
            };
        };
        let Some(Some(indent)) = self.indent_stack.last() else {
            return;
        };
        if self.column() == *indent && self.indent_stack.last().unwrap().is_some() {
            emit_semicolon!(self);
        } else if self.column() > *indent {
            // Do nothing, as the expression on the previous line continues on this one.
        } else {
            emit_semicolon!(self);
            loop {
                let Some(Some(indent)) = self.indent_stack.last() else {
                    return;
                };
                if *indent > self.column() {
                    // This `NonZeroUsize::new()` can never return `None`, so it is fine to use the
                    // returned option directly without checking if it is `None`.
                    self.emit_queue(Token::RightBrace(Some(*indent)));
                    self.indent_stack.pop();
                } else {
                    break;
                }
            }
            // The top of the indentation stack is equal to the current column, otherwise the
            // program is malformed.
            let indent = self.indent_stack.last().unwrap().unwrap();
            if indent != self.column() {
                // TODO: Emit and error instead of panicking.
                panic!("Indentation did not return to an allowed level.");
            }
        }
    }

    /// A helper for [`Self::scan_through_whitespace()`] that infers `{` if it is flagged that a
    /// `{` must appear at the current location.
    fn scan_through_whitespace_infer_braces(&mut self) {
        // Condition for whether to run this logic.
        if !self.next_must_be_brace {
            return;
        }
        self.next_must_be_brace = false;
        // Handle the `{` token.
        if self.peek() == Some('{') {
            // Do nothing, as this token will be lexer normally.
        } else {
            // Unwrapping is used instead of simply pushing the `Option<NonZeroUsize>` from
            // `NonZeroUsize::new()` to panic on the error if `self.column()` ever returns
            // zero (which it should not, by design).
            let indent = Some(self.column());
            self.indent_stack.push(indent);
            self.emit_queue(Token::LeftBrace(indent));
        }
    }

    /// Scan the next token.
    ///
    /// It is assumed that the lexer is either on a character that can start a token or at the end
    /// of the text.
    fn scan_next(&mut self) -> Option<Spanned> {
        self.mark();
        match self.peek_triplet() {
            //-------------------------------------------------------------------------------------
            // Comments.
            //-------------------------------------------------------------------------------------
            (Some('/'), Some('/'), Some('/'), ..) => {
                self.advance_until(pat!('\n'));
                self.emit(Token::DocComment)
            }
            (Some('/'), Some('/'), ..) => {
                self.advance_until(pat!('\n'));
                self.emit(Token::LineComment)
            }
            //-------------------------------------------------------------------------------------
            // Operators and punctuation.
            //-------------------------------------------------------------------------------------
            (Some('#'), Some('['), ..) => self.advance_by_and_emit(2, Token::HashLeftSquare),
            (Some('#'), ..) => self.advance_by_and_emit(1, Token::Hash),
            (Some('%'), ..) => self.advance_by_and_emit(1, Token::Percent),
            (Some('&'), Some('&'), ..) => self.advance_by_and_emit(2, Token::AmpAmp),
            (Some('&'), Some('['), ..) => self.advance_by_and_emit(2, Token::AmpLeftSquare),
            (Some('('), ..) => self.advance_by_and_emit(1, Token::LeftParen),
            (Some(')'), ..) => self.advance_by_and_emit(1, Token::RightParen),
            (Some('*'), Some('*'), ..) => self.advance_by_and_emit(2, Token::StarStar),
            (Some('*'), ..) => self.advance_by_and_emit(1, Token::Star),
            (Some('+'), ..) => self.advance_by_and_emit(1, Token::Plus),
            (Some(','), ..) => self.advance_by_and_emit(1, Token::Comma),
            (Some('-'), Some('>'), ..) => self.advance_by_and_emit(2, Token::MinusGt),
            (Some('-'), ..) => self.advance_by_and_emit(1, Token::Minus),
            (Some('.'), Some('.'), ..) => self.advance_by_and_emit(2, Token::DotDot),
            (Some('.'), ..) => self.advance_by_and_emit(1, Token::Dot),
            (Some('/'), Some('='), ..) => self.advance_by_and_emit(2, Token::SlashEq),
            (Some('/'), ..) => self.advance_by_and_emit(1, Token::Slash),
            (Some(':'), ..) => self.advance_by_and_emit(1, Token::Colon),
            (Some(';'), ..) => self.advance_by_and_emit(1, Token::Semicolon),
            (Some('<'), Some('='), ..) => self.advance_by_and_emit(2, Token::LtEq),
            (Some('<'), ..) => self.advance_by_and_emit(2, Token::Lt),
            (Some('='), Some('='), ..) => self.advance_by_and_emit(2, Token::EqEq),
            (Some('='), ..) => self.advance_by_and_emit(1, Token::Eq),
            (Some('>'), Some('='), ..) => self.advance_by_and_emit(2, Token::GtEq),
            (Some('>'), ..) => self.advance_by_and_emit(1, Token::Gt),
            (Some('@'), ..) => self.advance_by_and_emit(1, Token::At),
            (Some('['), ..) => self.advance_by_and_emit(1, Token::LeftSquare),
            (Some('\\'), ..) => self.advance_by_and_emit(1, Token::BackSlash),
            (Some(']'), ..) => self.advance_by_and_emit(1, Token::RightSquare),
            (Some('{'), ..) => {
                self.indent_stack.push(None);
                self.advance_by_and_emit(1, Token::LeftBrace(None))
            }
            (Some('|'), Some('|'), ..) => self.advance_by_and_emit(2, Token::PipePipe),
            (Some('|'), ..) => self.advance_by_and_emit(1, Token::Pipe),
            (Some('}'), ..) => {
                // TODO: Need to check if popping the appropriate thing.
                self.indent_stack.pop();
                self.advance_by_and_emit(1, Token::RightBrace(None))
            }
            //-------------------------------------------------------------------------------------
            // Numbers.
            //-------------------------------------------------------------------------------------
            (Some('0'), Some('b'), ..) => scan_int!(self, '0'..='1', Token::BinInt),
            (Some('0'), Some('o'), ..) => scan_int!(self, '0'..='7', Token::OctInt),
            (Some('0'), Some('x'), ..) => {
                scan_int!(self, '0'..='9' | 'A'..='F' | 'a'..='f', Token::HexInt)
            }
            (Some('0'), Some('B' | 'O' | 'X'), ..) => {
                self.advance_while(is_text_like);
                self.emit_error(LexicalError::BaseMarker)
            }
            (Some('0'..='9'), ..) => {
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
                if matches!(self.peek_pair(), (Some('.'), Some('0'..='9'))) {
                    kind_or_error = Ok(Token::Float);
                    self.advance(); // Consume '.'.
                    self.advance_while(pat!('0'..='9' | '_'));
                }
                // Phase 3: (e[+-]?\d[_\d]*)?
                if self.peek() == Some('e') {
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
                if self.peek().is_some_and(is_text_like) {
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
            (Some('"'), Some('"'), Some('"'), ..) => {
                self.advance_by(3); // Consume `"""`.
                self.scan_string(Token::BlockString, 3, 0, true)
            }
            (Some('"'), ..) => {
                self.advance(); // Consume `"`.
                self.scan_string(Token::String, 1, 0, true)
            }
            (Some('r'), Some('"'), ..) | (Some('r'), Some('#'), Some('#' | '"'), ..) => {
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
            (Some('A'..='Z' | 'a'..='z' | '_'), ..) => {
                self.advance_while(|c| is_text_like(c) || c == '#');
                match self.as_marked_str() {
                    // Keywords.
                    "as" => self.emit(Token::KwAs),
                    "do" => {
                        self.next_must_be_brace = true;
                        self.emit(Token::KwDo)
                    }
                    "else" => self.emit(Token::KwElse),
                    "for" => self.emit(Token::KwFor),
                    "if" => self.emit(Token::KwIf),
                    "immediate" => self.emit(Token::KwImmediate),
                    "import" => self.emit(Token::KwImport),
                    "let" => self.emit(Token::KwLet),
                    "match" => {
                        self.next_must_be_brace = true;
                        self.emit(Token::KwMatch)
                    }
                    "panic" => self.emit(Token::KwPanic),
                    "require" => self.emit(Token::KwRequire),
                    "return" => self.emit(Token::KwReturn),
                    "then" => self.emit(Token::KwThen),
                    "todo" => self.emit(Token::KwTodo),
                    "try" => self.emit(Token::KwTry),
                    "type" => self.emit(Token::KwType),
                    "unimplemented" => self.emit(Token::KwUnimplemented),
                    "unreachable" => self.emit(Token::KwUnreachable),
                    "use" => self.emit(Token::KwUse),
                    "when" => {
                        self.next_must_be_brace = true;
                        self.emit(Token::KwWhen)
                    }
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
            (Some(_), ..) => {
                self.advance();
                self.emit_error(LexicalError::Char)
            }
            // All done.
            (None, ..) => None,
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
            if self.peek().is_none() {
                return self.emit_error(LexicalError::UnclosedString);
            }
            // Advance the lexer until it is sitting on a `"` that can start the closing delimiter.
            if escapes {
                // Advance to the next string-like terminator or escape.
                self.advance_until(pat!('"' | '\\'));
                match self.peek() {
                    Some('"') => { /* Do nothing. */ }
                    Some('\\') => {
                        // TODO: Need more advanced logic. What about `\u{...}` escapes?
                        self.advance_by(2); // Skip the escape (e.g., the sequence `\"`).
                        continue;
                    }
                    None => continue, // End of input.
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

impl FusedIterator for Lexer<'_> {}

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

    /// Collect all of the tokens from the lexer into a vector ignoring the positions.
    ///
    /// This panics if the lexer emits any errors.
    macro_rules! collect_tokens {
        ($lexer:expr) => {
            $lexer.map(|spanned| spanned.unwrap().1).collect::<Vec<_>>()
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
        lexer!(mut lexer <- "abc");
        assert_eq!(lexer.text.as_str(), "abc");
        assert_eq!(lexer.peek(), Some('a'));
        assert_eq!(lexer.idx(), 0);
        assert_eq!(lexer.mark_idx, 0);
        assert_eq!(lexer.text.as_str(), "abc");
    }

    #[test]
    fn new_with_bom() {
        // The byte order mark is U+FEFF.
        lexer!(mut lexer <- "\u{feff}abc");
        assert_eq!(lexer.text.as_str(), "\u{feff}abc");
        assert_eq!(lexer.peek(), Some('a'));
        assert_eq!(lexer.idx(), '\u{feff}'.len_utf8());
        assert_eq!(lexer.mark_idx, '\u{feff}'.len_utf8());
        assert_eq!(lexer.text.as_str(), "\u{feff}abc");
    }

    #[test]
    fn mark() {
        lexer!(mut lexer <- "abcdef");
        assert_eq!(lexer.idx(), 0);
        assert_eq!(lexer.mark_idx, 0);
        lexer.advance();
        assert_eq!(lexer.idx(), 1);
        assert_eq!(lexer.mark_idx, 0);
        lexer.mark();
        assert_eq!(lexer.idx(), 1);
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
        assert_eq!(lexer.idx(), 0);
        assert_eq!(lexer.peek(), Some('a'));
        lexer.advance();
        assert_eq!(lexer.idx(), 1);
        assert_eq!(lexer.peek(), Some('b'));
        lexer.advance();
        assert_eq!(lexer.idx(), 2);
        assert_eq!(lexer.peek(), Some('c'));
        lexer.advance();
        assert_eq!(lexer.idx(), 3);
        assert_eq!(lexer.peek(), None);
        lexer.advance();
        // Test idempotency.
        assert_eq!(lexer.idx(), 3);
        assert_eq!(lexer.peek(), None);
    }

    #[test]
    fn advance_by() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_by(3);
        assert_eq!(lexer.idx(), 3);
    }

    #[test]
    fn advance_unicode() {
        // - U+000041 (A): Basic Latin/ASCII require 1 byte in UTF-8.
        // - U+0003B2 (B): Greek and Coptic require 2 bytes in UTF-8.
        // - U+001E8D (ẍ): Latin Extended Additional require 3 bytes in UTF-8.
        // - U+01D50D (𝔍): Mathematical Alphanumeric Symbols require 4 bytes in UTF-8.
        lexer!(mut lexer <- "\u{0041}\u{003B2}\u{001E8D}\u{01D50D}");
        assert_eq!(lexer.idx(), 0);
        lexer.advance();
        assert_eq!(lexer.idx(), 1);
        lexer.advance();
        assert_eq!(lexer.idx(), 3);
        lexer.advance();
        assert_eq!(lexer.idx(), 6);
        lexer.advance();
        assert_eq!(lexer.idx(), 10);
    }

    #[test]
    fn advance_if() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_if(|_| false);
        assert_eq!(lexer.idx(), 0);
        lexer.advance_if(|_| true);
        assert_eq!(lexer.idx(), 1);
    }

    #[test]
    fn advance_while() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_while(|_| false);
        assert_eq!(lexer.idx(), 0);
        lexer.advance_while(pat!('a'..='c'));
        assert_eq!(lexer.idx(), 3);
    }

    #[test]
    fn advance_until() {
        lexer!(mut lexer <- "abcdef");
        lexer.advance_until(|_| true);
        assert_eq!(lexer.idx(), 0);
        lexer.advance_until(pat!('d'..='f'));
        assert_eq!(lexer.idx(), 3);
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
    fn scan_shebang_comment() {
        // Valid.
        scan!("#!x", ok: Token::ShebangComment);
        scan!("#!x\na", Some(Ok((0, Token::ShebangComment, 3))));
        scan!("\u{feff}#!bom\na", Some(Ok((3, Token::ShebangComment, 8))));
        // Not a shebang comment.
        scan!(" #!x\na", Some(Ok((1, Token::Hash, 2))));
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
        scan!("{", ok: Token::LeftBrace(None));
        scan!("[", ok: Token::LeftSquare);
        scan!("(", ok: Token::LeftParen);
        scan!("}", ok: Token::RightBrace(None));
        scan!("]", ok: Token::RightSquare);
        scan!(")", ok: Token::RightParen);
        scan!("&[", ok: Token::AmpLeftSquare);
        scan!("#[", ok: Token::HashLeftSquare);
        scan!("&&", ok: Token::AmpAmp);
        scan!("@", ok: Token::At);
        scan!("\\", ok: Token::BackSlash);
        scan!(",", ok: Token::Comma);
        scan!(":", ok: Token::Colon);
        scan!(".", ok: Token::Dot);
        scan!("..", ok: Token::DotDot);
        scan!("=", ok: Token::Eq);
        scan!("==", ok: Token::EqEq);
        scan!(">", ok: Token::Gt);
        scan!(">=", ok: Token::GtEq);
        scan!("#", ok: Token::Hash);
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
        scan!("/=", ok: Token::SlashEq);
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
        scan!("do", ok: Token::KwDo);
        scan!("else", ok: Token::KwElse);
        scan!("for", ok: Token::KwFor);
        scan!("if", ok: Token::KwIf);
        scan!("immediate", ok: Token::KwImmediate);
        scan!("import", ok: Token::KwImport);
        scan!("let", ok: Token::KwLet);
        scan!("match", ok: Token::KwMatch);
        scan!("panic", ok: Token::KwPanic);
        scan!("require", ok: Token::KwRequire);
        scan!("return", ok: Token::KwReturn);
        scan!("then", ok: Token::KwThen);
        scan!("todo", ok: Token::KwTodo);
        scan!("try", ok: Token::KwTry);
        scan!("type", ok: Token::KwType);
        scan!("unimplemented", ok: Token::KwUnimplemented);
        scan!("unreachable", ok: Token::KwUnreachable);
        scan!("use", ok: Token::KwUse);
        scan!("when", ok: Token::KwWhen);
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
        // There are lots of illegal chararacters, but only 2 are tested.
        scan!("$", err: LexicalError::Char);
        scan!("ä", err: LexicalError::Char);
    }

    #[test]
    fn scan_eof() {
        scan!("", None);
        scan!("\n \r\n ", None);
    }

    #[test]
    fn iter_basic() {
        // This does not test every token, only a handful are needed to test the iterator.
        lexer!(lexer <- "let x = 123;");
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwLet,
                Token::LowerIdent,
                Token::Eq,
                Token::DecInt,
                Token::Semicolon,
            ]
        );
        lexer!(lexer <- "  let x = 123;\n");
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwLet,
                Token::LowerIdent,
                Token::Eq,
                Token::DecInt,
                Token::Semicolon,
            ]
        );
        lexer!(lexer <- "let x=123;");
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwLet,
                Token::LowerIdent,
                Token::Eq,
                Token::DecInt,
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn iter_infer_semicolons() {
        // Semicolons should be inferred after `x`, `y`, and `z`.
        lexer!(lexer <- concat!(
            "x\n",
            "y\n",
            "z\n",
        ));
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::LowerIdent,
                Token::Semicolon,
                Token::LowerIdent,
                Token::Semicolon,
                Token::LowerIdent,
                Token::Semicolon,
            ]
        );
        // Semicolons should be inferred after `x`, and `z`.
        lexer!(lexer <- concat!(
            "x\n",
            "  y\n",
            "z\n",
        ));
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::LowerIdent,
                Token::LowerIdent,
                Token::Semicolon,
                Token::LowerIdent,
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn iter_infer_braces() {
        lexer!(lexer <- "do");
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwDo,
                Token::LeftBrace(Some(NonZeroUsize::new(3).unwrap())),
                Token::RightBrace(Some(NonZeroUsize::new(3).unwrap())),
            ]
        );
        lexer!(lexer <- "match");
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwMatch,
                Token::LeftBrace(Some(NonZeroUsize::new(6).unwrap())),
                Token::RightBrace(Some(NonZeroUsize::new(6).unwrap())),
            ]
        );
        lexer!(lexer <- "when");
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwWhen,
                Token::LeftBrace(Some(NonZeroUsize::new(5).unwrap())),
                Token::RightBrace(Some(NonZeroUsize::new(5).unwrap())),
            ]
        );
    }

    #[test]
    fn iter_infer_nested_braces() {
        lexer!(lexer <- concat!(
            "do\n",
            "  do\n",
            "    x\n",
        ));
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwDo,
                Token::LeftBrace(Some(NonZeroUsize::new(3).unwrap())),
                Token::KwDo,
                Token::LeftBrace(Some(NonZeroUsize::new(5).unwrap())),
                Token::LowerIdent,
                Token::Semicolon,
                Token::RightBrace(Some(NonZeroUsize::new(5).unwrap())),
                Token::RightBrace(Some(NonZeroUsize::new(3).unwrap())),
            ]
        );
    }

    #[test]
    fn iter_do_not_infer_semicolons_in_explicit_braces() {
        // Do not infer semicolons within `{ ... }`."
        lexer!(lexer <- concat!(
            "do {\n",
            "  x\n",
            "  y\n",
            "}\n",
        ));
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwDo,
                Token::LeftBrace(None),
                Token::LowerIdent,
                Token::LowerIdent,
                Token::RightBrace(None),
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn iter_infer_semicolons_in_inferred_braces() {
        // Infer semicolons inside an inferred brace block inside and explicit `{ ... }`.
        lexer!(lexer <- concat!(
            "do {\n",
            "  do \n",
            "    x\n",
            "    y\n",
            "}\n",
        ));
        assert_eq!(
            collect_tokens!(lexer),
            vec![
                Token::KwDo,
                Token::LeftBrace(None),
                Token::KwDo,
                Token::LeftBrace(Some(NonZeroUsize::new(5).unwrap())),
                Token::LowerIdent,
                Token::Semicolon,
                Token::LowerIdent,
                Token::Semicolon,
                Token::RightBrace(Some(NonZeroUsize::new(5).unwrap())),
                Token::RightBrace(None),
                Token::Semicolon,
            ]
        );
    }
}
