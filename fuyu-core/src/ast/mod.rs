// SPDX-License-Identifier: MPL-2.0

//! The abstract syntax tree (AST) is a (mostly) abstract representation of a Fuyu module.
//!
//! The AST is said to be "mostly" abstract because it still tracks extraneous bits of information,
//! namely punctuation and [`Trivia`]. These are tracked so that the generated [`Ast`] is as
//! general as possible, and can be re-used for purposes such as reformatting and documentation
//! generation.

use crate::parse::Span;

mod decl;
mod expr;
mod ident;
mod pattern;
mod util;

pub use decl::*;
pub use expr::*;
pub use ident::*;
pub use pattern::*;
use util::*;

/// An abstract syntax tree for a Fuyu module.
#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'text> {
    /// Top-level declarations in the module.
    decls: Vec<Decl<'text>>,
    /// Trivia in the module.
    trivia: Trivia<'text>,
}

/// Trivia are significant elements of the source that do not affect the affect the meaning.
///
/// In general, trivia can be removed from the source without affecting the behavior of the program
/// at runtime (e.g., comments).
#[derive(Clone, Debug, PartialEq)]
pub struct Trivia<'text> {
    /// The shebang comment that can appear at the first byte of the source.
    pub shebang_comment: Option<Comment<'text>>,
    /// Line comments started by `//`.
    pub line_comments: Vec<Comment<'text>>,
    /// Documentation comments started by `///`.
    pub doc_comments: Vec<Comment<'text>>,
}

/// A comment is a specific type of [`Trivia`].
#[derive(Clone, Debug, PartialEq)]
pub struct Comment<'text> {
    #[doc = docs!(span: "comment"; including: "opening delimiter")]
    span: Span,
    /// Comment content not including opening delimiters and trailing newline.
    comment: &'text str,
}
