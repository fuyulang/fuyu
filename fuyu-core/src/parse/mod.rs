// SPDX-License-Identifier: MPL-2.0

//! The parser implementation includes all steps up to and including the generation of an abstract
//! syntax tree.
//!
//! The steps are:
//!
//! 1.  Produce a [`Text`].
//! 2.  Produce a [`Lexer`] that makes a [`Token`] stream from the [`Text`].
//! 3.  Parse the stream into an [`Ast`][crate::ast::Ast].

mod lexer;
mod text;
mod token;

pub use lexer::*;
pub use text::*;
pub use token::*;
