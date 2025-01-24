// SPDX-License-Identifier: MPL-2.0

//! The Fuyu compiler.
//!
//! The compiler is set up as a pipeline where the data path is linear. If any one step fails, then
//! none of the following steps are run as the validity of those outputs is uncertain. In other
//! words, if any step produces one or more errors then all following steps are cancelled and
//! compilation stops.
//!
//! 1. The [`Lexer`] is an iterator of [`Spanned`] [`Token`]s from a source [`Text`].

mod lexer;
mod text;
mod token;

pub use lexer::*;
pub use text::*;
pub use token::*;
