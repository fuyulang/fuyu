// SPDX-License-Identifier: MPL-2.0

use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(grammar, "/compiler/grammar.rs");

use super::ast::ModuleAst;
use super::lexer::{Lexer, LexerError};
use super::text::{ByteIdx, Text};
use super::token::Token;
use grammar::ModuleParser;

// TODO: Rename this?
/// TODO: Docs
#[derive(Debug)]
struct State<'a> {
    text: &'a str,
}

// TODO: Rename.
/// TODO: Docs.
#[derive(Debug)]
pub struct Parsed<'a> {
    text: &'a Text,
    ast: ModuleAst,
}

impl<'a> Parsed<'a> {
    /// TODO: Docs.
    pub fn new(
        text: &'a Text,
    ) -> Result<Self, ParseError<ByteIdx, Token, (ByteIdx, LexerError, ByteIdx)>> {
        // TODO: Handle comments separately so the parser does not need to deal with them.
        let lexer = Lexer::new(text);
        let parser = ModuleParser::new();
        let state = State {
            text: text.as_str(),
        };
        let ast = parser.parse(&state, lexer)?;
        Ok(Self { text, ast })
    }
}

#[cfg(test)]
mod tests {
    // TODO: Add tests.
}
