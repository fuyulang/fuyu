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

/// TODO: Docs
fn is_commment(kind: Token) -> bool {
    matches!(
        kind,
        Token::BlockComment | Token::LineComment | Token::DocComment | Token::ShebangComment
    )
}

impl<'a> Parsed<'a> {
    /// TODO: Docs.
    pub fn new(
        text: &'a Text,
    ) -> Result<Self, ParseError<ByteIdx, Token, (ByteIdx, LexerError, ByteIdx)>> {
        let lexer = Lexer::new(text);
        let parser = ModuleParser::new();
        let state = State {
            text: text.as_str(),
        };
        // Build a lexer that skips over the comments and extracts them. The `comments` vector
        // contains the comments in the order they were encountered in the source.
        let mut comments = vec![];
        let lexer_no_comments = lexer.filter(|spanned| match spanned {
            Ok(token @ (_, kind, _)) if is_commment(kind.clone()) => {
                comments.push(token.clone());
                false
            }
            _ => true,
        });
        // Do the parse.
        let ast = parser.parse(&state, lexer_no_comments)?;
        // Add the comments back.
        // TODO: Put the comments into the AST.
        Ok(Self { text, ast })
    }
}

#[cfg(test)]
mod tests {
    // TODO: Add tests.
}
