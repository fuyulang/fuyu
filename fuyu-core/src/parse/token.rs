// SPDX-License-Identifier: MPL-2.0

/// A `Token` is an abstract representation the token type not tied to any value.
///
/// This value is often called the token "kind" throughout this project.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    /// A line comment started with `//`.
    LineComment,
    /// A documentation comment started with `///`.
    DocComment,
    /// A shebang comment started with `#!` at the first byte in the source.
    ShebangComment,
    /// Identifiers whose first non-underscore character is a lowercase letter.
    LowerIdent,
    /// A raw [`LowerIdent`][Self::LowerIdent] including the `r#` prefix.
    RawLowerIdent,
    /// Identifiers whose first non-underscore character is an uppercase letter.
    UpperIdent,
    /// A raw [`UpperIdent`][Self::UpperIdent] including the `r#` prefix.
    RawUpperIdent,
    /// An underscore, `_`.
    Underscore,
    /// The `as` keyword.
    KwAs,
    /// The `for` keyword.
    KwFor,
    /// The `if` keyword.
    KwIf,
    /// The `immediate` keyword.
    KwImmediate,
    /// The `import` keyword.
    KwImport,
    /// The `let` keyword.
    KwLet,
    /// The `match` keyword.
    KwMatch,
    /// The `panic` keyword.
    KwPanic,
    /// The `require` keyword.
    KwRequire,
    /// The `return` keyword.
    KwReturn,
    /// The `todo` keyword.
    KwTodo,
    /// The `try` keyword.
    KwTry,
    /// The `type` keyword.
    KwType,
    /// The `unimplemented` keyword.
    KwUnimplemented,
    /// The `unreachable` keyword.
    KwUnreachable,
    /// The `use` keyword.
    KwUse,
    /// A binary literal integer prefixed with `0b`.
    BinInt,
    /// An octal literal integer prefixed with `0o`.
    OctInt,
    /// A hexadecimal literal integer prefixed with `0x`.
    HexInt,
    /// A decimal literal integer.
    DecInt,
    /// A literal floating-point number.
    Float,
    /// A literal `"..."` string including the leading and trailing `"`.
    ///
    /// The content is not checked for validity.
    String,
    /// A literal `"""..."""` block string including the leading and trailing `"""`.
    ///
    /// The content is not checked for validity.
    BlockString,
    /// A literal `r#"..."#` raw string including the leading `r#"` and trailing `"#`.
    ///
    /// The content is not checked for validity.
    RawString,
    /// `{`.
    LeftBrace,
    /// `[`.
    LeftSquare,
    /// `(`.
    LeftParen,
    /// `}`.
    RightBrace,
    /// `]`.
    RightSquare,
    /// `)`.
    RightParen,
    /// `@[`
    AtLeftSquare,
    /// `,`.
    Comma,
    /// `:`.
    Colon,
    /// `::`.
    ColonColon,
    /// `.`.
    Dot,
    /// `..`.
    DotDot,
    /// `=`.
    Eq,
    /// `==`.
    EqEq,
    /// `=>`.
    EqGt,
    /// `>`.
    Gt,
    /// `>=`.
    GtEq,
    /// `<`.
    Lt,
    /// `<=`.
    LtEq,
    /// `-`.
    Minus,
    /// `->`.
    MinusGt,
    /// `%`.
    Percent,
    /// `|`.
    Pipe,
    /// `+`.
    Plus,
    /// `;`.
    Semicolon,
    /// `/`.
    Slash,
    /// `/=`.
    SlashEq,
    /// `*`.
    Star,
    /// `**`.
    StarStar,
}
