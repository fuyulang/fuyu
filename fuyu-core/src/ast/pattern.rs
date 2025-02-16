// SPDX-License-Identifier: MPL-2.0

//! Patterns express shape and value. There are several kinds of patterns:
//!
//! - Integer patterns ([`IntPattern`]).
//! - Floating-point patterns ([`FloatPattern`]).
//! - String patterns ([`StringPattern`]).
//! - Tuple patterns ([`TuplePattern`]).
//! - List patterns ([`ListPattern`]).
//! - Identifier patterns ([`IdentPattern`]).
//! - Constructor patterns ([`ConstructorPattern`]).
//!
//! Patterns can generally be composed, thus, the [`Pattern`] enum represents any expression.

use crate::ast::{docs, Expr, Ident};
use crate::parse::Span;

/// A pattern that can appear in the source.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'text> {
    /// An integer pattern (refer to [`IntPattern`]).
    Int(IntPattern<'text>),
    /// A floating-point pattern (refer to [`FloatPattern`]).
    Float(FloatPattern<'text>),
    /// A string pattern (refer to [`StringPattern`]).
    String(StringPattern<'text>),
    /// A tuple pattern (refer to [`TuplePattern`]).
    Tuple(TuplePattern<'text>),
    /// A list pattern (refer to [`ListPattern`]).
    List(ListPattern<'text>),
    /// An identifier pattern (refer to [`IdentPattern`]).
    Ident(IdentPattern<'text>),
    /// A constructor pattern (refer to [`ConstructorPattern`]).
    Constructor(ConstructorPattern<'text>),
}

/// An integer literal pattern.
///
/// # Form examples
///
/// ```fuyu
/// 123_456     // Decimal.
/// 0b1010_0110 // Binary.
/// 0o755       // Octal.
/// 0x1a2b3c    // Hexadecimal.
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct IntPattern<'text> {
    #[doc = docs!(span: "integer")]
    pub span: Span,
    #[doc = docs!(literal: "integer")]
    pub literal: &'text str,
}

/// An floating-point literal pattern.
///
/// # Form examples
///
/// ```fuyu
/// 0.0
/// 1_2_.3_4_
/// 56.89e-1_9
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FloatPattern<'text> {
    #[doc = docs!(span: "float")]
    pub span: Span,
    #[doc = docs!(literal: "float")]
    pub literal: &'text str,
}

/// A string literal pattern.
///
/// # Form examples
///
/// ```fuyu
/// "string"
/// """
///     block
///     string
/// """
/// r"raw string"
/// r##"raw sring with hashes"##
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct StringPattern<'text> {
    #[doc = docs!(span: "string"; including: "`r`", "`#`", "`\"`")]
    pub span: Span,
    #[doc = docs!(literal: "string")]
    pub literal: &'text str,
    #[doc = docs!(literal value: "string")]
    pub value: String,
}

/// A tuple pattern.
///
/// # Form examples
///
/// ```fuyu
/// ()
/// (True,)
/// (1, 2, "hello")
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TuplePattern<'text> {
    #[doc = docs!(span: "tuple"; including: "`(`", "`)`")]
    pub span: Span,
    #[doc = docs!(patterns: "tuple")]
    pub patterns: Vec<Pattern<'text>>,
}

/// A list pattern.
///
/// # Form examples
///
/// ```fuyu
/// []
/// ["hello"]
/// [1, 2, 3, 4, 5]
/// [1, 2, ..rest]
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ListPattern<'text> {
    #[doc = docs!(span: "list"; including: "`[`", "`]`")]
    pub span: Span,
    #[doc = docs!(patterns: "list")]
    pub patterns: Vec<Pattern<'text>>,
    #[doc = docs!(rest_pattern)]
    pub rest: Option<ListRestPattern<'text>>,
}

/// A rest of list pattern.
///
/// This is part of a [`ListPattern`].
///
/// # Form examples
///
/// ```fuyu
/// [1, 2, x, 4, ..rest]
/// //           ^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum ListRestPattern<'text> {
    /// No rest pattern.
    Absent,
    /// A rest pattern was given.
    Given {
        #[doc = docs!(span: "list rest pattern"; including: "`..`")]
        span: Span,
        #[doc = docs!(name: "list rest pattern capture name")]
        ident: Option<Ident<'text>>,
    },
}

/// An identifier pattern.
///
/// Uppercase and namespaced names are not allowed.
///
/// # Form examples
///
/// ```fuyu
/// a_long_name
/// _
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct IdentPattern<'text> {
    #[doc = docs!(span: "identifier")]
    pub span: Span,
    #[doc = docs!(name: "identifier")]
    pub ident: Ident<'text>,
}

/// A type constructor pattern.
///
/// # Form examples
///
/// ```fuyu
/// SomeValue
/// namespace::SomeValue
/// ConstructorWithArgs(1, x, count: 3, name: "hello", ..)
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ConstructorPattern<'text> {
    #[doc = docs!(span: "constructor"; including: "`(`", "`)`")]
    pub span: Span,
    #[doc = docs!(name: "constructor")]
    pub name: Ident<'text>,
    #[doc = docs!(patterns: "constructor")]
    pub patterns: Vec<ConstructorArgPattern<'text>>,
    #[doc = docs!(rest_pattern)]
    pub rest: bool,
}

/// An argument in a constructor.
///
/// This is part of a [`ConstructorPattern`].
///
/// # Form examples
///
/// ```fuyu
/// Con1(x)
/// //   ^
/// Con2(x:)
/// //   ^^
/// Con3(x: (4, h, z))
/// //   ^^^^^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ConstructorArgPattern<'text> {
    #[doc = docs!(span: "constructor argument")]
    pub span: Span,
    #[doc = docs!(name: "constructor argument name")]
    pub name: Option<Ident<'text>>,
    #[doc = docs!(expr: "constructor argument value")]
    pub expr: Option<Expr<'text>>,
}
