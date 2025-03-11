// SPDX-License-Identifier: MPL-2.0

//! Declarations are statements that can appear at the top level in a module. Currently, these are:
//!
//! - `import` declarations ([`ImportDecl`]).
//! - `constant` declarations ([`ConstantDecl`]).
//! - `type` declarations ([`TypeDecl`]).
//! - `function` declarations ([`FunctionDecl`]).
//! - `proof` declarations ([`ProofDecl`]).
//!
//! Each declaration has its own type, however, they are all part of [`Decl`], which is an enum
//! that holds any type of declaration.

use crate::ast::{docs, Expr, Ident, TypeName};
use crate::parse::Span;

/// A declaration of any kind that can appear at the top level in a module.
#[derive(Clone, Debug, PartialEq)]
pub enum Decl<'text> {
    /// An import declaration (refer to [`ImportDecl`]).
    Import(ImportDecl<'text>),
    /// A constant declaration (refer to [`ConstantDecl`]).
    Const(ConstantDecl<'text>),
    /// A type declaration (refer to [`TypeDecl`]).
    Type(TypeDecl<'text>),
    /// A function declaration (refer to [`FunctionDecl`]).
    Function(FunctionDecl<'text>),
    /// A proof declaration (refer to [`ProofDecl`]).
    Proof(ProofDecl<'text>),
}

/// An attribute.
///
/// # Form examples
///
/// ```fuyu
/// @[x]
/// @[f(1, 2, 3)]
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Attribute<'text> {
    #[doc = docs!(span: "attribute")]
    pub span: Span,
    #[doc = docs!(attribute)]
    pub expr: Expr<'text>,
}

/// An import declaration.
///
/// # Form examples
///
/// ```fuyu
/// import a/b/c;
/// import a/b/c for x;
/// import a/b/c for type T, T as V, proof _;
/// import a/b/c as m for proof ctx, proof Add[_];
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ImportDecl<'text> {
    #[doc = docs!(span: "import"; including: "`;`")]
    pub span: Span,
    #[doc = docs!(kind: "import path")]
    pub path_kind: ImportDeclPathKind,
    #[doc = docs!(import: "path")]
    pub path: &'text str,
    #[doc = docs!(name: "renamed namespace")]
    pub rename: Option<Ident<'text>>,
    #[doc = docs!(import: "items")]
    pub items: Vec<ImportDeclItem<'text>>,
    #[doc = docs!(attributes: "import")]
    pub attributes: Vec<Attribute<'text>>,
}

/// The path settings for an import.
///
/// This is part of an [`ImportDecl`].
///
/// # Form examples
///
/// ```fuyu
/// import a/b/c;
/// import ./a/b/c;
/// import ../a/b/c;
/// import ../../a/b/c;
/// import /a/b/c;
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImportDeclPathKind {
    /// An import with no leading path specifier.
    Bare,
    /// A relative import with a leading path import of `./`, `../`, `../../`, and so on.
    ///
    /// This tracks the number of `../`. This is zero when the path specifier is `./`.
    Relative(usize),
    /// An absolute import with with a leading path specifier of `/`.
    Absolute,
}

/// Items in the import list.
///
/// This is part of an [`ImportDecl`].
///
/// # Form examples
///
/// ```fuyu
/// import a/b/c for type T, T as V, proof _;
/// //               ^^^^^^  ^^^^^^  ^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum ImportDeclItem<'text> {
    /// Import of a value by name.
    Value {
        #[doc = docs!(span: "value import"; including: "rename")]
        span: Span,
        #[doc = docs!(name: "item to import")]
        name: Ident<'text>,
        #[doc = docs!(name: "imported item")]
        rename: Option<Ident<'text>>,
    },

    /// A type by name with a `type` prefix.
    Type {
        #[doc = docs!(span: "type import"; including: "`type`", "rename")]
        span: Span,
        #[doc = docs!(name: "item to import")]
        name: Ident<'text>,
        #[doc = docs!(name: "imported item")]
        rename: Option<Ident<'text>>,
    },

    /// Import of a proof by name with a `proof` prefix.
    NamedProof {
        #[doc = docs!(span: "named proof"; including: "`proof`", "rename")]
        span: Span,
        #[doc = docs!(name: "item to import")]
        name: Ident<'text>,
        #[doc = docs!(name: "imported item")]
        rename: Option<Ident<'text>>,
    },

    /// Import of a proof by type with a `proof` prefix.
    ///
    /// Note that this type of import item cannot be renamed.
    TypedProof {
        #[doc = docs!(span: "typed proof"; including: "`proof`")]
        span: Span,
        #[doc = docs!(type_name: "typed proof")]
        type_name: TypeName<'text>,
    },
}

/// A constant declaration.
///
/// # Form examples
///
/// ```fuyu
/// constant magic_number: Int = 123;
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ConstantDecl<'text> {
    #[doc = docs!(span: "constant binding"; including: "`;`")]
    pub span: Span,
    #[doc = docs!(name: "constant binding")]
    pub ident: Ident<'text>,
    #[doc = docs!(type_name: "constant binding")]
    pub type_name: TypeName<'text>,
    #[doc = docs!(expr: "constant")]
    pub expr: Expr<'text>,
    #[doc = docs!(attributes: "constant")]
    pub attributes: Vec<Attribute<'text>>,
}

/// A type declaration.
///
/// # Form examples
///
/// ```fuyu
/// type Things[a, b] {
///     This,
///     That(a, value: b),
///     Other(a, b, more_things: Things[a, b]),
/// };
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TypeDecl<'text> {
    #[doc = docs!(span: "type declaration"; including: "`;`")]
    pub span: Span,
    #[doc = docs!(type_name: "type declaration")]
    pub type_name: TypeName<'text>,
    #[doc = docs!(constructors)]
    pub constructors: Vec<TypeDeclConstructor<'text>>,
    #[doc = docs!(attributes: "type")]
    pub attributes: Vec<Attribute<'text>>,
}

/// A constructor in a type declaration.
///
/// This is part of a [`TypeDecl`].
///
/// # Form examples
///
/// ```fuyu
/// type Things[a, b] {
///     This,
/// //  ^^^^
///     That(a, value: b),
/// //  ^^^^^^^^^^^^^^^^^
///     Other(a, b, more_things: Things[a, b]),
/// //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// };
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclConstructor<'text> {
    #[doc = docs!(span: "type  constructor")]
    pub span: Span,
    #[doc = docs!(name: "type constructor")]
    pub ident: Ident<'text>,
    #[doc = docs!(fields: "type constructor")]
    pub fields: Vec<TypeDeclField<'text>>,
}

/// A field of a constructor in a type declaration.
///
/// This is part of a [`TypeDecl`].
///
/// # Form examples
///
/// ```fuyu
/// type Things[a, b] {
///     This,
///     That(a, value: b),
/// //       ^  ^^^^^^^^
///     Other(a, b, more_things: Things[a, b]),
/// //        ^  ^  ^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclField<'text> {
    #[doc = docs!(span: "type constructor field")]
    pub span: Span,
    #[doc = docs!(name: "type constructor field")]
    pub name: Option<Ident<'text>>,
    #[doc = docs!(type_name: "type constructor field")]
    pub type_name: TypeName<'text>,
}

/// A function declaration.
///
/// # Form examples
///
/// ```fuyu
/// function f(value: Triple) {}
/// function f(named value: Triple) {}
/// function f(use named value: Triple) {}
/// function f(use named Triple(x, y, z): Triple) {}
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDecl<'text> {
    #[doc = docs!(span: "function declaration"; including: "`;`")]
    pub span: Span,
    #[doc = docs!(name: "function")]
    pub ident: Ident<'text>,
    #[doc = docs!(args: "function")]
    pub args: Vec<FunctionDeclArg<'text>>,
    #[doc = docs!(return_type)]
    pub return_type: Option<TypeName<'text>>,
    #[doc = docs!(exprs: "function")]
    pub exprs: Option<Vec<Expr<'text>>>,
    #[doc = docs!(evaluate_last)]
    pub evaluate_last: bool,
    #[doc = docs!(attributes: "function")]
    pub attributes: Vec<Attribute<'text>>,
}

/// An argument in a function declaration.
///
/// # Form examples
///
/// ```fuyu
/// function f(value: Triple) {}
/// //         ^^^^^^^^^^^^^^
/// function f(named value: Triple) {}
/// //         ^^^^^^^^^^^^^^^^^^^^
/// function f(use named value: Triple) {}
/// //         ^^^^^^^^^^^^^^^^^^^^^^^^
/// function f(use named Triple(x, y, z): Triple) {}
/// //          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclArg<'text> {
    #[doc = docs!(span: "function declaration argument")]
    pub span: Span,
    #[doc = docs!(using)]
    pub using: bool,
    #[doc = docs!(name: "argument")]
    pub ident: Option<Ident<'text>>,
    #[doc = docs!(pattern: "argument")]
    pub pattern: Expr<'text>,
    #[doc = docs!(type_name: "argument")]
    pub type_name: TypeName<'text>,
}

/// A proof declaration.
///
/// # Form examples
///
/// ```fuyu
/// proof Emphasis = Strong;
/// proof emphasis: Emphasis = Strong;
/// proof (use a: A): B = B(a.0);
/// proof b_value(use a: A): B = B(a.0);
/// proof (use A): B = B(1000);
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ProofDecl<'text> {
    #[doc = docs!(span: "proof"; including: "`;`")]
    span: Span,
    #[doc = docs!(name: "proof")]
    pub ident: Option<Ident<'text>>,
    #[doc = docs!(args: "proof")]
    pub args: Vec<ProofDeclArg<'text>>,
    #[doc = docs!(type_name: "proof")]
    pub type_name: TypeName<'text>,
    #[doc = docs!(expr: "proven value")]
    pub expr: Expr<'text>,
    #[doc = docs!(attributes: "proof")]
    pub attributes: Vec<Attribute<'text>>,
}

/// Implicit arguments to a proof declaration.
///
/// This is part of a [`ProofDecl`].
///
/// # Form examples
///
/// ```fuyu
/// proof (use a: A): B = B(a.0);
/// //     ^^^^^^^^
/// proof (use A): B = B(1000);
/// //     ^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ProofDeclArg<'text> {
    #[doc = docs!(span: "proof argument")]
    span: Span,
    #[doc = docs!(name: "argument")]
    ident: Option<Ident<'text>>,
    #[doc = docs!(type_name: "argument")]
    type_name: TypeName<'text>,
}
