// SPDX-License-Identifier: MPL-2.0

//! Declarations are statements that can appear at the top level in a module. Currently, these are:
//!
//! - `import` declarations ([`ImportDecl`]).
//! - `const` declarations ([`ConstDecl`]).
//! - `type` declarations ([`TypeDecl`]).
//! - `fn` declarations ([`FnDecl`]).
//! - `provide` declarations ([`ProvideDecl`]).
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
    /// A constant declaration (refer to [`ConstDecl`]).
    Const(ConstDecl<'text>),
    /// A type declaration (refer to [`TypeDecl`]).
    Type(TypeDecl<'text>),
    /// A function declaration (refer to [`FnDecl`]).
    Fn(FnDecl<'text>),
    /// A provide declaration (refer to [`ProvideDecl`]).
    Provide(ProvideDecl<'text>),
}

/// A visibility modifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Visibility {
    /// Visible only within a module.
    Private,
    /// Declared with the `pub` keyword and is publically visible.
    Public,
}

/// An import declaration.
///
/// # Form examples
///
/// ```fuyu
/// import a/b/c;
/// import a/b/c::x;
/// import a/b/c::{self as m, type T, T as V, use *};
/// import a/b/c::{use ctx, use Add[_]};
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ImportDecl<'text> {
    #[doc = docs!(span: "import"; including: "`;`")]
    pub span: Span,
    #[doc = docs!(kind: "import path")]
    pub path_kind: ImportDeclPathKind,
    #[doc = docs!(import: "path")]
    pub path: &'text str,
    #[doc = docs!(import: "items")]
    pub items: Vec<ImportDeclItem<'text>>,
}

/// The path settings for an import.
///
/// This is part of an [`ImportDecl`].
///
/// # Form examples
///
/// ```fuyu
/// import a/b/c;
/// import ../a/b/c;
/// import ../../a/b/c;
/// import /a/b/c;
/// import extern a/b/c;
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImportDeclPathKind {
    /// A relative import with respect to the current file.
    ///
    /// This tracks the number of `../` at the front.
    Relative(usize),
    /// An absolute import with respect to the root of the current package.
    Absolute,
    /// An import marked as `extern`.
    External,
}

/// Items in the import list.
///
/// This is part of an [`ImportDecl`].
///
/// # Form examples
///
/// ```fuyu
/// import a/b/c::{self as m, type T, T as V, use *};
/// //             ^^^^^^^^^  ^^^^^^  ^^^^^^  ^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum ImportDeclItem<'text> {
    /// The `self` keyword.
    ModuleSelf {
        #[doc = docs!(span: "self import"; including: "rename")]
        span: Span,
        #[doc = docs!(name: "imported item")]
        rename: Option<Ident<'text>>,
    },

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

    /// Import of a provision by name with a `provide` prefix.
    NamedProvision {
        #[doc = docs!(span: "named provision"; including: "`provide`", "rename")]
        span: Span,
        #[doc = docs!(name: "item to import")]
        name: Ident<'text>,
        #[doc = docs!(name: "imported item")]
        rename: Option<Ident<'text>>,
    },

    /// Import of a provision by type with a `provide` prefix.
    ///
    /// Note that this type of import item cannot be renamed.
    TypedProvision {
        #[doc = docs!(span: "typed provision"; including: "`provide`")]
        span: Span,
        #[doc = docs!(type_name: "typed provision")]
        type_name: TypeName<'text>,
    },
}

/// A constant declaration.
///
/// # Form examples
///
/// ```fuyu
/// const magic_number: Int = 123;
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ConstDecl<'text> {
    #[doc = docs!(span: "constant binding"; including: "modifiers", "`;`")]
    pub span: Span,
    #[doc = docs!(visibility)]
    pub visibility: Visibility,
    #[doc = docs!(name: "constant binding")]
    pub ident: Ident<'text>,
    #[doc = docs!(type_name: "constant binding")]
    pub type_name: TypeName<'text>,
    #[doc = docs!(expr: "constant")]
    pub expr: Expr<'text>,
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
    #[doc = docs!(span: "type declaration"; including: "modifiers", "`;`")]
    pub span: Span,
    #[doc = docs!(visibility)]
    pub visibility: Visibility,
    #[doc = docs!(transparent)]
    pub transparent: bool,
    #[doc = docs!(type_name: "type declaration")]
    pub type_name: TypeName<'text>,
    #[doc = docs!(constructors)]
    pub constructors: Vec<TypeDeclConstructor<'text>>,
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
/// fn f(value: Triple) {}
/// fn f(named value: Triple) {}
/// fn f(use named value: Triple) {}
/// fn f(use named Triple(x, y, z): Triple) {}
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl<'text> {
    #[doc = docs!(span: "function declaration"; including: "modifiers", "`;`")]
    pub span: Span,
    #[doc = docs!(visibility)]
    pub visibility: Visibility,
    #[doc = docs!(name: "function")]
    pub ident: Ident<'text>,
    #[doc = docs!(args: "function")]
    pub args: Vec<FnDeclArg<'text>>,
    #[doc = docs!(return_type)]
    pub return_type: Option<TypeName<'text>>,
    #[doc = docs!(exprs: "function")]
    pub exprs: Vec<Expr<'text>>,
    #[doc = docs!(evaluate_last)]
    pub evaluate_last: bool,
}

/// An argument in a function declaration.
///
/// # Form examples
///
/// ```fuyu
/// fn f(value: Triple) {}
/// //  ^^^^^^^^^^^^^^
/// fn f(named value: Triple) {}
/// //  ^^^^^^^^^^^^^^^^^^^^
/// fn f(use named value: Triple) {}
/// //  ^^^^^^^^^^^^^^^^^^^^^^^^
/// fn f(use named Triple(x, y, z): Triple) {}
/// //   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FnDeclArg<'text> {
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

/// A provide declaration.
///
/// # Form examples
///
/// ```fuyu
/// provide Emphasis => Strong;
/// provide emphasis: Emphasis => Strong;
/// provide (use a: A): B => B(10 * a.0);
/// provide b_value(use a: A): B => B(10 * a.0);
/// provide (use A): B => something_that_uses();
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ProvideDecl<'text> {
    #[doc = docs!(span: "provision"; including: "modifiers", "`;`")]
    span: Span,
    #[doc = docs!(visibility)]
    pub visibility: Visibility,
    #[doc = docs!(name: "provision")]
    pub ident: Option<Ident<'text>>,
    #[doc = docs!(args: "provision")]
    pub args: Vec<ProvideDeclArg<'text>>,
    #[doc = docs!(type_name: "provision")]
    pub type_name: TypeName<'text>,
    #[doc = docs!(expr: "provisioned value")]
    pub expr: Expr<'text>,
}

/// Implicit arguments to a provide declaration.
///
/// This is part of a [`ProvideDecl`].
///
/// # Form examples
///
/// ```fuyu
/// provide (use a: A): B = B(10 * a.0);
/// //       ^^^^^^^^
/// provide (use A): B = something_that_uses_a();
/// //       ^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ProvideDeclArg<'text> {
    #[doc = docs!(span: "provision argument")]
    span: Span,
    #[doc = docs!(name: "argument")]
    ident: Option<Ident<'text>>,
    #[doc = docs!(type_name: "argument")]
    type_name: TypeName<'text>,
}
