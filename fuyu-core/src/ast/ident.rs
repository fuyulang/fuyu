// SPDX-License-Identifier: MPL-2.0

//! These are identifiers that representing bindings.

use crate::ast::docs;
use crate::parse::Span;

/// The kind of an identifier depending on its casing and characters.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IdentKind {
    /// A lower case identifier.
    Lower,
    /// An uppercase identifier.
    Upper,
    /// The `_`.
    Discard,
}

/// An identifier.
///
/// # Form examples
///
/// ```fuyu
/// ident
/// NameOfAType
/// namespace::ident
/// long_module_name::LongConstructorName
/// _
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Ident<'text> {
    #[doc = docs!(span: "identifier"; including: "namespace")]
    pub span: Span,
    #[doc = docs!(name: "namespace")]
    pub namespace: Option<&'text str>,
    #[doc = docs!(name: "identifier")]
    pub ident: &'text str,
    #[doc = docs!(kind: "identifier")]
    pub kind: IdentKind,
}

/// The name of a type that appears in type position.
///
/// # Form examples
///
/// ```fuyu
/// SomeType
/// namespace::SomeType
/// a
/// (a, Int, namespace::SomeType[a, b])
/// a[b, c]
/// _
/// function(a, b)
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum TypeName<'text> {
    /// A named type that is either a concrete or parametric name.
    Named {
        #[doc = docs!(span: "type name"; including: "type parameters")]
        span: Span,
        #[doc = docs!(name: "type name")]
        ident: Ident<'text>,
        #[doc = docs!(type_params)]
        params: Vec<Self>,
    },
    /// A tuple type.
    Tuple {
        #[doc = docs!(span: "type name tuple"; including: "`(`", "`)`")]
        span: Span,
        #[doc = docs!(type_params)]
        params: Vec<Self>,
    },
    /// A function type.
    Function {
        #[doc = docs!(span: "type name tuple"; including: "`function(`", "`)`", "`->`")]
        span: Span,
        #[doc = docs!(type_names: "function arguments")]
        params: Vec<Self>,
        #[doc = docs!(return_type)]
        return_type: Option<Box<Self>>,
    },
}
