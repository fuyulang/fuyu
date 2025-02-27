// SPDX-License-Identifier: MPL-2.0

//! Expressions can be evaluated to obtain a value. There are several kinds of expressions:
//!
//! - Integer literals ([`IntExpr`]).
//! - Floating-point literals ([`FloatExpr`]).
//! - String literals ([`StringExpr`]).
//! - Tuples ([`TupleExpr`]).
//! - Lists ([`ListExpr`]).
//! - Identifiers ([`IdentExpr`]).
//! - The `immediate` keyword ([`ImmediateExpr`]).
//! - Block expressions ([`BlockExpr`]).
//! - Anonymous functions ([`FnExpr`]).
//! - Function calls ([`CallExpr`]).
//! - Binary infix operations ([`InfixExpr`]).
//! - Unary prefix operations ([`PrefixExpr`]).
//! - Computation pipelines ([`PipelineExpr`]).
//! - Parenthesized expressions ([`ParenExpr`]).
//! - If expressions ([`IfExpr`]).
//! - Match expressions ([`MatchExpr`]).
//! - Try expressions ([`TryExpr`]).
//! - Let expressions ([`LetExpr`]).
//! - Require expressions ([`RequireExpr`]).
//! - Proof expressions ([`ProofExpr`]).
//! - Return expressions ([`ReturnExpr`]).
//! - Panic expressions ([`PanicExpr`]).
//! - Todo expressions ([`TodoExpr`]).
//! - Unimplemented expressions ([`UnimplementedExpr`]).
//! - Unreachable expressions ([`UnreachableExpr`]).
//!
//! Expressions can generally be composed, thus, the [`Expr`] enum represents any expression.

use crate::ast::{docs, Ident, Pattern, Trivia, TypeName};
use crate::parse::Span;

/// An expression that can appear in the source.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'text> {
    /// An integer literal (refer to [`IntExpr`]).
    Int(IntExpr<'text>),
    /// A floating-point literal (refer to [`FloatExpr`]).
    Float(FloatExpr<'text>),
    /// A string literal (refer to [`StringExpr`]).
    String(StringExpr<'text>),
    /// A tuple (refer to [`TupleExpr`]).
    Tuple(TupleExpr<'text>),
    /// A list (refer to [`ListExpr`]).
    List(ListExpr<'text>),
    /// An identifier (refer to [`IdentExpr`]).
    Ident(IdentExpr<'text>),
    /// The `immediate` keyword (refer to [`ImmediateExpr`]).
    Immediate(ImmediateExpr),
    /// A block expression (refer to [`BlockExpr`]).
    Block(BlockExpr<'text>),
    /// Anonymous function (refer to [`FnExpr`]).
    Fn(FnExpr<'text>),
    /// Function call (refer to [`CallExpr`]).
    Call(CallExpr<'text>),
    /// A binary infix operation (refer to [`InfixExpr`]).
    Infix(InfixExpr<'text>),
    /// A unary prefix operation (refer to [`PrefixExpr`]).
    Prefix(PrefixExpr<'text>),
    /// A field access (refer to [`FieldAccessExpr`]).
    FieldAccess(FieldAccessExpr<'text>),
    /// A computation pipeline (refer to [`PipelineExpr`]).
    Pipeline(PipelineExpr<'text>),
    /// A parenthesized expression (refer to [`ParenExpr`]).
    Paren(ParenExpr<'text>),
    /// An if expression (refer to [`IfExpr`]).
    If(IfExpr<'text>),
    /// A match expression (refer to [`MatchExpr`]).
    Match(MatchExpr<'text>),
    /// A try expression (refer to [`TryExpr`]).
    Try(TryExpr<'text>),
    /// A let expression (refer to [`LetExpr`]).
    Let(LetExpr<'text>),
    /// A require expression (refer to [`RequireExpr`]).
    Require(RequireExpr<'text>),
    /// A proof expression (refer to [`ProofExpr`]).
    Proof(ProofExpr<'text>),
    /// A return expression (refer to [`ReturnExpr`]).
    Return(ReturnExpr<'text>),
    /// A panic expression (refer to [`PanicExpr`]).
    Panic(PanicExpr<'text>),
    /// A todo expression (refer to [`TodoExpr`]).
    Todo(TodoExpr<'text>),
    /// An unimplemented expression (refer to [`UnimplementedExpr`]).
    Unimplemented(UnimplementedExpr<'text>),
    /// An unreachable expression (refer to [`UnreachableExpr`]).
    Unreachable(UnreachableExpr<'text>),
}

/// An integer literal.
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
pub struct IntExpr<'text> {
    #[doc = docs!(span: "integer")]
    pub span: Span,
    #[doc = docs!(literal: "integer")]
    pub literal: &'text str,
}

/// An floating-point literal.
///
/// # Form examples
///
/// ```fuyu
/// 0.0
/// 1_2_.3_4_
/// 56.89e-1_9
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FloatExpr<'text> {
    #[doc = docs!(span: "float")]
    pub span: Span,
    #[doc = docs!(literal: "float")]
    pub literal: &'text str,
}

/// A string literal.
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
pub struct StringExpr<'text> {
    #[doc = docs!(span: "string"; including: "`r`", "`#`", "`\"`")]
    pub span: Span,
    #[doc = docs!(literal: "string")]
    pub literal: &'text str,
    #[doc = docs!(literal value: "string")]
    pub value: String,
}

/// A tuple.
///
/// # Form examples
///
/// ```fuyu
/// ()
/// (True,)
/// (1, 2, "hello")
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TupleExpr<'text> {
    #[doc = docs!(span: "tuple"; including: "`(`", "`)`")]
    pub span: Span,
    #[doc = docs!(exprs: "tuple")]
    pub exprs: Vec<Expr<'text>>,
}

/// A list.
///
/// # Form examples
///
/// ```fuyu
/// []
/// ["hello"]
/// [1, 2, 3, 4, 5]
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ListExpr<'text> {
    #[doc = docs!(span: "list"; including: "`[`", "`]`")]
    pub span: Span,
    #[doc = docs!(exprs: "list")]
    pub exprs: Vec<Expr<'text>>,
}

/// An identifier.
///
/// # Form examples
///
/// ```fuyu
/// x
/// List
/// a_long_name
/// namespace::name
/// a_long_namespace::ALongTypeName
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct IdentExpr<'text> {
    #[doc = docs!(span: "identifier")]
    pub span: Span,
    #[doc = docs!(name: "identifier")]
    pub ident: Ident<'text>,
}

/// The `immediate` keyword.
///
/// # Form examples
///
/// ```fuyu
/// immediate
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ImmediateExpr {
    #[doc = docs!(span: "`immediate` keyword")]
    pub span: Span,
}

/// A block expression.
///
/// # Form examples
///
/// ```fuyu
/// { True }
/// {
///   io::println("hello");
///   io::println("world");
/// }
/// { 1; 2; 3 }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct BlockExpr<'text> {
    #[doc = docs!(span: "block"; including: "`{`", "`}`")]
    pub span: Span,
    #[doc = docs!(exprs: "block")]
    pub exprs: Vec<Expr<'text>>,
    #[doc = docs!(evaluate_last)]
    pub evaluate_last: bool,
}

/// Anonymous function.
///
/// # Form examples
///
/// ```fuyu
/// fn() {}
/// fn(x) { x }
/// fn(use ctx: Context, x: Int) -> Int { ctx.next(x) }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FnExpr<'text> {
    #[doc = docs!(span: "function")]
    pub span: Span,
    #[doc = docs!(args: "function")]
    pub args: Vec<FnArg<'text>>,
    #[doc = docs!(return_type)]
    pub return_type: Option<TypeName<'text>>,
    #[doc = docs!(exprs: "function")]
    pub exprs: Box<Expr<'text>>,
}

/// An argument in an anonymous function.
///
/// This is part of a [`FnExpr`].
///
/// # Form examples
///
/// ```fuyu
/// fn(x) { x }
/// // ^
/// fn(use ctx: Context, x: Int) -> Int { ctx.next(x) }
/// // ^^^^^^^^^^^^^^^^  ^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FnArg<'text> {
    #[doc = docs!(span: "anonymous function argument")]
    pub span: Span,
    #[doc = docs!(using)]
    pub using: bool,
    #[doc = docs!(name: "argument")]
    pub name: Option<Ident<'text>>,
    #[doc = docs!(pattern: "argument")]
    pub pattern: Expr<'text>,
    #[doc = docs!(type_name: "argument")]
    pub type_name: Option<TypeName<'text>>,
}

/// Function call.
///
/// # Form examples
///
/// ```fuyu
/// f()
/// f {}
/// f(x)
/// f(x:)
/// g(x: 1 + 2)
/// h(use ctx) { |b| ctx.count + b }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct CallExpr<'text> {
    #[doc = docs!(span: "function call"; including: "`(`", "arguments", "`)`", "block")]
    pub span: Span,
    #[doc = docs!(expr: "function to be called")]
    pub function: Box<Expr<'text>>,
    #[doc = docs!(args: "function call")]
    pub args: Vec<CallArg<'text>>,
    #[doc = docs!(block_arg)]
    pub block: Option<FnCallBlock<'text>>,
}

/// An argument to a function call.
///
/// This is part of a [`CallExpr`].
///
/// # Form examples
///
/// ```fuyu
/// something(x)
/// //        ^
/// something_else(x:)
/// //             ^^
/// another_thing(x: 1 + 2)
/// //            ^^^^^^^^
/// with_context(use ctx) { |b| ctx.count + b }
/// //           ^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct CallArg<'text> {
    #[doc = docs!(span: "function call argument"; including: "`use`")]
    pub span: Span,
    #[doc = docs!(using)]
    pub using: bool,
    #[doc = docs!(name: "argument name")]
    pub name: Option<Ident<'text>>,
    #[doc = docs!(expr: "argument value")]
    pub expr: Option<Expr<'text>>,
}

/// A block function appended to a function call.
///
/// This is part of a [`CallExpr`].
///
/// # Form examples
///
/// ```fuyu
/// with { 1 + 2 }
/// //   ^^^^^^^^^
/// using(x) { |b| b + x }
/// //       ^^^^^^^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FnCallBlock<'text> {
    #[doc = docs!(span: "block"; including: "`{`", "`}`")]
    pub span: Span,
    #[doc = docs!(args: "block")]
    pub args: Vec<BlockArg<'text>>,
    #[doc = docs!(exprs: "block")]
    pub exprs: Vec<Expr<'text>>,
    #[doc = docs!(evaluate_last)]
    pub evaluate_last: bool,
}

/// An argument to a block function.
///
/// This is part of a [`CallExpr`].
///
/// # Form examples
///
/// ```fuyu
/// action() { |a, b| a + b }
/// //          ^  ^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct BlockArg<'text> {
    #[doc = docs!(span: "function call argument with value"; including: "`use`")]
    pub span: Span,
    #[doc = docs!(pattern: "argument")]
    pub pattern: Pattern<'text>,
}

/// A binary infix operation.
///
/// # Form examples
///
/// ```fuyu
/// a + b
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct InfixExpr<'text> {
    #[doc = docs!(span: "infix operation")]
    pub span: Span,
    #[doc = docs!(expr: "left-hand side")]
    pub lhs: Box<Expr<'text>>,
    #[doc = docs!(kind: "operator")]
    pub op: InfixOp,
    #[doc = docs!(expr: "right-hand side")]
    pub rhs: Box<Expr<'text>>,
}

/// A binary infix operator.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InfixOp {
    /// `and`.
    And,
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
    /// `or`.
    Or,
    /// `%`.
    Percent,
    /// `+`.
    Plus,
    /// `/`.
    Slash,
    /// `/=`.
    SlashEq,
    /// `*`.
    Star,
    /// `**`.
    StarStar,
}

/// A unary prefix operation.
///
/// ```fuyu
/// -height
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct PrefixExpr<'text> {
    #[doc = docs!(span: "prefix operation")]
    pub span: Span,
    #[doc = docs!(kind: "operator")]
    pub op: PrefixOp,
    #[doc = docs!(expr: "right-hand side")]
    pub expr: Box<Expr<'text>>,
}

/// A unary prefix operator.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PrefixOp {
    /// `..`.
    DotDot,
    /// `-`.
    Minus,
    /// `not`.
    Not,
}

/// A field access.
///
/// # Form examples
///
/// ```fuyu
/// person.name
/// data.0
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccessExpr<'text> {
    #[doc = docs!(span: "field access")]
    pub span: Span,
    #[doc = docs!(expr: "field access")]
    pub expr: Box<Expr<'text>>,
    #[doc = docs!(expr: "field name")]
    pub field: Box<Expr<'text>>,
}

/// A computation pipeline.
///
/// # Form examples
///
/// ```fuyu
/// x | f | g(3) | h("q", immedate)
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct PipelineExpr<'text> {
    #[doc = docs!(span: "pipeline")]
    pub span: Span,
    #[doc = docs!(expr: "left-side of the first `|`")]
    pub lhs: Box<Expr<'text>>,
    #[doc = docs!(exprs: "pipeline")]
    pub stages: Vec<Expr<'text>>,
}

/// A parenthesized expression.
///
/// A proper AST would not track this, however, this AST does so that the positions of comments
/// may be preserved.
///
/// # Form examples
///
/// ```fuyu
/// (1 + 2)
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ParenExpr<'text> {
    #[doc = docs!(span: "parenthesized expression"; including: "`(`", "`)`")]
    pub span: Span,
    #[doc = docs!(expr: "parenthesized group")]
    pub expr: Box<Expr<'text>>,
}

/// An if expression.
///
/// # Form examples
///
/// ```fuyu
/// if {
///     x > 100 => "big",
///     x > 10 => "medium",
///     _ => "small",
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr<'text> {
    #[doc = docs!(span: "if expression")]
    pub span: Span,
    #[doc = docs!(clauses: "`if` branches")]
    pub clauses: Vec<IfClause<'text>>,
}

/// A clause in the body of an if expression.
///
/// This is part of an [`IfExpr`].
///
/// # Form examples
///
/// ```fuyu
/// if {
///     x > 100 => "big",
/// //  ^^^^^^^^^^^^^^^^
///     x > 10 => "medium",
/// //  ^^^^^^^^^^^^^^^^^^
///     _ => "small",
/// //  ^^^^^^^^^^^^
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct IfClause<'text> {
    #[doc = docs!(span: "if clause")]
    pub span: Span,
    #[doc = docs!(condition: "if clause")]
    pub condition: Expr<'text>,
    #[doc = docs!(expr: "if clause body")]
    pub expr: Expr<'text>,
}

/// A match expression.
///
/// # Form examples
///
/// ```fuyu
/// value match {
///     Some(x) if x > 10 => x,
///     Some(x) => 2 * x,
///     _ => 0,
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr<'text> {
    #[doc = docs!(span: "match expression")]
    pub span: Span,
    #[doc = docs!(expr: "match argument to deconstruct")]
    pub expr: Box<Expr<'text>>,
    #[doc = docs!(clauses: "`match` arms")]
    pub clauses: Vec<MatchClause<'text>>,
}

/// A clause in the body of a match expression.
///
/// This is part of a [`MatchExpr`].
///
/// # Form examples
///
/// ```fuyu
/// value match {
///     Some(x) if x > 10 => x,
/// //  ^^^^^^^^^^^^^^^^^^^^^^
///     Some(x) => 2 * x,
/// //  ^^^^^^^^^^^^^^^^
///     _ => 0,
/// //  ^^^^^^
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct MatchClause<'text> {
    #[doc = docs!(span: "match clause")]
    pub span: Span,
    #[doc = docs!(pattern: "match clause")]
    pub pattern: Pattern<'text>,
    #[doc = docs!(condition: "match clause guard")]
    pub condition: Option<Expr<'text>>,
    #[doc = docs!(expr: "match clause body")]
    pub expr: Box<Expr<'text>>,
}

/// A try expression.
///
/// # Form examples
///
/// ```fuyu
/// try value
/// try f(1, 2, 3)
/// x | try g("abc")
/// //  ^^^^^^^^^^^^
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TryExpr<'text> {
    #[doc = docs!(span: "try")]
    pub span: Span,
    #[doc = docs!(expr: "try")]
    pub expr: Box<Expr<'text>>,
}

/// A return expression.
///
/// # Form examples
///
/// ```fuyu
/// return     // Empty.
/// return 123 // With expression.
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct ReturnExpr<'text> {
    #[doc = docs!(span: "return")]
    pub span: Span,
    #[doc = docs!(expr: "return")]
    pub expr: Option<Box<Expr<'text>>>,
}

/// A let expression.
///
/// # Form examples
///
/// ```fuyu
/// let a = 5
/// let (x, y) = (3, 4)
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct LetExpr<'text> {
    #[doc = docs!(span: "let binding")]
    pub span: Span,
    #[doc = docs!(pattern: "let binding")]
    pub pattern: Pattern<'text>,
    #[doc = docs!(type_name: "let binding")]
    pub type_name: Option<TypeName<'text>>,
    #[doc = docs!(expr: "let binding")]
    pub expr: Box<Expr<'text>>,
}

/// A require expression.
///
/// # Form examples
///
/// ```fuyu
/// require a = 5
/// require (x, y) = (3, 4)
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct RequireExpr<'text> {
    #[doc = docs!(span: "require binding")]
    pub span: Span,
    #[doc = docs!(pattern: "require binding")]
    pub pattern: Pattern<'text>,
    #[doc = docs!(type_name: "require binding")]
    pub type_name: Option<TypeName<'text>>,
    #[doc = docs!(expr: "require binding")]
    pub expr: Box<Expr<'text>>,
}

/// A proof expression.
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
pub struct ProofExpr<'text> {
    #[doc = docs!(span: "proof")]
    span: Span,
    #[doc = docs!(name: "proof")]
    pub ident: Option<Ident<'text>>,
    #[doc = docs!(args: "proof")]
    pub args: Vec<ProofArg<'text>>,
    #[doc = docs!(type_name: "proof")]
    pub type_name: TypeName<'text>,
    #[doc = docs!(expr: "proven value")]
    pub expr: Box<Expr<'text>>,
}

/// Implicit arguments to a proof declaration.
///
/// This is part of a [`ProofExpr`].
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
pub struct ProofArg<'text> {
    #[doc = docs!(span: "proof argument")]
    span: Span,
    #[doc = docs!(name: "argument")]
    ident: Option<Ident<'text>>,
    #[doc = docs!(type_name: "argument")]
    type_name: TypeName<'text>,
}

/// A panic expression.
///
/// # Form examples
///
/// ```fuyu
/// panic
/// panic "with a message"
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct PanicExpr<'text> {
    #[doc = docs!(span: "panic")]
    pub span: Span,
    #[doc = docs!(expr: "panic")]
    pub expr: Option<Box<Expr<'text>>>,
}

/// A todo expression.
///
/// # Form examples
///
/// ```fuyu
/// todo
/// todo "with a message"
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TodoExpr<'text> {
    #[doc = docs!(span: "todo")]
    pub span: Span,
    #[doc = docs!(expr: "todo")]
    pub expr: Option<Box<Expr<'text>>>,
}

/// A unimplemented expression.
///
/// # Form examples
///
/// ```fuyu
/// unimplemented
/// unimplemented "with a message"
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct UnimplementedExpr<'text> {
    #[doc = docs!(span: "unimplemented")]
    pub span: Span,
    #[doc = docs!(expr: "unimplemented")]
    pub expr: Option<Box<Expr<'text>>>,
}

/// A unreachable expression.
///
/// # Form examples
///
/// ```fuyu
/// unreachable
/// unreachable "with a message"
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct UnreachableExpr<'text> {
    #[doc = docs!(span: "unreachable")]
    pub span: Span,
    #[doc = docs!(expr: "unreachable")]
    pub expr: Option<Box<Expr<'text>>>,
}
