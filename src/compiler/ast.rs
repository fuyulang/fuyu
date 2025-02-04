// SPDX-License-Identifier: MPL-2.0

// TODO: Add `Option<Type>` into each node.
// TODO: Rename type_ to something. (typ? ty?)

use num_bigint::BigInt;

use super::text::ByteIdx;

// TODO: Move this.
/// A span in the source text.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Span {
    /// Inclusive start index.
    pub start: ByteIdx,
    /// Exclusive end index.
    pub end: ByteIdx,
}

/// TODO: Docs.
#[derive(Clone, Debug, PartialEq)]
pub struct ModuleAst {
    // TODO: Better fields (should this have separate import, export, let, fn, type, use, and provide lists?).
    nodes: Vec<Ast>,
}

/// This is used to provide consistent documentation across the types defined in this file.
///
/// The macro is useful since the same documentation is repeated several times.
macro_rules! doc {
    // Comments.
    // TODO: Make this more specific?
    (comment) => {
        "Comments that belong to the node.\n"
    };
    // Literal.
    (index: $about:expr) => {
        concat!("The index of the ", $about, ".\n")
    };
    // Literal.
    (literal: $about:expr) => {
        concat!("The value of the ", $about, " literal.\n")
    };
    // Span.
    (span: $about:expr) => {
        concat!("The span of the ", $about, ".\n")
    };
    (span: $about:expr; including: $($items:expr),+ $(,)?) => {
        concat!(
            doc!(span: $about),
            "\n",
            concat!("This includes the ", doc!(@list: $($items),+), ".\n"),
        )
    };
    // The `@list` takes a list of strings and formats them in proper English.
    (@list: $a:expr) => {
        $a
    };
    (@list: $a:expr, $b:expr $(,)?) => {
        concat!($a, " and ", $b)
    };
    (@list: $a:expr, $b:expr, $c:expr $(,)?) => {
        concat!($a, ", ", $b, ", and ", $c)
    };
    (@list: $a:expr, $($vs:expr),+ $(,)?) => {
        concat!($a, ", ", doc!(@list: $($vs),+))
    };
}

// TODO: Should this be changed to `Expr`?
/// Abstract syntax tree (AST).
///
/// The variants represent nodes that make up the AST.
#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    /// An integer literal.
    Int {
        #[doc = doc!(span: "integer")]
        span: Span,
        #[doc = doc!(literal: "integer")]
        value: BigInt,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A floating-point literal.
    Float {
        #[doc = doc!(span: "float")]
        span: Span,
        #[doc = doc!(literal: "float")]
        value: f64, // TODO: Should the float be stored in some lossless format?
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A string literal.
    String {
        #[doc = doc!(span: "string"; including: r#"leading `"`, `"""`, or `r#"`"#, r#"trailing `"` or `"""`"#)]
        span: Span,
        #[doc = doc!(literal: "string")]
        value: String,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A byte string literal.
    ByteString {
        #[doc = doc!(span: "byte string"; including: r#"leading `b"` or `br#"`"#, r#"trailing `"`"#)]
        span: Span,
        #[doc = doc!(literal: "byte string")]
        value: Vec<u8>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A tuple.
    Tuple {
        #[doc = doc!(span: "tuple"; including: "`(`", "`)`")]
        span: Span,
        /// The expressions that make up the items of tuple.
        exprs: Vec<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A list.
    List {
        #[doc = doc!(span: "list"; including: r"`[`", r"`]`")]
        span: Span,
        /// The expressions that make up the items of list.
        exprs: Vec<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// The name of a binding.
    Name {
        #[doc = doc!(span: "name"; including: "namespace", "`::`", "name")]
        span: Span,
        /// The namespace of the name.
        namespace: Option<Namespace>,
        #[doc = doc!(index: "name (which differs from `span.start` when a namespace is present)")]
        name_idx: ByteIdx,
        /// The kind of the name.
        kind: NameKind,
        // TODO: Add the name as a string?
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// The `immediate` keyword.
    Immediate {
        #[doc = doc!(span: "`immediate` keyword")]
        span: Span,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A function declaration.
    Function {
        #[doc = doc!(span: "function"; including: "leading `fn`", "trailing `}` of the body")]
        span: Span,
        // TODO: Add name_span.
        name: Option<String>,
        #[doc = doc!(span: "arguments"; including: "`(`", "`)`")]
        args_span: Span,
        /// Arguments to the function.
        args: Vec<FunctionArg>,
        /// Return type information.
        return_annotation: Option<FunctionReturnAnnotation>,
        /// Body the function.
        body: Box<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A call to a function.
    Call {
        #[doc = doc!(
            span: "function call";
            including: "function name (or expression)", "`(`", "arguments", "`)`", "block",
        )]
        span: Span,
        /// TODO Docs.
        function: Box<Self>,
        #[doc = doc!(span: "arguments"; including: "`(`", "`)`")]
        args_span: Option<Span>,
        /// TODO Docs.
        args: Vec<Self>,
        /// TODO Docs.
        block_arg: Option<Box<Self>>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    // TODO: Extract this and make separate types for block expressions (no args) and block functions (with args)?
    /// A block expression.
    Block {
        #[doc = doc!(span: "block"; including: r"`{`", r"`}`")]
        span: Span,
        #[doc = doc!(span: "arguments"; including: "leading `|`", "trailing `|`")]
        args_span: Option<Span>,
        /// The arguments to the block.
        args: Vec<FunctionArg>,
        // TODO: Rename this to exprs?
        /// The statements that make up the body of the function.
        stmts: Vec<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A binary infix operation.
    Infix {
        #[doc = doc!(
            span: "infix operation";
            including: "left-hand side", "operator", "right-hand side",
        )]
        span: Span,
        /// Left-hand side of the operation.
        lhs: Box<Self>,
        #[doc = doc!(index: "operator")]
        op_idx: ByteIdx,
        /// The operator.
        op: InfixOp,
        /// Right-hand side of the operation.
        rhs: Box<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A unary prefix operation.
    Prefix {
        #[doc = doc!(span: "prefix operation"; including: "operator", "right-hand side")]
        span: Span,
        /// The operator.
        op: PrefixOp,
        /// Right-hand side of the operation.
        expr: Box<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    // TODO: Make this store all stages of a pipeline in a Vec<Self>.
    /// A computation pipeline.
    Pipeline {
        #[doc = doc!(
            span: "pipeline";
            including: "left-hand side", "`|`", "right-hand side",
        )]
        span: Span,
        /// Left-hand side of the operation.
        lhs: Box<Self>,
        #[doc = doc!(index: "`|`")]
        pipe_idx: ByteIdx,
        /// Right-hand side of the operation that the `lhs` is piped into.
        rhs: Box<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A parenthesized expression.
    ///
    /// A proper AST would not track this, however, this AST does so that the positions of comments
    /// may be preserved.
    Parentheses {
        #[doc = doc!(span: "parenthesized expression"; including: "`(`", "`)`")]
        span: Span,
        /// The expression in the parentheses.
        expr: Box<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// An if expression.
    If {
        #[doc = doc!(span: "if expression"; including: "`if`", "trailing `}`")]
        span: Span,
        #[doc = doc!(index: "`{`")]
        clauses_idx: ByteIdx,
        /// The clauses to branch on.
        clauses: Vec<IfClause>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A match-with expression.
    Match {
        #[doc = doc!(span: "match expression"; including: "`match`", "trailing `}`")]
        span: Span,
        /// The value to deconstruct as a pattern match.
        value: Box<Self>,
        #[doc = doc!(index: "`with`")]
        with_idx: ByteIdx,
        #[doc = doc!(index: "`{`")]
        clauses_idx: ByteIdx,
        /// The clauses to match on.
        clauses: Vec<MatchClause>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A return expression.
    Return {
        #[doc = doc!(span: "return"; including: "`return`", "right-hand expression")]
        span: Span,
        /// The expression to return.
        expr: Box<Self>,
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// An import expression.
    Import {
        #[doc = doc!(span: "import"; including: "leading `import`", "path and items")]
        span: Span,
        // TODO: Add fields.
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// An export expression.
    Export {
        #[doc = doc!(span: "export"; including: "leading `export`", "path and items")]
        span: Span,
        // TODO: Add fields.
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A type declaration.
    Type {
        #[doc = doc!(
            span: "type declaration";
            including: "leading `export`, `nonexhaustive`, or `type`", "trailing `}`",
        )]
        span: Span,
        // TODO: Add fields.
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A let expression.
    Let {
        #[doc = doc!(
            span: "let binding";
            including: "leading `export` or `let`", "trailing expression",
        )]
        span: Span,
        // TODO: Add fields.
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A provide expression.
    Provide {
        #[doc = doc!(span: "provision"; including: "leading `export` or `provide`", "trailing expression")]
        span: Span,
        // TODO: Add fields.
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },

    /// A use expression.
    Use {
        #[doc = doc!(span: "use"; including: "`use`", "expression")]
        span: Span,
        // TODO: Add fields.
        #[doc = doc!(comment)]
        comments: Option<Comments>,
    },
}

/// TODO Docs.
#[derive(Clone, Debug, PartialEq)]
pub struct Comments {
    // TODO: Add fields.
}

/// The kind of a name depending on its casing and charaacters.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NameKind {
    /// A lower case name.
    Lower,
    /// An uppercase name.
    Upper,
    /// The `_`.
    Discard,
}

/// A binary infix operator.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InfixOp {
    /// `&&`.
    AmpAmp,
    /// `!=`.
    BangEq,
    /// `.`.
    Dot,
    /// `..`.
    DotDot,
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
    /// `%`.
    Percent,
    /// `||`.
    PipePipe,
    /// `+`.
    Plus,
    /// `/`.
    Slash,
    /// `*`.
    Star,
    /// `**`.
    StarStar,
}

/// A unary prefix operator.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PrefixOp {
    /// `!`.
    Bang,
    /// `-`.
    Minus,
}

/// A namespace prefix.
#[derive(Clone, Debug, PartialEq)]
pub struct Namespace {
    #[doc = doc!(span: "namespace"; including: "namespace", "`::`")]
    pub span: Span,
    // TODO: add the namespace as a string.
    #[doc = doc!(index: "`::`")]
    pub sep_idx: ByteIdx,
}

/// An argument in a function declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionArg {
    #[doc = doc!(span: "function declaration argument"; including: "name", "pattern", "type")]
    pub span: Span,
    // TODO: Add use keyword.
    /// The argument name.
    pub name: Option<Ast>, // TODO: String?
    /// The argument patter (which is the main part of the argument).
    pub pattern: Ast,
    /// The type of the argument.
    pub type_annotation: Option<FunctionArgTypeAnnotation>,
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

/// The type of an argument in a function declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionArgTypeAnnotation {
    #[doc = doc!(span: "function argument type annotation"; including: "`:`", "type")]
    pub span: Span,
    /// The type.
    pub type_: Ast,
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

/// The return type annotation of a function declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionReturnAnnotation {
    #[doc = doc!(span: "function return type"; including: "`->`", "type")]
    pub span: Span,
    /// The type.
    pub type_: Box<Ast>,
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

/// An argument to a function call.
#[derive(Clone, Debug, PartialEq)]
pub struct CallArg {
    #[doc = doc!(span: "function call argument"; including: "name", "value")]
    pub span: Span,
    // TODO: Add use keyword.
    /// The name of the argument.
    pub name: Option<CallArgName>,
    /// The value of the argument.
    pub expr: Ast,
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

// TODO: Merge with CallArg as the tuple (ByteIdx, String)?
/// The name of an argument in a function call.
#[derive(Clone, Debug, PartialEq)]
pub struct CallArgName {
    #[doc = doc!(span: "function call argument name"; including: "name", "`:`")]
    pub span: Span, // TODO: Currently the colon is at the last index.
    /// The name.
    pub name: Ast, // TODO: String?
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

/// A clause in the body of an if expression.
#[derive(Clone, Debug, PartialEq)]
pub struct IfClause {
    #[doc = doc!(span: "if clause"; including: "pattern", "`=>`", "body")]
    pub span: Span,
    /// The condition to evaluate.
    pub condition: Ast,
    #[doc = doc!(index: "`=>`")]
    pub arrow_idx: ByteIdx,
    /// The body to evaluate when the condition is satisfied.
    pub body: Ast,
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

/// A clause in the body of a match expression.
#[derive(Clone, Debug, PartialEq)]
pub struct MatchClause {
    #[doc = doc!(span: "match clause"; including: "pattern", "`=>`", "body")]
    pub span: Span,
    /// The pattern to check.
    pub pattern: Box<Ast>,
    /// The guard condition.
    pub guard: Option<MatchGuard>,
    #[doc = doc!(index: "`=>`")]
    pub arrow_idx: ByteIdx,
    /// The body to evaluate when the pattern is matched.
    pub body: Box<Ast>,
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

/// A clause in the body of a match expression.
#[derive(Clone, Debug, PartialEq)]
pub struct MatchGuard {
    #[doc = doc!(span: "match clause"; including: "`if`", "condition")]
    pub span: Span,
    /// The guard condition.
    pub condition: Ast,
    #[doc = doc!(comment)]
    pub comments: Option<Comments>,
}

// TODO: Move this to a different file?
impl Ast {
    /// TODO: Docs
    pub fn span(&self) -> Span {
        todo!()
    }
}
