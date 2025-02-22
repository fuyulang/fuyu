// SPDX-License-Identifier: MPL-2.0

/// This is used to provide consistent documentation across the types defined in this file.
///
/// The macro is useful since the same documentation is repeated several times.
macro_rules! docs {
    (args: $about:expr) => {
        concat!("The arguments to the ", $about, ".\n")
    };
    (attribute) => {
        concat!("The attribute body.\n")
    };
    (attributes: $about:expr) => {
        concat!("The attributes attached to the ", $about, ".\n")
    };
    (block_arg) => {
        concat!("The block argument to the function call.\n")
    };
    (clauses: $about:expr) => {
        concat!("The clauses that make up the ", $about, ".\n")
    };
    (condition: $about:expr) => {
        concat!("The condition the ", $about, ".\n")
    };
    (constructors) => {
        concat!("The type constructors.\n")
    };
    (evaluate_last) => {
        concat!(
            "Whether the semicolon (`;`) is omitted from the last expression in a block\n",
            "to indicate that the block takes on the value of that expression.\n",
        )
    };
    (expr: $about:expr) => {
        concat!("The expression of the ", $about, ".\n")
    };
    (exprs: $about:expr) => {
        concat!("The expressions that make up the ", $about, ".\n")
    };
    (fields: $about:expr) => {
        concat!("The fields of the ", $about, ".\n")
    };
    (kind: $about:expr) => {
        concat!("The kind of the ", $about, ".\n")
    };
    (import: $about:expr) => {
        concat!("The import ", $about, ".\n")
    };
    (literal: $about:expr) => {
        concat!("The ", $about, " literal as it appears in the text.\n")
    };
    (literal value: $about:expr) => {
        concat!("The value of the ", $about, " literal.\n")
    };
    (name: $about:expr) => {
        concat!("The name of the ", $about, ".\n")
    };
    (pattern: $about:expr) => {
        concat!("The pattern of the ", $about, ".\n")
    };
    (patterns: $about:expr) => {
        concat!("The patterns of the ", $about, ".\n")
    };
    (return_type) => {
        concat!("The return type from the function.\n")
    };
    (span: $about:expr) => {
        concat!("The span of the ", $about, ".\n")
    };
    (span: $about:expr; including: $($items:expr),+ $(,)?) => {
        concat!(
            docs!(span: $about),
            "\n",
            concat!("This includes the ", docs!(@list: $($items),+), ".\n"),
        )
    };
    (rest_pattern) => {
        concat!("The rest (`..`) pattern.\n")
    };
    (transparent) => {
        concat!("Whether the declaration is annotated with `transparent`.\n")
    };
    (type_name: $about:expr) => {
        concat!("The type name of the ", $about, ".\n")
    };
    (type_params) => {
        concat!("The parameters to a type.\n")
    };
    (using) => {
        concat!("Whether an argument is annotated with `use`.\n")
    };
    (visibility) => {
        concat!("The visibility level of the declaration.\n")
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
        concat!($a, ", ", docs!(@list: $($vs),+))
    };
}

// Make the `docs!` macro available in the parent scope.
pub(super) use docs;
