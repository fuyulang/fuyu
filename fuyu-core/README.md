# Fuyu Compiler Core

The core of the Fuyu compiler is implemented as a library crate so that it can
be easily tested separate from the compiler binary.

This crate implements all of the core compiler features, including parsing,
analysis, and code generation.

The compiler itself, `fuyuc`, is a separate crate and can be found on
[GitHub](https://github.com/fuyulang/fuyu) or
[crates.io](https://crates.io/crates/fuyuc).
