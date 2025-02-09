// SPDX-License-Identifier: MPL-2.0

//! TODO: Module level docs.

#[allow(unused)] // TODO: Remove once no longer needed to silence warnings.
mod ast;
#[allow(unused)] // TODO: Remove once no longer needed to silence warnings.
mod parse;

fn main() {
    println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
}
