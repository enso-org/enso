//! Debugging utilities for the parser.

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]

mod check;
mod s_expr;
pub mod test;

pub use check::validate_spans;
pub use s_expr::to_s_expr;
