//! The expression parser.

mod annotations;
mod application;
mod apply;
mod arity;
mod blocks;
mod compound_token;
pub mod consumer;
mod group;
mod named_app;
mod numbers;
mod operand;
mod parser;
mod reducer;
mod types;
mod whitespace;

// ===============
// === Exports ===
// ===============

pub use parser::ExpressionParser;
pub use types::Warnings;
pub use whitespace::Spacing;
