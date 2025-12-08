//! Syntactic structures, including [`Token`] and [`Tree`], known as well as Abstract Syntax
//! Tree, or AST.

// ==============
// === Export ===
// ==============

pub mod expression;
pub mod item;
pub mod statement;
pub mod token;
pub mod tree;

pub use expression::consumer::*;
pub use item::Item;
pub use statement::parse_args;
pub use token::Token;
pub use tree::SyntaxError;
pub use tree::Tree;
pub use tree::WARNINGS;
pub use tree::maybe_with_error;
