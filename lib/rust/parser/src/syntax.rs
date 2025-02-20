//! Syntactic structures, including [`Token`] and [`Tree`], known as well as Abstract Syntax
//! Tree, or AST.


// ==============
// === Export ===
// ==============

pub mod item;
pub mod operator;
pub mod statement;
pub mod token;
pub mod tree;



mod consumer;
mod treebuilding;

pub use consumer::*;
pub use item::Item;
pub use statement::parse_args;
pub use token::Token;
pub use tree::maybe_with_error;
pub use tree::Tree;
pub use tree::WARNINGS;
pub use treebuilding::TokenOrTree;
