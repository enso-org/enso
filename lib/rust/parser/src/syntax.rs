//! Syntactic structures, including [`Token`] and [`Tree`], known as well as Abstract Syntax
//! Tree, or AST.

use crate::prelude::*;

use crate::span;

pub mod token;
pub mod tree;

// pub use token::Token;
// pub use tree::Tree;


// // ============
// // === Item ===
// // ============
//
// /// Abstraction for [`Token`] and [`Tree`]. Some functions, such as macro resolver need to
// /// distinguish between two cases and need to handle both incoming tokens and already constructed
// /// [`Tree`] nodes. This structure provides handy utilities to work with such cases.
// #[derive(Clone, Debug)]
// #[allow(missing_docs)]
// pub enum Item {
//     Token(Token),
//     Tree(Tree),
// }
//
// impl Item {
//     /// Check whether the element is the provided token variant. Returns [`false`] if it was an
//     /// [`Tree`] node.
//     pub fn is_variant(&self, variant: token::TypeVariant) -> bool {
//         match self {
//             Item::Token(token) => token.is(variant),
//             _ => false,
//         }
//     }
//
//     /// [`location::Span`] of the element.
//     pub fn span(&self) -> span::Span {
//         match self {
//             Self::Token(t) => t.span,
//             Self::Tree(t) => t.span,
//         }
//     }
//
//     /// Remove left offset spacing information.
//     pub fn trim_left(&mut self) -> span::Span {
//         match self {
//             Self::Token(t) => t.trim_left(),
//             Self::Tree(t) => t.trim_left(),
//         }
//     }
// }
//
// impl From<Token> for Item {
//     fn from(t: Token) -> Self {
//         Item::Token(t)
//     }
// }
//
// impl From<Tree> for Item {
//     fn from(t: Tree) -> Self {
//         Item::Tree(t)
//     }
// }
