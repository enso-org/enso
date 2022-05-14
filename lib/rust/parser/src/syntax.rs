//! Syntactic structures, including [`Token`] and [`Tree`], known as well as Abstract Syntax
//! Tree, or AST.

// use crate::prelude::*;
//
use crate::source::span;

pub mod token;
pub mod tree;

pub use token::Token;
pub use tree::Tree;


// ============
// === Item ===
// ============

/// Abstraction for [`Token`] and [`Tree`]. Some functions, such as macro resolver need to
/// distinguish between two cases and need to handle both incoming tokens and already constructed
/// [`Tree`] nodes. This structure provides handy utilities to work with such cases.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum Item<'s> {
    Token(Token<'s>),
    Tree(Tree<'s>),
}

impl<'s> Item<'s> {
    /// Check whether the element is the provided token variant. Returns [`false`] if it was an
    /// [`Tree`] node.
    pub fn is_variant(&self, variant: token::variant::VariantVariant) -> bool {
        match self {
            Item::Token(token) => token.is(variant),
            _ => false,
        }
    }

    // /// [`location::Span`] of the element.
    // pub fn span(&self) -> span::Span {
    //     match self {
    //         Self::Token(t) => t.span,
    //         Self::Tree(t) => t.span,
    //     }
    // }

    /// Remove left offset spacing information.
    pub fn trim_as_first_child(&mut self) -> span::Span {
        match self {
            Self::Token(t) => t.trim_as_first_child(),
            Self::Tree(t) => t.span.trim_as_first_child(),
        }
    }
}

impl<'s> From<Token<'s>> for Item<'s> {
    fn from(t: Token<'s>) -> Self {
        Item::Token(t)
    }
}

impl<'s> From<Tree<'s>> for Item<'s> {
    fn from(t: Tree<'s>) -> Self {
        Item::Tree(t)
    }
}
