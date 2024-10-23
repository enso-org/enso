//! Syntactic structures, including [`Token`] and [`Tree`], known as well as Abstract Syntax
//! Tree, or AST.

use crate::prelude::*;
use crate::source::*;
use crate::syntax::*;



// ============
// === Item ===
// ============

/// Abstraction for [`Token`] and [`Tree`]. Some functions, such as macro resolver need to
/// distinguish between two cases and need to handle both incoming tokens and already constructed
/// [`Tree`] nodes. This structure provides handy utilities to work with such cases.
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Item<'s> {
    Token(Token<'s>),
    Block(Box<[Line<'s>]>),
    Tree(Tree<'s>),
    Group(Group<'s>),
}

/// A parenthesized subtree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Group<'s> {
    /// The opening parenthesis.
    pub open:  token::OpenSymbol<'s>,
    /// The parenthesized subtree.
    pub body:  Box<[Item<'s>]>,
    /// The closing parenthesis.
    pub close: Option<token::CloseSymbol<'s>>,
}

/// A line.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Line<'s> {
    /// The line-beginning token.
    pub newline: token::Newline<'s>,
    /// The line's contents.
    pub items:   Vec<Item<'s>>,
}

impl<'s> Item<'s> {
    /// [`location::Span`] of the element.
    pub fn left_visible_offset(&self) -> VisibleOffset {
        match self {
            Self::Token(t) => t.span().left_offset.visible,
            Self::Tree(t) => t.span.left_offset.visible,
            Self::Group(t) => t.open.left_offset.visible,
            Self::Block(_) => default(),
        }
    }

    /// If this item is an [`Item::Tree`], apply the given function to the contained [`Tree`] and
    /// return the result.
    pub fn map_tree<'t: 's, F>(self, f: F) -> Self
    where F: FnOnce(Tree<'s>) -> Tree<'t> {
        match self {
            Item::Tree(tree) => Item::Tree(f(tree)),
            _ => self,
        }
    }

    /// If this item is a token, return it.
    pub fn into_token(self) -> Option<Token<'s>> {
        match self {
            Item::Token(token) => Some(token),
            _ => None,
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

impl<'s> From<Group<'s>> for Item<'s> {
    fn from(group: Group<'s>) -> Self {
        Item::Group(group)
    }
}

impl<'s> AsRef<Item<'s>> for Item<'s> {
    fn as_ref(&self) -> &Self {
        self
    }
}


// ===========
// === Ref ===
// ===========

/// A borrowed version of [`Item`]. Used mostly by AST visitors.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum Ref<'s, 'a> {
    Token(token::Ref<'s, 'a>),
    Tree(&'a Tree<'s>),
}
