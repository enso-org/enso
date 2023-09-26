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
    Block(Vec<Line<'s>>),
    Tree(Tree<'s>),
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
    /// Check whether the element is the provided token variant. Returns [`false`] if it was not a
    /// token.
    pub fn is_variant(&self, variant: token::variant::VariantMarker) -> bool {
        match self {
            Item::Token(token) => token.is(variant),
            _ => false,
        }
    }

    /// [`location::Span`] of the element.
    pub fn left_visible_offset(&self) -> VisibleOffset {
        match self {
            Self::Token(t) => t.span().left_offset.visible,
            Self::Tree(t) => t.span.left_offset.visible,
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

impl<'s> TryAsRef<Item<'s>> for Item<'s> {
    fn try_as_ref(&self) -> Option<&Item<'s>> {
        Some(self)
    }
}

/// Given a sequence of [`Line`]s belonging to one block, create an AST block node, of a type
/// determined by the syntax of the lines in the block.
pub fn build_block<'s>(
    lines: impl IntoIterator<Item = Line<'s>>,
    parser: &mut operator::Precedence<'s>,
) -> Tree<'s> {
    let mut block_builder = tree::block::Builder::new();
    for Line { newline, items } in lines {
        block_builder.push(newline, items, parser);
    }
    block_builder.build()
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



// ======================
// === Variant Checks ===
// ======================

/// For each token variant, generates a function checking if the token is of the given variant. For
/// example, the `is_ident` function checks if the token is an identifier.
macro_rules! generate_variant_checks {
    (
        $(#$enum_meta:tt)*
        pub enum $enum:ident {
            $(
                $(#$variant_meta:tt)*
                $variant:ident $({
                    $($(#$field_meta:tt)* pub $field:ident : $field_ty:ty),* $(,)?
                })?
            ),* $(,)?
        }
    ) => { paste!{
        impl<'s> Item<'s> {
            $(
                $(#[$($variant_meta)*])*
                #[allow(missing_docs)]
                pub fn [<is_ $variant:snake:lower>](&self) -> bool {
                    self.is_variant(token::variant::VariantMarker::$variant)
                }
            )*
        }
    }};
}

crate::with_token_definition!(generate_variant_checks());
