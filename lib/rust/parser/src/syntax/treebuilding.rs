use crate::syntax::tree;
use crate::syntax::Token;
use crate::syntax::Tree;



mod block;
mod compound_token;
mod numbers;
mod whitespace;


// ===============
// === Exports ===
// ===============

pub use block::FlattenBlockTrees;
pub use compound_token::CompoundTokens;
pub use numbers::ParseNumbers;
pub use whitespace::PeekSpacing;
pub use whitespace::Spacing;
pub use whitespace::SpacingLookaheadTokenConsumer;
pub use whitespace::SpacingLookaheadTreeConsumer;


// ===================
// === TokenOrTree ===
// ===================

/// A token or tree.
#[derive(Debug)]
pub enum TokenOrTree<'s> {
    /// A token.
    Token(Token<'s>),
    /// A tree.
    Tree(Tree<'s>),
}

impl<'s> From<Token<'s>> for TokenOrTree<'s> {
    fn from(token: Token<'s>) -> Self {
        TokenOrTree::Token(token)
    }
}

impl<'s> From<Tree<'s>> for TokenOrTree<'s> {
    fn from(tree: Tree<'s>) -> Self {
        TokenOrTree::Tree(tree)
    }
}

impl<'s> From<TokenOrTree<'s>> for Tree<'s> {
    fn from(t: TokenOrTree<'s>) -> Self {
        match t {
            TokenOrTree::Token(token) => tree::to_ast(token),
            TokenOrTree::Tree(tree) => tree,
        }
    }
}
