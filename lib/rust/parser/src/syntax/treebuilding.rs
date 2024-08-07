use crate::syntax::tree;
use crate::syntax::Token;
use crate::syntax::Tree;



mod block;
mod compound_token;
mod group;
mod numbers;
mod whitespace;


// ===============
// === Exports ===
// ===============

pub use block::FlattenBlockTrees;
pub use compound_token::CompoundTokens;
pub use group::FlattenGroups;
pub use numbers::ParseNumbers;
pub use whitespace::PeekSpacing;
pub use whitespace::Spacing;
pub use whitespace::SpacingLookaheadTokenConsumer;
pub use whitespace::SpacingLookaheadTreeConsumer;


// ===================
// === TokenOrTree ===
// ===================

#[allow(missing_docs)]
#[derive(Debug)]
pub enum TokenOrTree<'s> {
    Token(Token<'s>),
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
