use crate::syntax::Token;
use crate::syntax::Tree;



mod block;
mod compound_token;
mod consumer;
mod whitespace;


// ===============
// === Exports ===
// ===============

pub use block::FlattenBlockTrees;
pub use compound_token::AssembleCompoundTokens;
pub use consumer::Finish;
pub use consumer::ItemConsumer;
pub use consumer::TreeConsumer;
pub use whitespace::PeekSpacing;
pub use whitespace::Spacing;
pub use whitespace::SpacingLookaheadTokenConsumer;


// ===================
// === TokenOrTree ===
// ===================

#[derive(Debug)]
enum TokenOrTree<'s> {
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
