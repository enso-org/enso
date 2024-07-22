use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::treebuilding::consumer::Finish;
use crate::syntax::treebuilding::consumer::TokenConsumer;
use crate::syntax::treebuilding::consumer::TreeConsumer;
use crate::syntax::treebuilding::TokenOrTree;
use crate::syntax::Token;
use crate::syntax::Tree;



// ===============
// === Spacing ===
// ===============

/// Whether a term is logically separated from the previous term by whitespace.
#[derive(Debug, Default, PartialEq, Eq, Copy, Clone)]
pub enum Spacing {
    #[default]
    Spaced,
    Unspaced,
}

impl Spacing {
    pub fn of_tree(tree: &Tree) -> Self {
        match tree_starts_new_no_space_group(tree) {
            false => Spacing::Unspaced,
            true => Spacing::Spaced,
        }
    }

    pub fn of_token<'a: 'b, 'b, T: Into<token::Ref<'a, 'b>>>(token: T) -> Self {
        match token_starts_new_no_space_group(token) {
            false => Spacing::Unspaced,
            true => Spacing::Spaced,
        }
    }
}

// Returns `true` for an item if that item should not follow any other item in a no-space group
// (i.e. the item has "space" before it).
fn token_starts_new_no_space_group<'a: 'b, 'b, T: Into<token::Ref<'a, 'b>>>(token: T) -> bool {
    let token = token.into();
    match &token.data {
        token::Variant::Operator(opr) if opr.properties.is_sequence() => true,
        _ => token.left_offset.visible.width_in_spaces != 0,
    }
}

fn tree_starts_new_no_space_group(tree: &Tree) -> bool {
    tree.span.left_offset.visible.width_in_spaces != 0
        || matches!(
            &tree.variant,
            box tree::Variant::BodyBlock(_)
                | box tree::Variant::OperatorBlockApplication(_)
                | box tree::Variant::ArgumentBlockApplication(_)
        )
}


// ============================
// === Whitespace Lookahead ===
// ============================

pub trait SpacingLookaheadTreeConsumer<'s> {
    fn push_tree(&mut self, tree: Tree<'s>, following_spacing: Option<Spacing>);
}

pub trait SpacingLookaheadTokenConsumer<'s> {
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>);
}

/// Maintains 1-token whitespace lookahead.
#[derive(Debug, Default)]
pub struct PeekSpacing<'s, T> {
    current: Option<TokenOrTree<'s>>,
    inner:   T,
}

impl<'s, T: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s>>
    PeekSpacing<'s, T>
{
    fn emit(&mut self, tt: Option<TokenOrTree<'s>>, rhs: Option<Spacing>) {
        match tt {
            Some(TokenOrTree::Token(token)) => self.inner.push_token(token, rhs),
            Some(TokenOrTree::Tree(tree)) => self.inner.push_tree(tree, rhs),
            None => {}
        }
    }
}

impl<'s, T: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s> + Finish> Finish
    for PeekSpacing<'s, T>
{
    type Result = T::Result;

    fn finish(&mut self) -> T::Result {
        let last = self.current.take();
        self.emit(last, None);
        self.inner.finish()
    }
}

impl<'s, T: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s>> TokenConsumer<'s>
    for PeekSpacing<'s, T>
{
    fn push_token(&mut self, token: Token<'s>) {
        let rhs = Spacing::of_token(&token);
        let next = self.current.replace(token.into());
        self.emit(next, Some(rhs))
    }
}

impl<'s, T: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s>> TreeConsumer<'s>
    for PeekSpacing<'s, T>
{
    fn push_tree(&mut self, tree: Tree<'s>) {
        let rhs = Spacing::of_tree(&tree);
        let next = self.current.replace(tree.into());
        self.emit(next, Some(rhs));
    }
}

impl<'s, T: TreeConsumer<'s>> SpacingLookaheadTreeConsumer<'s> for T {
    fn push_tree(&mut self, tree: Tree<'s>, _: Option<Spacing>) {
        self.push_tree(tree);
    }
}
