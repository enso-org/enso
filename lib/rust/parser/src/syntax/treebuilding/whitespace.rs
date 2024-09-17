use crate::syntax::consumer::Finish;
use crate::syntax::consumer::TokenConsumer;
use crate::syntax::consumer::TreeConsumer;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::treebuilding::TokenOrTree;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Item;
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

    pub fn of_item(item: &Item) -> Self {
        match item {
            Item::Token(token) => Spacing::of_token(token),
            Item::Tree(tree) => Spacing::of_tree(tree),
            Item::Group(group) => Spacing::of_token(&group.open),
            Item::Block(_) => Spacing::Spaced,
        }
    }
}

// Returns `true` for an item if that item should not follow any other item in a no-space group
// (i.e. the item has "space" before it).
fn token_starts_new_no_space_group<'a: 'b, 'b, T: Into<token::Ref<'a, 'b>>>(token: T) -> bool {
    let token = token.into();
    token.left_offset.visible.width_in_spaces != 0
        || matches!(token.data, token::Variant::CommaOperator(_))
}

fn tree_starts_new_no_space_group(tree: &Tree) -> bool {
    tree.span.left_offset.visible.width_in_spaces != 0
        || matches!(
            &tree.variant,
            tree::Variant::BodyBlock(_)
                | tree::Variant::OperatorBlockApplication(_)
                | tree::Variant::ArgumentBlockApplication(_)
                | tree::Variant::SuspendedDefaultArguments(_)
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
pub struct PeekSpacing<'s, Inner> {
    current: Option<TokenOrTree<'s>>,
    inner:   Inner,
}

impl<'s, Inner: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s>>
    PeekSpacing<'s, Inner>
{
    fn emit(&mut self, tt: Option<TokenOrTree<'s>>, rhs: Option<Spacing>) {
        match tt {
            Some(TokenOrTree::Token(token)) => self.inner.push_token(token, rhs),
            Some(TokenOrTree::Tree(tree)) => self.inner.push_tree(tree, rhs),
            None => {}
        }
    }

    fn flush(&mut self) {
        let last = self.current.take();
        self.emit(last, None);
    }
}

impl<'s, Inner: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s> + Finish>
    Finish for PeekSpacing<'s, Inner>
{
    type Result = Inner::Result;

    fn finish(&mut self) -> Inner::Result {
        self.flush();
        self.inner.finish()
    }
}

impl<'s, Inner: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s>>
    TokenConsumer<'s> for PeekSpacing<'s, Inner>
{
    fn push_token(&mut self, token: Token<'s>) {
        let rhs = Spacing::of_token(&token);
        let next = self.current.replace(token.into());
        self.emit(next, Some(rhs))
    }
}

impl<'s, Inner: SpacingLookaheadTreeConsumer<'s> + SpacingLookaheadTokenConsumer<'s>>
    TreeConsumer<'s> for PeekSpacing<'s, Inner>
{
    fn push_tree(&mut self, tree: Tree<'s>) {
        let rhs = Spacing::of_tree(&tree);
        let next = self.current.replace(tree.into());
        self.emit(next, Some(rhs));
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for PeekSpacing<'s, Inner>
where Inner: GroupHierarchyConsumer<'s>
        + SpacingLookaheadTreeConsumer<'s>
        + SpacingLookaheadTokenConsumer<'s>
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        let prev = self.current.take();
        self.emit(prev, Spacing::of_token(&open).into());
        self.inner.start_group(open);
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.flush();
        self.inner.end_group(close);
    }
}
