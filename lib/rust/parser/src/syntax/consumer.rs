use crate::prelude::*;

use crate::syntax::token;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::treebuilding::SpacingLookaheadTokenConsumer;
use crate::syntax::treebuilding::SpacingLookaheadTreeConsumer;
use crate::syntax::Item;
use crate::syntax::Token;
use crate::syntax::Tree;

/// Item consumer.
pub trait ItemConsumer<'s> {
    /// Push an item.
    fn push_item(&mut self, tree: Item<'s>);
}

/// Tree consumer.
pub trait TreeConsumer<'s> {
    /// Push a tree.
    fn push_tree(&mut self, tree: Tree<'s>);
}

/// Token consumer.
pub trait TokenConsumer<'s> {
    /// Push a token.
    fn push_token(&mut self, token: Token<'s>);
}

/// Newline consumer.
pub trait NewlineConsumer<'s> {
    /// Push a newline.
    fn push_newline(&mut self, newline: token::Newline<'s>);
}


/// Block hierarchy consumer.
pub trait BlockHierarchyConsumer {
    /// Start a block.
    fn start_block(&mut self);
    /// End the block.
    fn end_block(&mut self);
}

/// Parenthesized-group hierarchy consumer.
pub trait GroupHierarchyConsumer<'s> {
    /// Start a parenthesized group.
    fn start_group(&mut self, open: token::OpenSymbol<'s>);
    /// End the parenthesized group.
    fn end_group(&mut self, close: token::CloseSymbol<'s>);
}

/// Trait for a token consumer to enter a scope that will be handled independently.
pub trait ScopeHierarchyConsumer {
    /// The result of the scope ending.
    type Result;
    /// Start a scope.
    fn start_scope(&mut self);
    /// End the scope.
    fn end_scope(&mut self) -> Self::Result;
}

/// An operation that can be finished.
pub trait Finish {
    /// The output.
    type Result;

    /// Indicates end of input.
    fn finish(&mut self) -> Self::Result;
}

/// Trait for a type that wraps another type, and exposes it.
pub trait HasInner {
    /// The inner type.
    type Inner;

    /// Access the inner type.
    fn inner_mut(&mut self) -> &mut Self::Inner;
}

/// Process all retained state.
pub trait Flush {
    /// Process all retained state.
    fn flush(&mut self);
}


// ================
// === Adapters ===
// ================

/// Adapts a parser that consumes only tokens to fit into a more complex pipeline stages.
#[derive(Debug, Default)]
pub struct TokenOnlyParser<Parser> {
    parser: Parser,
}

impl<'s, Inner, Parser> SpacingLookaheadTokenConsumer<'s> for TokenOnlyParser<Parser>
where
    Parser: HasInner<Inner = Inner> + SpacingLookaheadTokenConsumer<'s>,
    Inner: SpacingLookaheadTokenConsumer<'s>,
{
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>) {
        self.parser.push_token(token, following_spacing);
    }
}

impl<'s, Parser, Inner> SpacingLookaheadTreeConsumer<'s> for TokenOnlyParser<Parser>
where
    Parser: HasInner<Inner = Inner> + Flush,
    Inner: SpacingLookaheadTreeConsumer<'s>,
{
    fn push_tree(&mut self, tree: Tree<'s>, following_spacing: Option<Spacing>) {
        self.parser.flush();
        self.parser.inner_mut().push_tree(tree, following_spacing)
    }
}

impl<'s, Parser, Inner> GroupHierarchyConsumer<'s> for TokenOnlyParser<Parser>
where
    Parser: HasInner<Inner = Inner> + Flush,
    Inner: GroupHierarchyConsumer<'s>,
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.parser.flush();
        self.parser.inner_mut().start_group(open)
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.parser.flush();
        self.parser.inner_mut().end_group(close)
    }
}

impl<Inner: Finish> Finish for TokenOnlyParser<Inner>
where Inner: Finish
{
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.parser.finish()
    }
}


// =================
// === Debugging ===
// =================

/// Debugging tool. Can be inserted into parsing pipeline at different stages to debug components.
#[derive(Debug, Default)]
pub struct Inspect<Inner>(pub(crate) Inner);

impl<Inner> Inspect<Inner> {
    pub(crate) fn observe(&self, event: &impl Debug) {
        eprintln!("-> {:?}", event);
    }

    pub(crate) fn observe_received(&self, event: &impl Debug) {
        eprintln!("<- {:?}", event);
    }

    pub(crate) fn observe_token<'a: 'b, 'b, T: Into<token::Ref<'a, 'b>>>(&self, token: T) {
        eprintln!("-> Token({})", token.into().code.repr.0);
    }
}

impl<T: Debug, Inner: ScopeHierarchyConsumer<Result = T>> ScopeHierarchyConsumer
    for Inspect<Inner>
{
    type Result = Inner::Result;

    fn start_scope(&mut self) {
        self.observe(&"StartScope");
        self.0.start_scope();
    }

    fn end_scope(&mut self) -> Self::Result {
        self.observe(&"EndScope");
        let result = self.0.end_scope();
        self.observe_received(&result);
        result
    }
}

impl<'s, Inner: GroupHierarchyConsumer<'s>> GroupHierarchyConsumer<'s> for Inspect<Inner> {
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.observe_token(&open);
        self.0.start_group(open);
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.observe_token(&close);
        self.0.end_group(close);
    }
}

impl<'s, Inner: SpacingLookaheadTokenConsumer<'s>> SpacingLookaheadTokenConsumer<'s>
    for Inspect<Inner>
{
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>) {
        self.observe_token(&token);
        self.0.push_token(token, following_spacing);
    }
}

impl<'s, Inner: SpacingLookaheadTreeConsumer<'s>> SpacingLookaheadTreeConsumer<'s>
    for Inspect<Inner>
{
    fn push_tree(&mut self, tree: Tree<'s>, following_spacing: Option<Spacing>) {
        self.observe(&tree);
        self.0.push_tree(tree, following_spacing);
    }
}

impl<'s, Inner: ItemConsumer<'s>> ItemConsumer<'s> for Inspect<Inner> {
    fn push_item(&mut self, item: Item<'s>) {
        match &item {
            Item::Token(token) => self.observe_token(token),
            _ => self.observe(&item),
        }
        self.0.push_item(item);
    }
}

impl<T: Debug, Inner: Finish<Result = T>> Finish for Inspect<Inner> {
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.observe(&"Finish");
        let result = self.0.finish();
        self.observe_received(&result);
        result
    }
}
