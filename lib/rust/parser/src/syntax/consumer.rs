use crate::prelude::*;

use crate::source;
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

impl<'s> TokenConsumer<'s> for Vec<Token<'s>> {
    fn push_token(&mut self, token: Token<'s>) {
        self.push(token);
    }
}

impl<'s> NewlineConsumer<'s> for Vec<Token<'s>> {
    fn push_newline(&mut self, token: token::Newline<'s>) {
        self.push(token.into());
    }
}

impl<'s> BlockHierarchyConsumer for Vec<Token<'s>> {
    fn start_block(&mut self) {
        self.push(Token(source::Offset::default(), default(), token::Variant::block_start()));
    }

    fn end_block(&mut self) {
        self.push(Token(source::Offset::default(), default(), token::Variant::block_end()));
    }
}

impl<'s> GroupHierarchyConsumer<'s> for Vec<Token<'s>> {
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.push(open.into())
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.push(close.into())
    }
}

impl<'s> Finish for Vec<Token<'s>> {
    type Result = Vec<Token<'s>>;

    fn finish(&mut self) -> Self::Result {
        mem::take(self)
    }
}

pub trait HasInner {
    type Inner;

    fn inner_mut(&mut self) -> &mut Self::Inner;
}

pub trait Flush {
    fn flush(&mut self);
}


// ================
// === Adapters ===
// ================

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
