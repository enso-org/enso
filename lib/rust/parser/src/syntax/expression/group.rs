use crate::prelude::*;
use crate::syntax::expression::types::*;

use crate::syntax::Finish;
use crate::syntax::Flush;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Item;
use crate::syntax::ItemConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::TokenConsumer;
use crate::syntax::Tree;
use crate::syntax::TreeConsumer;
use crate::syntax::item;
use crate::syntax::maybe_with_error;
use crate::syntax::token::CloseSymbol;
use crate::syntax::token::OpenSymbol;
use crate::syntax::tree::SyntaxError;

// =====================
// === FlattenGroups ===
// =====================

/// Consumes non-block `Item`s and passes their content to a token/tree consumer.
#[derive(Debug, Default, OperatorConsumer)]
#[operator_consumer(Forward)]
pub struct FlattenGroups<'s, Inner> {
    inner: Inner,
    stack: VecAllocation<(std::vec::IntoIter<Item<'s>>, Option<CloseSymbol<'s>>)>,
}

impl<'s, Inner> ItemConsumer<'s> for FlattenGroups<'s, Inner>
where
    Inner: TokenConsumer<'s> + TreeConsumer<'s> + GroupHierarchyConsumer<'s> + Finish,
{
    fn push_item(&mut self, item: Item<'s>) {
        match item {
            Item::Block(_) => unreachable!(),
            Item::Token(token) => self.inner.push_token(token),
            Item::Tree(tree) => self.inner.push_tree(tree),
            Item::Group(item::Group { open, body, mut close }) => {
                self.inner.start_group(open);
                let mut body = body.into_vec().into_iter();
                let mut stack = self.stack.take();
                loop {
                    while let Some(item) = body.next() {
                        match item {
                            Item::Token(token) => self.inner.push_token(token),
                            Item::Tree(tree) => self.inner.push_tree(tree),
                            Item::Group(group) => {
                                self.inner.start_group(group.open);
                                let outer_body =
                                    mem::replace(&mut body, group.body.into_vec().into_iter());
                                let outer_close = mem::replace(&mut close, group.close);
                                stack.push((outer_body, outer_close));
                                continue;
                            }
                            Item::Block(_) => unreachable!(),
                        }
                    }
                    self.inner.end_group(close);
                    if let Some((outer_body, outer_close)) = stack.pop() {
                        body = outer_body;
                        close = outer_close;
                    } else {
                        break;
                    }
                }
                debug_assert!(stack.is_empty());
                self.stack.set_from(stack);
            }
        }
    }
}

impl<Inner> Finish for FlattenGroups<'_, Inner>
where
    Inner: Finish,
{
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.inner.finish()
    }
}

// =====================
// === Group Builder ===
// =====================

/// Constructs parenthesized groups.
#[derive(Default, Debug, Finish, OperandConsumer, OperatorConsumer, ScopeHierarchyConsumer)]
#[operand_consumer(Forward)]
#[operator_consumer(Forward)]
#[scope_hierarchy_consumer(Forward)]
pub struct BuildGroups<'s, Inner> {
    open: Vec<OpenSymbol<'s>>,
    inner: Inner,
}

impl<'s, ScopeResult, Inner> Flush for BuildGroups<'s, Inner>
where
    ScopeResult: Into<Option<Tree<'s>>>,
    Inner: ScopeHierarchyConsumer<Result = ScopeResult> + OperandConsumer<'s>,
{
    fn flush(&mut self) {
        debug_assert!(self.open.is_empty());
    }
}

impl<'s, ScopeResult, Inner> GroupHierarchyConsumer<'s> for BuildGroups<'s, Inner>
where
    ScopeResult: Into<Option<Tree<'s>>>,
    Inner: ScopeHierarchyConsumer<Result = ScopeResult> + OperandConsumer<'s>,
{
    fn start_group(&mut self, open: OpenSymbol<'s>) {
        self.open.push(open);
        self.inner.start_scope();
    }

    fn end_group(&mut self, close: Option<CloseSymbol<'s>>) {
        let open = self.open.pop().unwrap();
        let expression = self.inner.end_scope().into();
        let error = close.is_none().then_some(SyntaxError::ExprUnclosedParen);
        let group = Tree::group(Some(open), expression, close);
        self.inner.push_operand(maybe_with_error(group, error).into());
    }
}
