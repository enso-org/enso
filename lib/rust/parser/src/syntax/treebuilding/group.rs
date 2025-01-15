use crate::prelude::*;

use crate::syntax::consumer::Finish;
use crate::syntax::consumer::TokenConsumer;
use crate::syntax::consumer::TreeConsumer;
use crate::syntax::item;
use crate::syntax::token;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Item;
use crate::syntax::ItemConsumer;



// =====================
// === FlattenGroups ===
// =====================

/// Consumes non-block `Item`s and passes their content to a token/tree consumer.
#[derive(Debug, Default)]
pub struct FlattenGroups<'s, Inner> {
    inner: Inner,
    stack: Vec<(std::vec::IntoIter<Item<'s>>, Option<token::CloseSymbol<'s>>)>,
}

impl<'s, Inner> ItemConsumer<'s> for FlattenGroups<'s, Inner>
where Inner: TokenConsumer<'s> + TreeConsumer<'s> + GroupHierarchyConsumer<'s> + Finish
{
    fn push_item(&mut self, item: Item<'s>) {
        match item {
            Item::Block(_) => unreachable!(),
            Item::Token(token) => self.inner.push_token(token),
            Item::Tree(tree) => self.inner.push_tree(tree),
            Item::Group(item::Group { open, body, mut close }) => {
                self.inner.start_group(open);
                let mut body = body.into_vec().into_iter();
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
                                self.stack.push((outer_body, outer_close));
                                continue;
                            }
                            Item::Block(_) => unreachable!(),
                        }
                    }
                    if let Some(close) = close {
                        self.inner.end_group(close);
                    }
                    if let Some((outer_body, outer_close)) = self.stack.pop() {
                        body = outer_body;
                        close = outer_close;
                    } else {
                        break;
                    }
                }
            }
        }
    }
}

impl<'s, Inner> Finish for FlattenGroups<'s, Inner>
where Inner: Finish
{
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.inner.finish()
    }
}
