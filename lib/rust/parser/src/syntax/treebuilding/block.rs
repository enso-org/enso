use crate::prelude::*;

use crate::syntax::consumer::Finish;
use crate::syntax::consumer::ItemConsumer;
use crate::syntax::consumer::TokenConsumer;
use crate::syntax::consumer::TreeConsumer;
use crate::syntax::item;
use crate::syntax::operator;
use crate::syntax::statement::BodyBlockParser;
use crate::syntax::token::TokenOperatorProperties;
use crate::syntax::tree::block;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Item;



// ==========================
// === BlockTreeFlattener ===
// ==========================

/// Consumes `Item`s and passes their content to a token/tree consumer, using an
/// [`operator::Precedence`] parser to flatten blocks.
#[derive(Debug, Default)]
pub struct FlattenBlockTrees<'s, Inner> {
    /// Consumes child blocks. Stores no semantic state, but is reused for performance.
    child:         Option<Box<operator::Precedence<'s>>>,
    block_context: BlockContext,
    block_builder: block::Builder<'s>,
    block_parser:  BodyBlockParser<'s>,
    inner:         Inner,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
enum BlockContext {
    #[default]
    Body,
    ArgumentOrOperator,
}

impl<'s, Inner> ItemConsumer<'s> for FlattenBlockTrees<'s, Inner>
where Inner: TokenConsumer<'s> + TreeConsumer<'s> + GroupHierarchyConsumer<'s>
{
    fn push_item(&mut self, item: Item<'s>) {
        self.block_context = match item {
            Item::Block(lines) => {
                let mut child = self.child.take().unwrap_or_default();
                self.inner.push_tree(match self.block_context {
                    BlockContext::Body =>
                        self.block_parser.parse_body_block(lines.into_vec(), &mut child),
                    BlockContext::ArgumentOrOperator => {
                        for item::Line { newline, items } in lines.into_vec() {
                            self.block_builder.push(newline, items, &mut child);
                        }
                        self.block_builder.build()
                    }
                });
                self.child = Some(child);
                BlockContext::ArgumentOrOperator
            }
            Item::Token(token) => {
                let properties = token.operator_properties();
                self.inner.push_token(token);
                match properties {
                    Some(properties) if properties.rhs_is_expression() => BlockContext::Body,
                    _ => BlockContext::ArgumentOrOperator,
                }
            }
            Item::Tree(tree) => {
                self.inner.push_tree(tree);
                BlockContext::ArgumentOrOperator
            }
            Item::Group(item::Group { open, body, mut close }) => {
                self.inner.start_group(open);
                let mut stack = vec![];
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
                                stack.push((outer_body, outer_close));
                                continue;
                            }
                            Item::Block(_) => unreachable!(),
                        }
                    }
                    if let Some(close) = close {
                        self.inner.end_group(close);
                    }
                    if let Some((outer_body, outer_close)) = stack.pop() {
                        body = outer_body;
                        close = outer_close;
                    } else {
                        break;
                    }
                }
                BlockContext::ArgumentOrOperator
            }
        };
    }
}

impl<'s, Inner: Finish> Finish for FlattenBlockTrees<'s, Inner> {
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.block_context = default();
        self.inner.finish()
    }
}
