use crate::prelude::*;

use crate::syntax::consumer::Finish;
use crate::syntax::item;
use crate::syntax::operator;
use crate::syntax::statement::BodyBlockParser;
use crate::syntax::token::TokenOperatorProperties;
use crate::syntax::tree::block;
use crate::syntax::Item;
use crate::syntax::ItemConsumer;



// ==========================
// === BlockTreeFlattener ===
// ==========================

/// Consumes `Item`s and passes their content to a token/tree consumer, using an
/// [`operator::Precedence`] parser to flatten blocks.
#[derive(Debug, Default)]
pub struct FlattenBlockTrees<'s, Inner> {
    /// Consumes child blocks. Stores no semantic state, but is reused for performance.
    child:         Option<Box<operator::Precedence<'s>>>,
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

impl<'s, Inner> FlattenBlockTrees<'s, Inner>
where Inner: ItemConsumer<'s> + Finish
{
    pub fn run(&mut self, start: usize, items: &mut Vec<Item<'s>>) -> Inner::Result {
        if let Some(Item::Block(_)) = items.last() {
            let Some(Item::Block(lines)) = items.pop() else { unreachable!() };
            let mut child = self.child.take().unwrap_or_default();
            let block_context = match items.last() {
                Some(Item::Token(token)) => match token.operator_properties() {
                    Some(properties) if properties.rhs_is_expression() => BlockContext::Body,
                    _ => BlockContext::ArgumentOrOperator,
                },
                None => BlockContext::Body,
                _ => BlockContext::ArgumentOrOperator,
            };
            items.push(Item::Tree(match block_context {
                BlockContext::Body =>
                    self.block_parser.parse_body_block(&mut lines.into_vec(), &mut child),
                BlockContext::ArgumentOrOperator => {
                    for item::Line { newline, items } in lines.into_vec() {
                        self.block_builder.push(newline, items, &mut child);
                    }
                    self.block_builder.build()
                }
            }));
            self.child = Some(child);
        }
        for item in items.drain(start..) {
            self.inner.push_item(item);
        }
        self.inner.finish()
    }
}
