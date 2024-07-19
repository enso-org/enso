use crate::syntax;
use crate::syntax::operator;
use crate::syntax::treebuilding::consumer::Finish;
use crate::syntax::treebuilding::consumer::ItemConsumer;
use crate::syntax::treebuilding::consumer::TokenConsumer;
use crate::syntax::treebuilding::consumer::TreeConsumer;
use crate::syntax::Item;



// ==========================
// === BlockTreeFlattener ===
// ==========================

/// Consumes `Item`s and passes their content to a token/tree consumer, using an
/// [`operator::Precedence`] parser to flatten blocks.
#[derive(Debug, Default)]
pub struct FlattenBlockTrees<'s, T> {
    inner: T,
    /// Consumes child blocks. Stores no semantic state, but is reused for performance.
    child: Option<Box<operator::Precedence<'s>>>,
}

impl<'s, T: TokenConsumer<'s> + TreeConsumer<'s>> ItemConsumer<'s> for FlattenBlockTrees<'s, T> {
    fn push_item(&mut self, item: Item<'s>) {
        match item {
            Item::Block(lines) => {
                let mut child = self.child.take().unwrap_or_default();
                self.inner.push_tree(syntax::item::build_block(lines, &mut child));
                self.child = Some(child);
            }
            Item::Token(token) => self.inner.push_token(token),
            Item::Tree(tree) => self.inner.push_tree(tree),
        }
    }
}

impl<'s, T: Finish> Finish for FlattenBlockTrees<'s, T> {
    type Result = T::Result;

    fn finish(&mut self) -> Self::Result {
        self.inner.finish()
    }
}
