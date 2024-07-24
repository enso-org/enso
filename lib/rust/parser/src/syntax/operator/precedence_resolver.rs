use crate::prelude::*;

use crate::syntax;
use crate::syntax::operator::application::InsertApps;
use crate::syntax::operator::arity::ClassifyArity;
use crate::syntax::operator::reducer::Reduce;
use crate::syntax::treebuilding;
use crate::syntax::treebuilding::Finish;
use crate::syntax::treebuilding::ItemConsumer;
use crate::syntax::Tree;



// ==================
// === Precedence ===
// ==================

/// Operator precedence resolver.
#[derive(Debug, Default)]
pub struct Precedence<'s> {
    #[rustfmt::skip]
    resolver:
        // Items -> Tokens/Trees
        treebuilding::FlattenBlockTrees<'s,
        // Tokens/Trees -> Tokens/Trees  (proper tokens only)
        treebuilding::AssembleCompoundTokens<'s,
        // Tokens/Trees -> Tokens/Trees + Spacing-lookahead
        treebuilding::PeekSpacing<'s,
        // Tokens/Trees + Spacing-lookahead -> Operators/Operands
        ClassifyArity<'s,
        // Operators/Operands -> Operators/Operands (balanced)
        InsertApps<
        // Operators/Operands -> Tree
        Reduce<'s>>>>>>,
}

impl<'s> Precedence<'s> {
    /// Return a new operator precedence resolver.
    pub fn new() -> Self {
        Self::default()
    }

    /// Resolve precedence in a context where the result cannot be an operator section or template
    /// function.
    pub fn resolve_non_section(
        &mut self,
        items: impl IntoIterator<Item = syntax::Item<'s>>,
    ) -> Option<Tree<'s>> {
        items.into_iter().for_each(|i| self.push(i));
        self.resolver.finish().map(|op| op.value)
    }

    /// Resolve precedence.
    pub fn resolve(
        &mut self,
        items: impl IntoIterator<Item = syntax::Item<'s>>,
    ) -> Option<Tree<'s>> {
        self.extend(items);
        self.finish()
    }

    /// Extend the expression with a token.
    pub fn push(&mut self, item: syntax::Item<'s>) {
        self.resolver.push_item(item);
    }

    /// Return the result.
    pub fn finish(&mut self) -> Option<Tree<'s>> {
        self.resolver.finish().map(Tree::from)
    }
}

impl<'s> Extend<syntax::Item<'s>> for Precedence<'s> {
    fn extend<T: IntoIterator<Item = syntax::Item<'s>>>(&mut self, iter: T) {
        for token in iter {
            self.push(token);
        }
    }
}
