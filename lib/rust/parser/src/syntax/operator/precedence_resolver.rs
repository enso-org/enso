use crate::prelude::*;

use crate::syntax;
use crate::syntax::operator::annotations::ParseAnnotations;
use crate::syntax::operator::application::InsertApps;
use crate::syntax::operator::arity::ClassifyArity;
use crate::syntax::operator::group::BuildGroups;
use crate::syntax::operator::named_app::ParseAppNames;
use crate::syntax::operator::reducer::Reduce;
use crate::syntax::treebuilding::CompoundTokens;
use crate::syntax::treebuilding::FlattenBlockTrees;
use crate::syntax::treebuilding::ParseNumbers;
use crate::syntax::treebuilding::PeekSpacing;
use crate::syntax::Finish;
use crate::syntax::ItemConsumer;
use crate::syntax::Tree;



// ==================
// === Precedence ===
// ==================

macro_rules! compose_types {
    ($ty:ident<'s>) => {
        $ty<'s>
    };
    ($ty:ident<'s, _>, $($tail:tt)*) => {
        $ty<'s, compose_types!($($tail)*)>
    };
    ($ty:ident<_>, $($tail:tt)*) => {
        $ty<compose_types!($($tail)*)>
    };
}

/// Operator precedence resolver.
#[derive(Debug, Default)]
pub struct Precedence<'s> {
    resolver: compose_types![
        FlattenBlockTrees<'s, _>, // Items -> Tokens/Trees/Groups
        CompoundTokens<'s, _>,
        ParseNumbers<'s, _>,
        PeekSpacing<'s, _>, // Tokens/Trees/Groups -> Tokens/Trees/Groups + Spacing-lookahead
        ParseAnnotations<'s, _>, // Tokens/Trees/Groups + S -> T/T/Operators/Groups + S
        ParseAppNames<'s, _>,
        ClassifyArity<'s, _>, // Tokens/Trees/Groups + Spacing-lookahead -> Oper*s/Groups
        InsertApps<_>,        // Operators/Operands/Groups -> Oper*s/Groups/Applications
        BuildGroups<'s, _>,   // Operators/Operands/Groups/Applications -> Oper*s/Applications
        Reduce<'s>            // Operators/Operands/Applications -> Tree
    ],
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
