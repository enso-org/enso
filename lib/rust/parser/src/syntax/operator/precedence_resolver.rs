use crate::prelude::*;

use crate::syntax::operator::annotations::ParseAnnotations;
use crate::syntax::operator::application::InsertApps;
use crate::syntax::operator::arity::ClassifyArity;
use crate::syntax::operator::group::BuildGroups;
use crate::syntax::operator::named_app::ParseAppNames;
use crate::syntax::operator::reducer::Reduce;
use crate::syntax::operator::section::MaybeSection;
use crate::syntax::treebuilding::CompoundTokens;
use crate::syntax::treebuilding::FlattenBlockTrees;
use crate::syntax::treebuilding::FlattenGroups;
use crate::syntax::treebuilding::ParseNumbers;
use crate::syntax::treebuilding::PeekSpacing;
use crate::syntax::Item;
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

type Resolver<'s> = compose_types![
    FlattenBlockTrees<'s, _>, // Items -> Tokens/Trees/GroupItems
    FlattenGroups<'s, _>,     // Tokens/Trees/GroupItems -> Tokens/Trees/Groups
    CompoundTokens<'s, _>,
    ParseNumbers<'s, _>,
    PeekSpacing<'s, _>, // Tokens/Trees/Groups -> Tokens/Trees/Groups + Spacing-lookahead
    ParseAnnotations<'s, _>, // Tokens/Trees/Groups + S -> T/T/Operators/Groups + S
    ParseAppNames<'s, _>,
    ClassifyArity<'s, _>, // Tokens/Trees/Groups + Spacing-lookahead -> Oper*s/Groups
    InsertApps<_>,        // Operators/Operands/Groups -> Oper*s/Groups/Applications
    BuildGroups<'s, _>,   // Operators/Operands/Groups/Applications -> Oper*s/Applications
    Reduce<'s>            // Operators/Operands/Applications -> Tree
];

/// Operator precedence resolver.
#[derive(Debug, Default)]
pub struct Precedence<'s> {
    resolver: Resolver<'s>,
}

impl<'s> Precedence<'s> {
    /// Return a new operator precedence resolver.
    pub fn new() -> Self {
        Self::default()
    }

    /// Resolve precedence in a context where the result cannot be an operator section or template
    /// function.
    pub fn resolve_non_section(&mut self, items: &mut Vec<Item<'s>>) -> Option<Tree<'s>> {
        self.resolve_non_section_offset(0, items)
    }

    /// Resolve precedence.
    pub fn resolve(&mut self, items: &mut Vec<Item<'s>>) -> Option<Tree<'s>> {
        self.resolve_offset(0, items)
    }

    /// Resolve precedence in a context where the result cannot be an operator section or template
    /// function.
    pub fn resolve_non_section_offset(
        &mut self,
        start: usize,
        items: &mut Vec<Item<'s>>,
    ) -> Option<Tree<'s>> {
        self.parse_item_tree(start, items).map(|op| op.value)
    }

    /// Resolve precedence.
    pub fn resolve_offset(&mut self, start: usize, items: &mut Vec<Item<'s>>) -> Option<Tree<'s>> {
        self.parse_item_tree(start, items).map(Tree::from)
    }

    fn parse_item_tree(
        &mut self,
        start: usize,
        items: &mut Vec<Item<'s>>,
    ) -> Option<MaybeSection<Tree<'s>>> {
        self.resolver.run(start, items)
    }
}
