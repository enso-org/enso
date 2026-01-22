use crate::prelude::*;

use crate::syntax::Item;
use crate::syntax::Tree;
use crate::syntax::expression::annotations::ParseAnnotations;
use crate::syntax::expression::application::InsertApps;
use crate::syntax::expression::arity::ClassifyArity;
use crate::syntax::expression::blocks::FlattenBlockTrees;
use crate::syntax::expression::compound_token::CompoundTokens;
use crate::syntax::expression::group::BuildGroups;
use crate::syntax::expression::group::FlattenGroups;
use crate::syntax::expression::named_app::ParseAppNames;
use crate::syntax::expression::numbers::ParseNumbers;
use crate::syntax::expression::operand::Operand;
use crate::syntax::expression::reducer::Reduce;
use crate::syntax::expression::whitespace::PeekSpacing;
use crate::unwrap_call;

// =========================
// === Expression Parser ===
// =========================

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

type Pipeline<'s> = compose_types![
    FlattenBlockTrees<'s, _>, // Items -> Tokens/Trees/GroupItems
    FlattenGroups<'s, _>,     // Tokens/Trees/Operators/GroupItems -> T/T/O/(Group hierarchy)
    // BEGIN `GroupHierarchyConsumer`s
    CompoundTokens<'s, _>,
    ParseNumbers<'s, _>,
    PeekSpacing<'s, _>, // Tokens/Trees/Operators -> Tokens/Trees/Operators + Spacing-lookahead
    ParseAnnotations<'s, _>, // Tokens/Trees/Operators + Spacing -> T/T/Operators + Spacing
    ParseAppNames<'s, _>,
    // BEGIN `ScopeHierarchyConsumer`s
    ClassifyArity<'s, _>, // Tokens/Trees + Spacing-lookahead -> Oper*s
    InsertApps<_>,        // Operators/Operands -> Oper*s/Applications
    BuildGroups<'s, _>,   // Operators/Operands/Applications -> Oper*s/Applications
    // END `GroupHierarchyConsumer`s
    Reduce<'s> // Operators/Operands/Applications -> Tree
];

/// Expression parser.
///
/// Internally, this is a pipeline of stack machines (many of which are simply reentrant FSMs),
/// terminating in an operator-precedence parser.
#[derive(Debug, Default)]
pub struct ExpressionParser<'s> {
    pipeline: Pipeline<'s>,
}

impl<'s> ExpressionParser<'s> {
    /// Return a new expression parser.
    pub fn new() -> Self {
        Self::default()
    }

    /// Parse an expression.
    pub fn parse(&mut self, items: &mut Vec<Item<'s>>) -> Option<Tree<'s>> {
        self.parse_offset(0, items)
    }

    /// Parse an expression in a context where the result cannot be an operator section or template
    /// function.
    pub fn parse_non_section(&mut self, items: &mut Vec<Item<'s>>) -> Option<Tree<'s>> {
        self.parse_non_section_offset(0, items)
    }

    /// Parse an expression.
    pub fn parse_offset(&mut self, start: usize, items: &mut Vec<Item<'s>>) -> Option<Tree<'s>> {
        self.parse_item_tree(start, items).map(Tree::from)
    }

    /// Parse an expression in a context where the result cannot be an operator section or template
    /// function.
    pub fn parse_non_section_offset(
        &mut self,
        start: usize,
        items: &mut Vec<Item<'s>>,
    ) -> Option<Tree<'s>> {
        self.parse_item_tree(start, items).map(|op| op.value).map(unwrap_call)
    }

    fn parse_item_tree(&mut self, start: usize, items: &mut Vec<Item<'s>>) -> Option<Operand<'s>> {
        self.pipeline.run(start, items)
    }
}
