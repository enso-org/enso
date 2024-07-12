//! Operator related functionalities.



mod application;
mod arity;
mod operand;
mod types;
mod reducer;

use crate::prelude::*;

use crate::syntax;
use crate::syntax::operator::application::InsertApps;
use crate::syntax::operator::arity::ClassifyArity;
use crate::syntax::operator::operand::Operand;
use crate::syntax::operator::types::Arity;
use crate::syntax::operator::types::BinaryOperand;
use crate::syntax::operator::types::ModifiedPrecedence;
use crate::syntax::operator::types::Operator;
use crate::syntax::operator::reducer::Reduce;
use crate::syntax::token;
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


// === Operator or Operand ===

#[derive(Debug)]
enum OperatorOrOperand<'s> {
    Operand(Operand<Tree<'s>>),
    Operator(Operator<'s>),
}

impl<'s> From<Operand<Tree<'s>>> for OperatorOrOperand<'s> {
    fn from(operand: Operand<Tree<'s>>) -> Self {
        OperatorOrOperand::Operand(operand)
    }
}

impl<'s> From<Operator<'s>> for OperatorOrOperand<'s> {
    fn from(operator: Operator<'s>) -> Self {
        OperatorOrOperand::Operator(operator)
    }
}


// === Applying operators ===

fn apply_operator<'s>(
    tokens: Vec<token::Operator<'s>>,
    lhs_section_termination: Option<SectionTermination>,
    reify_rhs_section: bool,
    lhs: Option<Operand<Tree<'s>>>,
    rhs_: Option<Operand<Tree<'s>>>,
) -> Operand<Tree<'s>> {
    if let Some(lhs_termination) = lhs_section_termination {
        let lhs = match lhs_termination {
            SectionTermination::Reify => lhs.map(Tree::from),
            SectionTermination::Unwrap => lhs.map(|op| op.value),
        };
        let rhs = rhs_.map(Tree::from);
        let ast = syntax::tree::apply_operator(lhs, tokens, rhs);
        Operand::from(ast)
    } else if tokens.len() < 2
        && let Some(opr) = tokens.first()
        && opr.properties.can_form_section()
    {
        let mut rhs = None;
        let mut elided = 0;
        let mut wildcards = 0;
        if let Some(rhs_) = rhs_ {
            if reify_rhs_section {
                rhs = Some(Tree::from(rhs_));
            } else {
                rhs = Some(rhs_.value);
                elided += rhs_.elided;
                wildcards += rhs_.wildcards;
            }
        }
        elided += lhs.is_none() as u32 + rhs.is_none() as u32;
        let mut operand =
            Operand::from(lhs).map(|lhs| syntax::tree::apply_operator(lhs, tokens, rhs));
        operand.elided += elided;
        operand.wildcards += wildcards;
        operand
    } else {
        let rhs = rhs_.map(Tree::from);
        let mut elided = 0;
        if tokens.len() != 1 || tokens[0].properties.can_form_section() {
            elided += lhs.is_none() as u32 + rhs.is_none() as u32;
        }
        let mut operand =
            Operand::from(lhs).map(|lhs| syntax::tree::apply_operator(lhs, tokens, rhs));
        operand.elided += elided;
        operand
    }
}

fn apply_unary_operator<'s>(
    token: token::Operator<'s>,
    rhs: Option<Operand<Tree<'s>>>,
    error: Option<Cow<'static, str>>,
) -> Operand<Tree<'s>> {
    match error {
        None => Operand::new(rhs).map(|item| syntax::tree::apply_unary_operator(token, item)),
        Some(error) => Operand::from(rhs)
            .map(|item| syntax::tree::apply_unary_operator(token, item).with_error(error)),
    }
}


// === Operator and Operand Consumers ===

trait OperandConsumer<'s> {
    fn push_operand(&mut self, operand: Operand<Tree<'s>>);
}

trait OperatorConsumer<'s> {
    fn push_operator(&mut self, operator: Operator<'s>);
}


// === SectionTermination ===

/// Operator-section/template-function termination behavior of an operator with regard to an
/// operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum SectionTermination {
    /// If the operand is an operator-section/template-function, indicate it by wrapping it in a
    /// suitable node.
    #[default]
    Reify,
    /// Discard any operator-section/template-function properties associated with the operand.
    Unwrap,
}
