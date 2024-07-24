use crate::syntax::operator::operand::Operand;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::Tree;

use std::borrow::Cow;
use std::cmp::Ordering;



// ================
// === Operator ===
// ================

/// An operator, whose arity and precedence have been determined.
#[derive(Debug)]
pub struct Operator<'s> {
    pub left_precedence:  Option<ModifiedPrecedence>,
    pub right_precedence: ModifiedPrecedence,
    pub associativity:    token::Associativity,
    pub arity:            Arity<'s>,
}


// === Arity ===

/// Classifies the role of an operator.
#[derive(Debug)]
pub enum Arity<'s> {
    Unary {
        token: token::Operator<'s>,
        error: Option<Cow<'static, str>>,
    },
    Binary {
        tokens:            Vec<token::Operator<'s>>,
        missing:           Option<BinaryOperand>,
        reify_rhs_section: bool,
    },
}

impl<'s> Arity<'s> {
    fn unary(token: token::Operator<'s>) -> Self {
        Self::Unary { token, error: None }
    }
}


// === Binary operand ===

#[derive(Debug)]
pub enum BinaryOperand {
    Left,
    Right,
}


// === Modified precedence ===

#[derive(Debug, Copy, Clone)]
pub struct ModifiedPrecedence {
    pub spacing:            Spacing,
    pub precedence:         token::Precedence,
    pub is_value_operation: bool,
}

pub struct ModifiedPrecedenceComparisonResult {
    pub is_greater:           bool,
    pub inconsistent_spacing: bool,
}

impl ModifiedPrecedence {
    pub fn compare(&self, other: &Self, include_eq: bool) -> ModifiedPrecedenceComparisonResult {
        let spacing_ordering = match (self.spacing, other.spacing) {
            (Spacing::Spaced, Spacing::Unspaced) => Some(Ordering::Less),
            (Spacing::Unspaced, Spacing::Spaced) => Some(Ordering::Greater),
            _ => None,
        };
        let use_spacing = !(self.is_value_operation && other.is_value_operation);
        let natural_ordering = self.precedence.cmp(&other.precedence);
        let natural_is_greater = natural_ordering == Ordering::Greater
            || (include_eq && natural_ordering == Ordering::Equal);
        let (is_greater, inconsistent_spacing) = match spacing_ordering {
            Some(spacing_ordering) => {
                let spacing_is_greater = spacing_ordering == Ordering::Greater
                    || (include_eq && spacing_ordering == Ordering::Equal);
                if use_spacing {
                    (spacing_is_greater, false)
                } else {
                    (natural_is_greater, natural_is_greater != spacing_is_greater)
                }
            }
            None => (natural_is_greater, false),
        };
        ModifiedPrecedenceComparisonResult { is_greater, inconsistent_spacing }
    }
}


// ================
// === Warnings ===
// ================

/// Warnings applicable to an operator.
#[derive(Debug, Default)]
#[allow(missing_copy_implementations)] // Future warnings may have attached information.
pub struct Warnings {
    inconsistent_spacing: bool,
}

impl Warnings {
    /// Mark the operator as having spacing inconsistent with effective precedence.
    pub fn set_inconsistent_spacing(&mut self) {
        self.inconsistent_spacing = true;
    }

    /// Attach the warnings to the tree.
    pub fn apply(self, tree: &mut Tree) {
        let Self { inconsistent_spacing } = self;
        if inconsistent_spacing {
            tree.warnings.push(tree::Warning::inconsistent_spacing());
        }
    }
}


// ======================================
// === Operator and Operand Consumers ===
// ======================================

pub trait OperandConsumer<'s> {
    fn push_operand(&mut self, operand: Operand<Tree<'s>>);
}

pub trait OperatorConsumer<'s> {
    fn push_operator(&mut self, operator: Operator<'s>);
}


// ===========================
// === Operator or Operand ===
// ===========================

#[derive(Debug)]
pub enum OperatorOrOperand<'s> {
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


// ==========================
// === SectionTermination ===
// ==========================

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
