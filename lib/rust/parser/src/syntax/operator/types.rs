use crate::syntax::operator::SectionTermination;
use crate::syntax::token;
use crate::syntax::treebuilding::Spacing;

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
        tokens:                  Vec<token::Operator<'s>>,
        lhs_section_termination: Option<SectionTermination>,
        missing:                 Option<BinaryOperand>,
        reify_rhs_section:       bool,
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ModifiedPrecedence {
    pub spacing:    Spacing,
    pub precedence: token::Precedence,
}

impl PartialOrd for ModifiedPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.spacing, other.spacing) {
            (Spacing::Spaced, Spacing::Unspaced) => Some(Ordering::Less),
            (Spacing::Unspaced, Spacing::Spaced) => Some(Ordering::Greater),
            _ => self.precedence.partial_cmp(&other.precedence),
        }
    }
}
