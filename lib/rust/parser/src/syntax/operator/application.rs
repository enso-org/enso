use enso_prelude::*;

use crate::syntax::operator::operand::Operand;
use crate::syntax::operator::types::Arity;
use crate::syntax::operator::types::BinaryOperand;
use crate::syntax::operator::types::ModifiedPrecedence;
use crate::syntax::operator::types::Operator;
use crate::syntax::operator::OperandConsumer;
use crate::syntax::operator::OperatorConsumer;
use crate::syntax::token;
use crate::syntax::treebuilding::Finish;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::Tree;



// ===================
// === Insert Apps ===
// ===================

/// Inserts applications between terms as needed.
#[derive(Default, Debug)]
pub struct InsertApps<Inner> {
    prev_applicable: bool,
    inner:           Inner,
}

impl<'s, Inner: OperatorConsumer<'s> + OperandConsumer<'s>> OperandConsumer<'s>
    for InsertApps<Inner>
{
    fn push_operand(&mut self, operand: Operand<Tree<'s>>) {
        if mem::replace(&mut self.prev_applicable, true) {
            self.inner.push_operator(application(Spacing::of_tree(&operand.value)));
        }
        self.inner.push_operand(operand)
    }
}

impl<'s, Inner: OperatorConsumer<'s>> OperatorConsumer<'s> for InsertApps<Inner> {
    fn push_operator(&mut self, operator: Operator<'s>) {
        let prev_applicable = mem::replace(
            &mut self.prev_applicable,
            matches!(operator.arity, Arity::Binary { missing: Some(BinaryOperand::Right), .. }),
        );
        if prev_applicable
            && matches!(
                operator.arity,
                Arity::Unary { .. } | Arity::Binary { missing: Some(BinaryOperand::Left), .. }
            )
        {
            self.inner.push_operator(application(Spacing::Spaced));
        }
        self.inner.push_operator(operator)
    }
}

impl<Inner: Finish> Finish for InsertApps<Inner> {
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.prev_applicable = false;
        self.inner.finish()
    }
}

fn application<'s>(spacing: Spacing) -> Operator<'s> {
    let precedence = ModifiedPrecedence { spacing, precedence: token::Precedence::application() };
    Operator {
        left_precedence:  Some(precedence),
        right_precedence: precedence,
        associativity:    token::Associativity::Left,
        arity:            Arity::Binary {
            tokens:                  default(),
            lhs_section_termination: default(),
            missing:                 None,
            reify_rhs_section:       true,
        },
    }
}
