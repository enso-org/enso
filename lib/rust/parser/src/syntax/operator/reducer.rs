use crate::syntax::operator::apply::*;
use crate::syntax::operator::types::*;

use crate::syntax::operator::operand::Operand;
use crate::syntax::token;
use crate::syntax::treebuilding::Finish;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::Tree;

use enso_prelude::VecOps;



// ===============
// === Reducer ===
// ===============

/// Stack machine that builds an expression from syntax nodes.
///
/// The operator-precedence algorithm[1] used is based on the shunting yard algorithm[2], extended
/// to support *operator sections*, function application, and unary operators, and correctly report
/// errors relating to consecutive operators.
///
/// [^1](https://en.wikipedia.org/wiki/Operator-precedence_parser)
/// [^2](https://en.wikipedia.org/wiki/Shunting_yard_algorithm)
#[derive(Default, Debug)]
pub struct Reduce<'s> {
    output:         Vec<Operand<Tree<'s>>>,
    operator_stack: Vec<StackOperator<'s>>,
}

impl<'s> OperandConsumer<'s> for Reduce<'s> {
    fn push_operand(&mut self, operand: Operand<Tree<'s>>) {
        self.output.push(operand)
    }
}

impl<'s> OperatorConsumer<'s> for Reduce<'s> {
    fn push_operator(&mut self, operator: Operator<'s>) {
        let Operator { left_precedence, right_precedence, associativity, arity } = operator;
        let warnings = if let Some(precedence) = left_precedence {
            self.reduce(precedence)
        } else {
            Default::default()
        };
        self.operator_stack.push(StackOperator {
            right_precedence,
            associativity,
            arity,
            warnings,
        });
    }
}

impl<'s> Finish for Reduce<'s> {
    type Result = Option<Operand<Tree<'s>>>;

    fn finish(&mut self) -> Self::Result {
        self.reduce(ModifiedPrecedence {
            spacing:            Spacing::Spaced,
            precedence:         token::Precedence::min(),
            is_value_operation: false,
        });
        let out = self.output.pop();
        debug_assert!(self.operator_stack.is_empty());
        debug_assert_eq!(
            &self.output,
            &[],
            "Internal error. Not all tokens were consumed while constructing the expression."
        );
        out
    }
}

impl<'s> Reduce<'s> {
    /// Given a starting value, replace it with the result of successively applying to it all
    /// operators in the `operator_stack` that have precedence greater than or equal to the
    /// specified value, consuming LHS values from the `output` stack as needed.
    fn reduce(&mut self, right_op_precedence: ModifiedPrecedence) -> Warnings {
        let mut rhs = self.output.pop();
        let mut right_op_warnings = Warnings::default();
        while let Some(opr) = self.operator_stack.pop_if_mut(|opr| {
            let ModifiedPrecedenceComparisonResult { is_greater, inconsistent_spacing } = opr
                .right_precedence
                .compare(&right_op_precedence, opr.associativity == token::Associativity::Left);
            if inconsistent_spacing {
                if is_greater { &mut right_op_warnings } else { &mut opr.warnings }
                    .set_inconsistent_spacing();
            }
            is_greater
        }) {
            let StackOperator { right_precedence: _, associativity: _, arity, warnings } = opr;
            match arity {
                Arity::Unary { token, error } => {
                    let rhs_ = rhs.take();
                    debug_assert_ne!(rhs_, None);
                    rhs = ApplyUnaryOperator::token(token)
                        .with_rhs(rhs_)
                        .with_error(error)
                        .with_warnings(warnings)
                        .finish()
                        .into();
                }
                Arity::Binary { tokens, missing, reify_rhs_section } => {
                    let operand = rhs.take();
                    debug_assert_ne!(operand, None);
                    let (lhs, rhs_) = match missing {
                        Some(BinaryOperand::Left) => (None, operand),
                        Some(BinaryOperand::Right) => (operand, None),
                        None => {
                            let lhs = self.output.pop();
                            debug_assert_ne!(lhs, None);
                            (lhs, operand)
                        }
                    };
                    rhs = ApplyOperator::tokens(tokens)
                        .with_lhs(lhs)
                        .with_rhs(rhs_, reify_rhs_section)
                        .with_warnings(warnings)
                        .finish()
                        .into();
                }
            };
        }
        if let Some(rhs) = rhs {
            self.output.push(rhs);
        }
        right_op_warnings
    }
}


// === Operator on-stack information ===

#[derive(Debug)]
struct StackOperator<'s> {
    right_precedence: ModifiedPrecedence,
    associativity:    token::Associativity,
    arity:            Arity<'s>,
    warnings:         Warnings,
}
