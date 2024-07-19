use crate::syntax::operator::apply_operator;
use crate::syntax::operator::apply_unary_operator;
use crate::syntax::operator::Arity;
use crate::syntax::operator::BinaryOperand;
use crate::syntax::operator::ModifiedPrecedence;
use crate::syntax::operator::Operand;
use crate::syntax::operator::OperandConsumer;
use crate::syntax::operator::Operator;
use crate::syntax::operator::OperatorConsumer;
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
    operator_stack: Vec<Operator<'s>>,
}

impl<'s> OperandConsumer<'s> for Reduce<'s> {
    fn push_operand(&mut self, operand: Operand<Tree<'s>>) {
        self.output.push(operand)
    }
}

impl<'s> OperatorConsumer<'s> for Reduce<'s> {
    fn push_operator(&mut self, operator: Operator<'s>) {
        if let Some(precedence) = operator.left_precedence {
            self.reduce(precedence);
        }
        self.operator_stack.push(operator);
    }
}

impl<'s> Finish for Reduce<'s> {
    type Result = Option<Operand<Tree<'s>>>;

    fn finish(&mut self) -> Self::Result {
        self.reduce(ModifiedPrecedence {
            spacing:    Spacing::Spaced,
            precedence: token::Precedence::min(),
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
    fn reduce(&mut self, prec: ModifiedPrecedence) {
        let mut rhs = self.output.pop();
        while let Some(opr) = self.operator_stack.pop_if(|opr| {
            opr.right_precedence > prec
                || (opr.right_precedence == prec && opr.associativity == token::Associativity::Left)
        }) {
            match opr.arity {
                Arity::Unary { token, error } => {
                    let rhs_ = rhs.take();
                    debug_assert_ne!(rhs_, None);
                    rhs = Some(apply_unary_operator(token, rhs_, error));
                }
                Arity::Binary { tokens, lhs_section_termination, missing, reify_rhs_section } => {
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
                    rhs = Some(apply_operator(
                        tokens,
                        lhs_section_termination,
                        reify_rhs_section,
                        lhs,
                        rhs_,
                    ));
                }
            };
        }
        if let Some(rhs) = rhs {
            self.output.push(rhs);
        }
    }
}
