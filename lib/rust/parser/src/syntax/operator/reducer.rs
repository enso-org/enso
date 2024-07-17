use crate::prelude::*;
use crate::syntax::operator::apply::*;
use crate::syntax::operator::types::*;

use crate::syntax::operator::section::MaybeSection;
use crate::syntax::token;
use crate::syntax::tree::apply;
use crate::syntax::Finish;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::Tree;



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
    output:         Vec<MaybeSection<Tree<'s>>>,
    operator_stack: Vec<StackOperator<'s>>,
    scope_stack:    Vec<(u32, u32)>,
}

impl<'s> OperandConsumer<'s> for Reduce<'s> {
    fn push_operand(&mut self, operand: MaybeSection<Tree<'s>>) {
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
    type Result = Option<MaybeSection<Tree<'s>>>;

    fn finish(&mut self) -> Self::Result {
        self.reduce(ModifiedPrecedence::min());
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

impl<'s> ScopeHierarchyConsumer for Reduce<'s> {
    type Result = Option<Tree<'s>>;

    fn start_scope(&mut self) {
        let operators = self.operator_stack.len() as u32;
        let operands = self.output.len() as u32;
        self.scope_stack.push((operators, operands));
    }

    fn end_scope(&mut self) -> Self::Result {
        let result = if self.output.len() > self.scope_start().1 {
            self.reduce(ModifiedPrecedence::min());
            self.output.pop().map(Tree::from)
        } else {
            None
        };
        self.scope_stack.pop();
        result
    }
}

impl<'s> Reduce<'s> {
    /// Given a starting value, replace it with the result of successively applying to it all
    /// operators in the `operator_stack` that have precedence greater than or equal to the
    /// specified value, consuming LHS values from the `output` stack as needed.
    fn reduce(&mut self, right_op_precedence: ModifiedPrecedence) -> Warnings {
        let mut operand = self.output.pop();
        let mut right_op_warnings = Warnings::default();
        let scope_start = self.scope_start().0;
        while self.operator_stack.len() > scope_start {
            let opr = self.operator_stack.last_mut().unwrap();
            let ModifiedPrecedenceComparisonResult { is_greater, inconsistent_spacing } = opr
                .right_precedence
                .compare(&right_op_precedence, opr.associativity == token::Associativity::Left);
            if inconsistent_spacing {
                if is_greater { &mut right_op_warnings } else { &mut opr.warnings }
                    .set_inconsistent_spacing();
            }
            if !is_greater {
                break;
            }
            let opr = self.operator_stack.pop().unwrap();
            let StackOperator { right_precedence: _, associativity: _, arity, warnings } = opr;
            match arity {
                Arity::Unary(token) => {
                    let rhs = operand.take();
                    debug_assert_ne!(rhs, None);
                    operand = ApplyUnaryOperator::token(token)
                        .with_rhs(rhs)
                        .with_warnings(warnings)
                        .finish()
                        .into();
                }
                Arity::Binary { tokens, missing, reify_rhs_section } => {
                    let op1 = operand.take();
                    debug_assert_ne!(op1, None);
                    let (lhs, rhs) = match missing {
                        Some(BinaryOperand::Left) => (None, op1),
                        Some(BinaryOperand::Right) => (op1, None),
                        None => {
                            let lhs = self.output.pop();
                            debug_assert_ne!(lhs, None);
                            (lhs, op1)
                        }
                    };
                    operand = ApplyOperator::tokens(tokens)
                        .with_lhs(lhs)
                        .with_rhs(rhs, reify_rhs_section)
                        .with_warnings(warnings)
                        .finish()
                        .into();
                }
                Arity::App => {
                    let (lhs, rhs) = (self.output.pop().unwrap(), operand.take());
                    let mut tree = lhs.map(|lhs| apply(lhs, rhs.unwrap().into()));
                    warnings.apply(&mut tree.value);
                    operand = tree.into();
                }
                Arity::NamedApp(box NamedApp { parens, name, equals, expression }) => {
                    let func = operand.take().unwrap();
                    let (open, close) = match parens {
                        None => (None, None),
                        Some((open, close)) => (Some(open), close),
                    };
                    let mut tree = func
                        .map(|func| Tree::named_app(func, open, name, equals, expression, close));
                    warnings.apply(&mut tree.value);
                    operand = tree.into();
                }
            };
        }
        if let Some(rhs) = operand {
            self.output.push(rhs);
        }
        right_op_warnings
    }

    fn scope_start(&self) -> (usize, usize) {
        let (operators, operands) = self.scope_stack.last().copied().unwrap_or_default();
        (operators as usize, operands as usize)
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
