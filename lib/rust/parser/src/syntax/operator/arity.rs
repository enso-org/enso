use crate::syntax::operator::apply::*;
use crate::syntax::operator::types::*;
use enso_prelude::*;

use crate::syntax::operator::operand::Operand;
use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::treebuilding::Finish;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::treebuilding::SpacingLookaheadTokenConsumer;
use crate::syntax::treebuilding::TreeConsumer;
use crate::syntax::Token;
use crate::syntax::Tree;



// ======================
// === Classify Arity ===
// ======================

/// Determines the number of operands consumed by each term.
#[derive(Default, Debug)]
pub struct ClassifyArity<'s, Inner> {
    /// Next item that will be emitted. If it is an operator, it may still be extended with
    /// additional operators to become a multiple-operator error.
    lhs_item: Option<OperatorOrOperand<'s>>,
    inner:    Inner,
}

impl<'s, Inner: OperandConsumer<'s> + OperatorConsumer<'s>> SpacingLookaheadTokenConsumer<'s>
    for ClassifyArity<'s, Inner>
{
    fn push_token(&mut self, tt: Token<'s>, rhs: Option<Spacing>) {
        match tt {
            Token { variant: token::Variant::Operator(opr), left_offset, code } =>
                self.operator(Token(left_offset, code, opr), rhs),
            token => self.push_tree(tree::to_ast(token)),
        }
    }
}

impl<'s, Inner: OperandConsumer<'s> + OperatorConsumer<'s>> TreeConsumer<'s>
    for ClassifyArity<'s, Inner>
{
    fn push_tree(&mut self, tree: Tree<'s>) {
        self.emit(Operand::from(tree))
    }
}

impl<'s, Inner: OperandConsumer<'s> + OperatorConsumer<'s> + Finish> Finish
    for ClassifyArity<'s, Inner>
{
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.step(None);
        self.inner.finish()
    }
}

impl<'s, Inner: OperandConsumer<'s> + OperatorConsumer<'s>> ClassifyArity<'s, Inner> {
    fn emit<T: Into<OperatorOrOperand<'s>>>(&mut self, item: T) {
        self.step(Some(item.into()));
    }

    fn step(&mut self, item: Option<OperatorOrOperand<'s>>) {
        match mem::replace(&mut self.lhs_item, item) {
            Some(OperatorOrOperand::Operand(item)) => self.inner.push_operand(item),
            Some(OperatorOrOperand::Operator(item)) => self.inner.push_operator(item),
            None => (),
        }
    }

    fn operator(&mut self, token: token::Operator<'s>, rhs: Option<Spacing>) {
        let properties = &token.variant.properties;
        let lhs = match self.lhs_item {
            Some(
                OperatorOrOperand::Operand(_)
                | OperatorOrOperand::Operator(Operator {
                    arity: Arity::Binary { missing: Some(BinaryOperand::Right), .. },
                    ..
                }),
            ) => Some(Spacing::of_token(&token)),
            _ => None,
        };
        // Asymmetric whitespace creates operator sections.
        // Exception: If an operator cannot form sections, and its LHS is unspaced, a spaced RHS is
        // accepted.
        let (lhs, rhs) = match (properties.can_form_section(), lhs, rhs) {
            (true, Some(Spacing::Unspaced), Some(Spacing::Spaced)) =>
                (Some(Spacing::Unspaced), None),
            (_, Some(Spacing::Spaced), Some(Spacing::Unspaced)) => (None, Some(Spacing::Unspaced)),
            (_, lhs, rhs) => (lhs, rhs),
        };
        let assoc = properties.associativity();
        let binary = properties.binary_infix_precedence();
        let unary = properties.unary_prefix_precedence();
        match (binary, unary, lhs, rhs) {
            (_, Some(unary), None, Some(Spacing::Unspaced)) =>
                self.unary_operator_applied(unary, assoc, token),
            (Some(binary), _, _, _) => self.binary_operator(binary, assoc, token, lhs, rhs),
            (_, Some(_), _, _) => self.unary_operator_section(token, rhs),
            (None, None, _, _) => unreachable!(),
        }
    }

    fn unary_operator_applied(
        &mut self,
        precedence: token::Precedence,
        associativity: token::Associativity,
        token: token::Operator<'s>,
    ) {
        let error = match self.lhs_item {
            Some(OperatorOrOperand::Operand(_))
                if token.left_offset.visible.width_in_spaces == 0 =>
                Some("Space required between term and unary-operator expression.".into()),
            _ => None,
        };
        let is_value_operation = token.properties.is_value_operation();
        self.emit(Operator {
            left_precedence: None,
            right_precedence: ModifiedPrecedence {
                spacing: Spacing::Unspaced,
                precedence,
                is_value_operation,
            },
            associativity,
            arity: Arity::Unary { token, error },
        });
    }

    fn unary_operator_section(&mut self, token: token::Operator<'s>, rhs: Option<Spacing>) {
        match &mut self.lhs_item {
            Some(OperatorOrOperand::Operator(Operator {
                arity: Arity::Binary { tokens, .. },
                ..
            })) if !(tokens.first().unwrap().left_offset.visible.width_in_spaces == 0
                && token.left_offset.visible.width_in_spaces == 0) =>
                self.multiple_operator_error(token, rhs),
            _ => self.emit(ApplyUnaryOperator::token(token).finish()),
        }
    }

    fn binary_operator(
        &mut self,
        precedence: token::Precedence,
        associativity: token::Associativity,
        token: token::Operator<'s>,
        lhs: Option<Spacing>,
        rhs: Option<Spacing>,
    ) {
        if let Some(OperatorOrOperand::Operator(Operator {
            arity: Arity::Binary { missing: None | Some(BinaryOperand::Left), .. },
            ..
        })) = &self.lhs_item
            && !matches!(rhs, Some(Spacing::Unspaced))
        {
            self.multiple_operator_error(token, rhs);
            return;
        }
        let missing = match (lhs, rhs) {
            (None, None) => {
                self.emit(ApplyOperator::token(token).finish());
                return;
            }
            (Some(_), None) => Some(BinaryOperand::Right),
            (None, Some(_)) => Some(BinaryOperand::Left),
            (Some(_), Some(_)) => None,
        };
        let reify_rhs_section = token.properties.can_form_section()
            && (lhs == Some(Spacing::Spaced) || rhs == Some(Spacing::Spaced));
        let is_value_operation = missing.is_none() && token.properties.is_value_operation();
        self.emit(Operator {
            left_precedence: lhs.map(|spacing| ModifiedPrecedence {
                spacing,
                precedence,
                is_value_operation,
            }),
            right_precedence: ModifiedPrecedence {
                spacing: rhs.or(lhs).unwrap(),
                precedence,
                is_value_operation,
            },
            associativity,
            arity: Arity::Binary { tokens: vec![token], missing, reify_rhs_section },
        });
    }

    fn multiple_operator_error(&mut self, token: token::Operator<'s>, rhs: Option<Spacing>) {
        match &mut self.lhs_item {
            Some(OperatorOrOperand::Operator(Operator {
                arity: Arity::Binary { tokens, missing, .. },
                ..
            })) => {
                tokens.push(token);
                if rhs.is_none() {
                    match missing {
                        None => *missing = Some(BinaryOperand::Right),
                        Some(BinaryOperand::Left) =>
                            self.lhs_item = Some(OperatorOrOperand::Operand(
                                ApplyOperator::tokens(mem::take(tokens)).finish(),
                            )),
                        Some(BinaryOperand::Right) => unreachable!(),
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}
