use crate::syntax::operator::apply::*;
use crate::syntax::operator::types::*;
use enso_prelude::*;

use crate::syntax::token;
use crate::syntax::token::OperatorProperties;
use crate::syntax::token::TokenOperatorProperties;
use crate::syntax::tree;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::treebuilding::SpacingLookaheadTokenConsumer;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::Token;



// ======================
// === Classify Arity ===
// ======================

/// Determines the number of operands consumed by each term.
#[derive(Default, Debug)]
pub struct ClassifyArity<'s, Inner> {
    /// Next item that will be emitted. If it is an operator, it may still be extended with
    /// additional operators to become a multiple-operator error.
    lhs_item: Option<MaybeOperator<'s>>,
    inner:    Inner,
}

impl<'s, Inner> SpacingLookaheadTokenConsumer<'s> for ClassifyArity<'s, Inner>
where Inner: NamedOperandConsumer<'s> + OperatorConsumer<'s>
{
    fn push_token(&mut self, token: Token<'s>, rhs: Option<Spacing>) {
        let properties = token.operator_properties();
        match properties {
            Some(properties) => self.operator(token, properties, rhs),
            None => self.push_operand(tree::to_ast(token).into()),
        }
    }
}

impl<'s, Inner> NamedOperandConsumer<'s> for ClassifyArity<'s, Inner>
where Inner: NamedOperandConsumer<'s> + OperatorConsumer<'s>
{
    fn push_maybe_named_operand(&mut self, operand: OperandMaybeNamed<'s>) {
        self.emit(MaybeOperator::Operand);
        self.inner.push_maybe_named_operand(operand);
    }
}

impl<'s, Inner> Finish for ClassifyArity<'s, Inner>
where Inner: NamedOperandConsumer<'s> + OperatorConsumer<'s> + Finish
{
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.flush();
        self.inner.finish()
    }
}

impl<'s, Inner> ClassifyArity<'s, Inner>
where Inner: NamedOperandConsumer<'s> + OperatorConsumer<'s>
{
    fn emit<T: Into<MaybeOperator<'s>>>(&mut self, item: T) {
        self.step(Some(item.into()));
    }

    fn flush(&mut self) {
        self.step(None);
    }

    fn step(&mut self, item: Option<MaybeOperator<'s>>) {
        if let Some(MaybeOperator::Operator(item)) = mem::replace(&mut self.lhs_item, item) {
            self.inner.push_operator(item)
        }
    }

    fn operator(&mut self, token: Token<'s>, properties: OperatorProperties, rhs: Option<Spacing>) {
        let lhs = if self.lhs_item.as_ref().is_some_and(|item| !item.expects_rhs()) {
            Some(Spacing::of_token(&token))
        } else {
            None
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
            (_, Some(_), _, _) => self.unary_operator_section(token),
            (None, None, _, _) => unreachable!(),
        }
    }

    fn unary_operator_applied(
        &mut self,
        precedence: token::Precedence,
        associativity: token::Associativity,
        token: Token<'s>,
    ) {
        let is_value_operation = token.operator_properties().unwrap().is_value_operation();
        self.emit(Operator {
            left_precedence: None,
            right_precedence: ModifiedPrecedence::new(
                Spacing::Unspaced,
                precedence,
                is_value_operation,
            ),
            associativity,
            arity: Arity::Unary(token.with_variant(token::variant::UnaryOperator())),
        });
    }

    fn unary_operator_section(&mut self, token: Token<'s>) {
        self.emit(MaybeOperator::Operand);
        self.inner.push_maybe_named_operand(OperandMaybeNamed::Unnamed(
            ApplyUnaryOperator::token(token.with_variant(token::variant::UnaryOperator())).finish(),
        ));
    }

    fn binary_operator(
        &mut self,
        precedence: token::Precedence,
        associativity: token::Associativity,
        token: Token<'s>,
        lhs: Option<Spacing>,
        rhs: Option<Spacing>,
    ) {
        if self.lhs_item.as_ref().is_some_and(|item| item.expects_rhs())
            && rhs != Some(Spacing::Unspaced)
        {
            self.multiple_operator_error(token, rhs);
            return;
        }
        let missing = match (lhs, rhs) {
            (None, None) => {
                self.inner.push_maybe_named_operand(OperandMaybeNamed::Unnamed(
                    ApplyOperator::token(token).finish(),
                ));
                self.emit(MaybeOperator::Operand);
                return;
            }
            (Some(_), None) => Some(BinaryOperand::Right),
            (None, Some(_)) => Some(BinaryOperand::Left),
            (Some(_), Some(_)) => None,
        };
        let properties = token.operator_properties().unwrap();
        let reify_rhs_section = properties.can_form_section()
            && (lhs == Some(Spacing::Spaced) || rhs == Some(Spacing::Spaced));
        let is_value_operation = missing.is_none() && properties.is_value_operation();
        self.emit(Operator {
            left_precedence: lhs
                .map(|spacing| ModifiedPrecedence::new(spacing, precedence, is_value_operation)),
            right_precedence: ModifiedPrecedence::new(
                rhs.or(lhs).unwrap(),
                precedence,
                is_value_operation,
            ),
            associativity,
            arity: Arity::Binary { tokens: vec![token], missing, reify_rhs_section },
        });
    }

    fn multiple_operator_error(&mut self, token: Token<'s>, rhs: Option<Spacing>) {
        match &mut self.lhs_item {
            Some(MaybeOperator::Operator(Operator {
                arity: Arity::Binary { tokens, missing, .. },
                ..
            })) => {
                tokens.push(token);
                if rhs.is_none() {
                    match missing {
                        None => *missing = Some(BinaryOperand::Right),
                        Some(BinaryOperand::Left) => {
                            let operand = OperandMaybeNamed::Unnamed(
                                ApplyOperator::tokens(mem::take(tokens)).finish(),
                            );
                            self.inner.push_maybe_named_operand(operand);
                            self.lhs_item = Some(MaybeOperator::Operand);
                        }
                        Some(BinaryOperand::Right) => unreachable!(),
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

impl<'s, Inner> ScopeHierarchyConsumer for ClassifyArity<'s, Inner>
where Inner: NamedOperandConsumer<'s> + OperatorConsumer<'s> + ScopeHierarchyConsumer
{
    type Result = Inner::Result;

    fn start_scope(&mut self) {
        self.flush();
        self.inner.start_scope()
    }

    fn end_scope(&mut self) -> Self::Result {
        self.flush();
        self.inner.end_scope()
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for ClassifyArity<'s, Inner>
where Inner: NamedOperandConsumer<'s> + OperatorConsumer<'s> + GroupHierarchyConsumer<'s>
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.flush();
        self.inner.start_group(open);
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.emit(MaybeOperator::Operand);
        self.inner.end_group(close);
    }
}

impl<'s, Inner> OperatorConsumer<'s> for ClassifyArity<'s, Inner>
where Inner: NamedOperandConsumer<'s> + OperatorConsumer<'s>
{
    fn push_operator(&mut self, operator: Operator<'s>) {
        self.emit(operator);
    }
}


// === Operator or Operand

#[derive(Debug)]
enum MaybeOperator<'s> {
    Operand,
    Operator(Operator<'s>),
}

impl<'s> From<Operator<'s>> for MaybeOperator<'s> {
    fn from(operator: Operator<'s>) -> Self {
        MaybeOperator::Operator(operator)
    }
}

impl<'s> MaybeOperator<'s> {
    fn expects_rhs(&self) -> bool {
        match self {
            MaybeOperator::Operand => false,
            MaybeOperator::Operator(op) => op.arity.expects_rhs(),
        }
    }
}
