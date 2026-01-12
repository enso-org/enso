use crate::prelude::*;

use crate::syntax::SyntaxError;
use crate::syntax::Token;
use crate::syntax::Tree;
use crate::syntax::expression::operand::Operand;
use crate::syntax::maybe_with_error;
use crate::syntax::token;
use crate::syntax::token::TokenOperatorProperties;

use crate::syntax::tree::{MultipleOperatorError, Variant};
use crate::{expression_to_type, unwrap_call};
// ==========================
// === Applying operators ===
// ==========================

pub fn apply_binary_operator<'s>(
    tokens: Vec<Token<'s>>,
    lhs: Option<Operand<'s>>,
    rhs: Option<Operand<'s>>,
) -> Operand<'s> {
    match tokens.len() {
        0 => unreachable!(),
        1 => {
            let token = tokens.into_iter().next().unwrap();
            ApplyOperator::token(token).with_lhs(lhs).with_rhs(rhs).finish()
        }
        _ => Tree::opr_app(
            lhs.map(Tree::from),
            Err(MultipleOperatorError {
                operators: NonEmptyVec::try_from(
                    tokens
                        .into_iter()
                        .map(|opr| opr.with_variant(token::variant::Operator()))
                        .collect::<Vec<_>>(),
                )
                .unwrap(),
            }),
            rhs.map(Tree::from),
        )
        .into(),
    }
}

fn apply_operator<'s>(lhs: Option<Tree<'s>>, opr: Token<'s>, rhs: Option<Tree<'s>>) -> Tree<'s> {
    let error = match (&opr.variant, lhs.as_ref().map(|tree| &tree.variant), &rhs) {
        (_, Some(Variant::AutoscopedIdentifier(_)), _) if !opr.is_spaced() => {
            Some(SyntaxError::UnexpectedUnspacedOperand)
        }
        (_, _, None) | (_, None, _) if opr.is_syntactic_binary_operator() => {
            Some(SyntaxError::SyntacticOperatorMissingOperand)
        }
        (token::Variant::Operator(_) | token::Variant::ArrowOperator(_), _, _) => None,
        _ => Some(SyntaxError::ExprUnexpectedSyntacticOperator),
    };
    let tree = Tree::opr_app(lhs, Ok(opr.with_variant(token::variant::Operator())), rhs);
    maybe_with_error(tree, error)
}

pub fn apply_unary_operator<'s>(
    token: token::UnaryOperator<'s>,
    rhs: Option<Operand<'s>>,
) -> Operand<'s> {
    ApplyUnaryOperator::token(token).with_rhs(rhs).finish()
}

// === Binary operators ===

#[derive(Debug)]
pub struct ApplyOperator<'s> {
    token: Token<'s>,
    lhs: Option<Operand<'s>>,
    rhs: Option<Operand<'s>>,
}

impl<'s> ApplyOperator<'s> {
    pub fn token(token: Token<'s>) -> Self {
        Self { token, lhs: None, rhs: None }
    }

    pub fn with_lhs(self, lhs: Option<Operand<'s>>) -> Self {
        Self { lhs, ..self }
    }

    pub fn with_rhs(self, rhs: Option<Operand<'s>>) -> Self {
        Self { rhs, ..self }
    }

    pub fn finish(self) -> Operand<'s> {
        let Self { token, lhs, mut rhs } = self;
        let props = token.operator_properties().unwrap();

        let wildcards = lhs
            .as_ref()
            .map(|operand| {
                operand.wildcards
                    && !matches!(
                        &token.variant,
                        token::Variant::AssignmentOperator(_) | token::Variant::ArrowOperator(_)
                    )
            })
            .unwrap_or_default()
            || rhs.as_ref().map(|operand| operand.wildcards).unwrap_or_default();

        if let Some(rhs) = &mut rhs {
            if !props.can_form_section() {
                rhs.wildcards = false;
            }
            if matches!(token.variant, token::Variant::DotOperator(_))
                && matches!(rhs.value.variant, Variant::Ident(_))
            {
                rhs.call = false;
            }
        }
        let rhs = rhs.map(Tree::from);

        let lhs = lhs.map(|operand| match &token.variant {
            token::Variant::AssignmentOperator(_) | token::Variant::ArrowOperator(_) => {
                unwrap_call(operand.value)
            }
            _ if operand.call => Tree::call(operand.value),
            _ => operand.value,
        });

        let mut call = false;
        let value = match (token.variant, lhs, rhs) {
            (token::Variant::TypeAnnotationOperator(annotation), Some(lhs), Some(rhs)) => {
                Tree::type_annotated(lhs, token.with_variant(annotation), expression_to_type(rhs))
            }
            (
                token::Variant::DotOperator(dot),
                lhs,
                Some(Tree { variant: Variant::Ident(rhs), span, .. }),
            ) => {
                let mut ident = rhs.token;
                ident.left_offset = span.left_offset;
                let value = Tree::property_access(lhs, token.with_variant(dot), ident);
                call = true;
                value
            }
            (_, lhs, rhs) => apply_operator(lhs, token, rhs),
        };

        Operand { value, wildcards, call }
    }
}

// === Unary operators ===

#[derive(Debug)]
pub struct ApplyUnaryOperator<'s> {
    token: token::UnaryOperator<'s>,
    rhs: Option<Operand<'s>>,
    error: Option<SyntaxError>,
}

impl<'s> ApplyUnaryOperator<'s> {
    pub fn token(token: token::UnaryOperator<'s>) -> Self {
        Self { token, rhs: default(), error: default() }
    }

    pub fn with_rhs(self, rhs: Option<Operand<'s>>) -> Self {
        Self { rhs, ..self }
    }

    pub fn with_error(self, error: Option<SyntaxError>) -> Self {
        Self { error, ..self }
    }

    pub fn finish(self) -> Operand<'s> {
        let Self { token, rhs, error } = self;
        let tree = match rhs {
            Some(rhs) => Tree::unary_opr_app(token, Tree::from(rhs)),
            None => Tree::opr_app(None, Ok(token.with_variant(token::variant::Operator())), None)
                .with_error(SyntaxError::SyntacticOperatorMissingOperandUnary),
        };
        Operand::from(maybe_with_error(tree, error))
    }
}
