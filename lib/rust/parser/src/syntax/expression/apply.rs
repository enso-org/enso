use crate::prelude::*;
use crate::syntax::expression::types::*;

use crate::syntax;
use crate::syntax::expression::section::MaybeSection;
use crate::syntax::maybe_with_error;
use crate::syntax::token;
use crate::syntax::token::TokenOperatorProperties;
use crate::syntax::SyntaxError;
use crate::syntax::Token;
use crate::syntax::Tree;

// ==========================
// === Applying operators ===
// ==========================

// === Binary operators ===

#[derive(Debug)]
pub struct ApplyOperator<'s> {
    tokens: Vec<Token<'s>>,
    lhs: Option<MaybeSection<Tree<'s>>>,
    rhs: Option<MaybeSection<Tree<'s>>>,
    reify_rhs_section: bool,
    warnings: Option<Warnings>,
}

impl<'s> ApplyOperator<'s> {
    pub fn tokens(tokens: Vec<Token<'s>>) -> Self {
        Self {
            tokens,
            lhs: default(),
            rhs: default(),
            reify_rhs_section: default(),
            warnings: default(),
        }
    }

    pub fn token(token: Token<'s>) -> Self {
        Self::tokens(vec![token])
    }

    pub fn with_lhs(self, lhs: Option<MaybeSection<Tree<'s>>>) -> Self {
        Self { lhs, ..self }
    }

    pub fn with_rhs(self, rhs: Option<MaybeSection<Tree<'s>>>, reify_rhs_section: bool) -> Self {
        Self { rhs, reify_rhs_section, ..self }
    }

    pub fn with_warnings(self, warnings: Warnings) -> Self {
        Self { warnings: Some(warnings), ..self }
    }

    pub fn finish(self) -> MaybeSection<Tree<'s>> {
        let Self { tokens, lhs, rhs: rhs_, reify_rhs_section, warnings } = self;
        let mut operand = if let Some(lhs_termination) = tokens
            .first()
            .and_then(|token| token.operator_properties().unwrap().lhs_section_termination())
        {
            // Section-terminating special operators

            let lhs = match lhs_termination {
                // This mainly serves to handle blanks in the LHS of an ("old-style") lambda,
                // preventing them from being treated as creating a template function: In that case,
                // the argument definition is parsed before the operator is encountered, so we don't
                // know to treat it as a pattern until we have already finished parsing it.
                SectionTermination::Unwrap => lhs.map(|op| op.value),
            };
            let rhs = rhs_.map(Tree::from);
            MaybeSection::from(syntax::tree::apply_operator(lhs, tokens, rhs))
        } else if tokens.len() < 2
            && tokens.first().is_some_and(|opr| !opr.is_syntactic_binary_operator())
        {
            // Normal, section-forming operator

            let mut rhs = None;
            let mut wildcards = 0;
            if let Some(rhs_) = rhs_ {
                if reify_rhs_section {
                    rhs = Some(Tree::from(rhs_));
                } else {
                    rhs = Some(rhs_.value);
                    wildcards += rhs_.wildcards;
                }
            }
            let mut operand =
                MaybeSection::from(lhs).map(|lhs| syntax::tree::apply_operator(lhs, tokens, rhs));
            operand.wildcards += wildcards;
            operand
        } else {
            // Non-section-forming (but not section-terminating) operator, or multiple-operator
            // error.

            let rhs = rhs_.map(Tree::from);
            MaybeSection::from(lhs).map(|lhs| syntax::tree::apply_operator(lhs, tokens, rhs))
        };
        if let Some(warnings) = warnings {
            warnings.apply(&mut operand.value);
        }
        operand
    }
}

// === Unary operators ===

#[derive(Debug)]
pub struct ApplyUnaryOperator<'s> {
    token: token::UnaryOperator<'s>,
    rhs: Option<MaybeSection<Tree<'s>>>,
    error: Option<SyntaxError>,
    warnings: Option<Warnings>,
}

impl<'s> ApplyUnaryOperator<'s> {
    pub fn token(token: token::UnaryOperator<'s>) -> Self {
        Self { token, rhs: default(), error: default(), warnings: default() }
    }

    pub fn with_rhs(self, rhs: Option<MaybeSection<Tree<'s>>>) -> Self {
        Self { rhs, ..self }
    }

    pub fn with_error(self, error: Option<SyntaxError>) -> Self {
        Self { error, ..self }
    }

    pub fn with_warnings(self, warnings: Warnings) -> Self {
        Self { warnings: Some(warnings), ..self }
    }

    pub fn finish(self) -> MaybeSection<Tree<'s>> {
        let Self { token, rhs, error, warnings } = self;
        MaybeSection::from(rhs).map(|rhs| {
            let mut tree = match rhs {
                Some(rhs) => Tree::unary_opr_app(token, rhs),
                None => {
                    Tree::opr_app(None, Ok(token.with_variant(token::variant::Operator())), None)
                        .with_error(SyntaxError::SyntacticOperatorMissingOperandUnary)
                }
            };
            if let Some(warnings) = warnings {
                warnings.apply(&mut tree);
            }
            maybe_with_error(tree, error)
        })
    }
}
