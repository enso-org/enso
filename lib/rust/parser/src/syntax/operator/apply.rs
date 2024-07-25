use crate::prelude::*;
use crate::syntax::operator::types::*;

use crate::syntax;
use crate::syntax::operator::operand::Operand;
use crate::syntax::token;
use crate::syntax::Tree;



// ==========================
// === Applying operators ===
// ==========================


// === Binary operators ===

#[derive(Debug)]
pub struct ApplyOperator<'s> {
    tokens:            Vec<token::Operator<'s>>,
    lhs:               Option<Operand<Tree<'s>>>,
    rhs:               Option<Operand<Tree<'s>>>,
    reify_rhs_section: bool,
    warnings:          Option<Warnings>,
}

impl<'s> ApplyOperator<'s> {
    pub fn tokens(tokens: Vec<token::Operator<'s>>) -> Self {
        Self {
            tokens,
            lhs: default(),
            rhs: default(),
            reify_rhs_section: default(),
            warnings: default(),
        }
    }

    pub fn token(token: token::Operator<'s>) -> Self {
        Self::tokens(vec![token])
    }

    pub fn with_lhs(self, lhs: Option<Operand<Tree<'s>>>) -> Self {
        Self { lhs, ..self }
    }

    pub fn with_rhs(self, rhs: Option<Operand<Tree<'s>>>, reify_rhs_section: bool) -> Self {
        Self { rhs, reify_rhs_section, ..self }
    }

    pub fn with_warnings(self, warnings: Warnings) -> Self {
        Self { warnings: Some(warnings), ..self }
    }

    pub fn finish(self) -> Operand<Tree<'s>> {
        let Self { tokens, lhs, rhs: rhs_, reify_rhs_section, warnings } = self;
        let mut operand = if let Some(lhs_termination) =
            tokens.first().and_then(|token| token.properties.lhs_section_termination())
        {
            let lhs = match lhs_termination {
                SectionTermination::Reify => lhs.map(Tree::from),
                SectionTermination::Unwrap => lhs.map(|op| op.value),
            };
            let rhs = rhs_.map(Tree::from);
            let ast = syntax::tree::apply_operator(lhs, tokens, rhs);
            Operand::from(ast)
        } else if tokens.len() < 2
            && let Some(opr) = tokens.first()
            && opr.properties.can_form_section()
        {
            let mut rhs = None;
            let mut elided = 0;
            let mut wildcards = 0;
            if let Some(rhs_) = rhs_ {
                if reify_rhs_section {
                    rhs = Some(Tree::from(rhs_));
                } else {
                    rhs = Some(rhs_.value);
                    elided += rhs_.elided;
                    wildcards += rhs_.wildcards;
                }
            }
            elided += lhs.is_none() as u32 + rhs.is_none() as u32;
            let mut operand =
                Operand::from(lhs).map(|lhs| syntax::tree::apply_operator(lhs, tokens, rhs));
            operand.elided += elided;
            operand.wildcards += wildcards;
            operand
        } else {
            let rhs = rhs_.map(Tree::from);
            let mut elided = 0;
            if tokens.len() != 1 || tokens[0].properties.can_form_section() {
                elided += lhs.is_none() as u32 + rhs.is_none() as u32;
            }
            let mut operand =
                Operand::from(lhs).map(|lhs| syntax::tree::apply_operator(lhs, tokens, rhs));
            operand.elided += elided;
            operand
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
    token:    token::Operator<'s>,
    rhs:      Option<Operand<Tree<'s>>>,
    error:    Option<Cow<'static, str>>,
    warnings: Option<Warnings>,
}

impl<'s> ApplyUnaryOperator<'s> {
    pub fn token(token: token::Operator<'s>) -> Self {
        Self { token, rhs: default(), error: default(), warnings: default() }
    }

    pub fn with_rhs(self, rhs: Option<Operand<Tree<'s>>>) -> Self {
        Self { rhs, ..self }
    }

    pub fn with_error(self, error: Option<Cow<'static, str>>) -> Self {
        Self { error, ..self }
    }

    pub fn with_warnings(self, warnings: Warnings) -> Self {
        Self { warnings: Some(warnings), ..self }
    }

    pub fn finish(self) -> Operand<Tree<'s>> {
        let Self { token, rhs, error, warnings } = self;
        Operand::new(rhs).map(|rhs| {
            let mut tree = syntax::tree::apply_unary_operator(token, rhs);
            if let Some(warnings) = warnings {
                warnings.apply(&mut tree);
            }
            match error {
                None => tree,
                Some(error) => tree.with_error(error),
            }
        })
    }
}
