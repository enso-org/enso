use crate::prelude::*;

use crate::syntax::Flush;
use crate::syntax::Token;
use crate::syntax::Tree;
use crate::syntax::expression::types::Arity;
use crate::syntax::expression::types::ModifiedPrecedence;
use crate::syntax::expression::types::Operator;
use crate::syntax::expression::types::OperatorConsumer;
use crate::syntax::expression::whitespace::Spacing;
use crate::syntax::expression::whitespace::SpacingLookaheadTokenConsumer;
use crate::syntax::expression::whitespace::SpacingLookaheadTreeConsumer;
use crate::syntax::token::AnnotationOperator;
use crate::syntax::token::Associativity;
use crate::syntax::token::Ident;
use crate::syntax::token::Precedence;
use crate::syntax::token::Variant;
use crate::syntax::tree;
use crate::syntax::tree::FunctionAnnotation;
use crate::syntax::tree::SyntaxError;

// ===================
// === Annotations ===
// ===================

#[derive(
    Debug,
    Default,
    Finish,
    SpacingLookaheadTreeConsumer,
    GroupHierarchyConsumer,
    OperatorConsumer
)]
#[tree_consumer(FlushAndForward)]
#[operator_consumer(FlushAndForward)]
pub struct ParseAnnotations<'s, Inner> {
    operator: Option<AnnotationOperator<'s>>,
    inner: Inner,
}

#[derive(Debug, ApplyToOperand)]
pub struct Annotation<'s> {
    operator: AnnotationOperator<'s>,
    ident: Ident<'s>,
}

impl<'s> Annotation<'s> {
    fn apply(self, operand: Option<Tree<'s>>) -> Tree<'s> {
        let Self { operator, ident } = self;
        if ident.is_type {
            Tree::annotated_builtin(operator, ident, default(), operand)
        } else {
            Tree::annotation(FunctionAnnotation { operator, annotation: ident, argument: operand })
                .with_error(SyntaxError::AnnotationUnexpectedInExpression)
        }
    }
}

impl<'s> From<Annotation<'s>> for Operator<'s> {
    fn from(value: Annotation<'s>) -> Self {
        Operator {
            left_precedence: None,
            right_precedence: ModifiedPrecedence::new(
                Spacing::Spaced,
                Precedence::Assignment,
                false,
            )
            .into(),
            associativity: Associativity::Left,
            arity: Arity::Annotation(value),
        }
    }
}

impl<'s> Annotation<'s> {
    pub fn spacing(&self) -> Spacing {
        Spacing::of_token(&self.operator)
    }
}

impl<'s, Inner> SpacingLookaheadTokenConsumer<'s> for ParseAnnotations<'s, Inner>
where
    Inner:
        SpacingLookaheadTokenConsumer<'s> + SpacingLookaheadTreeConsumer<'s> + OperatorConsumer<'s>,
{
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>) {
        match (self.operator.as_mut(), token.variant) {
            (None, Variant::AnnotationOperator(variant))
                if following_spacing == Some(Spacing::Unspaced) =>
            {
                self.operator = token.with_variant(variant).into()
            }
            (Some(_), Variant::Ident(variant)) => {
                let operator = self.operator.take().unwrap();
                let ident = token.with_variant(variant);
                let annotation = Annotation { operator, ident };
                if following_spacing.is_some() {
                    self.inner.push_operator(annotation.into());
                } else {
                    self.inner.push_tree(annotation.apply(None), None);
                }
            }
            _ => {
                self.flush();
                self.inner.push_token(token, following_spacing);
            }
        }
    }
}

impl<'s, Inner> Flush for ParseAnnotations<'s, Inner>
where
    Inner: SpacingLookaheadTreeConsumer<'s>,
{
    fn flush(&mut self) {
        if let Some(operator) = self.operator.take() {
            let tree = tree::to_ast(operator.into())
                .with_error(SyntaxError::AnnotationOpMustBeAppliedToIdent);
            self.inner.push_tree(tree, Some(Spacing::Unspaced));
        }
    }
}
