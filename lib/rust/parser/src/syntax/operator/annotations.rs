use crate::lexer::test::Precedence;
use crate::prelude::*;

use crate::syntax::operator::reducer::ApplyToOperand;
use crate::syntax::operator::section::MaybeSection;
use crate::syntax::operator::types::Arity;
use crate::syntax::operator::types::ModifiedPrecedence;
use crate::syntax::operator::types::Operator;
use crate::syntax::operator::types::OperatorConsumer;
use crate::syntax::token::AnnotationOperator;
use crate::syntax::token::Associativity;
use crate::syntax::token::Ident;
use crate::syntax::token::Variant;
use crate::syntax::tree;
use crate::syntax::tree::FunctionAnnotation;
use crate::syntax::tree::SyntaxError;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::treebuilding::SpacingLookaheadTokenConsumer;
use crate::syntax::treebuilding::SpacingLookaheadTreeConsumer;
use crate::syntax::Finish;
use crate::syntax::Flush;
use crate::syntax::HasInner;
use crate::syntax::Token;
use crate::syntax::TokenOnlyParser;
use crate::syntax::Tree;


// ===================
// === Annotations ===
// ===================

pub type ParseAnnotations<'s, Inner> = TokenOnlyParser<AnnotationParser<'s, Inner>>;

#[derive(Debug, Default)]
pub struct AnnotationParser<'s, Inner> {
    operator: Option<AnnotationOperator<'s>>,
    inner:    Inner,
}

#[derive(Debug)]
pub struct Annotation<'s> {
    operator: AnnotationOperator<'s>,
    ident:    Ident<'s>,
}

impl<'s> Annotation<'s> {
    fn apply(self, operand: Option<MaybeSection<Tree<'s>>>) -> Tree<'s> {
        let Self { operator, ident } = self;
        let operand = operand.map(Tree::from);
        if ident.is_type {
            Tree::annotated_builtin(operator, ident, default(), operand)
        } else {
            Tree::annotation(FunctionAnnotation { operator, annotation: ident, argument: operand })
                .with_error(SyntaxError::AnnotationUnexpectedInExpression)
        }
    }
}

impl<'s> ApplyToOperand<'s> for Annotation<'s> {
    fn apply_to_operand(self, operand: Option<MaybeSection<Tree<'s>>>) -> MaybeSection<Tree<'s>> {
        self.apply(operand).into()
    }
}

impl<'s> Annotation<'s> {
    pub fn spacing(&self) -> Spacing {
        Spacing::of_token(&self.operator)
    }
}

impl<'s, Inner> SpacingLookaheadTokenConsumer<'s> for AnnotationParser<'s, Inner>
where Inner:
        SpacingLookaheadTokenConsumer<'s> + SpacingLookaheadTreeConsumer<'s> + OperatorConsumer<'s>
{
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>) {
        match (self.operator.as_mut(), token.variant) {
            (None, Variant::AnnotationOperator(variant))
                if following_spacing == Some(Spacing::Unspaced) =>
                self.operator = token.with_variant(variant).into(),
            (Some(_), Variant::Ident(variant)) => {
                let operator = self.operator.take().unwrap();
                let ident = token.with_variant(variant);
                let annotation = Annotation { operator, ident };
                if following_spacing.is_some() {
                    self.inner.push_operator(Operator {
                        left_precedence:  None,
                        right_precedence: ModifiedPrecedence::new(
                            following_spacing.unwrap_or_default(),
                            Precedence::annotation(),
                            false,
                        ),
                        associativity:    Associativity::Left,
                        arity:            Arity::Annotation(annotation),
                    });
                } else {
                    self.inner.push_tree(annotation.apply(None), following_spacing);
                }
            }
            _ => {
                self.flush();
                self.inner.push_token(token, following_spacing);
            }
        }
    }
}

impl<'s, Inner> Flush for AnnotationParser<'s, Inner>
where Inner: SpacingLookaheadTreeConsumer<'s>
{
    fn flush(&mut self) {
        if let Some(operator) = self.operator.take() {
            let tree = tree::to_ast(operator.into())
                .with_error(SyntaxError::AnnotationOpMustBeAppliedToIdent);
            self.inner.push_tree(tree, Some(Spacing::Unspaced));
        }
    }
}

impl<'s, Inner> Finish for AnnotationParser<'s, Inner>
where Inner: Finish + SpacingLookaheadTreeConsumer<'s>
{
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.flush();
        self.inner.finish()
    }
}

impl<'s, Inner> HasInner for AnnotationParser<'s, Inner> {
    type Inner = Inner;

    fn inner_mut(&mut self) -> &mut Self::Inner {
        &mut self.inner
    }
}
