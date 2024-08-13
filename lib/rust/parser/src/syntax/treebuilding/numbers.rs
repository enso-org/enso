use crate::prelude::*;

use crate::syntax::token;
use crate::syntax::tree;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Token;
use crate::syntax::TokenConsumer;
use crate::syntax::Tree;
use crate::syntax::TreeConsumer;



#[derive(Debug, Default)]
pub struct ParseNumbers<'s, Inner> {
    state: State<'s>,
    inner: Inner,
}

#[derive(Debug, Default)]
struct State<'s> {
    prev_item_in_expression: bool,
    negation:                Option<Token<'s>>,
    number:                  Option<Number<'s>>,
}

#[derive(Debug)]
enum Number<'s> {
    Based { base: token::NumberBase<'s> },
    Fractional { digits: token::Digits<'s>, dot: Option<token::DotOperator<'s>> },
}

impl<'s, Inner: TokenConsumer<'s> + TreeConsumer<'s>> TokenConsumer<'s>
    for ParseNumbers<'s, Inner>
{
    fn push_token(&mut self, token: Token<'s>) {
        match (token.variant, &mut self.state) {
            (token::Variant::Digits(variant), State { number: Some(Number::Based { .. }), .. }) => {
                let State { negation, number: Some(Number::Based { base }), .. } =
                    mem::take(&mut self.state)
                else {
                    unreachable!()
                };
                self.inner.push_tree(maybe_negated(
                    negation,
                    Tree::number(Some(base), Some(token.with_variant(variant)), None),
                ));
            }
            (
                token::Variant::Digits(variant),
                State { number: Some(Number::Fractional { digits: _, dot: Some(_) }), .. },
            ) if token.left_offset.visible.width_in_spaces == 0 => {
                let State {
                    negation,
                    number: Some(Number::Fractional { digits, dot: Some(dot) }),
                    ..
                } = mem::take(&mut self.state)
                else {
                    unreachable!()
                };
                self.inner.push_tree(maybe_negated(
                    negation,
                    Tree::number(
                        None,
                        Some(digits),
                        Some(tree::FractionalDigits { dot, digits: token.with_variant(variant) }),
                    ),
                ));
            }
            (
                token::Variant::Operator(_) | token::Variant::NegationOperator(_),
                State { prev_item_in_expression, negation, number },
            ) if (token.is_spaced() || !*prev_item_in_expression) && token.code.repr.0 == "-" => {
                if negation.is_some() || number.is_some() {
                    flush(&mut self.inner, negation, number);
                }
                self.state.negation = Some(token);
            }
            (token::Variant::NumberBase(variant), State { negation, number, .. }) => {
                if number.is_some() {
                    flush(&mut self.inner, negation, number);
                } else if token.left_offset.visible.width_in_spaces != 0 {
                    if let Some(minus) = negation.take() {
                        self.inner.push_token(minus.with_variant(token::Variant::operator()));
                    }
                }
                *number = Some(Number::Based { base: token.with_variant(variant) })
            }
            (token::Variant::Digits(variant), State { negation, number, .. }) => {
                if number.is_some() {
                    flush(&mut self.inner, negation, number);
                } else if token.left_offset.visible.width_in_spaces != 0 {
                    if let Some(minus) = negation.take() {
                        self.inner.push_token(minus.with_variant(token::Variant::operator()));
                    }
                }
                *number =
                    Some(Number::Fractional { digits: token.with_variant(variant), dot: None });
            }
            (
                token::Variant::DotOperator(_),
                State { number: Some(Number::Fractional { digits: _, dot: dot @ None }), .. },
            ) if token.left_offset.visible.width_in_spaces == 0 =>
                *dot = Some(token.with_variant(token::variant::DotOperator())),
            _ => {
                self.flush();
                self.inner.push_token(token)
            }
        }
        self.state.prev_item_in_expression = true;
    }
}

impl<'s, Inner: TokenConsumer<'s> + TreeConsumer<'s>> TreeConsumer<'s> for ParseNumbers<'s, Inner> {
    fn push_tree(&mut self, tree: Tree<'s>) {
        self.flush();
        self.inner.push_tree(tree);
        self.state.prev_item_in_expression = true;
    }
}

impl<'s, Inner: TokenConsumer<'s> + TreeConsumer<'s>> ParseNumbers<'s, Inner> {
    fn flush(&mut self) {
        let State { negation, number, prev_item_in_expression: _ } = &mut self.state;
        flush(&mut self.inner, negation, number);
    }
}

fn flush<'s, Inner: TokenConsumer<'s> + TreeConsumer<'s>>(
    inner: &mut Inner,
    negation: &mut Option<Token<'s>>,
    number: &mut Option<Number<'s>>,
) {
    if let Some(number) = number.take() {
        let (number, trailing_token) = match number {
            Number::Based { base } => (Tree::number(Some(base), None, None), None),
            Number::Fractional { digits, dot } => (Tree::number(None, Some(digits), None), dot),
        };
        inner.push_tree(maybe_negated(negation.take(), number));
        if let Some(trailing_token) = trailing_token {
            inner.push_token(trailing_token.into());
        }
    } else if let Some(minus) = negation.take() {
        inner.push_token(minus);
    }
}

fn maybe_negated<'s>(minus: Option<Token<'s>>, tree: Tree<'s>) -> Tree<'s> {
    match minus {
        Some(minus) =>
            Tree::unary_opr_app(minus.with_variant(token::variant::UnaryOperator()), Some(tree)),
        None => tree,
    }
}

impl<'s, Inner: TokenConsumer<'s> + TreeConsumer<'s> + Finish> Finish for ParseNumbers<'s, Inner> {
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.flush();
        self.state.prev_item_in_expression = false;
        self.inner.finish()
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for ParseNumbers<'s, Inner>
where Inner: TokenConsumer<'s> + TreeConsumer<'s> + GroupHierarchyConsumer<'s>
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.flush();
        self.inner.start_group(open);
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.flush();
        self.inner.end_group(close);
    }
}
