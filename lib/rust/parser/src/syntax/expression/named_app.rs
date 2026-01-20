use crate::prelude::*;

use crate::syntax::Flush;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::Token;
use crate::syntax::Tree;
use crate::syntax::expression::reducer::ApplyToOperand;
use crate::syntax::expression::types::Arity;
use crate::syntax::expression::types::ModifiedPrecedence;
use crate::syntax::expression::types::NamedOperandConsumer;
use crate::syntax::expression::types::Operand;
use crate::syntax::expression::types::OperandMaybeNamed;
use crate::syntax::expression::types::Operator;
use crate::syntax::expression::types::OperatorConsumer;
use crate::syntax::expression::whitespace::Spacing;
use crate::syntax::expression::whitespace::SpacingLookaheadTokenConsumer;
use crate::syntax::expression::whitespace::SpacingLookaheadTreeConsumer;
use crate::syntax::maybe_with_error;
use crate::syntax::token;
use crate::syntax::tree::SyntaxError;

// ========================
// === Named-App Parser ===
// ========================

/// Parses named-application syntax.
#[derive(Default, Debug, Finish)]
pub struct ParseAppNames<'s, Inner> {
    inner: Inner,
    partial: Option<Partial<'s>>,
    stack: Vec<AppName<'s>>,
}

#[derive(Debug)]
pub struct NamedApp<'s> {
    parens: Option<(token::OpenSymbol<'s>, Option<token::CloseSymbol<'s>>)>,
    name: token::Ident<'s>,
    equals: token::AssignmentOperator<'s>,
    expression: Tree<'s>,
}

impl<'s> NamedApp<'s> {
    fn apply(self, func: Tree<'s>) -> Tree<'s> {
        let NamedApp { parens, name, equals, expression } = self;
        let (open, close, error) = match parens {
            None => (None, None, None),
            Some((open, Some(close))) => (Some(open), Some(close), None),
            Some((open, None)) => (Some(open), None, Some(SyntaxError::ExprUnclosedParen)),
        };
        maybe_with_error(Tree::named_app(func, open, name, equals, expression, close), error)
    }
}

impl<'s> From<NamedApp<'s>> for Operator<'s> {
    fn from(value: NamedApp<'s>) -> Self {
        let NamedApp { parens, name, equals, expression } = value;
        let spacing = if let Some((open, _)) = &parens {
            Spacing::of_token(open)
        } else {
            Spacing::of_token(&name)
        };
        let precedence = ModifiedPrecedence::new(spacing, token::Precedence::Application, false);
        Operator {
            left_precedence: Some(precedence),
            right_precedence: None,
            associativity: token::Associativity::Left,
            arity: Arity::NamedApp(NamedApp { parens, name, equals, expression }.into()),
        }
    }
}

impl<'s> From<NamedApp<'s>> for Operand<'s> {
    fn from(value: NamedApp<'s>) -> Self {
        let NamedApp { parens, name, equals, expression } = value;
        let mut tree = Tree::opr_app(
            Tree::ident(name).into(),
            Ok(equals.with_variant(token::variant::Operator())),
            expression.into(),
        );
        if let Some((open, close)) = parens {
            tree = Tree::group(Some(open), tree.into(), close);
        }
        // After removing support for old lambdas, we can make this an error.
        tree.into()
    }
}

impl<'s> ApplyToOperand<'s> for NamedApp<'s> {
    fn apply_to_operand(self, operand: Option<Operand<'s>>) -> Operand<'s> {
        let mut result = operand.unwrap().map(|func| self.apply(Tree::from(func)));
        result.call = true;
        result
    }
}

#[derive(Debug)]
enum Partial<'s> {
    ExpectingName { open: token::OpenSymbol<'s> },
    ExpectingEquals { open: Option<token::OpenSymbol<'s>>, name: token::Ident<'s> },
}

#[derive(Debug)]
struct AppName<'s> {
    open: Option<token::OpenSymbol<'s>>,
    name: token::Ident<'s>,
    equals: token::AssignmentOperator<'s>,
    spaceproof: bool,
    inner_parens: u32,
}

impl<'s> AppName<'s> {
    fn new(
        open: Option<token::OpenSymbol<'s>>,
        name: token::Ident<'s>,
        equals: token::AssignmentOperator<'s>,
        // This is not used; it is required to ensure an `AppName` is only constructed when
        // spacing-lookahead has indicated that an expression follows the `=` (`finish` relies on
        // this).
        _following_expression: Spacing,
    ) -> Self {
        Self { open, name, equals, spaceproof: false, inner_parens: 0 }
    }

    fn finish(
        self,
        inner: &mut impl ScopeHierarchyConsumer<Result = Option<Tree<'s>>>,
        close: &mut Option<token::CloseSymbol<'s>>,
    ) -> OperandMaybeNamed<'s> {
        let Self { open, name, equals, inner_parens: _, spaceproof: _ } = self;
        let expression = inner.end_scope();
        // An `AppName` is only pushed to the stack when lookahead indicates there's an expression
        // after the `=`.
        let expression = expression.unwrap();
        NamedApp { parens: open.map(|open| (open, close.take())), name, equals, expression }.into()
    }
}

impl<'s, Inner> ParseAppNames<'s, Inner>
where
    Inner: NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>
        + SpacingLookaheadTokenConsumer<'s>,
{
    fn maybe_end_unspaced_expression(
        &mut self,
        following_spacing: Option<Spacing>,
        is_syntactic_binary_operator: bool,
    ) {
        if let Some(last) = self.stack.last_mut() {
            if !last.spaceproof
                && last.inner_parens == 0
                && last.open.is_none()
                && following_spacing != Some(Spacing::Unspaced)
            {
                if is_syntactic_binary_operator {
                    last.spaceproof = true;
                } else {
                    self.flush_complete(None);
                }
            }
        }
    }

    fn flush_paren(&mut self, open: token::OpenSymbol<'s>) {
        self.inner.start_group(open);
        if let Some(last) = self.stack.last_mut() {
            last.inner_parens += 1;
        }
    }

    fn flush_paren_and_ident(
        &mut self,
        open: Option<token::OpenSymbol<'s>>,
        name: token::Ident<'s>,
        following_spacing: Option<Spacing>,
    ) {
        if let Some(open) = open {
            self.flush_paren(open);
        }
        self.inner.push_token(name.into(), following_spacing);
    }

    fn flush_partial(&mut self, following: impl FnOnce() -> Option<Spacing>) {
        match self.partial.take() {
            None => {}
            Some(Partial::ExpectingName { open }) => self.flush_paren(open),
            Some(Partial::ExpectingEquals { open, name }) => {
                self.flush_paren_and_ident(open, name, following())
            }
        };
    }

    fn flush_complete(&mut self, mut close: Option<token::CloseSymbol<'s>>) {
        let operand = self.stack.pop().unwrap().finish(&mut self.inner, &mut close);
        self.inner.push_maybe_named_operand(operand);
        if close.is_some() {
            self.inner.end_group(close);
        }
    }
}

impl<'s, Inner> SpacingLookaheadTokenConsumer<'s> for ParseAppNames<'s, Inner>
where
    Inner: SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>,
{
    fn push_token(&mut self, token: Token<'s>, following_spacing: Option<Spacing>) {
        self.partial = loop {
            self.maybe_end_unspaced_expression(Some(Spacing::of_token(&token)), false);
            break match (token.variant, (self.partial.take(), following_spacing)) {
                (token::Variant::Ident(variant), (None, Some(Spacing::Unspaced)))
                    if !variant.is_type && token.is_spaced() =>
                {
                    let name = token.with_variant(variant);
                    Some(Partial::ExpectingEquals { open: None, name })
                }
                (token::Variant::Ident(variant), (Some(Partial::ExpectingName { open }), _))
                    if !variant.is_type =>
                {
                    let name = token.with_variant(variant);
                    Some(Partial::ExpectingEquals { open: Some(open), name })
                }
                (
                    token::Variant::AssignmentOperator(variant),
                    (
                        Some(Partial::ExpectingEquals { open, name }),
                        Some(following @ Spacing::Unspaced),
                    )
                    | (
                        Some(Partial::ExpectingEquals { open: open @ Some(_), name }),
                        Some(following),
                    ),
                ) => {
                    let equals = token.with_variant(variant);
                    self.stack.push(AppName::new(open, name, equals, following));
                    self.inner.start_scope();
                    None
                }
                (_, (None, _)) => {
                    let is_syntactic_binary_operator = token.is_syntactic_binary_operator();
                    self.inner.push_token(token, following_spacing);
                    self.maybe_end_unspaced_expression(
                        following_spacing,
                        is_syntactic_binary_operator,
                    );
                    None
                }
                (_, (Some(Partial::ExpectingName { open }), _)) => {
                    self.flush_paren(open);
                    self.inner.push_token(token, following_spacing);
                    None
                }
                (_, (Some(Partial::ExpectingEquals { open, name }), _)) => {
                    self.flush_paren_and_ident(open, name, Spacing::of_token(&token).into());
                    continue;
                }
            };
        }
    }
}

impl<'s, Inner> SpacingLookaheadTreeConsumer<'s> for ParseAppNames<'s, Inner>
where
    Inner: SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>,
{
    fn push_tree(&mut self, tree: Tree<'s>, following_spacing: Option<Spacing>) {
        self.flush_partial(|| Spacing::of_tree(&tree).into());
        self.maybe_end_unspaced_expression(Some(Spacing::of_tree(&tree)), false);
        self.inner.push_maybe_named_operand(OperandMaybeNamed::Unnamed(Operand::from(tree)));
        self.maybe_end_unspaced_expression(following_spacing, false);
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for ParseAppNames<'s, Inner>
where
    Inner: GroupHierarchyConsumer<'s>
        + SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>,
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.flush_partial(|| Spacing::of_token(&open).into());
        self.partial = if open.is_spaced() {
            Some(Partial::ExpectingName { open })
        } else {
            self.flush_paren(open);
            None
        }
    }

    fn end_group(&mut self, close: Option<token::CloseSymbol<'s>>) {
        self.flush_partial(|| close.as_ref().map(Spacing::of_token));
        if let Some(last) = self.stack.last_mut() {
            if last.inner_parens > 0 {
                self.inner.end_group(close);
                last.inner_parens -= 1;
            } else {
                self.flush_complete(close);
            }
        } else {
            self.inner.end_group(close);
        }
    }
}

impl<'s, Inner> Flush for ParseAppNames<'s, Inner>
where
    Inner: SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>,
{
    fn flush(&mut self) {
        self.flush_partial(|| None);
        while !self.stack.is_empty() {
            self.flush_complete(None);
        }
    }
}

impl<'s, Inner> OperatorConsumer<'s> for ParseAppNames<'s, Inner>
where
    Inner: OperatorConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>
        + SpacingLookaheadTokenConsumer<'s>,
{
    fn push_operator(&mut self, operator: Operator<'s>) {
        self.flush_partial(|| None);
        self.maybe_end_unspaced_expression(Some(operator.spacing()), false);
        self.inner.push_operator(operator);
    }
}
