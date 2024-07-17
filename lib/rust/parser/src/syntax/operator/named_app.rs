use crate::prelude::*;

use crate::syntax::operator::section::MaybeSection;
use crate::syntax::operator::types::NamedOperandConsumer;
use crate::syntax::operator::types::OperandMaybeNamed;
use crate::syntax::token;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::treebuilding::SpacingLookaheadTokenConsumer;
use crate::syntax::treebuilding::SpacingLookaheadTreeConsumer;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::Token;
use crate::syntax::Tree;



// ========================
// === Named-App Parser ===
// ========================

/// Parses named-application syntax.
#[derive(Default, Debug)]
pub struct ParseAppNames<'s, Inner> {
    inner:   Inner,
    partial: Option<Partial<'s>>,
    stack:   Vec<AppName<'s>>,
}

#[derive(Debug)]
enum Partial<'s> {
    ExpectingName { open: token::OpenSymbol<'s> },
    ExpectingEquals { open: Option<token::OpenSymbol<'s>>, name: token::Ident<'s> },
}

#[derive(Debug, Default)]
struct AppName<'s> {
    open:         Option<token::OpenSymbol<'s>>,
    name:         token::Ident<'s>,
    equals:       token::AssignmentOperator<'s>,
    spaceproof:   bool,
    inner_parens: u32,
}

impl<'s> AppName<'s> {
    fn finish(
        self,
        expression: Option<Tree<'s>>,
        close: &mut Option<token::CloseSymbol<'s>>,
    ) -> OperandMaybeNamed<'s> {
        let Self { open, name, equals, inner_parens: _, spaceproof: _ } = self;
        // An `OuterAppName` is only constructed when lookahead indicates there's a
        // token after the `=`.
        let expression = expression.unwrap();
        OperandMaybeNamed::Named {
            parens: open.map(|open| (open, close.take())),
            name,
            equals,
            expression,
        }
    }
}

impl<'s, Inner> ParseAppNames<'s, Inner>
where Inner: NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>
        + SpacingLookaheadTokenConsumer<'s>
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
            Some(Partial::ExpectingEquals { open, name }) =>
                self.flush_paren_and_ident(open, name, following()),
        };
    }

    fn flush_complete(&mut self, mut close: Option<token::CloseSymbol<'s>>) {
        let expression = self.inner.end_scope();
        let operand = self.stack.pop().unwrap().finish(expression, &mut close);
        self.inner.push_operand(operand);
        if let Some(close) = close {
            self.inner.end_group(close);
        }
    }
}

impl<'s, Inner> SpacingLookaheadTokenConsumer<'s> for ParseAppNames<'s, Inner>
where Inner: SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>
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
                    (Some(Partial::ExpectingEquals { open, name }), Some(Spacing::Unspaced))
                    | (Some(Partial::ExpectingEquals { open: open @ Some(_), name }), _),
                ) => {
                    let equals = token.with_variant(variant);
                    self.stack.push(AppName {
                        open,
                        name,
                        equals,
                        inner_parens: 0,
                        spaceproof: false,
                    });
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
where Inner: SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>
{
    fn push_tree(&mut self, tree: Tree<'s>, following_spacing: Option<Spacing>) {
        self.flush_partial(|| Spacing::of_tree(&tree).into());
        self.maybe_end_unspaced_expression(Some(Spacing::of_tree(&tree)), false);
        self.inner.push_operand(OperandMaybeNamed::Unnamed(MaybeSection::from(tree)));
        self.maybe_end_unspaced_expression(following_spacing, false);
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for ParseAppNames<'s, Inner>
where Inner: GroupHierarchyConsumer<'s>
        + SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
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

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.flush_partial(|| Spacing::of_token(&close).into());
        if let Some(last) = self.stack.last_mut() {
            if last.inner_parens > 0 {
                self.inner.end_group(close);
                last.inner_parens -= 1;
            } else {
                self.flush_complete(close.into());
            }
        } else {
            self.inner.end_group(close);
        }
    }
}

impl<'s, Inner: Finish> Finish for ParseAppNames<'s, Inner>
where Inner: Finish
        + SpacingLookaheadTokenConsumer<'s>
        + NamedOperandConsumer<'s>
        + ScopeHierarchyConsumer<Result = Option<Tree<'s>>>
        + GroupHierarchyConsumer<'s>
{
    type Result = <Inner as Finish>::Result;

    fn finish(&mut self) -> Self::Result {
        self.flush_partial(|| None);
        while !self.stack.is_empty() {
            self.flush_complete(None);
        }
        self.inner.finish()
    }
}
