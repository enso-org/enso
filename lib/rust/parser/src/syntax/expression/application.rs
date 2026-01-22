use crate::prelude::*;
use crate::syntax::expression::types::*;

use crate::syntax::Flush;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::expression::whitespace::Spacing;
use crate::syntax::token;

// ===================
// === Insert Apps ===
// ===================

/// Inserts applications between terms as needed.
#[derive(Default, Debug, Finish)]
pub struct InsertApps<Inner> {
    prev_applicable: bool,
    stack: Vec<bool>,
    inner: Inner,
}

impl<'s, Inner> NamedOperandConsumer<'s> for InsertApps<Inner>
where
    Inner: OperatorConsumer<'s> + OperandConsumer<'s>,
{
    fn push_maybe_named_operand(&mut self, operand: OperandMaybeNamed<'s>) {
        let prev_applicable = mem::replace(&mut self.prev_applicable, true);
        match operand {
            OperandMaybeNamed::Unnamed(operand) => {
                if prev_applicable {
                    self.inner.push_operator(application(Spacing::of_tree(&operand.value)));
                }
                self.inner.push_operand(operand)
            }
            OperandMaybeNamed::Named(app) => {
                if prev_applicable {
                    self.inner.push_operator(app.into());
                } else {
                    self.inner.push_operand(app.into());
                }
            }
        }
    }
}

impl<'s, Inner: OperatorConsumer<'s>> OperatorConsumer<'s> for InsertApps<Inner> {
    fn push_operator(&mut self, operator: Operator<'s>) {
        let prev_applicable =
            mem::replace(&mut self.prev_applicable, operator.right_precedence.is_none());
        if prev_applicable && operator.left_precedence.is_none() {
            self.inner.push_operator(application(Spacing::Spaced));
        }
        self.inner.push_operator(operator)
    }
}

impl<Inner> Flush for InsertApps<Inner> {
    fn flush(&mut self) {
        self.prev_applicable = false;
        // start_scope/end_scope are balanced
        debug_assert!(self.stack.is_empty());
    }
}

fn application<'s>(spacing: Spacing) -> Operator<'s> {
    let precedence = Some(ModifiedPrecedence::new(spacing, token::Precedence::Application, false));
    Operator {
        left_precedence: precedence,
        right_precedence: precedence,
        associativity: token::Associativity::Left,
        arity: Arity::App,
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for InsertApps<Inner>
where
    Inner: OperatorConsumer<'s> + GroupHierarchyConsumer<'s>,
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        if mem::replace(&mut self.prev_applicable, false) {
            self.inner.push_operator(application(Spacing::of_token(&open)));
        }
        self.inner.start_group(open);
    }

    fn end_group(&mut self, close: Option<token::CloseSymbol<'s>>) {
        self.prev_applicable = true;
        self.inner.end_group(close);
    }
}

impl<'s, Inner> ScopeHierarchyConsumer for InsertApps<Inner>
where
    Inner: OperandConsumer<'s> + OperatorConsumer<'s> + ScopeHierarchyConsumer,
{
    type Result = Inner::Result;

    fn start_scope(&mut self) {
        let state = mem::replace(&mut self.prev_applicable, false);
        self.stack.push(state);
        self.inner.start_scope();
    }

    fn end_scope(&mut self) -> Self::Result {
        let state = self.stack.pop().unwrap();
        self.prev_applicable = state;
        self.inner.end_scope()
    }
}
