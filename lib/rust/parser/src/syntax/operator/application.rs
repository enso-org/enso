use crate::syntax::operator::types::*;
use enso_prelude::*;

use crate::syntax::operator::named_app::NamedApp;
use crate::syntax::token;
use crate::syntax::treebuilding::Spacing;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::Tree;



// ===================
// === Insert Apps ===
// ===================

/// Inserts applications between terms as needed.
#[derive(Default, Debug)]
pub struct InsertApps<Inner> {
    prev_applicable: bool,
    stack:           Vec<bool>,
    inner:           Inner,
}

impl<'s, Inner> NamedOperandConsumer<'s> for InsertApps<Inner>
where Inner: OperatorConsumer<'s> + OperandConsumer<'s>
{
    fn push_maybe_named_operand(&mut self, operand: OperandMaybeNamed<'s>) {
        match operand {
            OperandMaybeNamed::Unnamed(operand) => {
                if mem::replace(&mut self.prev_applicable, true) {
                    self.inner.push_operator(application(Spacing::of_tree(&operand.value)));
                }
                self.inner.push_operand(operand)
            }
            OperandMaybeNamed::Named { parens, name, equals, expression } => {
                if mem::replace(&mut self.prev_applicable, true) {
                    let spacing = if let Some((open, _)) = &parens {
                        Spacing::of_token(open)
                    } else {
                        Spacing::of_token(&name)
                    };
                    let precedence =
                        ModifiedPrecedence::new(spacing, token::Precedence::application(), false);
                    let right_precedence = ModifiedPrecedence::new(
                        // Named applications always have unspaced right-precedence; if it reads
                        // from left to right as a named application, a following operator can't
                        // cause the interpretation to change.
                        Spacing::Unspaced,
                        token::Precedence::application(),
                        false,
                    );
                    let operator = Operator {
                        left_precedence: Some(precedence),
                        right_precedence,
                        associativity: token::Associativity::Left,
                        arity: Arity::NamedApp(
                            NamedApp { parens, name, equals, expression }.into(),
                        ),
                    };
                    self.inner.push_operator(operator);
                } else {
                    let mut tree = Tree::opr_app(
                        Tree::ident(name).into(),
                        Ok(equals.with_variant(token::variant::Operator())),
                        expression.into(),
                    );
                    if let Some((open, close)) = parens {
                        tree = Tree::group(Some(open), tree.into(), close);
                    }
                    // After removing support for old lambdas, we can make this an error.
                    self.inner.push_operand(tree.into())
                }
            }
        }
    }
}

impl<'s, Inner: OperatorConsumer<'s>> OperatorConsumer<'s> for InsertApps<Inner> {
    fn push_operator(&mut self, operator: Operator<'s>) {
        let prev_applicable = mem::replace(
            &mut self.prev_applicable,
            matches!(operator.arity, Arity::Binary { missing: Some(BinaryOperand::Right), .. }),
        );
        if prev_applicable
            && matches!(
                operator.arity,
                Arity::Unary { .. } | Arity::Binary { missing: Some(BinaryOperand::Left), .. }
            )
        {
            self.inner.push_operator(application(Spacing::Spaced));
        }
        self.inner.push_operator(operator)
    }
}

impl<Inner: Finish> Finish for InsertApps<Inner> {
    type Result = Inner::Result;

    fn finish(&mut self) -> Self::Result {
        self.prev_applicable = false;
        self.inner.finish()
    }
}

fn application<'s>(spacing: Spacing) -> Operator<'s> {
    let precedence = ModifiedPrecedence::new(spacing, token::Precedence::application(), false);
    Operator {
        left_precedence:  Some(precedence),
        right_precedence: precedence,
        associativity:    token::Associativity::Left,
        arity:            Arity::App,
    }
}

impl<'s, Inner> GroupHierarchyConsumer<'s> for InsertApps<Inner>
where Inner: OperatorConsumer<'s> + GroupHierarchyConsumer<'s>
{
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        if mem::replace(&mut self.prev_applicable, false) {
            self.inner.push_operator(application(Spacing::of_token(&open)));
        }
        self.inner.start_group(open);
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.prev_applicable = true;
        self.inner.end_group(close);
    }
}

impl<'s, Inner> ScopeHierarchyConsumer for InsertApps<Inner>
where Inner: OperandConsumer<'s> + OperatorConsumer<'s> + ScopeHierarchyConsumer
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
