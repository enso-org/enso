use crate::syntax::operator::types::*;
use enso_prelude::*;

use crate::syntax::operator::section::MaybeSection;
use crate::syntax::token::CloseSymbol;
use crate::syntax::token::OpenSymbol;
use crate::syntax::tree::SyntaxError;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::ScopeHierarchyConsumer;
use crate::syntax::Tree;



// =====================
// === Group Builder ===
// =====================

/// Constructs parenthesized groups.
#[derive(Default, Debug)]
pub struct BuildGroups<'s, Inner> {
    open:  Vec<OpenSymbol<'s>>,
    inner: Inner,
}

impl<'s, Inner: OperandConsumer<'s>> OperandConsumer<'s> for BuildGroups<'s, Inner> {
    fn push_operand(&mut self, operand: MaybeSection<Tree<'s>>) {
        self.inner.push_operand(operand)
    }
}

impl<'s, Inner: OperatorConsumer<'s>> OperatorConsumer<'s> for BuildGroups<'s, Inner> {
    fn push_operator(&mut self, operator: Operator<'s>) {
        self.inner.push_operator(operator)
    }
}

impl<'s, ScopeResult, Inner> Finish for BuildGroups<'s, Inner>
where
    ScopeResult: Into<Option<Tree<'s>>>,
    Inner: Finish + ScopeHierarchyConsumer<Result = ScopeResult> + OperandConsumer<'s>,
{
    type Result = <Inner as Finish>::Result;

    fn finish(&mut self) -> Self::Result {
        for open in self.open.drain(..).rev() {
            let expression = self.inner.end_scope().into();
            self.inner.push_operand(
                Tree::group(Some(open), expression, None)
                    .with_error(SyntaxError::ExprUnclosedParen)
                    .into(),
            );
        }
        self.inner.finish()
    }
}

impl<'s, ScopeResult, Inner> GroupHierarchyConsumer<'s> for BuildGroups<'s, Inner>
where
    ScopeResult: Into<Option<Tree<'s>>>,
    Inner: ScopeHierarchyConsumer<Result = ScopeResult> + OperandConsumer<'s>,
{
    fn start_group(&mut self, open: OpenSymbol<'s>) {
        self.open.push(open);
        self.inner.start_scope();
    }

    fn end_group(&mut self, close: CloseSymbol<'s>) {
        let open = self.open.pop().unwrap();
        let expression = self.inner.end_scope().into();
        self.inner.push_operand(Tree::group(Some(open), expression, Some(close)).into());
    }
}

impl<'s, Inner> ScopeHierarchyConsumer for BuildGroups<'s, Inner>
where Inner: ScopeHierarchyConsumer
{
    type Result = Inner::Result;

    fn start_scope(&mut self) {
        self.inner.start_scope()
    }

    fn end_scope(&mut self) -> Self::Result {
        self.inner.end_scope()
    }
}
