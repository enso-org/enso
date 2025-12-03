use enso_prelude::*;

use crate::syntax::Tree;
use crate::syntax::tree;

// ===============
// === Operand ===
// ===============

/// Wraps a value to perform a bottom-up analysis that inserts template function boundaries and
/// method call nodes.
#[derive(Default, Debug, PartialEq, Eq)]
pub struct Operand<'s> {
    pub value: Tree<'s>,
    pub wildcards: bool,
    pub call: bool,
}

/// Unit. Creates a Operand from a node.
impl<'s> From<Tree<'s>> for Operand<'s> {
    fn from(mut value: Tree<'s>) -> Self {
        enum Evaluation {
            Deferred,
            Immediate,
        }
        let evaluation = match &value.variant {
            tree::Variant::Ident(ident) if ident.token.is_type => Some(Evaluation::Deferred),
            tree::Variant::Ident(_) => Some(Evaluation::Immediate),
            _ => None,
        };
        if let Some(Evaluation::Immediate) = evaluation {
            //value = Tree::call(value);
        }
        let call = matches!(evaluation, Some(Evaluation::Deferred));
        let wildcards = matches!(value.variant, tree::Variant::Wildcard(_));
        Self { value, wildcards, call }
    }
}

/// Counit. Bakes any information about elided operands into the tree.
impl<'s> From<Operand<'s>> for Tree<'s> {
    fn from(operand: Operand<'s>) -> Self {
        let Operand { mut value, wildcards, call } = operand;
        if call && !matches!(value.variant, tree::Variant::Invalid(_)) {
            //value = Tree::call(value);
        }
        if wildcards {
            value = Tree::template_function(value);
        }
        value
    }
}

impl<'s> Operand<'s> {
    /// Operate on the contained value without altering the elided-operand information.
    pub fn map<'s1>(self, f: impl FnOnce(Tree<'s>) -> Tree<'s1>) -> Operand<'s1> {
        let Self { value, wildcards, call } = self;
        let value = f(value);
        Operand { value, wildcards, call }
    }
}
