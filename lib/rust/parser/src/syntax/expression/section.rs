use enso_prelude::*;

use crate::syntax::tree;
use crate::syntax::Tree;

// ====================
// === MaybeSection ===
// ====================

/// Wraps a value, tracking the number of wildcards operands within it.
#[derive(Default, Debug, PartialEq, Eq)]
pub struct MaybeSection<T> {
    pub value: T,
    /// Number of wildcards in the subtree, potentially forming a *template function*.
    pub wildcards: u32,
}

/// Transpose.
impl<T> From<Option<MaybeSection<T>>> for MaybeSection<Option<T>> {
    fn from(operand: Option<MaybeSection<T>>) -> Self {
        match operand {
            Some(MaybeSection { value, wildcards }) => Self { value: Some(value), wildcards },
            None => default(),
        }
    }
}

/// Unit. Creates a MaybeSection from a node.
impl<'s> From<Tree<'s>> for MaybeSection<Tree<'s>> {
    fn from(mut value: Tree<'s>) -> Self {
        let wildcards = if let Tree { variant: tree::Variant::Wildcard(wildcard), .. } = &mut value
        {
            debug_assert_eq!(wildcard.de_bruijn_index, None);
            wildcard.de_bruijn_index = Some(0);
            1
        } else {
            0
        };
        Self { value, wildcards }
    }
}

/// Counit. Bakes any information about elided operands into the tree.
impl<'s> From<MaybeSection<Tree<'s>>> for Tree<'s> {
    fn from(operand: MaybeSection<Tree<'s>>) -> Self {
        let MaybeSection { mut value, wildcards } = operand;
        if wildcards != 0 {
            value = Tree::template_function(wildcards, value);
        }
        value
    }
}

impl<T> MaybeSection<T> {
    /// Operate on the contained value without altering the elided-operand information.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> MaybeSection<U> {
        let Self { value, wildcards } = self;
        let value = f(value);
        MaybeSection { value, wildcards }
    }
}
