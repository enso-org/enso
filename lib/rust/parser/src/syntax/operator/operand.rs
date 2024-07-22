use crate::syntax::tree;
use crate::syntax::Tree;

use enso_prelude::default;



// ===============
// === Operand ===
// ===============

/// Wraps a value, tracking the number of wildcards or elided operands within it.
#[derive(Default, Debug, PartialEq, Eq)]
pub struct Operand<T> {
    pub value:     T,
    /// Number of elided operands in the subtree, potentially forming an *operator section*.
    pub elided:    u32,
    /// Number of wildcards in the subtree, potentially forming a *template function*.
    pub wildcards: u32,
}

/// Transpose. Note that an absent input will not be treated as an elided value; for that
/// conversion, use [`Operand::new`].
impl<T> From<Option<Operand<T>>> for Operand<Option<T>> {
    fn from(operand: Option<Operand<T>>) -> Self {
        match operand {
            Some(Operand { value, elided, wildcards }) =>
                Self { value: Some(value), elided, wildcards },
            None => default(),
        }
    }
}

/// Unit. Creates an Operand from a node.
impl<'s> From<Tree<'s>> for Operand<Tree<'s>> {
    fn from(mut value: Tree<'s>) -> Self {
        let elided = 0;
        let wildcards = if let Tree {
            variant: box tree::Variant::Wildcard(tree::Wildcard { de_bruijn_index, .. }),
            ..
        } = &mut value
        {
            debug_assert_eq!(*de_bruijn_index, None);
            *de_bruijn_index = Some(0);
            1
        } else {
            0
        };
        Self { value, wildcards, elided }
    }
}

/// Counit. Bakes any information about elided operands into the tree.
impl<'s> From<Operand<Tree<'s>>> for Tree<'s> {
    fn from(operand: Operand<Tree<'s>>) -> Self {
        let Operand { mut value, elided, wildcards } = operand;
        if elided != 0 {
            value = Tree::opr_section_boundary(elided, value);
        }
        if wildcards != 0 {
            value = Tree::template_function(wildcards, value);
        }
        value
    }
}

impl<T> Operand<Option<T>> {
    /// Lift an option value to a potentially-elided operand.
    pub fn new(value: Option<Operand<T>>) -> Self {
        match value {
            None => Self { value: None, elided: 1, wildcards: default() },
            Some(value) => {
                let Operand { value, elided, wildcards } = value;
                Self { value: Some(value), elided, wildcards }
            }
        }
    }
}

impl<T> Operand<T> {
    /// Operate on the contained value without altering the elided-operand information.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Operand<U> {
        let Self { value, elided, wildcards } = self;
        let value = f(value);
        Operand { value, elided, wildcards }
    }
}
