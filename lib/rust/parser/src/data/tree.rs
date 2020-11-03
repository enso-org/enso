//! A map-based tree for use in the macro resolver.

use crate::prelude::*;



// ============
// === Tree ===
// ============

/// A tree with a variable number of branches per node.
#[derive(Derivative)]
#[derivative(Clone)]
#[derivative(Debug(bound="K:Debug+Eq+Hash+PartialEq, V:Debug+Eq+PartialEq"))]
#[derivative(Default(bound="K:Default+Eq+Hash"))]
#[derivative(PartialEq(bound="K:Eq+Hash, V:PartialEq"))]
#[derivative(PartialEq(bound="K:Eq+Hash, V:Eq"))]
pub struct Tree<K,V> {
    value : Option<V>,
    branches : HashMap<K,Tree<K,V>>
}

impl<K,V> Tree<K,V> {
    /// Create an empty tree.
    pub fn empty() -> Self {
        default()
    }

    /// Create a singleton tree.
    pub fn singleton(value:V) -> Self {
        let value    = Some(value);
        let branches = default();
        Self{value,branches}
    }

    /// Insert the provided `value` into the tree at the provided `path`.
    pub fn insert(&mut self, path:Vec<K>, value:V) {
        if path.is_empty() {
            self.value = Some(value)
        } else {
            unimplemented!()
        }
    }

    /// Check if `self` is a leaf of the tree.
    pub fn is_leaf(&self) -> boolean {
        self.branches.is_empty()
    }

    /// Check if `self` is a non-leaf node in the tree.
    pub fn is_non_leaf(&self) -> boolean {
        !self.is_leaf()
    }
}



// =============
// === Tests ===
// =============
