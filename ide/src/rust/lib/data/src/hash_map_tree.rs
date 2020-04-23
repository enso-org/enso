//! A tree structure build on top of the `HashMap`.

use crate::prelude::*;



// ===================
// === HashMapTree ===
// ===================

/// A tree build on top of the `HashMap`. Each node in the tree can have zero or more branches
/// accessible by the given key type.
#[derive(Derivative)]
#[derivative(Debug   (bound="K:Eq+Hash+Debug , T:Debug"))]
#[derivative(Default (bound="K:Eq+Hash       , T:Default"))]
pub struct HashMapTree<K,T> {
    /// Value of the current tree node.
    pub value : T,
    /// Branches of the current tree node.
    pub branches : HashMap<K,HashMapTree<K,T>>
}

impl<K,T> HashMapTree<K,T>
where K:Eq+Hash {
    /// Constructor.
    pub fn new() -> Self where T:Default {
        default()
    }

    /// Constructor with explicit root value.
    pub fn from_value(value:T) -> Self {
        let branches = default();
        Self {value,branches}
    }

    /// Iterates over keys in `path`. For each key, traverses into the appropriate branch. In case
    /// the branch does not exist, a default instance will be created. Returns mutable reference to
    /// the target tree node.
    #[inline]
    pub fn get_node<P,I>(&mut self, path:P) -> &mut HashMapTree<K,T>
        where P:IntoIterator<Item=I>, T:Default, I:Into<K> {
        self.get_node_with(path,default)
    }

    /// Iterates over keys in `path`. For each key, traverses into the appropriate branch. In case
    /// the branch does not exist, uses `cons` to construct it. Returns mutable reference to the
    /// target tree node.
    #[inline]
    pub fn get_node_with<P,I,F>(&mut self, path:P, f:F) -> &mut HashMapTree<K,T>
        where P:IntoIterator<Item=I>, I:Into<K>, F:FnMut()->T {
        self.get_node_traversing_with(path,f,|_|{})
    }

    /// Iterates over keys in `path`. For each key, traverses into the appropriate branch. In case
    /// the branch does not exist, uses `cons` to construct it. Moreover, for each traversed branch
    /// the `callback` is evaluated. Returns mutable reference to the target tree node.
    #[inline]
    pub fn get_node_traversing_with<P,I,F,M>
    (&mut self, path:P, mut cons:F, mut callback:M) -> &mut HashMapTree<K,T>
    where P:IntoIterator<Item=I>, I:Into<K>, F:FnMut()->T, M:FnMut(&mut HashMapTree<K,T>) {
        path.into_iter().fold(self,|map,t| {
            let key  = t.into();
            let node = map.branches.entry(key).or_insert_with(|| HashMapTree::from_value(cons()));
            callback(node);
            node
        })
    }
}

impl<K,T> HashMapTree<K,Option<T>>
where K:Eq+Hash {
    /// Gets the current value or creates new default one if missing.
    pub fn value_or_default(&mut self) -> &mut T where T:Default {
        self.value_or_set_with(default)
    }

    /// Gets the current value or creates new one if missing.
    pub fn value_or_set(&mut self, val:T) -> &mut T {
        self.value_or_set_with(move || val)
    }

    /// Gets the current value or creates new one if missing.
    pub fn value_or_set_with<F>(&mut self, cons:F) -> &mut T
    where F:FnOnce()->T {
        if self.value.is_none() {
            self.value = Some(cons());
        };
        self.value.as_mut().unwrap()
    }
}


// === Impls ===

impl<'a,K,T> IntoIterator for &'a HashMapTree<K,T> {
    type Item     = (&'a K, &'a HashMapTree<K,T>);
    type IntoIter = std::collections::hash_map::Iter<'a,K,HashMapTree<K,T>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.branches.iter()
    }
}

impl<'a,K,T> IntoIterator for &'a mut HashMapTree<K,T> {
    type Item     = (&'a K, &'a mut HashMapTree<K,T>);
    type IntoIter = std::collections::hash_map::IterMut<'a,K,HashMapTree<K,T>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.branches.iter_mut()
    }
}
