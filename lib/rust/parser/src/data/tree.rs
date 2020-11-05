//! A map-based tree for use in the macro resolver.

use crate::prelude::*;



// ============
// === Tree ===
// ============

/// The type of branches in the tree.
pub type Branches<K,V> = HashMap<K,Tree<K,V>>;

/// A tree with a variable number of branches per node.
#[derive(Derivative)]
#[derivative(Clone)]
#[derivative(Debug(bound="K:Debug+Eq+Hash+PartialEq, V:Debug+Eq+PartialEq"))]
#[derivative(Default(bound="K:Default+Eq+Hash"))]
#[derivative(PartialEq(bound="K:Eq+Hash, V:PartialEq"))]
#[derivative(Eq(bound="K:Eq+Hash, V:Eq"))]
pub struct Tree<K,V> {
    /// The value at the current node.
    pub value : Option<V>,
    /// The branches at the current node.
    pub branches : Branches<K,V>
}

impl<K,V> Tree<K,V> {
    /// Check if `self` is a leaf of the tree.
    pub fn is_leaf(&self) -> bool {
        self.branches.is_empty()
    }

    /// Check if `self` is a non-leaf node in the tree.
    pub fn is_non_leaf(&self) -> bool {
        !self.is_leaf()
    }
}

impl<K,V> Tree<K,V>
where K : Clone+Default+Eq+Hash+PartialEq,
      V : Default
{
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
    pub fn insert(&mut self, path:&[K], value:V) {
        if let Some((first,rest)) = path.split_first() {
            if let Some(existing_branch) = self.branches.get_mut(first) {
                existing_branch.insert(rest,value);
            } else {
                let mut new_branch = Self::empty();
                new_branch.insert(rest,value);
                self.branches.insert(first.clone(),new_branch);
            }
        } else {
            self.value = Some(value)
        }
    }

    /// Map the provided `f` over `self`, mutating the tree.
    ///
    /// This may change the value type stored in the tree.
    ///
    /// ## NOTE
    /// This function is only suitable for use on trees with small depths as it is implemented in a
    /// recursive fashion.
    pub fn map<S,F:Copy+Fn(V) -> S>(self, f:F) -> Tree<K,S> {
        let value         = self.value.map(f);
        let branches_iter = self.branches.into_iter().map(|(k,v)| (k,v.map(f)));
        let branches      = branches_iter.collect::<Branches<K,S>>();
        Tree{value,branches}
    }

    /// Map the provided `f` over `self`, mutating the tree in place.
    ///
    /// ## NOTE
    /// This function is only suitable for use on trees with small depths as it is implemented in a
    /// recursive fashion.
    pub fn map_in_place<F:Copy+Fn(&mut V) -> V>(&mut self, f:F) {
        self.value.iter_mut().for_each(|value| *value = f(value));
        for value in self.branches.values_mut() {
            value.map_in_place(f);
        }
    }

    /// Drop all values from the tree, replacing them with unit.
    pub fn drop_values(self) -> Tree<K,()> {
        self.map(|_| ())
    }

    /// Get the tree at the provided path.
    pub fn get(&self, path:&[K]) -> Option<&Self> {
        if let Some((first,rest)) = path.split_first() {
            self.branches.get(first).map(|t| t.get(rest)).flatten()
        } else { Some(self) }
    }

    /// Get the tree at the provided path.
    pub fn get_mut(&mut self, path:&[K]) -> Option<&mut Self> {
        if let Some((first,rest)) = path.split_first() {
            self.branches.get_mut(first).map(|t| t.get_mut(rest)).flatten()
        } else { Some(self) }
    }

    /// Get the tree in the current level for the provided `path_segment`.
    pub fn get_at_current_level(&self, path_segment:&K) -> Option<&Self> {
        self.branches.get(path_segment)
    }

    /// Get the tree in the current level for the provided `path_segment`.
    pub fn get_at_current_level_mut(&mut self, path_segment:&K) -> Option<&mut Self> {
        self.branches.get_mut(path_segment)
    }

    /// Get the value at the provided path.
    pub fn get_value(&self, path:&[K]) -> Option<&V> {
        self.get(path).map(|n| n.value.as_ref()).flatten()
    }

    /// Get the value at the provided path.
    pub fn get_value_mut(&mut self, path:&[K]) -> Option<&mut V> {
        self.get_mut(path).map(|n| n.value.as_mut()).flatten()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn single_insert_get() {
        let value    = "String";
        let path     = &[1,2,4,3];
        let mut tree = Tree::<i32,String>::empty();
        tree.insert(path,value.to_string());
        let obtained_val = tree.get_value(path);
        assert!(obtained_val.is_some());
        assert_eq!(obtained_val.unwrap().as_str(),value);
    }

    #[test]
    fn multi_insert_get() {
        let mut tree = Tree::<i32,i32>::empty();
        let values   = vec![1,2,3,4,5];
        let paths    = vec![vec![1,2],vec![2,2,1,3],vec![1,3],vec![1,2,4,1],vec![1,3,1]];
        for (val,path) in values.iter().zip(&paths) {
            tree.insert(path.as_slice(),*val)
        }
        for (val,path) in values.iter().zip(&paths) {
            let obtained_val = tree.get_value(path);
            assert!(obtained_val.is_some());
            assert_eq!(obtained_val.unwrap(),val)
        }
    }

    #[test]
    fn is_leaf() {
        let tree_1 = Tree::<i32,i32>::singleton(1);
        let tree_2 = Tree::<i32,i32>::empty();
        let mut tree_3 = Tree::<i32,i32>::empty();
        tree_3.insert(&[1],1);
        assert!(tree_1.is_leaf());
        assert!(tree_2.is_leaf());
        assert!(tree_3.is_non_leaf());
    }

    #[test]
    fn map() {
        let mut tree = Tree::<i32,i32>::empty();
        let values   = vec![1,2,3,4,5];
        let paths    = vec![vec![1,2],vec![2,2,1,3],vec![1,3],vec![1,2,4,1],vec![1,3,1]];
        for (val,path) in values.iter().zip(&paths) {
            tree.insert(path.as_slice(),*val)
        }
        let new_tree = tree.map(|v| format!("{}",v));
        for (val,path) in values.iter().zip(&paths) {
            let output = new_tree.get_value(path.as_slice()).unwrap().clone();
            assert_eq!(output,val.to_string());
        }
    }

    #[test]
    fn map_in_place() {
        let mut tree = Tree::<i32,i32>::empty();
        let values   = vec![1,2,3,4,5];
        let paths    = vec![vec![1,2],vec![2,2,1,3],vec![1,3],vec![1,2,4,1],vec![1,3,1]];
        for (val,path) in values.iter().zip(&paths) {
            tree.insert(path.as_slice(),*val)
        }
        tree.map_in_place(|v| (*v) * 2);
        for (val,path) in values.iter().zip(&paths) {
            let output = tree.get_value(path.as_slice()).unwrap().clone();
            assert_eq!(output,val*2);
        }
    }
}
