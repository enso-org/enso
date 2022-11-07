//! A dependency graph implementation optimized for sorting depth-indexed components.

use crate::prelude::*;

use std::collections::BTreeSet;



// ============
// === Node ===
// ============

/// A dependency graph node. Registers all incoming and outgoing edges. Incoming enges are
/// considered sources of this node. They need to be sorted before this node when performing the
/// topological sorting.
///
/// Please note that the input and output edges are stored in a vector because in most cases there
/// would be small amount of them (zero or one).
#[derive(Clone, Debug)]
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
#[allow(missing_docs)]
pub struct Node<Edge> {
    pub ins: Vec<Edge>,
    pub out: Vec<Edge>,
}

impl<Edge> Node<Edge> {
    /// Check whether this node does not have any input and output dependencies to other nodes.
    pub fn is_empty(&self) -> bool {
        self.ins.is_empty() && self.out.is_empty()
    }
}



// =======================
// === DependencyGraph ===
// =======================

/// Dependency graph keeping track of [`Node`]s and their dependencies.
///
/// The primary use case of this graph is topological sorting of dependencies. Please note that this
/// graph implementation is not DAG, it can contain cycles. In case a cycle occurs it will be
/// automatically broken on the lowest node id.
#[derive(Clone)]
#[derive(Derivative)]
#[derivative(Default(bound = "T:Eq+Hash+Ord"))]
#[derivative(Debug(bound = "T:Debug+Eq+Hash"))]
pub struct DependencyGraph<T> {
    nodes: BTreeMap<T, Node<T>>,
}

impl<T: Clone + Eq + Hash + Ord> DependencyGraph<T> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Insert a new dependency to the graph. Returns [`true`] if the insertion was successful
    /// (the dependency was not present already), or [`false`] otherwise.
    pub fn insert_dependency(&mut self, first: T, second: T) -> bool {
        let fst_key = first.clone();
        let snd_key = second.clone();
        let fst_out = &mut self.nodes.entry(fst_key).or_default().out;
        let exists = fst_out.contains(&second);
        if !exists {
            fst_out.push(second);
            self.nodes.entry(snd_key).or_default().ins.push(first);
        }
        !exists
    }

    /// Remove a dependency from the graph. Returns [`true`] if the dependency was found, or
    /// [`false`] otherwise.
    pub fn remove_dependency(&mut self, first: T, second: T) -> bool {
        let fst_found = self.nodes.get_mut(&first).map(|t| t.out.remove_item(&second).is_some());
        let snd_found = self.nodes.get_mut(&second).map(|t| t.ins.remove_item(&first).is_some());
        if self.nodes.get(&first).map(|t| t.is_empty()) == Some(true) {
            self.nodes.remove(&first);
        }
        if self.nodes.get(&second).map(|t| t.is_empty()) == Some(true) {
            self.nodes.remove(&second);
        }
        fst_found == Some(true) && snd_found == Some(true)
    }

    /// Removes all (incoming and outgoing) dependencies from nodes whose indexes do not belong to
    /// the provided slice.
    pub fn keep_only(&mut self, keys: &[T]) {
        self.unchecked_keep_only(keys.iter().cloned().sorted())
    }

    /// Removes all (incoming and outgoing) dependencies from nodes whose indexes do not belong to
    /// the provided slice.
    pub fn kept_only(mut self, keys: &[T]) -> Self {
        self.keep_only(keys);
        self
    }

    /// Just like [`keep_only`], but the provided slice must be sorted.
    pub fn unchecked_keep_only(&mut self, sorted_keys: impl IntoIterator<Item = T>) {
        let mut keep = sorted_keys.into_iter();
        let mut next_to_keep = keep.next();
        let keys = self.nodes.keys().cloned().collect_vec();
        let mut keys_iter = keys.iter();
        let mut opt_key = keys_iter.next();
        while let Some(key) = opt_key {
            match next_to_keep.as_ref().map(|t| t.cmp(key)) {
                Some(std::cmp::Ordering::Less) => next_to_keep = keep.next(),
                Some(std::cmp::Ordering::Equal) => {
                    next_to_keep = keep.next();
                    opt_key = keys_iter.next();
                }
                _ => {
                    if let Some(node) = self.nodes.get_mut(key) {
                        let node = mem::take(node);
                        for key2 in node.ins {
                            self.nodes.get_mut(&key2).for_each(|t| t.out.remove_item(key))
                        }
                        for key2 in node.out {
                            self.nodes.get_mut(&key2).for_each(|t| t.ins.remove_item(key))
                        }
                    }
                    opt_key = keys_iter.next();
                }
            }
        }
    }

    /// Just like [`kept_only`], but the provided slice must be sorted.
    pub fn unchecked_kept_only(mut self, sorted_keys: impl IntoIterator<Item = T>) -> Self {
        self.unchecked_keep_only(sorted_keys);
        self
    }

    /// Sorts the provided indexes in topological order based on the rules recorded in the graph.
    /// In case the graph is not a DAG, it will still be sorted by breaking cycles on elements with
    /// the smallest index.
    pub fn topo_sort(&self, keys: &[T]) -> Vec<T> {
        self.unchecked_topo_sort(keys.iter().cloned().sorted().collect_vec())
    }

    /// Just like [`topo_sort`], but consumes the current dependency graph instead of cloning it.
    pub fn into_topo_sort(self, keys: &[T]) -> Vec<T> {
        self.into_unchecked_topo_sort(keys.iter().cloned().sorted().collect_vec())
    }

    /// Just like [`topo_sort`], but the provided slice must be sorted.
    pub fn unchecked_topo_sort(&self, sorted_keys: Vec<T>) -> Vec<T> {
        self.clone().into_unchecked_topo_sort(sorted_keys)
    }

    /// Just like [`unchecked_topo_sort`], bbut consumes the current dependency graph instead of
    /// cloning it.
    pub fn into_unchecked_topo_sort(self, sorted_keys: Vec<T>) -> Vec<T> {
        let mut sorted = Vec::<T>::new();
        let mut orphans = BTreeSet::<T>::new();
        let mut non_orphans = BTreeSet::<T>::new();
        let this = self.unchecked_kept_only(sorted_keys.iter().cloned());
        sorted.reserve_exact(sorted_keys.len());

        let mut nodes = this.nodes;
        for key in sorted_keys.into_iter() {
            let ins_empty = nodes.get(&key).map(|t| t.ins.is_empty()) != Some(false);
            if ins_empty {
                orphans.insert(key);
            } else {
                non_orphans.insert(key);
            }
        }

        loop {
            match orphans.iter().next().cloned() {
                None => {
                    match non_orphans.iter().next().cloned() {
                        None => break,
                        Some(ix) => {
                            // Non DAG, contains cycle. Let's break them on the smallest node `ix`.
                            non_orphans.remove(&ix);
                            orphans.insert(ix);
                        }
                    }
                }
                Some(ix) => {
                    sorted.push(ix.clone());
                    orphans.remove(&ix);
                    if let Some(node) = nodes.get_mut(&ix) {
                        for ix2 in mem::take(&mut node.out) {
                            if let Some(node2) = nodes.get_mut(&ix2) {
                                let ins = &mut node2.ins;
                                ins.remove_item(&ix);
                                if ins.is_empty() && non_orphans.remove(&ix2) {
                                    orphans.insert(ix2);
                                }
                            }
                        }
                    }
                }
            }
        }
        sorted
    }
}


impl<'a, T> IntoIterator for &'a DependencyGraph<T> {
    type Item = (&'a T, &'a Node<T>);
    type IntoIter = std::collections::btree_map::Iter<'a, T, Node<T>>;
    fn into_iter(self) -> std::collections::btree_map::Iter<'a, T, Node<T>> {
        self.nodes.iter()
    }
}

impl<T> IntoIterator for DependencyGraph<T> {
    type Item = (T, Node<T>);
    type IntoIter = std::collections::btree_map::IntoIter<T, Node<T>>;
    fn into_iter(self) -> std::collections::btree_map::IntoIter<T, Node<T>> {
        self.nodes.into_iter()
    }
}

impl<T: Ord> Extend<(T, Node<T>)> for DependencyGraph<T> {
    fn extend<I: IntoIterator<Item = (T, Node<T>)>>(&mut self, iter: I) {
        self.nodes.extend(iter)
    }
}



// ==============
// === Macros ===
// ==============

/// Utility macro allowing easy construction of the [`DependencyGraph`]. The following code:
/// ```
/// use crate::enso_data_structures::dependency_graph;
/// dependency_graph!(1->2, 2->3);
/// ```
/// will produce:
/// ```text
/// {
///     let mut graph = DependencyGraph::new();
///     graph.insert_dependency(1, 2);
///     graph.insert_dependency(2, 3);
///     graph
/// }
/// ```
#[macro_export]
macro_rules! dependency_graph {
    ($($fst:tt -> $snd:tt),* $(,)?) => {
        {
            #[allow(unused_mut)]
            let mut graph = $crate::dependency_graph::DependencyGraph::new();
            $(graph.insert_dependency($fst,$snd);)*
            graph
        }
    };
}



// =============
// === Tests ===
// =============

extern crate test;

/// Asserts whether the graph will sort the provided slice in the same order as it was provided.
/// Please note, that the slice is sorted in order before being sorted topologically.
pub fn assert_valid_sort(graph: &DependencyGraph<usize>, sorted: &[usize]) {
    let sorted = sorted.to_vec();
    assert_eq!(graph.topo_sort(&sorted), sorted);
}

/// The same as [`assert_valid_sort`] but with a shorter syntax. Learn more about it by looking at
/// its usage below.
#[cfg(test)]
macro_rules! assert_valid_sort {
    ($($sorted:tt for {$($rules:tt)*})*) => {
        $({
            let graph = dependency_graph!{$($rules)*};
            assert_valid_sort(&graph,&$sorted);
        })*
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identity() {
        assert_valid_sort! {
            []        for {}
            [0]       for {}
            [0,1]     for {}
            [0,1,2]   for {}
            [0,1,2,3] for {}
        }
    }

    #[test]
    fn test_non_overlapping_rules() {
        assert_valid_sort! {
            [1,0]     for {1->0}
            [0,2,1,3] for {2->1}
            [1,0,3,2] for {1->0,3->2}
        }
    }

    #[test]
    fn test_overlapping_rules() {
        assert_valid_sort! {
            [4,3,2,1,0]       for {4->3,3->2,2->1,1->0}
            [4,3,2,1,0]       for {3->2,4->3,1->0,2->1}
            [4,3,2,1,0]       for {1->0,2->1,3->2,4->3}
            [1,8,2,7,3,6,4,5] for {1->8,8->2,2->7,7->3,3->6,6->4,4->5}
        }
    }

    #[test]
    fn test_non_dag() {
        assert_valid_sort! {
            [0]     for {0->0}
            [1,2,0] for {0->0}
            [0,2,1] for {1->1}
            [0,1,2] for {2->2}
            [0,1]   for {0->1,1->0}
            [0,1,2] for {0->1,1->2,2->0}
            [0,1,2] for {0->0,0->1,0->2,1->0,1->1,1->2,2->0,2->1,2->2}
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use test::Bencher;

    /// # Results (ms)
    ///
    ///   iters | time(ms) |
    ///   10^3  | 0.47     |
    ///   10^4  | 5.2      |
    ///   10^5  | 74.2     |
    #[bench]
    fn bench_ascending(b: &mut Bencher) {
        let iters = 1_000;
        let out = (0..iters).collect_vec();
        let mut graph = DependencyGraph::new();
        for (i, j) in out.iter().zip(out.iter().skip(1)) {
            graph.insert_dependency(*i, *j);
        }
        b.iter(move || assert_eq!(graph.topo_sort(&out), out));
    }

    /// # Results (ms)
    ///
    ///   iters | time(ms) |
    ///   10^3  | 0.5      |
    ///   10^4  | 6.2      |
    ///   10^5  | 86.8     |
    #[bench]
    fn bench_descending(b: &mut Bencher) {
        let iters = 1_000;
        let out = (0..iters).rev().collect_vec();
        let mut graph = DependencyGraph::new();
        for (i, j) in out.iter().zip(out.iter().skip(1)) {
            graph.insert_dependency(*i, *j);
        }
        b.iter(move || assert_eq!(graph.topo_sort(&out), out));
    }
}
