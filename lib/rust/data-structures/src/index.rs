//! This module defines a typed index struct. Useful to introduce type safety when using indexes of
//! several indexable containers.

use crate::prelude::*;



// =============
// === Index ===
// =============

/// Typed wrapper for `usize` meant to be used as a typed index.
///
/// Useful to introduce type safety when using indexes of several indexable containers, for example:
///
/// ```text
/// # use enso_data_structures::index::Index;
/// # struct Edge {}
/// # struct Vertex {}
/// # fn do_something(_e: &Edge, _v : &Vertex) {}
/// struct Graph {
///     edges:    Vec<Edge>,
///     vertices: Vec<Vertex>,
/// }
///
/// impl Graph {
///     /// When calling this function, you won't mix the edge id with vertex id.
///     fn do_something_with_vertex_and_edge(&self, v: Index<Vertex>, e: Index<Edge>) {
///         do_something(&self.edges[e.raw], &self.vertices[v.raw]);
///     }
/// }
/// ```
pub struct Index<T> {
    /// Raw value.
    pub raw:  usize,
    _phantom: ZST<T>,
}

impl<T> Index<T> {
    /// Constructor
    pub fn new(raw: usize) -> Self {
        let _phantom = default();
        Self { raw, _phantom }
    }
}

// === Impls ===

impl<T> Copy for Index<T> {}
impl<T> Eq for Index<T> {}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Hash for Index<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state)
    }
}

impl<T> PartialEq for Index<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T> From<Index<T>> for usize {
    fn from(t: Index<T>) -> Self {
        t.raw
    }
}

impl<T> From<&Index<T>> for usize {
    fn from(t: &Index<T>) -> Self {
        t.raw
    }
}

impl<T> From<usize> for Index<T> {
    fn from(t: usize) -> Self {
        Self::new(t)
    }
}

impl<T> From<&usize> for Index<T> {
    fn from(t: &usize) -> Self {
        Self::new(*t)
    }
}

impl<T> Debug for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.raw)
    }
}

impl<T> Display for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.raw)
    }
}
