//! An immutable linked list implementation.

use crate::prelude::*;


// ============
// === List ===
// ============

/// Immutable linked list containing values of type [`T`]. As every node of the list is kept in
/// [`Rc`], cloning of any subsection of this list is very fast.
#[derive(Derivative, Deref)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
#[allow(missing_docs)]
pub struct List<T> {
    pub data: Option<NonEmpty<T>>,
}

/// Non-empty list. It is guaranteed to have at least one element. See [`List`] to learn more.
#[derive(Derivative, Deref, Debug)]
#[derivative(Clone(bound = ""))]
#[allow(missing_docs)]
pub struct NonEmpty<T> {
    pub node: Rc<Node<T>>,
}

/// A node of the [`List`]. Contains the current value and link to list [`tail`].
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct Node<T> {
    pub head: T,
    pub tail: List<T>,
}

impl<T> Node<T> {
    /// Constructor.
    pub fn singleton(head: T) -> Self {
        let tail = default();
        Self { head, tail }
    }
}

impl<T> NonEmpty<T> {
    /// Constructor.
    pub fn singleton(head: T) -> Self {
        let node = Rc::new(Node::singleton(head));
        Self { node }
    }

    /// Convert this non-empty list to list of unknown length.
    pub fn into_list(self) -> List<T> {
        let data = Some(self);
        List { data }
    }

    /// Replace the head element of this list.
    pub fn with_head(self, head: T) -> Self {
        self.into_list().with_head(head)
    }

    /// Get the head element of this list.
    pub fn head(&self) -> &T {
        &self.head
    }

    /// Get tail of this list.
    pub fn tail(&self) -> &List<T> {
        &self.tail
    }

    /// Check whether this list is empty.
    pub fn is_empty(&self) -> bool {
        false
    }

    /// Convert this list to a vector.
    fn to_vec(&self) -> Vec<&T> {
        let mut out = vec![&self.head];
        let mut list = self.tail();
        loop {
            match list.head() {
                None => break,
                Some(head) => {
                    out.push(head);
                    match list.tail() {
                        None => break,
                        Some(tail) => list = tail,
                    }
                }
            }
        }
        out
    }
}

impl<T> List<T> {
    /// Replace head of this list.
    pub fn with_head(self, head: T) -> NonEmpty<T> {
        let tail = self;
        let node = Rc::new(Node { head, tail });
        NonEmpty { node }
    }

    /// Get the head element.
    pub fn head(&self) -> Option<&T> {
        self.as_ref().map(|t| t.head())
    }

    /// Get the tail of this list.
    pub fn tail(&self) -> Option<&List<T>> {
        self.as_ref().map(|t| t.tail())
    }

    /// Check whether this list is empty.
    pub fn is_empty(&self) -> bool {
        self.is_none()
    }

    /// Convert this list to a vector.
    fn to_vec(&self) -> Vec<&T> {
        self.data.as_ref().map(|t| t.to_vec()).unwrap_or_default()
    }

    /// Convert this list to a non-empty list. Return [`None`] if the list is empty.
    pub fn try_to_non_empty(&self) -> &Option<NonEmpty<T>> {
        &self.data
    }

    /// Convert this list to a non-empty list. Return [`None`] if the list is empty.
    pub fn try_into_non_empty(self) -> Option<NonEmpty<T>> {
        self.data
    }
}

impl<T> From<NonEmpty<T>> for List<T> {
    fn from(list: NonEmpty<T>) -> Self {
        list.into_list()
    }
}

impl<T: Debug> Debug for List<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.to_vec(), f)
    }
}

impl<'a, T> IntoIterator for &'a List<T> {
    type Item = &'a T;
    type IntoIter = std::vec::IntoIter<&'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a NonEmpty<T> {
    type Item = &'a T;
    type IntoIter = std::vec::IntoIter<&'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}
