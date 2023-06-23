//! A vector with a cap for its size. If the vector is full, adding a new element will remove
//! an old one.

use std::collections::VecDeque;



// ==========================
// === SizeCappedVecDeque ===
// ==========================

/// A vector with a cap for its size. If the vector is full, adding a new element will remove an old
/// one.
#[derive(Clone, Debug)]
pub struct SizeCappedVecDeque<T> {
    capacity: usize,
    vec:      VecDeque<T>,
}

impl<T> SizeCappedVecDeque<T> {
    /// Constructor.
    pub fn new(capacity: usize) -> Self {
        let vec = VecDeque::with_capacity(capacity);
        Self { capacity, vec }
    }

    /// Check whether the vector is empty.
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// The capacity of the vector.
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Check whether the vector is full.
    pub fn is_full(&self) -> bool {
        self.len() == self.capacity
    }

    /// Push a new element at the beginning of the vector. if the vector is full, the last element
    /// will be dropped.
    pub fn push_front(&mut self, value: T) {
        if self.is_full() {
            self.vec.pop_back();
        }
        self.vec.push_front(value);
    }

    /// Push a new element at the end of the vector. if the vector is full, the first element will
    /// be dropped.
    pub fn push_back(&mut self, value: T) {
        if self.is_full() {
            self.vec.pop_front();
        }
        self.vec.push_back(value);
    }

    /// Pop the first element of the vector.
    pub fn pop_front(&mut self) -> Option<T> {
        self.vec.pop_front()
    }

    /// Pop the last element of the vector.
    pub fn pop_back(&mut self) -> Option<T> {
        self.vec.pop_back()
    }

    /// Get the element at the given index.
    pub fn get(&self, index: usize) -> Option<&T> {
        self.vec.get(index)
    }

    /// Get a mutable reference to the element at the given index.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.vec.get_mut(index)
    }

    /// Get the last element of the vector, if any.
    pub fn last(&self) -> Option<&T> {
        self.vec.back()
    }

    /// Run the provided function on the last `n` elements of the vector.
    pub fn with_last_n_elems(&mut self, n: usize, mut f: impl FnMut(&mut T)) {
        let len = self.len();
        let start = len.saturating_sub(n);
        for i in start..len {
            f(self.vec.get_mut(i).unwrap());
        }
    }

    /// Run the provided function on the `n`-th element of the vector counted from back.
    pub fn with_last_nth_elem(&mut self, n: usize, f: impl FnOnce(&mut T)) {
        let len = self.len();
        if len > n {
            f(self.vec.get_mut(len - n - 1).unwrap());
        }
    }
}
