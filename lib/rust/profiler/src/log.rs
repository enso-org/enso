//! Data structure supporting limited interior mutability, to build up a collection.
//!
//! # Implementation
//!
//! Note [Log Safety]
//! =============================
//! When obtaining a reference from the UnsafeCell, all accessors follow these rules:
//! - There must be a scope that the reference doesn't escape.
//! - There must be no other references obtained in the same scope.
//! Consistently following these rules ensures the no-alias rule of mutable references is
//! satisfied.

use std::cell;
use std::mem;



// ===========
// === Log ===
// ===========

/// Data structure supporting limited interior mutability, to build up a collection.
#[derive(Debug)]
pub struct Log<T>(cell::UnsafeCell<Vec<T>>);

#[allow(unsafe_code)]
impl<T> Log<T> {
    #[allow(clippy::new_without_default)]
    /// Create a new, empty vec builder.
    pub fn new() -> Self {
        Self(cell::UnsafeCell::new(vec![]))
    }

    /// Push an element.
    pub fn append(&self, element: T) {
        // Note [Log Safety]
        unsafe {
            (*self.0.get()).push(element);
        }
    }

    /// Return (and consume) all elements pushed so far.
    pub fn take_all(&self) -> Vec<T> {
        // Note [Log Safety]
        unsafe { mem::take(&mut *self.0.get()) }
    }

    /// The number of elements that are currently available.
    pub fn len(&self) -> usize {
        // Note [Log Safety]
        unsafe { &*self.0.get() }.len()
    }

    /// Returns true if no elements are available.
    pub fn is_empty(&self) -> bool {
        // Note [Log Safety]
        unsafe { &*self.0.get() }.is_empty()
    }
}

#[allow(unsafe_code)]
impl<T: Clone> Log<T> {
    /// Return clones of all elements pushed so far.
    pub fn clone_all(&self) -> Vec<T> {
        // Note [Log Safety]
        unsafe { &*self.0.get() }.to_vec()
    }
}

// This can't be derived without requiring T: Default, which is not otherwise needed.
// See: https://github.com/rust-lang/rust/issues/26925
impl<T> Default for Log<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}



// ======================
// === ThreadLocalLog ===
// ======================

/// Wraps a thread-local [`Log`] instance.
#[derive(Debug)]
pub struct ThreadLocalLog<T: 'static>(std::thread::LocalKey<Log<T>>);

impl<T: 'static> ThreadLocalLog<T> {
    /// New.
    pub const fn new(log: std::thread::LocalKey<Log<T>>) -> Self {
        Self(log)
    }

    /// Push an element.
    pub fn append(&'static self, element: T) {
        self.0.with(|log| log.append(element))
    }

    /// Return (and consume) all elements pushed so far.
    pub fn take_all(&'static self) -> Vec<T> {
        self.0.with(|log| log.take_all())
    }

    /// The number of elements that are currently available.
    pub fn len(&'static self) -> usize {
        self.0.with(|log| log.len())
    }

    /// Returns true if no elements are available.
    pub fn is_empty(&'static self) -> bool {
        self.0.with(|log| log.is_empty())
    }
}

impl<T: Clone> ThreadLocalLog<T> {
    /// Return clones of all elements pushed so far.
    pub fn clone_all(&'static self) -> Vec<T> {
        self.0.with(|log| log.clone_all())
    }
}
