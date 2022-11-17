//! Simple watch counter for a resource.

use crate::prelude::*;



// ==============
// === Handle ===
// ==============

/// Handle for the `Counter`. When created increases, and when dropped decreases the counter.
#[derive(Debug)]
pub struct Handle {
    _count: Weak<()>,
}

impl Handle {
    /// Constructor. The constructed `Handle` increases the Counter count by one until it is
    /// dropped.
    pub fn new(counter: &Counter) -> Self {
        Self { _count: Rc::downgrade(&counter.count) }
    }

    /// Create a placeholder handle that doesn't correspond to any live counter. Creating or
    /// dropping this handle has no effect.
    pub fn null() -> Self {
        Self { _count: Weak::new() }
    }
}



// ===============
// === Counter ===
// ===============

/// Counter for a resource that is being watched. You can create new watchers by using the
/// `new_watch` method.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct Counter {
    count: Rc<()>,
}

impl Counter {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Checks whether the counter is zero.
    pub fn is_zero(&self) -> bool {
        Rc::weak_count(&self.count) == 0
    }

    /// Creates a new watch and returns a handle.
    pub fn new_watch(&self) -> Handle {
        Handle::new(self)
    }
}



// ===========
// === Ref ===
// ===========

/// A simple wrapper for anything with attached `Handle`. Could be used as a handy type for
/// storing both a reference to a resource and its watch handle.
#[derive(Debug, Shrinkwrap)]
pub struct Ref<T> {
    /// The underlying type.
    #[shrinkwrap(main_field)]
    pub target: T,
    #[allow(dead_code)]
    /// This is not accessed in this implementation but it needs to be kept so the handle stays
    /// alive at least as long as this struct.
    handle:     Handle,
}

impl<T> Ref<T> {
    /// Constructor.
    pub fn new(target: T, handle: Handle) -> Self {
        Self { target, handle }
    }
}
