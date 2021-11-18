//! Simple watch counter for a resource.

use crate::prelude::*;



// ==============
// === Handle ===
// ==============

/// Handle for the `Counter`. When created increases, and when dropped decreases the counter.
#[derive(Debug)]
pub struct Handle {
    counter: Counter,
}

impl Handle {
    /// Constructor.
    pub fn new(counter: &Counter) -> Self {
        let counter = counter.clone_ref();
        counter.increase();
        Self { counter }
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        self.counter.decrease()
    }
}



// ===============
// === Counter ===
// ===============

/// Counter for a resource that is being watched. You can create new watchers by using the
/// `new_watch` method.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct Counter {
    count: Rc<Cell<usize>>,
}

impl Counter {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Checks whether the counter is zero.
    pub fn is_zero(&self) -> bool {
        self.count.get() == 0
    }

    /// Creates a new watch and returns a handle.
    pub fn new_watch(&self) -> Handle {
        Handle::new(self)
    }

    fn increase(&self) {
        self.count.set(self.count.get() + 1);
    }

    fn decrease(&self) {
        self.count.set(self.count.get() - 1);
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
