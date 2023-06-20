//! Data structure supporting:
//! - O(1) append (amortized), with low overhead.
//! - O(1) random-access reads.
//! - Single-threaded shared mutability.
//!
//! # Implementation
//!
//! Note [Log Safety]
//! =============================
//! Soundness of shared-mutable data requires avoiding reference conflicts: The data must not be
//! mutated while a shared-reference to it exists. This is ensured by:
//! - No public interface of [`Log`] allows keeping a reference with lifetime derived from the data.
//! - References taken within [`Log`]'s implementation don't overlap with other references in the
//!   scope.

use std::cell;



/// Allocation unit of events within a [`Log`].
#[cfg_attr(debug_assertions, allow(dead_code))]
const BLOCK: usize = 1024;



// ===========
// === Log ===
// ===========

#[cfg(not(debug_assertions))]
pub use fast::Log;

#[cfg(debug_assertions)]
pub use safe::Log;

/// Fast implementation used when debug assertions are disabled.
#[cfg(not(debug_assertions))]
mod fast {
    use super::*;

    use std::mem;

    /// A shared-mutable data structure supporting append and random-access read.
    #[derive(Debug)]
    pub struct Log<T> {
        current:   cell::UnsafeCell<Box<[mem::MaybeUninit<T>; BLOCK]>>,
        completed: cell::UnsafeCell<Vec<Box<[T; BLOCK]>>>,
        len:       cell::Cell<usize>,
    }

    impl<T> Log<T> {
        /// Create a new, empty [`Log`].
        pub fn new() -> Self {
            Self {
                current:   cell::UnsafeCell::new(Box::new(mem::MaybeUninit::uninit_array())),
                completed: cell::UnsafeCell::new(Default::default()),
                len:       Default::default(),
            }
        }

        /// Push an element.
        #[inline]
        #[allow(unsafe_code)] // Note [Log Safety]
        pub fn push(&self, element: T) {
            unsafe {
                let i = self.len.get();
                (*self.current.get())[i % BLOCK].write(element);
                let i1 = i + 1;
                if i1 % BLOCK == 0 {
                    // Current gradually-initialized block is full. Read it, cast it to a
                    // fully-initialized block, and replace it with a new empty block.
                    let empty = Box::new(mem::MaybeUninit::uninit_array());
                    let block = self.current.get().replace(empty);
                    let block =
                        mem::transmute::<Box<[mem::MaybeUninit<T>; BLOCK]>, Box<[T; BLOCK]>>(block);
                    // Add the old block to our collection of completed blocks.
                    (*self.completed.get()).push(block);
                }
                self.len.set(i1);
            }
        }

        /// Returns the number of entries in the log.
        #[inline]
        pub fn len(&self) -> usize {
            self.len.get()
        }

        /// Returns true if the log contains no entries.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }

        /// Applies a function to each entry in the log.
        #[allow(unsafe_code)] // Note [Log Safety]
        pub fn for_each<F>(&self, mut f: F)
        where F: FnMut(&T) {
            unsafe {
                let blocks = self.len() / BLOCK;
                let n = self.len() % BLOCK;
                for i in 0..blocks {
                    // Safety: The contents of a completed block are never modified, so we can hold
                    // a borrow while calling the function (which may append to
                    // the log).
                    let block = &(*self.completed.get())[i];
                    block.iter().for_each(&mut f);
                }
                // Safety: The elements in the completed portion of the block are never modified, so
                // we can hold a borrow while calling the function (which may append
                // to the log).
                let current = &(*self.current.get())[..n];
                current.iter().map(|elem| elem.assume_init_ref()).for_each(f);
            }
        }

        /// Pass the element at the specified index to the given function. Returns `None` if the
        /// index is out of bounds.
        #[inline]
        #[allow(unsafe_code)] // Note [Log Safety]
        pub fn get<U>(&self, index: usize, f: impl FnOnce(&T) -> U) -> Option<U> {
            unsafe {
                let block_i = index / BLOCK;
                let i = index % BLOCK;
                let blocks = &*self.completed.get();
                let value = if let Some(block) = blocks.get(block_i) {
                    Some(&block[i])
                } else if block_i == blocks.len() && i < self.len.get() % BLOCK {
                    Some((*self.current.get())[i].assume_init_ref())
                } else {
                    None
                };
                // Safety: Whether the element is in a completed block, or in the completed portion
                // of the current block, it will never be moved or modified; so we
                // can hold a borrow while calling the function (which may append to
                // the log).
                value.map(f)
            }
        }
    }

    impl<T: Clone> Log<T> {
        /// Return a collection of all entries currently in the log.
        pub fn clone_all<C>(&self) -> C
        where C: Default + Extend<T> {
            let mut result = C::default();
            self.for_each(|elem| result.extend_one(elem.clone()));
            result
        }
    }
}

/// Checked implementation used when debug assertions are enabled.
#[cfg(debug_assertions)]
mod safe {
    use super::*;

    /// A shared-mutable data structure supporting append and random-access read.
    #[derive(Debug)]
    pub struct Log<T> {
        data: cell::RefCell<Vec<T>>,
    }

    impl<T> Log<T> {
        /// Create a new, empty [`Log`].
        #[inline]
        pub fn new() -> Self {
            let data = cell::RefCell::new(Vec::new());
            Self { data }
        }

        /// Push an element.
        #[inline]
        pub fn push(&self, element: T) {
            self.data.borrow_mut().push(element)
        }

        /// Returns the number of entries in the log.
        #[inline]
        pub fn len(&self) -> usize {
            self.data.borrow().len()
        }

        /// Returns true if the log contains no entries.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }

        /// Applies a function to each entry in the log.
        #[inline]
        pub fn for_each<F>(&self, f: F)
        where F: FnMut(&T) {
            self.data.borrow().iter().for_each(f)
        }

        /// Pass the element at the specified index to the given function. Returns `None` if the
        /// index is out of bounds.
        #[inline]
        pub fn get<U>(&self, index: usize, f: impl FnOnce(&T) -> U) -> Option<U> {
            self.data.borrow().get(index).map(f)
        }
    }

    impl<T: Clone> Log<T> {
        /// Return a collection of all entries currently in the log.
        pub fn clone_all<C>(&self) -> C
        where C: Default + Extend<T> {
            let mut result = C::default();
            result.extend(self.data.borrow().iter().cloned());
            result
        }
    }
}

// This can't be derived without requiring T: Default, which is not otherwise needed.
// See: https://github.com/rust-lang/rust/issues/26925
impl<T> Default for Log<T> {
    fn default() -> Self {
        Log::new()
    }
}



// ======================
// === ThreadLocalLog ===
// ======================

/// Wraps a [`Log`] for thread-local access.
#[derive(Debug)]
pub struct ThreadLocalLog<T: 'static>(std::thread::LocalKey<Log<T>>);

impl<T: 'static> ThreadLocalLog<T> {
    #[allow(missing_docs)]
    pub const fn new(log: std::thread::LocalKey<Log<T>>) -> Self {
        Self(log)
    }

    /// Append an entry to the log.
    pub fn push(&'static self, t: T) {
        self.0.with(|this| this.push(t));
    }

    /// Return the number of entries in the log. Note that as the log is thread-local but
    /// append-only, any function in the thread may increase this value, but it will never
    /// decrease.
    pub fn len(&'static self) -> usize {
        self.0.with(|this| this.len())
    }

    /// Returns true if the log contains no entries.
    pub fn is_empty(&'static self) -> bool {
        self.0.with(|this| this.is_empty())
    }

    /// Get the entry at the given index, and pass it to a function; return the result of the
    /// function.
    ///
    /// Panics if the index is not less than [`len`].
    pub fn get<U>(&'static self, i: usize, f: impl FnOnce(&T) -> U) -> U {
        self.try_get(i, f).unwrap()
    }

    /// Get the entry at the given index, and pass it to a function; return the result of the
    /// function.
    ///
    /// Returns [`None`] if the index is not less than [`len`].
    pub fn try_get<U>(&'static self, i: usize, f: impl FnOnce(&T) -> U) -> Option<U> {
        self.0.with(|this| this.get(i, f))
    }
}

impl<T: 'static + Clone> ThreadLocalLog<T> {
    /// Return a collection of log entries since the program was started.
    pub fn clone_all<C>(&'static self) -> C
    where C: Default + Extend<T> {
        self.0.with(|this| this.clone_all())
    }
}
