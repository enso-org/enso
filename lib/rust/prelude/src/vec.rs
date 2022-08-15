//! This module defines utilities for working with the [`std::vec::Vec`] type.

use derivative::Derivative;
use failure::_core::hint::unreachable_unchecked;



// ==============
// === VecOps ===
// ==============

pub trait VecOps<T>: AsMut<Vec<T>> + Sized {
    /// Pushes the provided `item` onto the [`std::vec::Vec`], and then returns an immutable
    /// reference to the item.
    fn push_and_get(&mut self, item: T) -> &T {
        let vec = self.as_mut();
        vec.push(item);
        let item_ix = vec.len() - 1;
        #[allow(unsafe_code)]
        unsafe {
            vec.get(item_ix).unwrap_or_else(|| unreachable_unchecked())
        }
    }

    /// Pushes the provided `item` onto the [`std::vec::Vec`], and then returns a mutable reference
    /// to the item.
    fn push_and_get_mut(&mut self, item: T) -> &mut T {
        let vec = self.as_mut();
        vec.push(item);
        let item_ix = vec.len() - 1;
        #[allow(unsafe_code)]
        unsafe {
            vec.get_mut(item_ix).unwrap_or_else(|| unreachable_unchecked())
        }
    }

    /// Extend the vector with the provided `iter`.
    fn extended<I: IntoIterator<Item = T>>(mut self, iter: I) -> Self {
        self.as_mut().extend(iter);
        self
    }

    /// Push element to the vector.
    fn pushed(mut self, item: T) -> Self {
        self.as_mut().push(item);
        self
    }

    /// Self but reversed.
    fn reversed(mut self) -> Self {
        self.as_mut().reverse();
        self
    }

    /// Remove first element equal to `item` and returns it if any.
    fn remove_item(&mut self, item: &T) -> Option<T>
    where T: PartialEq<T> {
        let vec = self.as_mut();
        let index = vec.iter().position(|x| *x == *item);
        index.map(|i| vec.remove(i))
    }

    /// Attempts to remove `T` if its `index` is valid. If not, it returns `None`.
    fn try_remove(&mut self, index: usize) -> Option<T> {
        let vec = self.as_mut();
        if index < vec.len() {
            Some(vec.remove(index))
        } else {
            None
        }
    }

    /// Attempts to remove the first element of `Vec<T>`, returns `None` if its length is zero.
    fn pop_front(&mut self) -> Option<T> {
        self.try_remove(0)
    }

    /// Removes the last `n` elements from the vector. Returns true if the elements were removed.
    fn remove_last_n(&mut self, n: usize) -> bool {
        let vec = self.as_mut();
        let new_size = vec.len().checked_sub(n);
        new_size.map(|new_size| vec.truncate(new_size)).is_some()
    }

    /// Pop and return the last element, if the vector is non-empty and the given predicate returns
    /// true when applied to the last element.
    fn pop_if<F>(&mut self, f: F) -> Option<T>
    where F: FnOnce(&T) -> bool {
        let vec = self.as_mut();
        if let Some(last) = vec.last() {
            if f(last) {
                return vec.pop();
            }
        }
        None
    }
}

impl<T> VecOps<T> for Vec<T> {}



// =====================
// === VecAllocation ===
// =====================

/// Owns a storage allocation for a [`std::vec::Vec`], but no elements.
///
/// # Usage
///
/// This data structure implements an optimization when creating temporary vectors. The use case
/// occurs when:
/// - Within some scope, a `Vec` is created, added to, and discarded.
/// - The scope may be entered multiple times.
///
/// The optimization is to reuse an allocation between entries to the scope. This is sometimes done
/// by storing and reusing the `Vec`, but that pattern is misleading; owning a `Vec` suggests that
/// values may be retained between entries to the scope. This type explicitly has only one logical
/// state (empty).
///
/// ```
/// # use enso_prelude::*;
/// #[derive(Default)]
/// struct NumberAdder {
///     // In a more complex struct it would be important to be able to tell what state the object
///     // retains from its fields.
///     temporary_nums: VecAllocation<f64>,
/// }
///
/// impl NumberAdder {
///     /// Add some numbers, with better precision than simply adding `f32` values in a loop.
///     /// (For the sake of example, ignore that this is not a fast or accurate approach.)
///     ///
///     /// Because we reuse an allocation, if this method is called repeatedly it will only have to
///     /// allocate enough space to accommodate the largest single input it processes. Thus, rather
///     /// than performing a number of reallocations that scales linearly in the number of batches
///     /// of input (assuming batch size has some constant geometric mean), it performs a number of
///     /// allocations that scales with the log of the size of the largest batch; the worst case of
///     /// this implementation has the same performance as the best case of an implementation that
///     /// doesn't reuse its allocation.
///     pub fn add_nums(&mut self, inputs: impl IntoIterator<Item = f32>) -> f32 {
///         let mut extended_precision = self.temporary_nums.take();
///         extended_precision.extend(inputs.into_iter().map(f64::from));
///         let result = extended_precision.drain(..).fold(0.0, f64::add);
///         self.temporary_nums.set_from(extended_precision);
///         result as f32
///     }
/// }
/// ```
#[derive(Clone, Debug, Derivative, Eq, PartialEq)]
#[derivative(Default(bound = ""))]
pub struct VecAllocation<T> {
    data: Vec<T>,
}

impl<T> VecAllocation<T> {
    /// Create a new, empty allocation.
    pub fn new() -> Self {
        Self::default()
    }

    /// Drop any elements from the given `Vec`, keeping its allocated memory. It can be retrieved
    /// later with `take`.
    pub fn set_from(&mut self, mut data: Vec<T>) {
        data.clear();
        self.data = data;
    }

    /// Return a `Vec` containing no elements, whose allocated storage comes from the most recent
    /// call to `set_from`, unless `take` has been called since then. Any subsequent call before the
    /// next `set_from` would return a newly-created `Vec` with no allocated memory.
    pub fn take(&mut self) -> Vec<T> {
        std::mem::take(&mut self.data)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    struct Test {
        pub item: usize,
    }

    #[test]
    fn test_push_and_get() {
        let mut vec = Vec::new();
        let item = Test { item: 10 };
        let item_in_vec = vec.push_and_get(item);
        assert_eq!(item_in_vec.item, 10)
    }

    #[test]
    fn test_push_and_get_mut() {
        let mut vec = Vec::new();
        let item = Test { item: 10 };
        let item_in_vec = vec.push_and_get_mut(item);
        item_in_vec.item = 20;
        assert_eq!(item_in_vec.item, 20);
    }
}
