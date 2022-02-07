//! This module defines utilities for working with the [`std::vec::Vec`] type.

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
}

impl<T> VecOps<T> for Vec<T> {}

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
