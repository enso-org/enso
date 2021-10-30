//! This module defines utilities for working with the [`std::vec::Vec`] type.

use failure::_core::hint::unreachable_unchecked;



// ==============
// === VecOps ===
// ==============

pub trait VecOps {
    type Item;

    /// Pushes the provided `item` onto the [`std::vec::Vec`], and then returns an immutable
    /// reference to the item.
    fn push_and_get(&mut self, item:Self::Item) -> &Self::Item;

    /// Pushes the provided `item` onto the [`std::vec::Vec`], and then returns a mutable reference
    /// to the item.
    fn push_and_get_mut(&mut self, item:Self::Item) -> &mut Self::Item;

    /// Extend the vector with the provided `iter`.
    fn extended<I:IntoIterator<Item=Self::Item>>(self, iter:I) -> Self;

    /// Push element to the vector.
    fn pushed(self, item:Self::Item) -> Self;

    /// Self but reversed.
    fn reversed(self) -> Self;

    /// Remove first element equal to `item` and returns it if any.
    fn remove_item(&mut self, item:&Self::Item) -> Option<Self::Item>
    where Self::Item : PartialEq<Self::Item>;
}

impl<T> VecOps for Vec<T> {
    type Item = T;

    fn push_and_get(&mut self, item:Self::Item) -> &Self::Item {
        self.push(item);
        let item_ix = self.len() - 1;
        #[allow(unsafe_code)]
            unsafe { self.get(item_ix).unwrap_or_else(||unreachable_unchecked()) }
    }

    fn push_and_get_mut(&mut self, item:Self::Item) -> &mut Self::Item {
        self.push(item);
        let item_ix = self.len() - 1;
        #[allow(unsafe_code)]
            unsafe { self.get_mut(item_ix).unwrap_or_else(||unreachable_unchecked()) }
    }

    fn extended<I:IntoIterator<Item=Self::Item>>(mut self, iter:I) -> Self {
        self.extend(iter);
        self
    }

    fn pushed(mut self, item:Self::Item) -> Self {
        self.push(item);
        self
    }

    fn reversed(mut self) -> Self {
        self.reverse();
        self
    }

    fn remove_item(&mut self, item:&T) -> Option<T>
    where T: PartialEq<T> {
        let index = self.iter().position(|x| *x == *item);
        index.map(|i| self.remove(i))
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    struct Test {
        pub item: usize
    }

    #[test]
    fn test_push_and_get() {
        let mut vec     = Vec::new();
        let item        = Test {item:10};
        let item_in_vec = vec.push_and_get(item);
        assert_eq!(item_in_vec.item, 10)
    }

    #[test]
    fn test_push_and_get_mut() {
        let mut vec      = Vec::new();
        let item         = Test {item:10};
        let item_in_vec  = vec.push_and_get_mut(item);
        item_in_vec.item = 20;
        assert_eq!(item_in_vec.item, 20);
    }
}
