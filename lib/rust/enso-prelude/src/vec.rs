//! This module defines utilities for working with the [`std::vec::Vec`] type.


// ==============
// === VecOps ===
// ==============

pub trait VecOps {
    type Item;

    /// Pushes the provided `item` onto the [`std::vec::Vec`], and then returns an immutable
    /// reference to the item.
    fn push_and_get(&mut self, item:Self::Item) -> Option<&Self::Item>;

    /// Pushes the provided `item` onto the [`std::vec::Vec`], and then returns a mutable reference
    /// to the item.
    fn push_and_get_mut(&mut self, item:Self::Item) -> Option<&mut Self::Item>;
}

impl <T> VecOps for Vec<T> {
    type Item = T;

    fn push_and_get(&mut self, item:Self::Item) -> Option<&Self::Item> {
        self.push(item);
        let item_ix = self.len() - 1;
        self.get(item_ix)
    }

    fn push_and_get_mut(&mut self, item:Self::Item) -> Option<&mut Self::Item> {
        self.push(item);
        let item_ix = self.len() - 1;
        self.get_mut(item_ix)
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
        let mut vec = Vec::new();
        let item = Test {item:10};
        let item_in_vec = vec.push_and_get(item);

        assert!(item_in_vec.is_some());
        assert_eq!(item_in_vec.unwrap().item, 10)
    }

    #[test]
    fn test_push_and_get_mut() {
        let mut vec = Vec::new();
        let item = Test {item:10};
        let item_in_vec = vec.push_and_get_mut(item);

        assert!(item_in_vec.is_some());

        let item_ref = item_in_vec.unwrap();

        item_ref.item = 20;
        assert_eq!(item_ref.item, 20);
    }
}
