use crate::prelude::*;

// ==============
// === OptVec ===
// ==============

/// A contiguous growable sparse array type. Similar to `Vec<T>`, but allowing
/// missing values. After a value is removed, it remembers the index for reuse
/// in the future.
#[derive(Clone, Debug, Shrinkwrap)]
pub struct OptVec<T> {
    #[shrinkwrap(main_field)]
    pub items: Vec<Option<T>>,
    pub free_ixs: Vec<usize>,
}

impl<T> OptVec<T> {
    /// Constructs a new, empty `Vec<T>`. It will not allocate until elements
    /// are pushed onto it.
    pub const fn new() -> Self {
        let items = Vec::new();
        let free_ixs = Vec::new();
        Self { items, free_ixs }
    }

    /// Finds a free index and inserts the element. The index is re-used in case
    /// the array is sparse or is added in case of no free places.
    pub fn insert<F: FnOnce(usize) -> T>(&mut self, f: F) -> usize {
        match self.free_ixs.pop() {
            None => {
                let ix = self.items.len();
                self.items.push(Some(f(ix)));
                ix
            }
            Some(ix) => {
                self.items[ix] = Some(f(ix));
                ix
            }
        }
    }

    /// Removes the element at provided index and marks the index to be reused.
    /// Does nothing if the index was already empty. Panics if the index was out
    /// of bounds.
    pub fn remove(&mut self, ix: usize) -> Option<T> {
        let item = self.items[ix].take();
        item.iter().for_each(|_| self.free_ixs.push(ix));
        item
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut v = OptVec::new();
        let ix1 = v.insert(|_| 1);
        let ix2 = v.insert(|_| 2);
        v.remove(ix1);
        let ix3 = v.insert(|_| 3);
        let ix4 = v.insert(|_| 4);
        assert_eq!(ix1, 0);
        assert_eq!(ix2, 1);
        assert_eq!(ix3, 0);
        assert_eq!(ix4, 2);
    }
}
