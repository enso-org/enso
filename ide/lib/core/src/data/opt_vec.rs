use crate::prelude::*;
use super::types::Index;

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
    pub free_ixs: Vec<Index>,
}

impl<T> Default for OptVec<T> {
    fn default() -> Self {
        let items = Default::default();
        let free_ixs = Default::default();
        Self { items, free_ixs }
    }
}

impl<T> OptVec<T> {
    /// Constructs a new, empty `Vec<T>`. It will not allocate until elements
    /// are pushed onto it.
    pub fn new() -> Self { default() }

    /// Finds a free index and inserts the element. The index is re-used in case
    /// the array is sparse or is added in case of no free places.
    pub fn insert<F: FnOnce(usize) -> T>(&mut self, f: F) -> Index {
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
    pub fn remove(&mut self, ix: Index) -> Option<T> {
        let item = self.items[ix].take();
        item.iter().for_each(|_| self.free_ixs.push(ix));
        item
    }

    /// Returns the number of elements in the vector, also referred to as its 'length'.
    pub fn len(&self) -> usize {
        self.items.len() - self.free_ixs.len()
    }

    /// Returns true if vector contains no element.
    pub fn is_empty(&self) -> bool {
        self.items.len() == self.free_ixs.len()
    }
}

// ============
// === Iter ===
// ============

/// We can use Iter to iterate over OptVec<T> similarly to Vec<T>.
pub struct Iter<'a, T> {
    iter : std::slice::Iter<'a, Option<T>>
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(opt_item) = self.iter.next() {
            // If opt_item has some item, we return it, else we try the next one
            if let Some(item) = opt_item { Some(item) } else { self.next() }
        } else { None }
    }
}

impl<'a, T> IntoIterator for &'a OptVec<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        Iter { iter : (&self.items).iter() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut v = OptVec::new();
        assert!(v.is_empty(), "OptVec should be created empty");

        let ix1 = v.insert(|_| 1);
        assert_eq!(ix1, 0, "ix1 should be indexed at 0");
        assert_eq!(v.len(), 1, "OptVec should have 1 item now");
        assert!(!v.is_empty(), "OptVec is no longer empty now");

        let ix2 = v.insert(|_| 2);
        assert_eq!(ix2, 1, "ix2 should be indexed at 1");
        assert_eq!(v.len(), 2);

        v.remove(ix1); // remove ix1 (0) and make 0 index free
        assert_eq!(v.len(), 1); // removing should decrease len by 1

        v.remove(ix2); // remove ix2 (1) and make 1 index free
        assert_eq!(v.len(), 0);
        assert!(v.is_empty(), "OptVec should be empty now");

        let ix3 = v.insert(|_| 3);
        assert_eq!(v.len(), 1);
        let ix4 = v.insert(|_| 4);
        assert_eq!(ix3, 1, "ix3 should be the first freed index");
        assert_eq!(ix4, 0, "ix4 should be the second freed index");
        assert_eq!(v.len(), 2);
    }

    #[test]
    fn test_iter() {
        let mut v = OptVec::new();

        let  ix1 = v.insert(|_| 0);
        let _ix2 = v.insert(|_| 1);
        let _ix3 = v.insert(|_| 2);

        assert_eq!(v.len(), 3, "OptVec should have 3 items");

        for (i, value) in v.into_iter().enumerate() {
            assert_eq!(i, *value);
        }

        v.remove(ix1);
        assert_eq!(v.len(), 2, "OptVec should have 2 items");
        for (i, value) in v.into_iter().enumerate() {
            // we add + 1, because the fisrt item is 1 now.
            assert_eq!(i + 1, *value);
        }
    }
}
