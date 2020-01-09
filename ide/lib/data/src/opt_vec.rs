 #![allow(missing_docs)]

use crate::prelude::*;
use std::iter::FilterMap;
use std::slice;

// ==============
// === OptVec ===
// ==============

/// A contiguous growable sparse array type. Similar to `Vec<T>`, but allowing missing values.
/// After a value is removed, it remembers the index for reuse in the future.
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
#[derive(Clone,Debug,Shrinkwrap)]
pub struct OptVec<T> {
    #[shrinkwrap(main_field)]
    pub items: Vec<Option<T>>,
    pub free_ixs: SmallVec<[Ix; 128]>,
}

pub type Ix                     = usize;
pub type Iter           <'t, T> = FilterMap<slice::Iter   <'t, Option<T>>, OptionAsRef   <T>>;
pub type IterMut        <'t, T> = FilterMap<slice::IterMut<'t, Option<T>>, OptionAsRefMut<T>>;
pub type OptionAsRef        <T> = for<'r> fn(&'r     Option<T>) -> Option<&'r     T>;
pub type OptionAsRefMut     <T> = for<'r> fn(&'r mut Option<T>) -> Option<&'r mut T>;

impl<T> OptVec<T> {
    /// Constructs a new, empty `Vec<T>`. It will not allocate until elements are pushed onto it.
    pub fn new() -> Self {
        default()
    }

    /// Inserts the provided element to the vector. It reuses free indexes if any.
    pub fn insert(&mut self, item: T) -> Ix {
        self.insert_with_ix(|_| item)
    }

    /// Iterator.
    pub fn iter(&self) -> Iter<T> {
        self.items.iter().filter_map(Option::as_ref)
    }

    /// Mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.items.iter_mut().filter_map(Option::as_mut)
    }

    /// Finds a free index and inserts the element. The index is re-used in case the array is sparse
    /// or is added in case of no free places.
    pub fn insert_with_ix<F: FnOnce(Ix) -> T>(&mut self, f: F) -> Ix {
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

    /// Reserve an index for further reuse. Please remember that you cannot use the index to read
    /// values unless the value is set.
    pub fn reserve_ix(&mut self) -> Ix {
        self.free_ixs.pop().unwrap_or_else(|| {
            let ix = self.items.len();
            self.items.push(None);
            ix
        })
    }

    /// Sets the value at given index. Panics if the index was already freed.
    pub fn set(&mut self, ix:Ix, t:T) {
        self.items[ix] = Some(t);
    }

    /// Removes the element at provided index and marks the index to be reused. Does nothing if the
    /// index was already empty. Panics if the index was out of bounds.
    pub fn remove(&mut self, ix: Ix) -> Option<T> {
        let item = self.items[ix].take();
        item.iter().for_each(|_| self.free_ixs.push(ix));
        item
    }

    /// Returns the number of elements in the vector, including reserved indexes. Also referred to
    /// as its 'length'.
    pub fn len(&self) -> usize {
        self.items.len() - self.free_ixs.len()
    }

    /// Returns true if vector contains no element.
    pub fn is_empty(&self) -> bool {
        self.items.len() == self.free_ixs.len()
    }
}


// === Indexing ===

impl<T> Index<usize> for OptVec<T> {
    type Output = T;
    fn index(&self, ix: usize) -> &Self::Output {
        self.items.index(ix).as_ref().unwrap()
    }
}

impl<T> IndexMut<usize> for OptVec<T> {
    fn index_mut(&mut self, ix: usize) -> &mut Self::Output {
        self.items.index_mut(ix).as_mut().unwrap()
    }
}


// === Iterators ===

impl<'a, T> IntoIterator for &'a OptVec<T> {
    type Item     = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut OptVec<T> {
    type Item     = &'a mut T;
    type IntoIter = IterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut v = OptVec::new();
        assert!(v.is_empty());

        let ix1 = v.insert(1);
        assert_eq!(ix1,0);
        assert_eq!(v.len(),1);
        assert!(!v.is_empty());

        let ix2 = v.insert(2);
        assert_eq!(ix2,1);
        assert_eq!(v.len(),2);

        v.remove(ix1);
        assert_eq!(v.len(),1);

        v.remove(ix2);
        assert_eq!(v.len(),0);
        assert!(v.is_empty());

        let ix3 = v.insert(3);
        assert_eq!(v.len(),1);

        let ix4 = v.insert(4);
        assert_eq!(ix3,1);
        assert_eq!(ix4,0);
        assert_eq!(v.len(),2);
    }

    #[test]
    fn test_iter() {
        let mut v = OptVec::new();

        let  ix1 = v.insert(0);
        let _ix2 = v.insert(1);
        let _ix3 = v.insert(2);
        assert_eq!(v.len(),3);

        for (i,value) in v.into_iter().enumerate() {
            assert_eq!(i, *value);
        }

        v.remove(ix1);
        assert_eq!(v.len(),2);
        for (i,value) in v.into_iter().enumerate() {
            assert_eq!(i + 1, *value);
        }
    }

    #[test]
    fn test_iter_mut() {
        let mut v = OptVec::new();

        let  ix1 = v.insert(0);
        let _ix2 = v.insert(1);
        let _ix3 = v.insert(2);
        assert_eq!(v.len(),3);

        v.remove(ix1);
        assert_eq!(v.len(),2);

        for value in &mut v { *value *= 2; }
        for (i, value) in v.into_iter().enumerate() {
            assert_eq!((i + 1) * 2, *value);
        }
    }
}
