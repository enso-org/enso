use crate::prelude::*;
use std::iter::FilterMap;
use std::slice;

// ==============
// === OptVec ===
// ==============

/// A contiguous growable sparse array type. Similar to `Vec<T>`, but allowing
/// missing values. After a value is removed, it remembers the index for reuse
/// in the future.
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
#[derive(Clone, Debug, Shrinkwrap)]
pub struct OptVec<T> {
    #[shrinkwrap(main_field)]
    pub vec  : Vec<Option<T>>,
    pub free : Vec<Ix>,
}

pub type Ix             = usize;
pub type Iter<'t, T>    = FilterMap<slice::Iter<'t, Option<T>>, OptionAsRef<T>>;
pub type IterMut<'t, T> = FilterMap<slice::IterMut<'t, Option<T>>, OptionAsRefMut<T>>;
pub type OptionAsRef<T> = for<'r> fn(&'r Option<T>) -> Option<&'r T>;
pub type OptionAsRefMut<T> = for<'r> fn(&'r mut Option<T>) -> Option<&'r mut T>;

impl<T> OptVec<T> {
    /// Constructs a new, empty `Vec<T>`. It will not allocate until elements
    /// are pushed onto it.
    pub fn new() -> Self {
        let vec  = default();
        let free = default();
        Self { vec, free }
    }

    pub fn insert(&mut self, item: T) -> Ix {
        self.insert_with_ix(|_| item)
    }

    pub fn iter(&self) -> Iter<T> {
        self.vec.iter().filter_map(Option::as_ref)
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.vec.iter_mut().filter_map(Option::as_mut)
    }

    /// Finds a free index and inserts the element. The index is re-used in case
    /// the array is sparse or is added in case of no free places.
    pub fn insert_with_ix<F: FnOnce(Ix) -> T>(&mut self, f: F) -> Ix {
        match self.free.pop() {
            None => {
                let ix = self.vec.len();
                self.vec.push(Some(f(ix)));
                ix
            }
            Some(ix) => {
                self.vec[ix] = Some(f(ix));
                ix
            }
        }
    }

    /// Removes the element at provided index and marks the index to be reused.
    /// Does nothing if the index was already empty. Panics if the index was out
    /// of bounds.
    pub fn remove(&mut self, ix: Ix) -> Option<T> {
        let item = self.vec[ix].take();
        item.iter().for_each(|_| self.free.push(ix));
        item
    }
}

impl<T> Index<usize> for OptVec<T> {
    type Output = T;
    fn index(&self, ix: usize) -> &Self::Output {
        self.vec.index(ix).as_ref().unwrap()
    }
}

impl<T> IndexMut<usize> for OptVec<T> {
    fn index_mut(&mut self, ix: usize) -> &mut Self::Output {
        self.vec.index_mut(ix).as_mut().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let mut v = OptVec::new();
        let ix1 = v.insert(1);
        let ix2 = v.insert(2);
        v.remove(ix1);
        let ix3 = v.insert(3);
        let ix4 = v.insert(4);
        assert_eq!(ix1, 0);
        assert_eq!(ix2, 1);
        assert_eq!(ix3, 0);
        assert_eq!(ix4, 2);
    }
}
