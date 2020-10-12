//! A sparse vector implementation.

use crate::prelude::*;
use std::iter::FilterMap;
use std::slice;



// ==============
// === OptVec ===
// ==============

// === Definition ===

/// A contiguous growable sparse array type. Similar to `Vec<T>`, but allowing missing values.
/// After a value is removed, it remembers the index for reuse in the future. Unlike `Vec`, it is
/// parametrized with optional `Index` type variable which will be used for indexing the vector.
/// Index have to implement the `Index` trait.
#[derive(Derivative)]
#[derivative(Default(bound=""))]
#[derive(Clone,Debug,Shrinkwrap)]
pub struct OptVec<T,Index=usize> {
    #[shrinkwrap(main_field)]
    items    : Vec<Option<T>>,
    free_ixs : SmallVec<[Index; 128]>,
}


// === Types ===

/// A trait for any vector index type.
pub trait Index = Debug + Copy + Into<usize> where usize : Into<Self>;

/// Iterator type of this vector.
pub type Iter<'t,T> = FilterMap<slice::Iter<'t,Option<T>>, OptionAsRef<T>>;

/// Mutable iterator type of this vector.
pub type IterMut<'t,T> = FilterMap<slice::IterMut<'t, Option<T>>, OptionAsRefMut<T>>;

/// Subtype of `Iter`.
pub type OptionAsRef    <T> = for<'r> fn(&'r Option<T>) -> Option<&'r T>;

/// Subtype of `IterMut`.
pub type OptionAsRefMut <T> = for<'r> fn(&'r mut Option<T>) -> Option<&'r mut T>;


// === Construction ===

impl<T,I:Index> OptVec<T,I> {
    /// Constructs a new, empty `Vec<T>`. It will not allocate until elements are pushed onto it.
    pub fn new() -> Self {
        default()
    }
}


// === Status Checks ===

impl<T,I:Index> OptVec<T,I> {
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


// === Modifiers ===

impl<T,I:Index> OptVec<T,I> {
    /// Inserts the provided element to the vector. It reuses free indexes if any.
    pub fn insert(&mut self, item: T) -> I {
        self.insert_with_ix(|_| item)
    }

    /// Finds a free index and inserts the element. The index is re-used in case the array is sparse
    /// or is added in case of no free places.
    pub fn insert_with_ix<F:FnOnce(I) -> T>(&mut self, f: F) -> I {
        match self.free_ixs.pop() {
            None => {
                let index = self.items.len().into();
                self.items.push(Some(f(index)));
                index
            }
            Some(index) => {
                self.items[index.into()] = Some(f(index));
                index
            }
        }
    }

    /// Reserve an index for further reuse. Please remember that you cannot use the index to read
    /// values unless the value is set.
    pub fn reserve_index(&mut self) -> I {
        self.free_ixs.pop().unwrap_or_else(|| {
            let index = self.items.len().into();
            self.items.push(None);
            index
        })
    }

    /// Sets the value at given index. Panics if the index was already freed.
    pub fn set(&mut self, index:I, t:T) {
        self.items[index.into()] = Some(t);
    }

    /// Removes the element at provided index and marks the index to be reused. Does nothing if the
    /// index was already empty. Panics if the index was out of bounds.
    pub fn remove(&mut self, index:I) -> Option<T> {
        let item = self.items[index.into()].take();
        item.iter().for_each(|_| self.free_ixs.push(index));
        item
    }
}


// === Indexing ===

impl<T,I:Index> OptVec<T,I> {
    /// Index into vector. Returns `None` if the key was already freed.
    pub fn safe_index(&self, index:I) -> Option<&T> {
        self.items[index.into()].as_ref()
    }

    /// Index into vector. Returns `None` if the key was already freed.
    pub fn safe_index_mut(&mut self, index:I) -> Option<&mut T> {
        self.items[index.into()].as_mut()
    }
}

impl<T,I:Index> std::ops::Index<I> for OptVec<T,I> {
    type Output = T;
    fn index(&self, index:I) -> &Self::Output {
        let error = || panic!(format!("Trying to access removed index `{:?}`.",index));
        self.items.index(index.into()).as_ref().unwrap_or_else(error)
    }
}

impl<T,I:Index> std::ops::IndexMut<I> for OptVec<T,I> {
    fn index_mut(&mut self, index:I) -> &mut Self::Output {
        let error = || panic!(format!("Trying to access removed index `{:?}`.",index));
        self.items.index_mut(index.into()).as_mut().unwrap_or_else(error)
    }
}


// === Iterators ===

impl<T,I:Index> OptVec<T,I> {
    /// Iterator.
    pub fn iter(&self) -> Iter<T> {
        self.items.iter().filter_map(Option::as_ref)
    }

    /// Mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.items.iter_mut().filter_map(Option::as_mut)
    }
}

impl<'a,T,I:Index> IntoIterator for &'a OptVec<T,I> {
    type Item     = &'a T;
    type IntoIter = Iter<'a,T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a,T,I:Index> IntoIterator for &'a mut OptVec<T,I> {
    type Item     = &'a mut T;
    type IntoIter = IterMut<'a,T>;
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
        let mut v = OptVec::<usize>::new();
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
        let mut v = OptVec::<usize>::new();

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
        let mut v = OptVec::<usize>::new();

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
