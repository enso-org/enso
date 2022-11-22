//! This file contains an implementation of Vec that can't be empty.

use crate::*;

use crate::data::vec_indexed_by;
use crate::VecIndexedBy;

use std::ops::Bound;
use std::vec::Drain;



// ===================
// === NonEmptyVec ===
// ===================

/// A version of [`std::vec::Vec`] that can't be empty.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Deref, DerefMut, Reflect)]
#[reflect(transparent)]
#[cfg_attr(feature = "serde", derive(crate::serde_reexports::Serialize))]
#[cfg_attr(feature = "serde", derive(crate::serde_reexports::Deserialize))]
pub struct NonEmptyVec<T, I = usize> {
    #[reflect(as = "Vec<T>")]
    pub elems: VecIndexedBy<T, I>,
}

impl<T, I> NonEmptyVec<T, I> {
    /// Construct a new non-empty vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #![allow(unused_mut)]
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec: NonEmptyVec<usize> = NonEmptyVec::new(0, vec![]);
    /// ```
    pub fn new(first: T, rest: Vec<T>) -> NonEmptyVec<T, I> {
        let mut elems = VecIndexedBy::with_capacity(1 + rest.len());
        elems.push(first);
        elems.extend(rest);
        NonEmptyVec { elems }
    }

    /// Construct a new non-empty vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #![allow(unused_mut)]
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec: NonEmptyVec<usize> = NonEmptyVec::new_with_last(vec![], 0);
    /// ```
    pub fn new_with_last(mut elems: Vec<T>, last: T) -> NonEmptyVec<T, I> {
        elems.push(last);
        NonEmptyVec { elems: elems.into() }
    }

    /// Length of the vector.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.elems.len()
    }

    /// Return the last valid index.
    pub fn last_valid_index(&self) -> I
    where I: From<usize> {
        (self.len() - 1).into()
    }

    /// Construct a `NonEmptyVec` containing a single element.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let vec = NonEmptyVec::singleton(0);
    /// assert_eq!(vec.get(0), Some(&0));
    /// assert_eq!(vec.len(), 1);
    /// ```
    pub fn singleton(first: T) -> NonEmptyVec<T, I> {
        let elems = vec![first];
        Self { elems: elems.into() }
    }

    /// Construct a new, `NonEmptyVec<T, I>` containing the provided element and with the
    /// provided `capacity`.
    ///
    /// If `capacity` is 0, then the vector will be allocated with capacity for the provided `first`
    /// element. The vector will be able to hold exactly `capacity` elements without reallocating.
    ///
    /// It is important to note that although the returned vector has the *capacity* specified, the
    /// vector will have a length of 1.
    ///
    /// # Panics
    ///
    /// Panics if `capacity` is not > 0.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::<_, usize>::with_capacity(0, 10);
    ///
    /// // The vector contains one item, even though it has capacity for more
    /// assert_eq!(vec.len(), 1);
    ///
    /// // These are all done without reallocating...
    /// for i in 1..10 {
    ///     vec.push(i);
    /// }
    ///
    /// // ...but this may make the vector reallocate
    /// vec.push(11);
    /// ```
    pub fn with_capacity(first: T, capacity: usize) -> NonEmptyVec<T, I> {
        debug_assert_ne!(capacity, 0, "Capacity must be greater than zero for a NonEmptyVec.");
        let mut elems = VecIndexedBy::with_capacity(capacity);
        elems.push(first);
        NonEmptyVec { elems }
    }

    /// Reserve capacity for at least `additional` more elements to be inserted in the given
    /// `Vec<T>`.
    ///
    /// The collection may reserve more space to avoid frequent reallocations. After calling
    /// `reserve`, capacity will be greater than or equal to `self.len() + additional`. Does nothing
    /// if capacity is already sufficient.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::<_, usize>::new(0, vec![]);
    /// vec.reserve(10);
    /// assert!(vec.capacity() >= 11);
    /// ```
    pub fn reserve(&mut self, additional: usize) {
        self.elems.reserve(additional);
    }

    /// Shrinks the capacity of the `NonEmotyVec` as much as possible.
    ///
    /// It will drop down as close as possible to the length, but the allocator may still inform the
    /// vector that there is space for a few more elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::<_, usize>::with_capacity(0, 10);
    /// assert_eq!(vec.capacity(), 10);
    /// vec.shrink_to_fit();
    /// assert!(vec.capacity() < 10);
    /// ```
    pub fn shrink_to_fit(&mut self) {
        self.elems.shrink_to_fit();
    }

    /// Append an element to the back of a collection.
    ///
    /// # Panics
    ///
    /// Panics if the number of elements in the vector overflows a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::<_, usize>::new(0, vec![1, 2]);
    /// vec.push(3);
    /// assert_eq!(vec.len(), 4);
    /// ```
    pub fn push(&mut self, value: T) {
        self.elems.push(value)
    }

    /// Remove an element from the back of the collection, returning it.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::<_, usize>::new(0, vec![1]);
    /// assert!(vec.pop_if_has_more_than_1_elem().is_some());
    /// assert!(vec.pop_if_has_more_than_1_elem().is_none());
    /// assert_eq!(vec.len(), 1);
    /// ```
    pub fn pop_if_has_more_than_1_elem(&mut self) -> Option<T> {
        (self.len() > 1).and_option_from(|| self.elems.pop())
    }

    /// Remove an element from the back of the collection, returning it and a new possibly empty
    /// vector.
    pub fn pop(mut self) -> (T, VecIndexedBy<T, I>) {
        let first = self.elems.pop().unwrap();
        (first, self.elems)
    }

    /// Obtain an immutable reference to the head of the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let vec = NonEmptyVec::<_, usize>::new(0, vec![1, 2]);
    /// assert_eq!(*vec.first(), 0);
    /// ```
    pub fn first(&self) -> &T {
        self.elems.first().unwrap_or_else(|| unreachable!())
    }

    /// Obtain a mutable reference to the head of the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::<_, usize>::new(0, vec![1, 2]);
    /// assert_eq!(*vec.first_mut(), 0);
    /// ```
    pub fn first_mut(&mut self) -> &mut T {
        self.elems.first_mut().unwrap_or_else(|| unreachable!())
    }

    /// Obtain an immutable reference to the last element in the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let vec = NonEmptyVec::<_, usize>::new(0, vec![1, 2]);
    /// assert_eq!(*vec.last(), 2)
    /// ```
    pub fn last(&self) -> &T {
        self.elems.last().unwrap_or_else(|| unreachable!())
    }

    /// Obtain a mutable reference to the last element in the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::<_, usize>::new(0, vec![1, 2]);
    /// assert_eq!(*vec.last_mut(), 2)
    /// ```
    pub fn last_mut(&mut self) -> &mut T {
        self.elems.last_mut().unwrap_or_else(|| unreachable!())
    }

    /// Convert this non-empty vector to vector.
    pub fn into_vec(self) -> Vec<T> {
        self.elems.into()
    }

    /// Consume this non-empty vector, map each element with a function, and produce a new one.
    pub fn mapped<S>(self, f: impl FnMut(T) -> S) -> NonEmptyVec<S> {
        let elems = self.elems.into_iter().map(f).collect();
        NonEmptyVec { elems }
    }
}

impl<T, I> NonEmptyVec<T, I>
where I: vec_indexed_by::Index
{
    /// Obtain a mutable reference to the element in the vector at the specified `index`.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0, vec![1, 2]);
    /// let reference = vec.get_mut(0);
    /// assert!(reference.is_some());
    /// assert_eq!(*reference.unwrap(), 0);
    /// ```
    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.elems.get_mut(index)
    }

    /// Get the tail reference.
    pub fn tail(&mut self) -> &[T]
    where I: From<u8> {
        &self.elems[I::from(1_u8)..]
    }

    /// Get the mutable tail reference.
    pub fn tail_mut(&mut self) -> &mut [T]
    where I: From<u8> {
        &mut self.elems[I::from(1_u8)..]
    }

    /// Create a draining iterator that removes the specified range in the vector and yields the
    /// removed items.
    ///
    /// It will never remove the root element of the vector.
    ///
    /// # Panics
    ///
    /// Panics if the starting point is greater than the end point or if the end point is greater
    /// than the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use enso_prelude::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0, vec![1, 2, 3, 4, 5]);
    /// let drained: Vec<i32> = vec.drain(1..=5).collect();
    /// assert_eq!(drained, [1, 2, 3, 4, 5])
    /// ```
    pub fn drain<R>(&mut self, range: R) -> Drain<T>
    where
        R: RangeBounds<I>,
        I: PartialOrd + Copy + From<u8>, {
        if range.contains(&I::from(0_u8)) {
            match range.end_bound() {
                Bound::Included(n) => self.elems.drain(I::from(1_u8)..=*n),
                Bound::Excluded(n) => self.elems.drain(I::from(1_u8)..*n),
                Bound::Unbounded => self.elems.drain(I::from(1_u8)..),
            }
        } else {
            self.elems.drain(range)
        }
    }
}


// === Trait Impls ===

impl<T: Default, I> Default for NonEmptyVec<T, I> {
    fn default() -> Self {
        Self::singleton(default())
    }
}

impl<T, I> TryFrom<Vec<T>> for NonEmptyVec<T, I> {
    type Error = ();
    fn try_from(elems: Vec<T>) -> Result<Self, Self::Error> {
        (!elems.is_empty()).as_result_from(|| NonEmptyVec { elems: elems.into() }, || ())
    }
}

impl<T, I> From<NonEmptyVec<T, I>> for Vec<T> {
    fn from(v: NonEmptyVec<T, I>) -> Self {
        v.elems.into()
    }
}

impl<T, I> IntoIterator for NonEmptyVec<T, I> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.elems.into_iter()
    }
}

impl<'a, T, I> IntoIterator for &'a NonEmptyVec<T, I> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.elems.iter()
    }
}

impl<'a, T, I> IntoIterator for &'a mut NonEmptyVec<T, I> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.elems.iter_mut()
    }
}
