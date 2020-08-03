//! This file contains an implementation of Vec that can't be empty.



// ===================
// === NonEmptyVec ===
// ===================

// TODO [AA] Decent selection of `Vec` methods.

/// A version of [`std::vec::Vec`] that can't be empty.
#[allow(missing_docs)]
#[derive(Clone,Debug,PartialEq)]
pub struct NonEmptyVec<T> {
    first: T,
    rest: Vec<T>
}

impl<T> NonEmptyVec<T> {
    /// Construct a new non-empty vector.
    ///
    /// The vector will not allocate more than the space required to contain `first` and `rest`.
    ///
    /// # Examples
    ///
    /// ```
    /// #![allow(unused_mut)]
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec: NonEmptyVec<usize> = NonEmptyVec::new(0,vec![]);
    /// ```
    pub fn new(first:T, rest:Vec<T>) -> NonEmptyVec<T> {
        NonEmptyVec{first,rest}
    }

    /// Construct a new, `NonEmptyVec<T>` containing the provided element and with the provided
    /// `capacity`.
    ///
    /// If `capacity` is 0, then the vector will be allocated with capacity for the provided `first`
    /// element. The vector will be able to hold exactly `capacity` elements without reallocating.
    ///
    /// It is important to note that although the returned vector has the *capacity* specified, the
    /// vector will have a length of 1.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec = NonEmptyVec::with_capacity(0, 10);
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
    pub fn with_capacity(first:T, capacity:usize) -> NonEmptyVec<T> {
        let rest = Vec::with_capacity(capacity-1);
        NonEmptyVec{first,rest}
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
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0,vec![]);
    /// vec.reserve(10);
    /// assert!(vec.capacity() >= 11);
    /// ```
    pub fn reserve(&mut self, additional:usize) {
        self.rest.reserve(additional);
    }

    /// Return the number of elements the vector can hold without reallocating.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let vec: NonEmptyVec<usize> = NonEmptyVec::with_capacity(0, 10);
    /// assert_eq!(vec.capacity(), 10);
    /// ```
    pub fn capacity(&self) -> usize {
        self.rest.capacity() + 1
    }

    /// Return the number of elements in the vector, also referred to as its 'length'.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let a = NonEmptyVec::new(0,vec![1,2,3]);
    /// assert_eq!(a.len(), 4);
    /// ```
    pub fn len(&self) -> usize {
        1 + self.rest.len()
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
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0,vec![1,2]);
    /// vec.push(3);
    /// assert_eq!(vec.len(),4);
    /// ```
    pub fn push(&mut self,value:T) {
        self.rest.push(value)
    }

    /// Obtain an immutable reference to the element in the vector at the specified `index`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let vec = NonEmptyVec::new(0,vec![1,2]);
    ///
    /// let item_at_0 = vec.get(0);
    /// assert!(item_at_0.is_some());
    /// assert_eq!(*item_at_0.unwrap(),0);
    ///
    /// let item_at_2 = vec.get(2);
    /// assert!(item_at_2.is_some());
    /// assert_eq!(*item_at_2.unwrap(),2);
    ///
    /// let item_at_10 = vec.get(10);
    /// assert!(item_at_10.is_none());
    /// ```
    pub fn get(&self, index:usize) -> Option<&T> {
        if index == 0 {
            Some(&self.first)
        } else {
            self.rest.get(index - 1)
        }
    }

    /// Obtain a mutable reference to teh element in the vector at the specified `index`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec   = NonEmptyVec::new(0,vec![1,2]);
    /// let reference = vec.get_mut(0);
    /// assert!(reference.is_some());
    /// assert_eq!(*reference.unwrap(),0);
    /// ```
    pub fn get_mut(&mut self, index:usize) -> Option<&mut T> {
        if index == 0 {
            Some(&mut self.first)
        } else {
            self.rest.get_mut(index - 1)
        }
    }

    /// Obtain an immutable reference to the head of the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let vec = NonEmptyVec::new(0,vec![1,2]);
    /// assert_eq!(*vec.head(), 0);
    /// ```
    pub fn head(&self) -> &T {
        &self.first
    }

    /// Obtain a mutable reference to the head of the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0,vec![1,2]);
    /// assert_eq!(*vec.head_mut(), 0);
    /// ```
    pub fn head_mut(&mut self) -> &mut T {
        &mut self.first
    }

    /// Obtain an immutable slice of the tail of the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let vec = NonEmptyVec::new(0,vec![1,2]);
    /// assert_eq!(vec.tail(),[1,2])
    /// ```
    pub fn tail(&self) -> &[T] {
        self.rest.as_slice()
    }

    /// Obtain a mutable slice of the tail of the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0,vec![1,2]);
    /// assert_eq!(vec.tail_mut(),[1,2])
    /// ```
    pub fn tail_mut(&mut self) -> &mut [T] {
        self.rest.as_mut_slice()
    }

    /// Obtain an immutable reference to the last element in the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let vec = NonEmptyVec::new(0,vec![1,2]);
    /// assert_eq!(*vec.last(),2)
    /// ```
    pub fn last(&self) -> &T {
        self.get(self.len() - 1).expect("There is always one element in a NonEmptyVec.")
    }

    /// Obtain a mutable reference to the last element in the `NonEmptyVec`.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0,vec![1,2]);
    /// assert_eq!(*vec.last_mut(),2)
    /// ```
    pub fn last_mut(&mut self) -> &mut T {
        self.get_mut(self.len() - 1).expect("There is always one element in a NonEmptyVec.")
    }

    /// Set the element at `index` to `value`.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use flexer::data::non_empty_vec::NonEmptyVec;
    /// let mut vec = NonEmptyVec::new(0,vec![1,2]);
    /// vec.set(0,10);
    ///
    /// let elem_0 = vec.get(0);
    /// assert!(elem_0.is_some());
    /// assert_eq!(*elem_0.unwrap(),10)
    /// ```
    pub fn set(&mut self, index:usize, value:T) {
        if index == 0 {
            self.first = value
        } else {
            self.rest[index - 1] = value
        }
    }

    /// Return an iterator over the `NonEmptyVec`.
    pub fn iter(&self) -> Iter<T> {
        Iter::new(self)
    }
}


// =================
// === Iterators ===
// =================

#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub struct Iter<'a, T> {
    index: usize,
    non_empty_vec: &'a NonEmptyVec<T>
}

impl<T> Iter<'_, T> {
    /// Construct a new iterator.
    pub fn new(non_empty_vec: &NonEmptyVec<T>) -> Iter<T> {
        let index = 0;
        Iter{index,non_empty_vec}
    }
}

#[allow(missing_docs)]
#[derive(Debug)]
pub struct IterMut<'a, T> {
    index: usize,
    non_empty_vec: &'a mut NonEmptyVec<T>
}


// === Trait Impls ===

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if self.index == 0 {
            Some(&self.non_empty_vec.first)
        } else {
            self.non_empty_vec.rest.get(self.index - 1)
        };

        self.index += 1;
        result
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if self.index == 0 {
            Some(&mut self.non_empty_vec.first)
        } else {
            self.non_empty_vec.rest.get_mut(self.index - 1)
        };

        self.index += 1;
        result
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {

}
