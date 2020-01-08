#![allow(missing_docs)]

use crate::prelude::*;
use crate::data::opt_vec::*;

type Index = usize;



// =============
// === Scene ===
// =============

/// A collection for holding 3D `Object`s.
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
pub struct Scene<T> {
    objects : OptVec<T>
}

impl<T> Scene<T> {
    /// Searches for a HtmlElement identified by id and appends to it.
    pub fn new() -> Self { default() }

    /// Moves a HTMLObject to the Scene and returns an index to it.
    pub fn add(&mut self, object: T) -> Index {
        self.objects.insert(object)
    }

    /// Removes and retrieves a HTMLObject based on the index provided by
    pub fn remove(&mut self, index: usize) -> Option<T> {
        self.objects.remove(index)
    }

    /// Returns the number of `Object`s in the Scene,
    /// also referred to as its 'length'.
    pub fn len(&self) -> usize {
        self.objects.len()
    }

    /// Returns true if the Scene contains no `Object`s.
    pub fn is_empty(&self) -> bool {
        self.objects.is_empty()
    }

    /// Gets mutable iterator.
    pub fn iter_mut(&mut self) -> IterMut<'_, T> { self.objects.iter_mut() }

    /// Gets iterator.
    pub fn iter(&self) -> Iter<'_, T> { self.objects.iter() }
}

impl<'a, T> IntoIterator for &'a Scene<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Scene<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>  ;
    fn into_iter(self) -> Self::IntoIter { self.iter_mut() }
}
