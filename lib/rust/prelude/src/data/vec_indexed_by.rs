use crate::*;

use core::marker::PhantomData;
use std::alloc::Allocator;
use std::ops::Bound;



// =====================
// === Helper macros ===
// =====================

macro_rules! ranged_fn {
    ($name:ident $([$($parm:tt)*])? ($($arg:ident : $arg_tp:ty),* ) -> $out:ty ) => {
        pub fn $name $(<$($parm)*>)? (&mut self, range: impl RangeBounds<I>  $(,$arg:$arg_tp)* ) -> $out
        where I: Copy + From<usize> {
            let start = range.start_bound();
            let end = range.end_bound();
            match start {
                Bound::Included(t) => match end {
                    Bound::Included(u) => self.vec.$name((*t).into()..=(*u).into() $(,$arg)* ),
                    Bound::Excluded(u) => self.vec.$name((*t).into()..(*u).into() $(,$arg)*),
                    Bound::Unbounded => self.vec.$name((*t).into().. $(,$arg)*),
                },
                Bound::Excluded(t) => {
                    let t: usize = (*t).into();
                    match end {
                        Bound::Included(u) => self.vec.$name(t + 1..=(*u).into() $(,$arg)*),
                        Bound::Excluded(u) => self.vec.$name(t + 1..(*u).into() $(,$arg)*),
                        Bound::Unbounded => self.vec.$name(t + 1.. $(,$arg)*),
                    }
                }
                Bound::Unbounded => match end {
                    Bound::Included(u) => self.vec.$name(..=(*u).into() $(,$arg)*),
                    Bound::Excluded(u) => self.vec.$name(..(*u).into() $(,$arg)*),
                    Bound::Unbounded => self.vec.$name(.. $(,$arg)*),
                },
            }
        }
    };
}



// ====================
// === VecIndexedBy ===
// ====================

pub trait Index = Copy + From<usize> + Into<usize>;

#[cfg_attr(feature = "serde", derive(crate::serde_reexports::Serialize))]
#[cfg_attr(feature = "serde", derive(crate::serde_reexports::Deserialize))]
pub struct VecIndexedBy<T, I = usize, A: Allocator = std::alloc::Global> {
    #[cfg_attr(
        feature = "serde",
        serde(bound(
            serialize = "Vec<T, A>: crate::serde_reexports::Serialize",
            deserialize = "Vec<T, A>: crate::serde_reexports::Deserialize<'de>"
        ))
    )]
    vec: Vec<T, A>,
    key: PhantomData<I>,
}

impl<T, I> VecIndexedBy<T, I> {
    pub fn with_capacity(capacity: usize) -> Self {
        Vec::with_capacity(capacity).into()
    }
}

impl<T, I, A> VecIndexedBy<T, I, A>
where A: Allocator
{
    /// Length of the vector.
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Return the last valid index, if any.
    pub fn last_valid_index(&self) -> Option<I>
    where I: From<usize> {
        if self.vec.is_empty() {
            None
        } else {
            Some((self.len() - 1).into())
        }
    }

    /// Check if the vector is empty.
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// Returns the number of elements the vector can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.vec.capacity()
    }

    /// Push a new element to the vector.
    pub fn push(&mut self, value: T) {
        self.vec.push(value)
    }

    /// Extend the vector with new elements.
    pub fn extend<Iter: IntoIterator<Item = T>>(&mut self, iter: Iter) {
        self.vec.extend(iter)
    }

    /// Reserves capacity for at least additional more elements to be inserted in the given vector.
    pub fn reserve(&mut self, additional: usize) {
        self.vec.reserve(additional)
    }

    /// Shrinks the capacity of the vector as much as possible.
    pub fn shrink_to_fit(&mut self) {
        self.vec.shrink_to_fit()
    }

    /// Removes the last element from a vector and returns it, or [`None`] if it is empty.
    pub fn pop(&mut self) -> Option<T> {
        self.vec.pop()
    }

    /// Returns the first element of the slice, or [`None`] if it is empty.
    pub fn first(&self) -> Option<&T> {
        self.vec.first()
    }

    /// Returns the last element of the slice, or [`None`] if it is empty.
    pub fn last(&self) -> Option<&T> {
        self.vec.last()
    }

    /// Returns a mutable pointer to the first element of the slice, or [`None`] if it is empty.
    pub fn first_mut(&mut self) -> Option<&mut T> {
        self.vec.first_mut()
    }

    /// Returns a mutable pointer to the last element of the slice, or [`None`] if it is empty.
    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.vec.last_mut()
    }

    /// Returns an iterator over the slice.
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.vec.iter()
    }

    /// Returns a mutable iterator over the slice.
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, T> {
        self.vec.iter_mut()
    }

    /// Resizes the Vec in-place so that len is equal to new_len.
    pub fn resize_with(&mut self, new_len: usize, f: impl FnMut() -> T) {
        self.vec.resize_with(new_len, f)
    }

    /// Shortens the vector, keeping the first len elements and dropping the rest.
    pub fn truncate(&mut self, len: usize) {
        self.vec.truncate(len)
    }

    /// Extracts a slice containing the entire vector.
    pub fn as_slice(&self) -> &[T] {
        self.vec.as_slice()
    }
}



// ==============
// === Traits ===
// ==============

define_not_same_trait!();

impl<T, I, A> VecIndexedBy<T, I, A>
where
    A: Allocator,
    I: Index,
{
    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.vec.get_mut(index.into())
    }

    pub fn get(&self, index: I) -> Option<&T> {
        self.vec.get(index.into())
    }

    pub fn insert(&mut self, index: I, element: T) {
        self.vec.insert(index.into(), element)
    }

    ranged_fn! {drain() -> std::vec::Drain<'_, T, A>}
    ranged_fn! {splice[Iter: IntoIterator<Item = T>](replace_with: Iter) -> std::vec::Splice<'_, Iter::IntoIter, A>}
}

impl<T, I, A> Clone for VecIndexedBy<T, I, A>
where
    T: Clone,
    A: Allocator + Clone,
{
    fn clone(&self) -> Self {
        self.vec.clone().into()
    }
}

impl<T, I, A> Debug for VecIndexedBy<T, I, A>
where
    T: Debug,
    A: Allocator,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.vec, f)
    }
}

impl<T, I, A> Default for VecIndexedBy<T, I, A>
where
    A: Allocator,
    Vec<T, A>: Default,
{
    fn default() -> Self {
        let vec = default();
        Self { vec, key: PhantomData }
    }
}

impl<T, I, A: Allocator> Eq for VecIndexedBy<T, I, A> where Vec<T, A>: Eq {}
impl<T, I, A: Allocator> PartialEq for VecIndexedBy<T, I, A>
where Vec<T, A>: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.vec.eq(&other.vec)
    }
}

impl<T, I, A> From<Vec<T, A>> for VecIndexedBy<T, I, A>
where A: Allocator
{
    fn from(vec: Vec<T, A>) -> Self {
        Self { vec, key: default() }
    }
}

impl<T, I, A> From<VecIndexedBy<T, I, A>> for Vec<T, A>
where A: Allocator
{
    fn from(vec: VecIndexedBy<T, I, A>) -> Self {
        vec.vec
    }
}

impl<T, I, A> From<&Vec<T, A>> for VecIndexedBy<T, I, A>
where
    T: Clone,
    A: Allocator + Clone,
{
    fn from(vec: &Vec<T, A>) -> Self {
        Self { vec: vec.clone(), key: default() }
    }
}

impl<T, I, A> From<&VecIndexedBy<T, I, A>> for VecIndexedBy<T, I, A>
where
    T: Clone,
    A: Allocator + Clone,
{
    fn from(vec: &VecIndexedBy<T, I, A>) -> Self {
        vec.clone()
    }
}


impl<T, I, A> std::ops::Index<I> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    type Output = T;
    fn index(&self, index: I) -> &Self::Output {
        &self.vec[index.into()]
    }
}

impl<T, I, A> std::ops::Index<Range<I>> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    type Output = [T];
    fn index(&self, range: Range<I>) -> &Self::Output {
        &self.vec[range.start.into()..range.end.into()]
    }
}

impl<T, I, A> std::ops::Index<RangeFrom<I>> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    type Output = [T];
    fn index(&self, range: RangeFrom<I>) -> &Self::Output {
        &self.vec[range.start.into()..]
    }
}

impl<T, I, A> std::ops::Index<RangeTo<I>> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    type Output = [T];
    fn index(&self, range: RangeTo<I>) -> &Self::Output {
        &self.vec[..range.end.into()]
    }
}

impl<T, I, A> std::ops::Index<RangeFull> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
    (RangeFull, I): NotSame,
{
    type Output = [T];
    fn index(&self, _range: RangeFull) -> &Self::Output {
        &self.vec[..]
    }
}

impl<T, I, A> IndexMut<I> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.vec[index.into()]
    }
}

impl<T, I, A> IndexMut<Range<I>> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    fn index_mut(&mut self, range: Range<I>) -> &mut Self::Output {
        &mut self.vec[range.start.into()..range.end.into()]
    }
}

impl<T, I, A> IndexMut<RangeFrom<I>> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    fn index_mut(&mut self, range: RangeFrom<I>) -> &mut Self::Output {
        &mut self.vec[range.start.into()..]
    }
}

impl<T, I, A> IndexMut<RangeTo<I>> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
{
    fn index_mut(&mut self, range: RangeTo<I>) -> &mut Self::Output {
        &mut self.vec[..range.end.into()]
    }
}

impl<T, I, A> IndexMut<RangeFull> for VecIndexedBy<T, I, A>
where
    I: Index,
    A: Allocator,
    (RangeFull, I): NotSame,
{
    fn index_mut(&mut self, _range: RangeFull) -> &mut Self::Output {
        &mut self.vec[..]
    }
}

impl<T, I, A> IntoIterator for VecIndexedBy<T, I, A>
where A: Allocator
{
    type Item = T;
    type IntoIter = std::vec::IntoIter<T, A>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

impl<'a, T, I, A> IntoIterator for &'a VecIndexedBy<T, I, A>
where A: Allocator
{
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

impl<'a, T, I, A> IntoIterator for &'a mut VecIndexedBy<T, I, A>
where A: Allocator
{
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter_mut()
    }
}

impl<T, I> FromIterator<T> for VecIndexedBy<T, I> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> VecIndexedBy<T, I> {
        let vec = Vec::from_iter(iter);
        Self { vec, key: default() }
    }
}

impl<T, I> Deref for VecIndexedBy<T, I> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}
