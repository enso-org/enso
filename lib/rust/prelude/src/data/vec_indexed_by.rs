use crate::*;

use core::marker::PhantomData;
use std::alloc::Allocator;
use std::ops::Bound;



// =====================
// === Helper macros ===
// =====================

macro_rules! ranged_fn {
    ($name:ident $([$($parm:tt)*])? ($($arg:ident : $arg_tp:ty),* ) -> $out:ty ) => {
        pub fn $name $(<$($parm)*>)?
        (&mut self, range: impl RangeBounds<I>  $(,$arg:$arg_tp)* ) -> $out {
            let map_bound = |bound| match bound {
                Bound::<&I>::Included(t) => Bound::Included((*t).into()),
                Bound::<&I>::Excluded(t) => Bound::Excluded((*t).into()),
                Bound::<&I>::Unbounded => Bound::Unbounded,
            };
            let start = map_bound(range.start_bound());
            let end = map_bound(range.end_bound());
            self.vec.$name((start, end) $(,$arg)*)
        }
    };
}



// ====================
// === VecIndexedBy ===
// ====================

pub trait Index = Copy + From<usize> + Into<usize>;

#[derive(crate::serde_reexports::Serialize)]
#[derive(crate::serde_reexports::Deserialize)]
#[derive(Derivative, Deref, DerefMut, From, Into)]
#[derivative(Clone(bound = "T: Clone, A: Allocator + Clone"))]
#[derivative(Debug(bound = "T: Debug, A: Allocator"))]
#[derivative(Default(bound = "A: Allocator, Vec<T, A>: Default"))]
#[derivative(PartialEq(bound = "Vec<T, A>: PartialEq"))]
#[derivative(Eq(bound = "Vec<T, A>: PartialEq"))]
pub struct VecIndexedBy<T, I = usize, A: Allocator = std::alloc::Global> {
    #[serde(bound(
        serialize = "Vec<T, A>: crate::serde_reexports::Serialize",
        deserialize = "Vec<T, A>: crate::serde_reexports::Deserialize<'de>"
    ))]
    #[deref]
    #[deref_mut]
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
    /// Return the last valid index, if any.
    pub fn last_valid_index(&self) -> Option<I>
    where I: From<usize> {
        if self.vec.is_empty() {
            None
        } else {
            Some((self.len() - 1).into())
        }
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

    pub fn remove(&mut self, index: I) -> T {
        self.vec.remove(index.into())
    }

    ranged_fn! {drain() -> std::vec::Drain<'_, T, A>}
    ranged_fn! {splice[Iter: IntoIterator<Item = T>](replace_with: Iter) -> std::vec::Splice<'_, Iter::IntoIter, A>}
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
