// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::prelude::*;

use crate::data::function::traits::FnMut0;
use crate::data::function::traits::FnMut1;



// ==================
// === Observable ===
// ==================

/// Wrapper for array-like type. It allows attaching callbacks which fire when the underlying
/// structure changes.
#[derive(Deref)]
#[derive(Derivative)]
#[derivative(Clone, Debug(bound = "T:Debug"))]
pub struct Observable<T, OnMut, OnResize> {
    #[deref]
    pub data:      T,
    #[derivative(Debug = "ignore")]
    pub on_mut:    OnMut,
    #[derivative(Debug = "ignore")]
    pub on_resize: OnResize,
}

impl<T: Default, OnMut, OnResize> Observable<T, OnMut, OnResize> {
    pub fn new(on_mut: OnMut, on_resize: OnResize) -> Self {
        let data = default();
        Self { data, on_mut, on_resize }
    }
}

impl<T, OnMut, OnResize: FnMut0> Observable<Vec<T>, OnMut, OnResize> {
    #[inline]
    pub fn splice<R, I>(
        &mut self,
        range: R,
        replace_with: I,
    ) -> std::vec::Splice<'_, <I as IntoIterator>::IntoIter>
    where
        R: RangeBounds<usize>,
        I: IntoIterator<Item = T>,
    {
        self.on_resize.call();
        self.data.splice(range, replace_with)
    }

    #[inline]
    pub fn retain<F>(&mut self, f: F)
    where F: FnMut(&T) -> bool {
        self.on_resize.call();
        self.data.retain(f)
    }
}

impl<T: Index<Ix>, OnMut, OnResize, Ix> Index<Ix> for Observable<T, OnMut, OnResize> {
    type Output = <T as Index<Ix>>::Output;
    #[inline]
    fn index(&self, index: Ix) -> &Self::Output {
        &self.data[index]
    }
}

impl<T: IndexMut<Ix>, OnMut: FnMut1<Ix>, OnResize, Ix: Copy> IndexMut<Ix>
    for Observable<T, OnMut, OnResize>
{
    #[inline]
    fn index_mut(&mut self, index: Ix) -> &mut Self::Output {
        self.on_mut.call(index);
        &mut self.data[index]
    }
}

impl<T: Extend<S>, S, OnMut, OnResize: FnMut0> Extend<S> for Observable<T, OnMut, OnResize> {
    #[inline]
    fn extend<I: IntoIterator<Item = S>>(&mut self, iter: I) {
        self.on_resize.call();
        self.data.extend(iter)
    }
}
