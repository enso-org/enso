use crate::prelude::*;
use crate::data::function::callback::{Callback0,Callback1};


// ==================
// === Observable ===
// ==================

/// Wrapper for array-like type. It allows attaching callbacks which fire when
/// the underlying structure changes.

#[derive(Shrinkwrap)]
#[derive(Derivative)]
#[derivative(Debug(bound="T:Debug"))]
pub struct Observable<T,OnSet,OnResize> {
    #[shrinkwrap(main_field)]
    pub data: T,
    #[derivative(Debug="ignore")]
    pub on_set: OnSet,
    #[derivative(Debug="ignore")]
    pub on_resize: OnResize,
}

impl<T,OnSet,OnResize>
Observable<T,OnSet,OnResize> {
    pub fn new_from(data: T, on_set: OnSet, on_resize: OnResize) -> Self {
        Self { data, on_set, on_resize }
    }
}

impl<T:Default,OnSet,OnResize>
Observable<T,OnSet,OnResize> {
    pub fn new(on_set:OnSet, on_resize:OnResize) -> Self {
        Self::new_from(default(), on_set, on_resize)
    }
}

impl<T:Index<Ix>,OnSet,OnResize,Ix>
Index<Ix> for Observable<T,OnSet,OnResize> {
    type Output = <T as Index<Ix>>::Output;
    #[inline]
    fn index(&self, index:Ix) -> &Self::Output {
        &self.data[index]
    }
}

impl<T:IndexMut<Ix>,OnSet:Callback1<Ix>,OnResize,Ix:Copy>
IndexMut<Ix> for Observable<T,OnSet,OnResize> {
    #[inline]
    fn index_mut(&mut self, index:Ix) -> &mut Self::Output {
        self.on_set.call(index);
        &mut self.data[index]
    }
}

impl <T:Extend<S>,S,OnSet,OnResize:Callback0>
Extend<S> for Observable<T,OnSet,OnResize> {
    #[inline]
    fn extend<I: IntoIterator<Item=S>>(&mut self, iter:I) {
        self.on_resize.call();
        self.data.extend(iter)
    }
}