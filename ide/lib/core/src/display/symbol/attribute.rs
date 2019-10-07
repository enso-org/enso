use std::ops::{Index, IndexMut};
use std::slice::SliceIndex;

// =====================
// === ObservableVec ===
// =====================

struct ObservableVec<T> {
    pub vec:       Vec<T>,
    pub on_change: fn(usize),
}

impl<T> ObservableVec<T> {}

impl<T, I: SliceIndex<[T]>> Index<I> for ObservableVec<T> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        &self.vec.index(index)
    }
}

impl<T> IndexMut<usize> for ObservableVec<T> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        (self.on_change)(index);
        self.vec.index_mut(index)
    }
}

// =================
// === Attribute ===
// =================

// struct Attribute {
//    data: ObservableVec,
//}
