use crate::prelude::*;
use std::cell::UnsafeCell;



pub mod preallocation {
    pub struct Default;
    pub struct Zeroed;
    pub struct Disabled;
}


// =================
// === LinkedVec ===
// =================

derive_zeroable! {
    /// [`Vec`]-like structure that:
    /// 1. Can be configured (the [`B`] parameter) to preallocate memory for items, allowing for
    ///    almost zero-cost push with [`Self::push_new`] if the preallocated memory is not fully
    ///    occupied. The preallocation can be performed either by initializing values with their
    ///    defaults, or by zeroing the memory.
    /// 2. Can be initialized with zeroed memory.
    /// 3. Allows pushing new elements without requiring mutable access to self. This is safe, as
    ///    it never re-allocates the underlying memory. In case there is not enough space, a new
    ///    memory segment will be allocated and linked to the previous segment.
    #[derive(Debug, Derivative)]
    #[derivative(Default(bound = ""))]
    pub struct LinkedArray
    [T, N, B][T, const N: usize, B][T, const N: usize, B = preallocation::Zeroed] {
        size:          Cell<usize>,
        first_segment: InitCell<ZeroableOption<Segment<T, N, B>>>,
    }
}

impl<T, const N: usize, B> LinkedArray<T, N, B>
where B: AllocationBehavior<T, N>
{
    /// Constructor.
    #[inline(always)]
    pub fn new() -> Self {
        Self::default()
    }

    /// Remove all items.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.size.set(0);
        if let Some(first_segment) = self.first_segment.opt_item_mut() {
            AllocationBehavior::clear_segments(first_segment);
        }
    }

    /// Number of items stored.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.size.get()
    }

    /// Check whether the container is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.size.get() == 0
    }

    /// Add an item at the end and return its index.
    #[inline(always)]
    pub fn push(&self, item: T) -> usize {
        let index = self.size.get();
        self.get_or_init_first_segment().push(index, item);
        self.size.modify(|t| *t += 1);
        index
    }

    /// Add a new item at the end and return its index. If the container uses preallocated memory,
    /// this operation is almost zero-cost.
    pub fn push_new(&self) -> usize {
        AllocationBehavior::push_new(self)
    }

    /// Return an iterator over the items.
    #[inline(always)]
    pub fn iter(&self) -> Iter<T, N, B> {
        IntoIterator::into_iter(self)
    }

    /// Consume the container and return an iterator over its items.
    #[inline(always)]
    pub fn into_iter(self) -> IntoIter<T, N, B> {
        IntoIterator::into_iter(self)
    }

    /// Clone all elements to a vector.
    pub fn to_vec(&self) -> Vec<T>
    where T: Clone {
        let mut vec = Vec::with_capacity(self.size.get());
        vec.extend(self.iter().cloned());
        vec
    }

    #[inline(always)]
    fn init_first_segment(&self) {
        self.first_segment.init_if_empty(|| AllocationBehavior::new_segment());
    }

    #[inline(always)]
    fn get_or_init_first_segment(&self) -> &Segment<T, N, B> {
        self.init_first_segment();
        self.first_segment.opt_item().unwrap()
    }

    #[inline(always)]
    fn get_or_init_first_segment_mut(&mut self) -> &mut Segment<T, N, B> {
        self.init_first_segment();
        self.first_segment.opt_item_mut().unwrap()
    }

    #[inline(always)]
    fn push_new_assume_initialized(&self) -> usize {
        let index = self.size.get();
        if index % N == 0 {
            self.get_or_init_first_segment().add_tail_segment();
        }
        self.size.modify(|t| *t += 1);
        index
    }
}

impl<T: Default, const N: usize, B> LinkedArray<T, N, B>
where B: AllocationBehavior<T, N>
{
    /// Retain only the elements that satisfy the predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where F: FnMut(&T) -> bool {
        let size = self.size.get();
        let first_segment = self.get_or_init_first_segment_mut();
        first_segment.retain_stage1(size, f);
        let new_size = first_segment.retain_stage2();
        self.size.set(new_size);
    }
}


// === Indexing ===

impl<T, const N: usize, B> Index<usize> for LinkedArray<T, N, B>
where B: AllocationBehavior<T, N>
{
    type Output = T;
    #[inline(always)]
    fn index(&self, offset: usize) -> &Self::Output {
        self.get_or_init_first_segment().index(offset)
    }
}

impl<T, const N: usize, B> IndexMut<usize> for LinkedArray<T, N, B>
where B: AllocationBehavior<T, N>
{
    #[inline(always)]
    fn index_mut(&mut self, offset: usize) -> &mut Self::Output {
        self.get_or_init_first_segment_mut().index_mut(offset)
    }
}



// ===============
// === Segment ===
// ===============

#[derive(Debug)]
pub struct Segment<T, const N: usize, B> {
    _allocation_behavior: PhantomData<B>,
    items:                Vec<UnsafeCell<T>>,
    next:                 InitCell<Option<Box<Segment<T, N, B>>>>,
}

impl<T, const N: usize, B> Default for Segment<T, N, B>
where B: AllocationBehavior<T, N>
{
    #[inline(always)]
    fn default() -> Self {
        AllocationBehavior::new_segment()
    }
}

impl<T, const N: usize, B> Segment<T, N, B> {
    /// Get a mutable reference to the next segment if it exists;
    #[inline(always)]
    fn items_and_next_mut(&mut self) -> (&mut Vec<UnsafeCell<T>>, Option<&mut Box<Self>>) {
        let next = self.next.opt_item_mut();
        let items = &mut self.items;
        (items, next)
    }
}

impl<T, const N: usize, B> Segment<T, N, B>
where B: AllocationBehavior<T, N>
{
    #[inline(always)]
    fn clear(&mut self) {
        unsafe { std::intrinsics::volatile_set_memory(self.items.as_mut_ptr(), 0, N) }
    }

    #[inline(always)]
    fn init_next_segment(&self) {
        self.next.init_if_empty(|| default());
    }

    #[inline(always)]
    fn push(&self, offset: usize, elem: T) {
        if offset < N {
            // # Safety
            // The item at `offset` either:
            // 1. Did not exist yet.
            // 2. Was removed, which required `self` to be mutably borrowed, so there are no other
            //    mutable borrows to the element.
            #[allow(unsafe_code)]
            unsafe {
                *self.items[offset].unchecked_borrow_mut() = elem
            };
        } else {
            self.init_next_segment();
            self.next.opt_item().unwrap().push(offset - N, elem)
        }
    }

    #[inline(always)]
    #[allow(unconditional_recursion)]
    fn add_tail_segment(&self) {
        self.init_next_segment();
        self.next.opt_item().unwrap().add_tail_segment()
    }
}

impl<T: Default, const N: usize, B> Segment<T, N, B>
where B: AllocationBehavior<T, N>
{
    /// For each segment, retain the items. Segment lengths will be shortened if necessary, however,
    /// two adjacent not-full segments will not be merged. Merging will be performed in stage 2.
    #[inline(always)]
    pub fn retain_stage1<F>(&mut self, len: usize, mut f: F)
    where F: FnMut(&T) -> bool {
        if len < N {
            // # Safety
            // We are shortening the vec length to the real size, skipping pre-allocated elements
            // at the end.
            #[allow(unsafe_code)]
            unsafe {
                self.items.set_len(len)
            }
        }
        // # Safety
        // All mutable borrows of an item require `self` to be mutably borrowed as well, so there
        // are no other mutable borrows currently. The only exception is [`Self::push`] which
        // mutably borrows the newly added item.
        #[allow(unsafe_code)]
        self.items.retain(|t| f(unsafe { t.unchecked_borrow() }));
        if let Some(next) = self.next.opt_item_mut() {
            next.retain_stage1(len - N, f);
        }
    }

    /// For each segment, merge two adjacent not-full segments.
    #[inline(always)]
    pub fn retain_stage2(&mut self) -> usize {
        while self.items.len() < N {
            let new_next_segment = {
                let (items, mut next_segment) = self.items_and_next_mut();
                next_segment.as_mut().and_then(|next| {
                    let end = next.items.len().min(N - items.len());
                    items.extend(next.items.drain(0..end));
                    let empty_next_segment = next.items.is_empty();
                    let next_next_segment = next.next.opt_item_mut();
                    empty_next_segment.then(|| next_next_segment.map(|t| mem::take(t)))
                })
            };
            if let Some(new_next_segment) = new_next_segment {
                self.next.set_value(new_next_segment)
            }
            if !self.next.has_item() {
                break;
            }
        }
        let len = self.items.len();
        for _ in len..N {
            // FIXME: can be optimized
            self.items.push(default());
        }
        len + self.next.opt_item_mut().map(|next| next.retain_stage2()).unwrap_or_default()
    }
}

impl<T, const N: usize, B> Index<usize> for Segment<T, N, B> {
    type Output = T;

    #[inline(always)]
    fn index(&self, offset: usize) -> &Self::Output {
        if offset < N {
            // # Safety
            // All mutable borrows of an item require `self` to be mutably borrowed as well, so
            // there are no other mutable borrows currently. The only exception is
            // [`Self::push`] which mutably borrows the newly added item.
            #[allow(unsafe_code)]
            unsafe {
                self.items[offset].unchecked_borrow()
            }
        } else {
            self.next.opt_item().unwrap().index(offset - N)
        }
    }
}

impl<T, const N: usize, B> IndexMut<usize> for Segment<T, N, B> {
    #[inline(always)]
    fn index_mut(&mut self, offset: usize) -> &mut Self::Output {
        if offset < N {
            // # Safety
            // All mutable borrows of an item require `self` to be mutably borrowed as well, so
            // there are no other mutable borrows currently. The only exception is
            // [`Self::push`] which mutably borrows the newly added item.
            #[allow(unsafe_code)]
            unsafe {
                self.items[offset].unchecked_borrow_mut()
            }
        } else {
            self.next.opt_item_mut().unwrap().index_mut(offset - N)
        }
    }
}



// ==========================
// === AllocationBehavior ===
// ==========================

pub trait AllocationBehavior<T, const N: usize>: Sized {
    /// Add a new item at the end and return its index. If the container uses preallocated memory,
    /// this operation is almost zero-cost.
    fn push_new(array: &LinkedArray<T, N, Self>) -> usize;
    /// Create a new segment.
    fn new_segment() -> Segment<T, N, Self>;
    /// Clear the memory in all the segments. If the allocation behavior pre-allocates items, the
    /// memory will be populated with new instances.
    fn clear_segments(segment: &mut Segment<T, N, Self>);
}

impl<T: Default, const N: usize> AllocationBehavior<T, N> for preallocation::Default {
    #[inline(always)]
    fn push_new(array: &LinkedArray<T, N, Self>) -> usize {
        array.push_new_assume_initialized()
    }

    #[inline(always)]
    fn new_segment() -> Segment<T, N, Self> {
        let mut items = Vec::with_capacity(N);
        for _ in 0..N {
            items.push(default());
        }
        let next = default();
        let _allocation_behavior = PhantomData;
        Segment { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn clear_segments(segment: &mut Segment<T, N, Self>) {
        segment.items.clear();
        for _ in 0..N {
            segment.items.push(default());
        }
        if let Some(next) = segment.next.opt_item_mut() {
            Self::clear_segments(next);
        }
    }
}

impl<T: Zeroable, const N: usize> AllocationBehavior<T, N> for preallocation::Zeroed {
    #[inline(always)]
    fn push_new(array: &LinkedArray<T, N, Self>) -> usize {
        array.push_new_assume_initialized()
    }

    #[inline(always)]
    fn new_segment() -> Segment<T, N, Self> {
        let items = {
            let layout = std::alloc::Layout::array::<UnsafeCell<T>>(N).unwrap();
            // # Safety
            // The bound `T: Zeroable` guarantees that `T` can be initialized with zeroed memory.
            #[allow(unsafe_code)]
            unsafe {
                Vec::from_raw_parts(std::alloc::alloc_zeroed(layout) as *mut _, N, N)
            }
        };
        let next = default();
        let _allocation_behavior = PhantomData;
        Segment { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn clear_segments(segment: &mut Segment<T, N, Self>) {
        unsafe { std::intrinsics::volatile_set_memory(segment.items.as_mut_ptr(), 0, N) }
        if let Some(next) = segment.next.opt_item_mut() {
            Self::clear_segments(next);
        }
    }
}

impl<T: Default, const N: usize> AllocationBehavior<T, N> for preallocation::Disabled {
    #[inline(always)]
    fn push_new(array: &LinkedArray<T, N, Self>) -> usize {
        array.push(default())
    }

    #[inline(always)]
    fn new_segment() -> Segment<T, N, Self> {
        let items = default();
        let next = default();
        let _allocation_behavior = PhantomData;
        Segment { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn clear_segments(segment: &mut Segment<T, N, Self>) {
        segment.clear();
        if let Some(next) = segment.next.opt_item_mut() {
            Self::clear_segments(next);
        }
    }
}



// =================
// === Iterators ===
// =================

// === Iter for &T ===

/// An iterator over immutable references to [`LinkedArray`] items.
#[derive(Debug)]
pub struct Iter<'a, T, const N: usize, B> {
    segment:     &'a Segment<T, N, B>,
    next_offset: usize,
    max_offset:  usize,
}

impl<'a, T, const N: usize, B> Iterator for Iter<'a, T, N, B> {
    type Item = &'a T;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_offset == self.max_offset {
            return None;
        }
        if self.next_offset >= N {
            self.segment = self.segment.next.opt_item().unwrap();
            self.next_offset = 0;
            self.max_offset -= N;
        }
        let item = self.segment.index(self.next_offset);
        self.next_offset += 1;
        Some(item)
    }
}

impl<'a, T, const N: usize, B> IntoIterator for &'a LinkedArray<T, N, B>
where B: AllocationBehavior<T, N>
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T, N, B>;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            segment:     self.get_or_init_first_segment(),
            next_offset: 0,
            max_offset:  self.size.get(),
        }
    }
}


// === Iter for T ===

/// An iterator over owned [`LinkedArray`] items.
#[derive(Debug)]
pub struct IntoIter<T, const N: usize, B> {
    next_segment:    Option<Box<Segment<T, N, B>>>,
    current_segment: std::vec::IntoIter<UnsafeCell<T>>,
    next_offset:     usize,
    max_offset:      usize,
}

impl<T, const N: usize, B> IntoIterator for LinkedArray<T, N, B>
where B: AllocationBehavior<T, N>
{
    type Item = T;
    type IntoIter = IntoIter<T, N, B>;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.init_first_segment();
        let first_segment = self.first_segment.into_inner().unwrap();
        let current_segment = first_segment.items.into_iter();
        let next_segment = first_segment.next.into_inner();
        IntoIter { next_segment, current_segment, next_offset: 0, max_offset: self.size.get() }
    }
}

impl<T, const N: usize, B> Iterator for IntoIter<T, N, B> {
    type Item = T;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_offset == self.max_offset {
            return None;
        }
        if self.next_offset >= N {
            if let Some(next_segment) = self.next_segment.take() {
                self.current_segment = next_segment.items.into_iter();
                self.next_segment = next_segment.next.into_inner();
            }
        }
        self.next_offset += 1;
        self.current_segment.next().map(|t| t.into_inner())
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests2 {
    use super::*;

    #[test]
    fn test_push() {
        let array = LinkedArray::<usize, 2>::new();
        array.push(1);
        assert_eq!(&array.to_vec(), &[1]);
        array.push(2);
        assert_eq!(&array.to_vec(), &[1, 2]);
        array.push(3);
        assert_eq!(&array.to_vec(), &[1, 2, 3]);
        array.push(4);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4]);
        array.push(5);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_retain() {
        let mut array = LinkedArray::<usize, 2>::new();
        array.push(1);
        array.push(2);
        array.push(3);
        array.push(4);
        array.push(5);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4, 5]);
        array.retain(|t| *t > 0);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4, 5]);
        array.retain(|t| *t > 3);
        assert_eq!(&array.to_vec(), &[4, 5]);
        array.push(6);
        array.push(7);
        array.push(8);
        assert_eq!(&array.to_vec(), &[4, 5, 6, 7, 8]);
        array.retain(|t| t % 2 == 0);
        assert_eq!(&array.to_vec(), &[4, 6, 8]);
        array.retain(|_| false);
        assert!(&array.is_empty());
    }
}
