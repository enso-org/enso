use crate::prelude::*;
use std::cell::UnsafeCell;



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
    ///    it never re-allocates the underlying memory when adding new items. In case there is not
    ///    enough space, a new memory segment will be allocated and linked to the previous one.
    ///
    /// [`LinkedArray`] will never automatically shrink itself, even if completely empty. This
    /// ensures no unnecessary allocations or deallocations occur. Emptying a [`LinkedArray`] and
    /// then filling it back up to the same len should incur no calls to the allocator. If you wish
    /// to free up unused memory, use [`Self::shrink_to_fit`] or [`Self::shrink_to`].
    #[derive(Derivative)]
    #[derivative(Default(bound = ""))]
    pub struct LinkedArray
    [T, N, B][T, const N: usize, B][T, const N: usize, B = prealloc::Zeroed] {
        len:           Cell<usize>,
        capacity:      Cell<usize>,
        first_segment: InitCell<ZeroableOption<Segment<T, N, B>>>,
    }
}

impl<T: Debug, const N: usize, B> Debug for LinkedArray<T, N, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LinkedArray")
            .field("len", &self.len.get())
            .field("capacity", &self.capacity.get())
            .field("first_segment", &self.first_segment)
            .finish()
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

    /// The capacity of the container.
    #[inline(always)]
    pub fn capacity(&self) -> usize {
        self.capacity.get()
    }

    /// Number of items stored.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len.get()
    }

    /// Check whether the container is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len.get() == 0
    }

    /// Remove all items. It does not deallocate the memory.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.len.set(0);
        if let Some(first_segment) = self.first_segment.opt_item_mut() {
            AllocationBehavior::clear_segments(first_segment);
        }
    }

    /// Deallocate segments that are not used.
    pub fn shrink_to_fit(&mut self) {
        self.shrink_to(self.len())
    }

    /// Shrink the capacity of the container with a lower bound.
    ///
    /// The capacity will remain at least as large as both the length and the supplied value.
    pub fn shrink_to(&mut self, capacity: usize) {
        let new_capacity = self.len().max(capacity);
        if new_capacity == 0 {
            self.first_segment.set_default();
        } else {
            if let Some(first_segment) = self.first_segment.opt_item_mut() {
                let new_capacity = first_segment.shrink_to_fit(new_capacity);
                self.capacity.set(new_capacity)
            }
        }
    }

    /// Number of segments used to store the items.
    #[inline(always)]
    pub fn segment_count(&self) -> usize {
        self.first_segment.opt_item().map(|t| t.segment_count()).unwrap_or(0)
    }

    /// Add the provided item at the end and return its index.
    #[inline(always)]
    pub fn push(&self, item: T) -> usize {
        let index = self.len.get();
        self.len.modify(|t| *t += 1);
        let new_segment_added = self.get_or_init_first_segment().push(index, item);
        if new_segment_added {
            self.capacity.modify(|t| *t += N);
        }
        index
    }

    /// Add a new item at the end and return its index. If the container uses preallocated memory
    /// and there is enough space, this operation is almost zero-cost.
    pub fn push_new(&self) -> usize {
        self.push_new_multiple(1)
    }

    /// Add several new items at the end  and return the index of the first added item. If the
    /// container uses preallocated memory and there is enough space, this operation is almost
    /// zero-cost.
    #[inline(always)]
    pub fn push_new_multiple(&self, count: usize) -> usize {
        AllocationBehavior::push_new_multiple(self, count)
    }

    /// Add a new element by shifting the element counter and assuming that the memory is already
    /// initialized.
    #[inline(always)]
    fn push_new_assume_initialized(&self, count: usize) -> usize {
        let index = self.len.get();
        self.len.modify(|t| *t += count);
        let last_index = self.len.get() - 1;
        let new_segments_needed = (last_index + N).saturating_sub(self.capacity.get()) / N;
        if new_segments_needed > 0 {
            self.add_tail_segments(new_segments_needed);
        }
        index
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
        let mut vec = Vec::with_capacity(self.len.get());
        vec.extend(self.iter().cloned());
        vec
    }

    /// Convert the container into a vector.
    pub fn into_vec(self) -> Vec<T>
    where T: Clone {
        let mut vec = Vec::with_capacity(self.len.get());
        vec.extend(self.into_iter());
        vec
    }

    #[inline(always)]
    fn init_first_segment(&self) {
        self.first_segment.init_if_empty(|| {
            self.capacity.set(N);
            AllocationBehavior::new_segment()
        });
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

    fn add_tail_segments(&self, count: usize) {
        self.get_or_init_first_segment().add_tail_segments(count);
        self.capacity.modify(|t| *t += N * count);
    }
}

impl<T: Default, const N: usize, B> LinkedArray<T, N, B>
where B: AllocationBehavior<T, N>
{
    /// Retain only the elements that satisfy the predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where F: FnMut(&T) -> bool {
        let len = self.len.get();
        let first_segment = self.get_or_init_first_segment_mut();
        first_segment.retain_stage1(len, f);
        let (new_size, new_capacity) = first_segment.retain_stage2();
        self.len.set(new_size);
        self.capacity.set(new_capacity);
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

pub struct Segment<T, const N: usize, B> {
    _allocation_behavior: PhantomData<B>,
    items:                UnsafeCell<Vec<UnsafeCell<T>>>,
    next:                 InitCell<Option<Box<Segment<T, N, B>>>>,
}

impl<T: Debug, const N: usize, B> Debug for Segment<T, N, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let items = self.items().iter().map(|t| unsafe { t.unchecked_borrow() }).collect_vec();
        f.debug_struct("Segment").field("items", &items).field("next", &self.next).finish()
    }
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
    #[inline(always)]
    fn segment_count(&self) -> usize {
        self.next.opt_item().map(|t| t.segment_count()).unwrap_or(0) + 1
    }

    #[inline(always)]
    fn shrink_to_fit(&mut self, count: usize) -> usize {
        if count < N {
            self.next.set_default();
            N
        } else {
            self.next.opt_item_mut().unwrap().shrink_to_fit(count - N)
        }
    }

    /// Get a mutable reference to the next segment if it exists;
    #[inline(always)]
    fn items_and_next_mut(&mut self) -> (&mut Vec<UnsafeCell<T>>, Option<&mut Box<Self>>) {
        let next = self.next.opt_item_mut();
        let items = unsafe { self.items.unchecked_borrow_mut() };
        (items, next)
    }

    #[inline(always)]
    fn items(&self) -> &Vec<UnsafeCell<T>> {
        unsafe { self.items.unchecked_borrow() }
    }

    #[inline(always)]
    fn items_mut(&mut self) -> &mut Vec<UnsafeCell<T>> {
        unsafe { self.items.unchecked_borrow_mut() }
    }
}

impl<T, const N: usize, B> Segment<T, N, B>
where B: AllocationBehavior<T, N>
{
    #[inline(always)]
    fn init_next_segment(&self) -> bool {
        let mut was_empty = false;
        self.next.init_if_empty(|| {
            was_empty = true;
            default()
        });
        was_empty
    }

    #[inline(always)]
    fn push(&self, offset: usize, elem: T) -> bool {
        AllocationBehavior::push_to_segment(self, offset, elem)
    }

    #[inline(always)]
    fn push_internal(&self, offset: usize, elem: T, resize_items: bool) -> bool {
        if offset < N {
            if resize_items {
                // # Safety
                // The vector was initialized with capacity of `N` elements, so it is safe to set
                // its new size here, as the value will be written below.
                #[allow(unsafe_code)]
                unsafe {
                    self.items.unchecked_borrow_mut().set_len(offset + 1);
                }
            }

            // # Safety
            // The item at `offset` either:
            // 1. Did not exist yet.
            // 2. Was removed, which required `self` to be mutably borrowed, so there are no other
            //    mutable borrows to the element.
            #[allow(unsafe_code)]
            unsafe {
                *self.items()[offset].unchecked_borrow_mut() = elem
            };
            false
        } else {
            let next_segment_added = self.init_next_segment();
            let some_segment_added =
                self.next.opt_item().unwrap().push_internal(offset - N, elem, resize_items);
            next_segment_added || some_segment_added
        }
    }

    #[inline(always)]
    #[allow(unconditional_recursion)]
    fn add_tail_segments(&self, count: usize) {
        if count > 0 {
            if !self.init_next_segment() {
                self.next.opt_item().unwrap().add_tail_segments(count - 1)
            }
        }
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
                self.items_mut().set_len(len)
            }
        }
        // # Safety
        // All mutable borrows of an item require `self` to be mutably borrowed as well, so there
        // are no other mutable borrows currently. The only exception is [`Self::push`] which
        // mutably borrows the newly added item.
        #[allow(unsafe_code)]
        self.items_mut().retain(|t| f(unsafe { t.unchecked_borrow() }));
        if let Some(next) = self.next.opt_item_mut() {
            next.retain_stage1(len - N, f);
        }
    }

    /// For each segment, merge two adjacent not-full segments.
    #[inline(always)]
    pub fn retain_stage2(&mut self) -> (usize, usize) {
        while self.items().len() < N {
            let new_next_segment = {
                let (items, mut next_segment) = self.items_and_next_mut();
                next_segment.as_mut().and_then(|next| {
                    let end = next.items().len().min(N - items.len());
                    items.extend(next.items_mut().drain(0..end));
                    let empty_next_segment = next.items().is_empty();
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
        match self.next.opt_item_mut() {
            None => {
                let len = self.items().len();
                AllocationBehavior::preallocate_missing_segment_items(self);
                (len, N)
            }
            Some(next) => {
                let (next_len, next_capacity) = next.retain_stage2();
                (N + next_len, N + next_capacity)
            }
        }
        // let len = self.items().len();
        // AllocationBehavior::preallocate_missing_segment_items(self);
        // len + self.next.opt_item_mut().map(|next| next.retain_stage2()).unwrap_or_default()
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
                self.items()[offset].unchecked_borrow()
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
                self.items()[offset].unchecked_borrow_mut()
            }
        } else {
            self.next.opt_item_mut().unwrap().index_mut(offset - N)
        }
    }
}



// ==========================
// === AllocationBehavior ===
// ==========================

/// Operations that differ depending on the preallocation behavior.
pub trait AllocationBehavior<T, const N: usize>: Sized {
    /// Add new items at the end and return the index of the first added item. If the container uses
    /// preallocated memory, this operation is almost zero-cost.
    fn push_new_multiple(array: &LinkedArray<T, N, Self>, count: usize) -> usize;
    /// Create a new segment.
    fn new_segment() -> Segment<T, N, Self>;
    /// Push new item to the segment. Return `true` if a new segment was added.
    fn push_to_segment(segment: &Segment<T, N, Self>, offset: usize, elem: T) -> bool;
    /// Clear the memory in all the segments. If the allocation behavior pre-allocates items, the
    /// memory will be populated with new instances.
    fn clear_segments(segment: &mut Segment<T, N, Self>);
    /// Preallocate missing items in the segment. If the container was configured not to preallocate
    /// items, this is a no-op.
    fn preallocate_missing_segment_items(segment: &mut Segment<T, N, Self>);
}

impl<T: Default, const N: usize> AllocationBehavior<T, N> for prealloc::Default {
    #[inline(always)]
    fn push_new_multiple(array: &LinkedArray<T, N, Self>, count: usize) -> usize {
        array.push_new_assume_initialized(count)
    }

    #[inline(always)]
    fn new_segment() -> Segment<T, N, Self> {
        let mut items = Vec::with_capacity(N);
        for _ in 0..N {
            items.push(default());
        }
        let items = UnsafeCell::new(items);
        let next = default();
        let _allocation_behavior = PhantomData;
        Segment { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_segment(segment: &Segment<T, N, Self>, offset: usize, elem: T) -> bool {
        segment.push_internal(offset, elem, false)
    }

    #[inline(always)]
    fn clear_segments(segment: &mut Segment<T, N, Self>) {
        segment.items_mut().clear();
        for _ in 0..N {
            segment.items_mut().push(default());
        }
        if let Some(next) = segment.next.opt_item_mut() {
            Self::clear_segments(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_segment_items(segment: &mut Segment<T, N, Self>) {
        let len = segment.items().len();
        for _ in len..N {
            segment.items_mut().push(default());
        }
    }
}

impl<T: Zeroable, const N: usize> AllocationBehavior<T, N> for prealloc::Zeroed {
    #[inline(always)]
    fn push_new_multiple(array: &LinkedArray<T, N, Self>, count: usize) -> usize {
        array.push_new_assume_initialized(count)
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
        let items = UnsafeCell::new(items);
        let next = default();
        let _allocation_behavior = PhantomData;
        Segment { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_segment(segment: &Segment<T, N, Self>, offset: usize, elem: T) -> bool {
        segment.push_internal(offset, elem, false)
    }

    #[inline(always)]
    fn clear_segments(segment: &mut Segment<T, N, Self>) {
        unsafe { std::intrinsics::volatile_set_memory(segment.items_mut().as_mut_ptr(), 0, N) }
        if let Some(next) = segment.next.opt_item_mut() {
            Self::clear_segments(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_segment_items(segment: &mut Segment<T, N, Self>) {
        let len = segment.items().len();
        for _ in len..N {
            segment.items_mut().push(Zeroable::zeroed());
        }
    }
}

impl<T: Default, const N: usize> AllocationBehavior<T, N> for prealloc::Disabled {
    #[inline(always)]
    fn push_new_multiple(array: &LinkedArray<T, N, Self>, count: usize) -> usize {
        let index = array.len();
        for _ in 0..count {
            array.push(default());
        }
        index
    }

    #[inline(always)]
    fn new_segment() -> Segment<T, N, Self> {
        let items = {
            let layout = std::alloc::Layout::array::<UnsafeCell<T>>(N).unwrap();
            // # Safety
            // The bound `T: Zeroable` guarantees that `T` can be initialized with zeroed memory.
            #[allow(unsafe_code)]
            unsafe {
                Vec::from_raw_parts(std::alloc::alloc_zeroed(layout) as *mut _, 0, N)
            }
        };
        let items = UnsafeCell::new(items);
        let next = default();
        let _allocation_behavior = PhantomData;
        Segment { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_segment(segment: &Segment<T, N, Self>, offset: usize, elem: T) -> bool {
        segment.push_internal(offset, elem, true)
    }

    #[inline(always)]
    fn clear_segments(segment: &mut Segment<T, N, Self>) {
        segment.items_mut().clear();
        if let Some(next) = segment.next.opt_item_mut() {
            Self::clear_segments(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_segment_items(_segment: &mut Segment<T, N, Self>) {}
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
            max_offset:  self.len.get(),
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
        let current_segment = first_segment.items.into_inner().into_iter();
        let next_segment = first_segment.next.into_inner();
        IntoIter { next_segment, current_segment, next_offset: 0, max_offset: self.len.get() }
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
                self.current_segment = next_segment.items.into_inner().into_iter();
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
mod tests {
    use super::*;

    macro_rules! test_all_configs {
        ($f:ident) => {
            $f(&mut LinkedArray::<usize, 1, prealloc::Default>::new());
            $f(&mut LinkedArray::<usize, 1, prealloc::Zeroed>::new());
            $f(&mut LinkedArray::<usize, 1, prealloc::Disabled>::new());

            $f(&mut LinkedArray::<usize, 2, prealloc::Default>::new());
            $f(&mut LinkedArray::<usize, 2, prealloc::Zeroed>::new());
            $f(&mut LinkedArray::<usize, 2, prealloc::Disabled>::new());

            $f(&mut LinkedArray::<usize, 4, prealloc::Default>::new());
            $f(&mut LinkedArray::<usize, 4, prealloc::Zeroed>::new());
            $f(&mut LinkedArray::<usize, 4, prealloc::Disabled>::new());

            $f(&mut LinkedArray::<usize, 8, prealloc::Default>::new());
            $f(&mut LinkedArray::<usize, 8, prealloc::Zeroed>::new());
            $f(&mut LinkedArray::<usize, 8, prealloc::Disabled>::new());

            $f(&mut LinkedArray::<usize, 16, prealloc::Default>::new());
            $f(&mut LinkedArray::<usize, 16, prealloc::Zeroed>::new());
            $f(&mut LinkedArray::<usize, 16, prealloc::Disabled>::new());

            // $f(&mut LinkedArray::<usize, 2, prealloc::Zeroed>::new());
        };
    }


    // === Push ===

    #[test]
    fn test_push() {
        test_all_configs!(test_template_push);
    }

    fn test_template_push<const N: usize, B>(array: &mut LinkedArray<usize, N, B>)
    where B: AllocationBehavior<usize, N> {
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


    // === Clear ===

    #[test]
    fn test_clear() {
        test_all_configs!(test_template_clear);
    }

    fn test_template_clear<const N: usize, B>(array: &mut LinkedArray<usize, N, B>)
    where B: AllocationBehavior<usize, N> {
        array.push(1);
        array.push(2);
        array.push(3);
        array.push(4);
        array.push(5);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4, 5]);
        array.clear();
        assert!(&array.is_empty());
        array.push_new();
        array.push_new();
        array.push_new();
        array.push_new();
        array.push_new();
        assert_eq!(&array.to_vec(), &[0, 0, 0, 0, 0]);
    }



    // === Retain ===

    #[test]
    fn test_retain() {
        test_all_configs!(test_template_retain);
    }

    fn test_template_retain<const N: usize, B>(array: &mut LinkedArray<usize, N, B>)
    where B: AllocationBehavior<usize, N> {
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
