use crate::prelude::*;
use std::cell::UnsafeCell;



pub trait OptionLike {
    type Value;
    fn new_some(value: Self::Value) -> Self;
    fn as_ref(&self) -> Option<&Self::Value>;
    fn as_mut(&mut self) -> Option<&mut Self::Value>;
    fn is_none(&self) -> bool;
    fn is_some(&self) -> bool;
}

impl<T> OptionLike for Option<T> {
    type Value = T;

    fn new_some(value: Self::Value) -> Self {
        Some(value)
    }
    fn as_ref(&self) -> Option<&Self::Value> {
        Option::as_ref(self)
    }
    fn as_mut(&mut self) -> Option<&mut Self::Value> {
        Option::as_mut(self)
    }
    fn is_none(&self) -> bool {
        Option::is_none(self)
    }
    fn is_some(&self) -> bool {
        Option::is_some(self)
    }
}

impl<T> OptionLike for ZeroableOption<T> {
    type Value = T;

    fn new_some(value: Self::Value) -> Self {
        ZeroableOption::Some(value)
    }
    fn as_ref(&self) -> Option<&Self::Value> {
        ZeroableOption::as_ref(self)
    }
    fn as_mut(&mut self) -> Option<&mut Self::Value> {
        ZeroableOption::as_mut(self)
    }
    fn is_none(&self) -> bool {
        ZeroableOption::is_none(self)
    }
    fn is_some(&self) -> bool {
        ZeroableOption::is_some(self)
    }
}

#[derive(Debug)]
pub struct OptionInitCell<T> {
    // # Safety
    // Please note that the implementation never exposes reference to this field. If it was
    // exposed, the implementation of [`Self::init_if_none`] would be unsound, as the user
    // might acquire a reference to a [`None`] value and then have it changed with
    // [`Self::init_if_none`], which does not require a mutable reference to self.
    not_exposed: UnsafeCell<T>,
}

impl<T: OptionLike> OptionInitCell<T> {
    pub fn init_if_none(&self, f: impl FnOnce() -> T::Value) {
        if self.is_none() {
            // # Safety
            // We checked that the current value is [`Nothing`]. We also know that no one has a
            // reference to [`Self::not_exposed`] (see the docs of [`Self`] to learn more).
            // Therefore, it is safe to overwrite the current value.
            #[allow(unsafe_code)]
            unsafe {
                *self.not_exposed.unchecked_borrow_mut() = T::new_some(f())
            };
        }
    }
}

impl<T: OptionLike> OptionLike for OptionInitCell<T> {
    type Value = T::Value;

    fn new_some(value: Self::Value) -> Self {
        Self { not_exposed: UnsafeCell::new(T::new_some(value)) }
    }

    fn as_ref(&self) -> Option<&Self::Value> {
        // # Safety
        // Every mutable access to the value stored in [`Self::not_exposed`] requires a mutable
        // access to self. The only exception is [`Self::init_if_none`], which is safe, as explained
        // there.
        #[allow(unsafe_code)]
        let not_exposed = unsafe { self.not_exposed.unchecked_borrow() };
        not_exposed.as_ref()
    }

    fn as_mut(&mut self) -> Option<&mut Self::Value> {
        // # Safety
        // Every mutable access to the value stored in [`Self::not_exposed`] requires a mutable
        // access to self. The only exception is [`Self::init_if_none`], which is safe, as explained
        // there.
        #[allow(unsafe_code)]
        let not_exposed = unsafe { self.not_exposed.unchecked_borrow_mut() };
        not_exposed.as_mut()
    }

    fn is_some(&self) -> bool {
        self.as_ref().is_some()
    }

    fn is_none(&self) -> bool {
        self.as_ref().is_none()
    }
}


// =================
// === LinkedVec ===
// =================

derive_zeroable! {
    /// [`Vec`]-like structure that:
    /// 1. Can be initialized with zeroed memory.
    /// 2. Is represented as linked list of arrays under the hood and can grow while being immutably
    ///    borrowed.
    /// 3. Keeps items in [`RefCell`]s, allowing many operations to be performed without mutable
    ///    borrow. For example, `push` can be performed without mutable borrow always safely, as the
    ///    struct can grow without reallocating the underlying memory.
    ///
    /// # Zeroed memory representation
    /// It is guaranteed that all reserved, but not-used memory of this structure is zeroed. For
    /// example, after removing an item or after cleaning the container, the reserved memory will be
    /// zeroed. It allows for a very efficient implementation of several utilities, such as
    /// [`Self::push_zeroed`], which for big enough [`Self::N`] performs in O(1) time.
    #[derive(Debug, Derivative)]
    #[derivative(Default(bound = ""))]
    pub struct ZeroableLinkedArrayRefCell[T, N][T, const N: usize] {
        size:          Cell<usize>,
        first_segment: UnsafeCell<ZeroableOption<Segment<T, N>>>,
    }
}

impl<T: Default + Zeroable, const N: usize> ZeroableLinkedArrayRefCell<T, N> {
    /// Constructor.
    #[inline(always)]
    pub fn new() -> Self {
        Self::default()
    }

    /// Clear all data in the container.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.size.set(0);
        // # Safety
        // All mutable borrows of the first segment require `self` to be mutably borrowed as well.
        // The only exception is [`Self::init_first_segment`].
        #[allow(unsafe_code)]
        let first_segment_opt = unsafe { self.first_segment.unchecked_borrow_mut() };
        if let Some(first_segment) = first_segment_opt.as_mut() {
            first_segment.clear();
        }
    }

    /// Number of elements in the container.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.size.get()
    }

    /// Check whether the container is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.size.get() == 0
    }

    /// Add a new element to the container and return its index.
    #[inline(always)]
    pub fn push(&self, elem: T) -> usize {
        let index = self.size.get();
        self.get_or_init_first_segment().push(index, elem);
        self.size.modify(|t| *t += 1);
        index
    }

    /// Add a new element to the container, initializing it with zeroed memory, and return its
    /// index.
    #[inline(always)]
    pub fn push_zeroed(&self) -> usize {
        let index = self.size.get();
        if index % N == 0 {
            self.get_or_init_first_segment().add_tail_segment();
        }
        self.size.modify(|t| *t += 1);
        index
    }
}

impl<T: Zeroable, const N: usize> ZeroableLinkedArrayRefCell<T, N> {
    #[inline(always)]
    fn init_first_segment(&self) {
        // # Safety
        // We are mutably borrowing the first segment only if it did not yet exist, so it is
        // guaranteed that there is no other borrow yet.
        #[allow(unsafe_code)]
        unsafe {
            let no_first_segment = self.first_segment.unchecked_borrow().is_none();
            if no_first_segment {
                *self.first_segment.unchecked_borrow_mut() = ZeroableOption::Some(Segment::new());
            }
        }
    }

    #[inline(always)]
    fn first_segment(&self) -> &ZeroableOption<Segment<T, N>> {
        // # Safety
        // All mutable borrows of the first segment require `self` to be mutably borrowed as well.
        // The only exception is [`Self::init_first_segment`].
        #[allow(unsafe_code)]
        unsafe {
            self.first_segment.unchecked_borrow()
        }
    }

    #[inline(always)]
    fn first_segment_mut(&mut self) -> &mut ZeroableOption<Segment<T, N>> {
        // # Safety
        // All mutable borrows of the first segment require `self` to be mutably borrowed as well.
        // The only exception is [`Self::init_first_segment`].
        #[allow(unsafe_code)]
        unsafe {
            self.first_segment.unchecked_borrow_mut()
        }
    }

    #[inline(always)]
    fn get_or_init_first_segment(&self) -> &Segment<T, N> {
        self.init_first_segment();
        self.first_segment().as_ref().unwrap()
    }

    #[inline(always)]
    fn get_or_init_first_segment_mut(&mut self) -> &mut Segment<T, N> {
        self.init_first_segment();
        self.first_segment_mut().as_mut().unwrap()
    }

    // /// Perform a function on every borrowed element.
    // #[inline(always)]
    // pub fn for_item_borrow(&self, f: impl FnMut(&T)) {
    //     self.first_segment().for_item_borrow(self.size.get(), f)
    // }
    //
    // /// Perform a function on every mutably borrowed element.
    // #[inline(always)]
    // pub fn for_item_borrow_mut(&self, f: impl FnMut(&mut T)) {
    //     self.first_segment().for_item_borrow_mut(self.size.get(), f)
    // }

    /// Retain only the elements that satisfy the predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where F: FnMut(&T) -> bool {
        let size = self.size.get();
        let first_segment = self.get_or_init_first_segment_mut();
        first_segment.retain_stage1(size, f);
        let new_size = first_segment.retain_stage2();
        self.size.set(new_size);
    }

    #[inline(always)]
    pub fn iter(&self) -> Iter<T, N> {
        self.into_iter()
    }

    /// Clone all elements to a vector.
    pub fn to_vec(&self) -> Vec<T>
    where T: Clone {
        let mut vec = Vec::with_capacity(self.size.get());
        vec.extend(self.iter().cloned());
        vec
    }
}

// impl<T: Copy + Zeroable, const N: usize> ZeroableLinkedArrayRefCell<T, N> {
//     /// Get the element at the given index.
//     #[inline(always)]
//     pub fn get(&self, index: usize) -> T {
//         self.with_item_borrow(index, |t| *t)
//     }
// }


impl<T: Zeroable, const N: usize> Index<usize> for ZeroableLinkedArrayRefCell<T, N> {
    type Output = T;
    #[inline(always)]
    fn index(&self, offset: usize) -> &Self::Output {
        self.get_or_init_first_segment().index(offset)
    }
}

impl<T: Zeroable, const N: usize> IndexMut<usize> for ZeroableLinkedArrayRefCell<T, N> {
    #[inline(always)]
    fn index_mut(&mut self, offset: usize) -> &mut Self::Output {
        self.get_or_init_first_segment_mut().index_mut(offset)
    }
}

pub struct Iter<'a, T, const N: usize> {
    segment:     &'a Segment<T, N>,
    next_offset: usize,
    max_offset:  usize,
}

impl<'a, T, const N: usize> Iterator for Iter<'a, T, N> {
    type Item = &'a T;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_offset == self.max_offset {
            return None;
        }
        if self.next_offset >= N {
            self.segment = self.segment.next_unchecked();
            self.next_offset = 0;
            self.max_offset -= N;
        }
        let item = self.segment.index(self.next_offset);
        self.next_offset += 1;
        Some(item)
    }
}

impl<'a, T: Zeroable, const N: usize> IntoIterator for &'a ZeroableLinkedArrayRefCell<T, N> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T, N>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            segment:     self.get_or_init_first_segment(),
            next_offset: 0,
            max_offset:  self.size.get(),
        }
    }
}


// ===============
// === Segment ===
// ===============

#[derive(Debug)]
struct Segment<T, const N: usize> {
    items: Vec<UnsafeCell<T>>,
    next:  UnsafeCell<Option<Box<Segment<T, N>>>>,
}

impl<T: Zeroable, const N: usize> Segment<T, N> {
    #[inline(always)]
    fn new() -> Self {
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
        Self { items, next }
    }
}

impl<T: Zeroable, const N: usize> Default for Segment<T, N> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> Segment<T, N> {
    /// Get a reference to the next segment if it exists;
    #[inline(always)]
    fn next(&self) -> &Option<Box<Self>> {
        // # Safety
        // All mutable borrows of the next segment require `self` to be mutably borrowed as
        // well. The only exception is [`Self::init_next_segment`].
        #[allow(unsafe_code)]
        unsafe {
            self.next.unchecked_borrow()
        }
    }

    /// Get a mutable reference to the next segment if it exists;
    #[inline(always)]
    fn next_mut(&mut self) -> &mut Option<Box<Self>> {
        // # Safety
        // All mutable borrows of the next segment require `self` to be mutably borrowed as
        // well. The only exception is [`Self::init_next_segment`].
        #[allow(unsafe_code)]
        unsafe {
            self.next.unchecked_borrow_mut()
        }
    }

    /// Get a mutable reference to the next segment if it exists;
    #[inline(always)]
    fn items_and_next_mut(&mut self) -> (&mut Vec<UnsafeCell<T>>, &mut Option<Box<Self>>) {
        // # Safety
        // All mutable borrows of the next segment require `self` to be mutably borrowed as
        // well. The only exception is [`Self::init_next_segment`].
        #[allow(unsafe_code)]
        let next = unsafe { self.next.unchecked_borrow_mut() };
        let items = &mut self.items;
        (items, next)
    }

    /// Get a reference to the next segment. The existence of the next segment is not checked. If it
    /// does not exist, the function will panic.
    #[inline(always)]
    fn next_unchecked(&self) -> &Self {
        // # Safety
        // All mutable borrows of the next segment require `self` to be mutably borrowed as
        // well. The only exception is [`Self::init_next_segment`].
        #[allow(unsafe_code)]
        unsafe {
            self.next.unchecked_borrow().as_ref().unwrap()
        }
    }

    /// Get a mutablke reference to the next segment. The existence of the next segment is not
    /// checked. If it does not exist, the function will panic.
    #[inline(always)]
    fn next_unchecked_mut(&mut self) -> &mut Self {
        // # Safety
        // All mutable borrows of the next segment require `self` to be mutably borrowed as
        // well. The only exception is [`Self::init_next_segment`].
        #[allow(unsafe_code)]
        unsafe {
            self.next.unchecked_borrow_mut().as_mut().unwrap()
        }
    }
}

impl<T: Zeroable, const N: usize> Segment<T, N> {
    #[inline(always)]
    fn clear(&mut self) {
        unsafe { std::intrinsics::volatile_set_memory(self.items.as_mut_ptr(), 0, N) }
    }

    #[inline(always)]
    fn init_next_segment(&self) {
        let no_next = self.next().is_none();
        if no_next {
            // # Safety
            // We are mutably borrowing the next segment only if it did not yet exist, so it is
            // guaranteed that there is no other borrow yet.
            #[allow(unsafe_code)]
            unsafe {
                *self.next.unchecked_borrow_mut() = Some(default());
            }
        }
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
            self.next_unchecked().push(offset - N, elem)
        }
    }

    #[inline(always)]
    #[allow(unconditional_recursion)]
    fn add_tail_segment(&self) {
        self.init_next_segment();
        self.next_unchecked().add_tail_segment()
    }

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
        if let Some(next) = self.next_mut() {
            next.retain_stage1(len - N, f);
        }
    }

    /// For each segment, merge two adjacent not-full segments.
    #[inline(always)]
    pub fn retain_stage2(&mut self) -> usize {
        while self.items.len() < N {
            let new_next_segment = {
                let (items, next_segment) = self.items_and_next_mut();
                next_segment.as_mut().and_then(|next| {
                    let end = next.items.len().min(N - items.len());
                    items.extend(next.items.drain(0..end));
                    let empty_next_segment = next.items.is_empty();
                    let next_next_segment = next.next_mut();
                    empty_next_segment.then(|| mem::take(next_next_segment))
                })
            };
            if let Some(new_next_segment) = new_next_segment {
                *self.next_mut() = new_next_segment;
            }
            if self.next().is_none() {
                break;
            }
        }
        let len = self.items.len();
        for _ in len..N {
            self.items.push(Zeroable::zeroed());
        }
        len + self.next_mut().as_mut().map(|next| next.retain_stage2()).unwrap_or_default()
    }
}

impl<T, const N: usize> Index<usize> for Segment<T, N> {
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
            self.next().as_ref().unwrap().index(offset - N)
        }
    }
}

impl<T, const N: usize> IndexMut<usize> for Segment<T, N> {
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
            self.next_mut().as_mut().unwrap().index_mut(offset - N)
        }
    }
}

impl<T, const N: usize> Segment<T, N> {
    // #[inline(always)]
    // fn set(&self, offset: usize, elem: T) {
    //     self.with_item_borrow_mut(offset, |t| *t = elem);
    // }

    // #[inline(always)]
    // fn with_item_borrow<U>(&self, offset: usize, f: impl FnOnce(&T) -> U) -> U {
    //     if offset < N {
    //         f(self.items[offset].unchecked_borrow())
    //     } else {
    //         self.next.unchecked_borrow().as_ref().unwrap().with_item_borrow(offset - N, f)
    //     }
    // }
    //
    // #[inline(always)]
    // fn with_item_borrow_mut<U>(&self, offset: usize, f: impl FnOnce(&mut T) -> U) -> U {
    //     if offset < N {
    //         f(self.items[offset].unchecked_borrow_mut())
    //     } else {
    //         self.next.unchecked_borrow().as_ref().unwrap().with_item_borrow_mut(offset - N, f)
    //     }
    // }
    //
    // #[inline(always)]
    // fn for_item_borrow(&self, count: usize, mut f: impl FnMut(&T)) {
    //     for item in self.items.iter().take(count) {
    //         f(item.unchecked_borrow());
    //     }
    //     if count > N {
    //         self.next.unchecked_borrow().as_ref().unwrap().for_item_borrow(count - N, f);
    //     }
    // }
    //
    // #[inline(always)]
    // fn for_item_borrow_mut(&self, count: usize, mut f: impl FnMut(&mut T)) {
    //     for item in self.items.iter().take(count) {
    //         f(item.unchecked_borrow_mut());
    //     }
    //     if count > N {
    //         self.next.unchecked_borrow().as_ref().unwrap().for_item_borrow_mut(count - N, f);
    //     }
    // }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests2 {
    use super::*;

    #[test]
    fn test_push() {
        let array = ZeroableLinkedArrayRefCell::<usize, 2>::new();
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
        let mut array = ZeroableLinkedArrayRefCell::<usize, 2>::new();
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
