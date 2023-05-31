use crate::prelude::*;

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
    pub struct ZeroableLinkedArrayRefCell[T, N][T, const N: usize][T, const N: usize] {
        size:          Cell<usize>,
        first_segment: OptRefCell<ZeroableOption<Segment<T, N>>>,
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
    pub fn clear(&self) {
        self.size.set(0);
        if let Some(first_segment) = self.first_segment.borrow_mut().as_mut() {
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
        self.with_first_segment(|s| s.push(index, elem));
        self.size.modify(|t| *t += 1);
        index
    }

    /// Add a new element to the container, initializing it with zeroed memory, and return its
    /// index.
    #[inline(always)]
    pub fn push_zeroed(&self) -> usize {
        let index = self.size.get();
        if index % N == 0 {
            self.with_first_segment(|s| s.add_tail_segment());
        }
        self.size.modify(|t| *t += 1);
        index
    }
}

impl<T: Zeroable, const N: usize> ZeroableLinkedArrayRefCell<T, N> {
    #[inline(always)]
    fn with_first_segment<R>(&self, f: impl FnOnce(&Segment<T, N>) -> R) -> R {
        if self.first_segment.borrow().is_none() {
            *self.first_segment.borrow_mut() = ZeroableOption::Some(Segment::default());
        }
        f(&*self.first_segment.borrow().as_ref().unwrap())
    }

    #[inline(always)]
    fn with_first_segment_mut<R>(&self, f: impl FnOnce(&mut Segment<T, N>) -> R) -> R {
        if self.first_segment.borrow().is_none() {
            *self.first_segment.borrow_mut() = ZeroableOption::Some(Segment::default());
        }
        f(&mut *self.first_segment.borrow_mut().as_mut().unwrap())
    }

    /// Set the element at the given index.
    #[inline(always)]
    pub fn set(&self, index: usize, elem: T) {
        self.with_first_segment(|s| s.set(index, elem));
    }

    /// Get the clone of an element at the given index.
    #[inline(always)]
    pub fn get_cloned(&self, index: usize) -> T
    where T: Clone {
        self.with_item_borrow(index, Clone::clone)
    }

    /// Perform a function on the borrowed element at the given index.
    #[inline(always)]
    pub fn with_item_borrow<U>(&self, index: usize, f: impl FnOnce(&T) -> U) -> U {
        self.with_first_segment(|s| s.with_item_borrow(index, f))
    }

    /// Perform a function on the mutably borrowed element at the given index.
    #[inline(always)]
    pub fn with_item_borrow_mut<U>(&self, index: usize, f: impl FnOnce(&mut T) -> U) -> U {
        self.with_first_segment(|s| s.with_item_borrow_mut(index, f))
    }

    /// Perform a function on every borrowed element.
    #[inline(always)]
    pub fn for_item_borrow(&self, f: impl FnMut(&T)) {
        self.with_first_segment(|s| s.for_item_borrow(self.size.get(), f));
    }

    /// Perform a function on every mutably borrowed element.
    #[inline(always)]
    pub fn for_item_borrow_mut(&self, f: impl FnMut(&mut T)) {
        self.with_first_segment(|s| s.for_item_borrow_mut(self.size.get(), f));
    }

    /// Retain only the elements that satisfy the predicate.
    pub fn retain<F>(&self, mut f: F)
    where F: FnMut(&T) -> bool {
        self.with_first_segment_mut(|s| {
            s.retain_stage1(self.size.get(), f);
            self.size.set(s.retain_stage2());
        })
    }

    /// Clone all elements to a vector.
    pub fn to_vec(&self) -> Vec<T>
    where T: Clone {
        let mut vec = Vec::with_capacity(self.size.get());
        self.for_item_borrow(|t| vec.push(t.clone()));
        vec
    }
}

impl<T: Copy + Zeroable, const N: usize> ZeroableLinkedArrayRefCell<T, N> {
    /// Get the element at the given index.
    #[inline(always)]
    pub fn get(&self, index: usize) -> T {
        self.with_item_borrow(index, |t| *t)
    }
}



// ===============
// === Segment ===
// ===============

#[derive(Debug)]
struct Segment<T, const N: usize> {
    items: Vec<OptRefCell<T>>,
    next:  OptRefCell<Option<Box<Segment<T, N>>>>,
}

impl<T: Zeroable, const N: usize> Segment<T, N> {
    #[inline(always)]
    fn new() -> Self {
        let items = {
            let layout = std::alloc::Layout::array::<OptRefCell<T>>(N).unwrap();
            unsafe { Vec::from_raw_parts(std::alloc::alloc_zeroed(layout) as *mut _, N, N) }
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


impl<T: Zeroable, const N: usize> Segment<T, N> {
    #[inline(always)]
    fn clear(&mut self) {
        unsafe { std::intrinsics::volatile_set_memory(self.items.as_mut_ptr(), 0, N) }
    }


    #[inline(always)]
    fn push(&self, offset: usize, elem: T) {
        if offset < N {
            *self.items[offset].borrow_mut() = elem;
        } else {
            let no_next = self.next.borrow().is_none();
            if no_next {
                *self.next.borrow_mut() = Some(default());
            }
            self.next.with_borrowed(|t| t.as_ref().unwrap().push(offset - N, elem));
        }
    }

    #[inline(always)]
    fn add_tail_segment(&self) {
        let no_next = self.next.borrow().is_none();
        if no_next {
            *self.next.borrow_mut() = Some(default());
        } else {
            self.next.with_borrowed(|t| t.as_ref().unwrap().add_tail_segment());
        }
    }

    /// For each segment, retain the items. Segment lengths will be shortened if necessary, however,
    /// two adjacent not-full segments will not be merged. Merging will be performed in stage 2.
    #[inline(always)]
    pub fn retain_stage1<F>(&mut self, len: usize, mut f: F)
    where F: FnMut(&T) -> bool {
        if len < N {
            unsafe { self.items.set_len(len) }
        }
        self.items.retain(|t| f(&*t.borrow()));
        if let Some(next) = self.next.borrow_mut().as_mut() {
            next.retain_stage1(len - N, f);
        }
    }

    /// For each segment, merge two adjacent not-full segments.
    #[inline(always)]
    pub fn retain_stage2(&mut self) -> usize {
        while self.items.len() < N {
            let new_next_segment = self.next.borrow_mut().as_mut().and_then(|next| {
                let end = next.items.len().min(N - self.items.len());
                self.items.extend(next.items.drain(0..end));
                next.items.is_empty().then(|| mem::take(&mut *next.next.borrow_mut()))
            });
            if let Some(new_next_segment) = new_next_segment {
                *self.next.borrow_mut() = new_next_segment;
            }
            if self.next.borrow().is_none() {
                break;
            }
        }
        let len = self.items.len();
        for _ in len..N {
            self.items.push(Zeroable::zeroed());
        }
        len + self.next.borrow_mut().as_mut().map(|next| next.retain_stage2()).unwrap_or_default()
    }
}

impl<T, const N: usize> Segment<T, N> {
    #[inline(always)]
    fn set(&self, offset: usize, elem: T) {
        self.with_item_borrow_mut(offset, |t| *t = elem);
    }

    #[inline(always)]
    fn with_item_borrow<U>(&self, offset: usize, f: impl FnOnce(&T) -> U) -> U {
        if offset < N {
            f(&*self.items[offset].borrow())
        } else {
            self.next.with_borrowed(|t| t.as_ref().unwrap().with_item_borrow(offset - N, f))
        }
    }

    #[inline(always)]
    fn with_item_borrow_mut<U>(&self, offset: usize, f: impl FnOnce(&mut T) -> U) -> U {
        if offset < N {
            f(&mut *self.items[offset].borrow_mut())
        } else {
            self.next.with_borrowed(|t| t.as_ref().unwrap().with_item_borrow_mut(offset - N, f))
        }
    }

    #[inline(always)]
    fn for_item_borrow(&self, count: usize, mut f: impl FnMut(&T)) {
        for item in self.items.iter().take(count) {
            f(&*item.borrow());
        }
        if count > N {
            self.next.with_borrowed(|t| t.as_ref().unwrap().for_item_borrow(count - N, f));
        }
    }

    #[inline(always)]
    fn for_item_borrow_mut(&self, count: usize, mut f: impl FnMut(&mut T)) {
        for item in self.items.iter().take(count) {
            f(&mut *item.borrow_mut());
        }
        if count > N {
            self.next.with_borrowed(|t| t.as_ref().unwrap().for_item_borrow_mut(count - N, f));
        }
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
        let array = ZeroableLinkedArrayRefCell::<usize, 2>::new();
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
