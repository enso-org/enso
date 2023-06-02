use crate::prelude::Index as IndexOps;
use crate::prelude::*;
use std::cell::UnsafeCell;



// =============
// === Index ===
// =============

/// Trait that list indexes should implemented. If not parametrized explicitly, the list will be
/// indexed by `usize`.
pub trait Index = Sized + Copy + Eq + From<usize> + Into<usize>;



// ==========================
// === UnrolledLinkedList ===
// ==========================

derive_zeroable! {
    /// A variation on the linked list which stores multiple elements in each [`Node`]. It can
    /// dramatically increase cache performance, while decreasing the memory overhead associated
    /// with storing list metadata such as references. It is related to the B-tree. To learn more,
    /// see: https://en.wikipedia.org/wiki/Unrolled_linked_list
    ///
    /// The [`UnrolledLinkedList`] has several uncommon features:
    /// 1. It allows pushing new elements without requiring mutable access to self. This is safe, as
    ///    it never re-allocates the underlying memory when adding new items. In case there is not
    ///    enough space, a new memory node will be allocated and linked to the previous one.
    /// 2. Can be configured (the [`B`] parameter) to preallocate memory for items, allowing for
    ///    almost zero-cost push with [`Self::push_new`] if the preallocated memory is not fully
    ///    occupied. The preallocation can be performed either by initializing values with their
    ///    defaults, or by zeroing the memory.
    /// 3. Can be initialized with zeroed memory.
    ///
    /// [`UnrolledLinkedList`] will never automatically shrink itself, even if completely empty.
    /// This ensures no unnecessary allocations or deallocations occur. Emptying it and then filling
    /// it back up to the same len should incur no calls to the allocator. If you wish to free up
    /// unused memory, use [`Self::shrink_to_fit`] or [`Self::shrink_to`].
    #[derive(Derivative)]
    #[derivative(Default(bound = ""))]
    pub struct UnrolledLinkedList
    [T, N, I, B][T, const N: usize, I, B][T, const N: usize, I = usize, B = prealloc::Zeroed] {
        len:        Cell<usize>,
        capacity:   Cell<usize>,
        first_node: InitCell<ZeroableOption<Node<T, N, B>>>,
        _index_tp:  PhantomData<I>,
    }
}

impl<T: Debug, const N: usize, I, B> Debug for UnrolledLinkedList<T, N, I, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnrolledLinkedList")
            .field("len", &self.len())
            .field("capacity", &self.capacity())
            .field("first_node", &self.first_node)
            .finish()
    }
}

impl<T: Debug, const N: usize, I, B> Display for UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T, N>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}


// === Node Management ===

impl<T, const N: usize, I, B> UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T, N>,
{
    #[inline(always)]
    fn init_first_node_if_empty(&self) {
        self.first_node.init_if_empty(|| {
            self.capacity.set(N);
            AllocationBehavior::new_node()
        });
    }

    #[inline(always)]
    fn get_or_init_first_node(&self) -> &Node<T, N, B> {
        self.init_first_node_if_empty();
        self.first_node.opt_item().unwrap()
    }

    fn add_tail_nodes(&self, count: usize) {
        self.get_or_init_first_node().add_tail_nodes(count);
        self.capacity.modify(|t| *t += N * count);
    }
}


// === Public API ===

impl<T, const N: usize, I, B> UnrolledLinkedList<T, N, I, B> {
    /// Constructor.
    #[inline(always)]
    pub fn new() -> Self {
        Self::default()
    }

    /// The capacity of the list, which is the count of items the list arrays can hold before
    /// allocating another node.
    #[inline(always)]
    pub fn capacity(&self) -> usize {
        self.capacity.get()
    }

    /// Number of items stored.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len.get()
    }

    /// Check whether the list is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len.get() == 0
    }
}

impl<T, const N: usize, I, B> UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T, N>,
{
    /// Remove all items. It does not deallocate the memory.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.len.set(0);
        if let Some(first_node) = self.first_node.opt_item_mut() {
            AllocationBehavior::clear_nodes(first_node);
        }
    }

    /// Deallocate nodes that are not used.
    pub fn shrink_to_fit(&mut self) {
        self.shrink_to(self.len())
    }

    /// Shrink the capacity of the list with a lower bound.
    ///
    /// The capacity will remain at least as large as both the length and the supplied value.
    pub fn shrink_to(&mut self, capacity: usize) {
        let new_capacity = self.len().max(capacity);
        if new_capacity == 0 {
            self.first_node.set_default();
        } else {
            if let Some(first_node) = self.first_node.opt_item_mut() {
                let new_capacity = first_node.shrink_to_fit(new_capacity);
                self.capacity.set(new_capacity)
            }
        }
    }

    /// Number of nodes used to store the items.
    #[inline(always)]
    pub fn node_count(&self) -> usize {
        self.first_node.opt_item().map(|t| t.node_count()).unwrap_or(0)
    }

    /// Add the provided item at the end and return its index.
    #[inline(always)]
    pub fn push(&self, item: T) -> I {
        let index = self.len.get();
        self.len.modify(|t| *t += 1);
        let new_node_added = self.get_or_init_first_node().push(index, item);
        if new_node_added {
            self.capacity.modify(|t| *t += N);
        }
        index.into()
    }

    /// Add a new item at the end and return its index. If the list uses preallocated memory and
    /// there is enough space, this operation is almost zero-cost.
    pub fn push_new(&self) -> I {
        self.push_new_multiple(1)
    }

    /// Add several new items at the end  and return the index of the first added item. If the list
    /// uses preallocated memory and there is enough space, this operation is almost zero-cost.
    #[inline(always)]
    pub fn push_new_multiple(&self, count: usize) -> I {
        AllocationBehavior::push_new_multiple(self, count).into()
    }

    /// Add a new element by shifting the element counter and assuming that the memory is already
    /// initialized.
    #[inline(always)]
    fn push_new_assume_initialized(&self, count: usize) -> usize {
        let index = self.len.get();
        self.len.modify(|t| *t += count);
        let last_index = self.len.get() - 1;
        let new_nodes_needed = (last_index + N).saturating_sub(self.capacity.get()) / N;
        if new_nodes_needed > 0 {
            self.add_tail_nodes(new_nodes_needed);
        }
        index
    }

    /// Return an iterator over the items.
    #[inline(always)]
    pub fn iter(&self) -> Iter<T, N, B> {
        IntoIterator::into_iter(self)
    }

    // FIXME: Implement it.
    // /// Return a mutable iterator over the items.
    // #[inline(always)]
    // pub fn iter_mut(&mut self) -> IterMut<T, N, B> {
    //     IntoIterator::into_iter(self)
    // }

    /// Consume the list and return an iterator over its items.
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

    /// Convert the list into a vector.
    pub fn into_vec(self) -> Vec<T> {
        let mut vec = Vec::with_capacity(self.len.get());
        vec.extend(self.into_iter());
        vec
    }

    /// Retain only the elements that satisfy the predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where F: FnMut(&T) -> bool {
        if let Some(first_node) = self.first_node.opt_item_mut() {
            let len = self.len.get();
            first_node.retain_stage1(len, f);
            let (new_size, new_capacity) = first_node.retain_stage2();
            self.len.set(new_size);
            self.capacity.set(new_capacity);
        }
    }
}


// === Indexing ===

impl<T, const N: usize, I: Index, B> std::ops::Index<I> for UnrolledLinkedList<T, N, I, B> {
    type Output = T;
    #[inline(always)]
    fn index(&self, index: I) -> &Self::Output {
        if let Some(first_node) = self.first_node.opt_item() {
            first_node.index(index.into())
        } else {
            panic!("Index out of bounds: the list is empty.")
        }
    }
}

impl<T, const N: usize, I: Index, B> IndexMut<I> for UnrolledLinkedList<T, N, I, B> {
    #[inline(always)]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        if let Some(first_node) = self.first_node.opt_item_mut() {
            first_node.index_mut(index.into())
        } else {
            panic!("Index out of bounds: the list is empty.")
        }
    }
}



// ============
// === Node ===
// ============

/// A single node in the [`UnrolledLinkedList`]. It stores up to `N` items in an array and contains
/// a pointer to the next node, if any.
pub struct Node<T, const N: usize, B> {
    _allocation_behavior: PhantomData<B>,
    items:                UnsafeCell<Vec<UnsafeCell<T>>>,
    next:                 InitCell<Option<Box<Node<T, N, B>>>>,
}

impl<T: Debug, const N: usize, B> Debug for Node<T, N, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let items = self.items().iter().map(|t| unsafe { t.unchecked_borrow() }).collect_vec();
        f.debug_struct("Node").field("items", &items).field("next", &self.next).finish()
    }
}

impl<T, const N: usize, B> Default for Node<T, N, B>
where B: AllocationBehavior<T, N>
{
    #[inline(always)]
    fn default() -> Self {
        AllocationBehavior::new_node()
    }
}

impl<T, const N: usize, B> Node<T, N, B> {
    #[inline(always)]
    fn node_count(&self) -> usize {
        self.next.opt_item().map(|t| t.node_count()).unwrap_or(0) + 1
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

    /// Get a mutable reference to the next node if it exists;
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

impl<T, const N: usize, B> Node<T, N, B>
where B: AllocationBehavior<T, N>
{
    #[inline(always)]
    fn init_next_node(&self) -> bool {
        let mut was_empty = false;
        self.next.init_if_empty(|| {
            was_empty = true;
            default()
        });
        was_empty
    }

    #[inline(always)]
    fn push(&self, offset: usize, elem: T) -> bool {
        AllocationBehavior::push_to_node(self, offset, elem)
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
            let next_node_added = self.init_next_node();
            let some_node_added =
                self.next.opt_item().unwrap().push_internal(offset - N, elem, resize_items);
            next_node_added || some_node_added
        }
    }

    #[inline(always)]
    #[allow(unconditional_recursion)]
    fn add_tail_nodes(&self, count: usize) {
        if count > 0 {
            if !self.init_next_node() {
                self.next.opt_item().unwrap().add_tail_nodes(count - 1)
            }
        }
    }
}

impl<T, const N: usize, B> Node<T, N, B>
where B: AllocationBehavior<T, N>
{
    /// For each node, retain the items. Node lengths will be shortened if necessary, however,
    /// two adjacent not-full nodes will not be merged. Merging will be performed in stage 2.
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

    /// For each node, merge two adjacent not-full nodes.
    #[inline(always)]
    pub fn retain_stage2(&mut self) -> (usize, usize) {
        while self.items().len() < N {
            let new_next_node = {
                let (items, mut next_node) = self.items_and_next_mut();
                next_node.as_mut().and_then(|next| {
                    let end = next.items().len().min(N - items.len());
                    items.extend(next.items_mut().drain(0..end));
                    let empty_next_node = next.items().is_empty();
                    let next_next_node = next.next.opt_item_mut();
                    empty_next_node.then(|| next_next_node.map(|t| mem::take(t)))
                })
            };
            if let Some(new_next_node) = new_next_node {
                self.next.set_value(new_next_node)
            }
            if !self.next.has_item() {
                break;
            }
        }
        match self.next.opt_item_mut() {
            None => {
                let len = self.items().len();
                AllocationBehavior::preallocate_missing_node_items(self);
                (len, N)
            }
            Some(next) => {
                let (next_len, next_capacity) = next.retain_stage2();
                (N + next_len, N + next_capacity)
            }
        }
    }
}

impl<T, const N: usize, B> std::ops::Index<usize> for Node<T, N, B> {
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

impl<T, const N: usize, B> IndexMut<usize> for Node<T, N, B> {
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
    /// Add new items at the end and return the index of the first added item. If the list uses
    /// preallocated memory, this operation is almost zero-cost.
    fn push_new_multiple<I: Index>(
        array: &UnrolledLinkedList<T, N, I, Self>,
        count: usize,
    ) -> usize;
    /// Create a new node.
    fn new_node() -> Node<T, N, Self>;
    /// Push new item to the node. Return `true` if a new node was added.
    fn push_to_node(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool;
    /// Clear the memory in all the nodes. If the allocation behavior pre-allocates items, the
    /// memory will be populated with new instances.
    fn clear_nodes(node: &mut Node<T, N, Self>);
    /// Preallocate missing items in the node. If the list was configured not to preallocate
    /// items, this is a no-op.
    fn preallocate_missing_node_items(node: &mut Node<T, N, Self>);
}

impl<T: Default, const N: usize> AllocationBehavior<T, N> for prealloc::Default {
    #[inline(always)]
    fn push_new_multiple<I: Index>(
        array: &UnrolledLinkedList<T, N, I, Self>,
        count: usize,
    ) -> usize {
        array.push_new_assume_initialized(count)
    }

    #[inline(always)]
    fn new_node() -> Node<T, N, Self> {
        let mut items = Vec::with_capacity(N);
        for _ in 0..N {
            items.push(default());
        }
        let items = UnsafeCell::new(items);
        let next = default();
        let _allocation_behavior = PhantomData;
        Node { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_node(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool {
        node.push_internal(offset, elem, false)
    }

    #[inline(always)]
    fn clear_nodes(node: &mut Node<T, N, Self>) {
        node.items_mut().clear();
        for _ in 0..N {
            node.items_mut().push(default());
        }
        if let Some(next) = node.next.opt_item_mut() {
            Self::clear_nodes(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_node_items(node: &mut Node<T, N, Self>) {
        let len = node.items().len();
        for _ in len..N {
            node.items_mut().push(default());
        }
    }
}

impl<T: Zeroable, const N: usize> AllocationBehavior<T, N> for prealloc::Zeroed {
    #[inline(always)]
    fn push_new_multiple<I: Index>(
        array: &UnrolledLinkedList<T, N, I, Self>,
        count: usize,
    ) -> usize {
        array.push_new_assume_initialized(count)
    }

    #[inline(always)]
    fn new_node() -> Node<T, N, Self> {
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
        Node { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_node(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool {
        node.push_internal(offset, elem, false)
    }

    #[inline(always)]
    fn clear_nodes(node: &mut Node<T, N, Self>) {
        unsafe { std::intrinsics::volatile_set_memory(node.items_mut().as_mut_ptr(), 0, N) }
        if let Some(next) = node.next.opt_item_mut() {
            Self::clear_nodes(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_node_items(node: &mut Node<T, N, Self>) {
        let len = node.items().len();
        for _ in len..N {
            node.items_mut().push(Zeroable::zeroed());
        }
    }
}

impl<T: Default, const N: usize> AllocationBehavior<T, N> for prealloc::Disabled {
    #[inline(always)]
    fn push_new_multiple<I: Index>(
        array: &UnrolledLinkedList<T, N, I, Self>,
        count: usize,
    ) -> usize {
        let index = array.len();
        for _ in 0..count {
            array.push(default());
        }
        index
    }

    #[inline(always)]
    fn new_node() -> Node<T, N, Self> {
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
        Node { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_node(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool {
        node.push_internal(offset, elem, true)
    }

    #[inline(always)]
    fn clear_nodes(node: &mut Node<T, N, Self>) {
        node.items_mut().clear();
        if let Some(next) = node.next.opt_item_mut() {
            Self::clear_nodes(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_node_items(_node: &mut Node<T, N, Self>) {}
}



// =================
// === Iterators ===
// =================

// === Iter for &T ===

/// An iterator over immutable references to [`UnrolledLinkedList`] items.
#[derive(Debug)]
pub struct Iter<'a, T, const N: usize, B> {
    node:        &'a Node<T, N, B>,
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
            self.node = self.node.next.opt_item().unwrap();
            self.next_offset = 0;
            self.max_offset -= N;
        }
        let item = self.node.index(self.next_offset);
        self.next_offset += 1;
        Some(item)
    }
}

impl<'a, T, const N: usize, I, B> IntoIterator for &'a UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T, N>,
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T, N, B>;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            node:        self.get_or_init_first_node(),
            next_offset: 0,
            max_offset:  self.len.get(),
        }
    }
}


// === Iter for T ===

/// An iterator over owned [`UnrolledLinkedList`] items.
#[derive(Debug)]
pub struct IntoIter<T, const N: usize, B> {
    next_node:    Option<Box<Node<T, N, B>>>,
    current_node: std::vec::IntoIter<UnsafeCell<T>>,
    next_offset:  usize,
    max_offset:   usize,
}

impl<T, const N: usize, I, B> IntoIterator for UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T, N>,
{
    type Item = T;
    type IntoIter = IntoIter<T, N, B>;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.init_first_node_if_empty();
        let first_node = self.first_node.into_inner().unwrap();
        let current_node = first_node.items.into_inner().into_iter();
        let next_node = first_node.next.into_inner();
        IntoIter { next_node, current_node, next_offset: 0, max_offset: self.len.get() }
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
            if let Some(next_node) = self.next_node.take() {
                self.current_node = next_node.items.into_inner().into_iter();
                self.next_node = next_node.next.into_inner();
            }
        }
        self.next_offset += 1;
        self.current_node.next().map(|t| t.into_inner())
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
            $f(&mut UnrolledLinkedList::<usize, 1, usize, prealloc::Default>::new());
            $f(&mut UnrolledLinkedList::<usize, 1, usize, prealloc::Zeroed>::new());
            $f(&mut UnrolledLinkedList::<usize, 1, usize, prealloc::Disabled>::new());

            $f(&mut UnrolledLinkedList::<usize, 2, usize, prealloc::Default>::new());
            $f(&mut UnrolledLinkedList::<usize, 2, usize, prealloc::Zeroed>::new());
            $f(&mut UnrolledLinkedList::<usize, 2, usize, prealloc::Disabled>::new());

            $f(&mut UnrolledLinkedList::<usize, 4, usize, prealloc::Default>::new());
            $f(&mut UnrolledLinkedList::<usize, 4, usize, prealloc::Zeroed>::new());
            $f(&mut UnrolledLinkedList::<usize, 4, usize, prealloc::Disabled>::new());

            $f(&mut UnrolledLinkedList::<usize, 8, usize, prealloc::Default>::new());
            $f(&mut UnrolledLinkedList::<usize, 8, usize, prealloc::Zeroed>::new());
            $f(&mut UnrolledLinkedList::<usize, 8, usize, prealloc::Disabled>::new());

            $f(&mut UnrolledLinkedList::<usize, 16, usize, prealloc::Default>::new());
            $f(&mut UnrolledLinkedList::<usize, 16, usize, prealloc::Zeroed>::new());
            $f(&mut UnrolledLinkedList::<usize, 16, usize, prealloc::Disabled>::new());

            // $f(&mut UnrolledLinkedList::<usize, 2, prealloc::Zeroed>::new());
        };
    }


    // === Push ===

    #[test]
    fn test_push() {
        test_all_configs!(test_template_push);
    }

    fn test_template_push<const N: usize, I, B>(array: &mut UnrolledLinkedList<usize, N, I, B>)
    where
        I: Index,
        B: AllocationBehavior<usize, N>, {
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

    fn test_template_clear<const N: usize, I, B>(array: &mut UnrolledLinkedList<usize, N, I, B>)
    where
        I: Index,
        B: AllocationBehavior<usize, N>, {
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

    fn test_template_retain<const N: usize, I, B>(array: &mut UnrolledLinkedList<usize, N, I, B>)
    where
        I: Index,
        B: AllocationBehavior<usize, N>, {
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
