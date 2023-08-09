//! A variation on the linked list which stores multiple elements in each [`Node`]. It can
//! dramatically increase cache performance, while decreasing the memory overhead associated with
//! storing list metadata such as references. It is related to the B-tree. To learn more, see:
//! https://en.wikipedia.org/wiki/Unrolled_linked_list

use crate::prelude::*;

use crate::prelude::Index as IndexOps;

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
    ///
    /// # Pushing new elements during iteration
    /// Please note that as adding new elements does not require mutable access to self, it is
    /// possible to add new elements during iteration. However, the iterator will not yield the new
    /// elements. If you wish to iterate over the new elements, you need to create a new iterator.
    pub struct UnrolledLinkedList
    [T, N, I, B][T, const N: usize, I, B][T, const N: usize, I = usize, B = prealloc::Disabled] {
        len:        Cell<usize>,
        capacity:   Cell<usize>,
        first_node: InitCell<ZeroableOption<Node<T, N, B>>>,
        _index_tp:  ZST<I>,
    }
}

impl<T, const N: usize, I, B> Default for UnrolledLinkedList<T, N, I, B> {
    fn default() -> Self {
        Self::new()
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

impl<T: Debug, const N: usize, I: Index, B> Display for UnrolledLinkedList<T, N, I, B>
where B: AllocationBehavior<T>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}


// === Node Management ===

impl<T, const N: usize, I, B> UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T>,
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

    #[inline(always)]
    fn add_tail_nodes(&self, count: usize) {
        self.get_or_init_first_node().add_tail_nodes(count);
        self.capacity.modify(|t| *t += N * count);
    }
}


// === Public API ===

impl<T, const N: usize, I, B> UnrolledLinkedList<T, N, I, B> {
    /// Constructor.
    pub fn new() -> Self {
        if N == 0 {
            panic!("UnrolledLinkedList: N must be greater than 0.");
        }
        let len = default();
        let capacity = default();
        let first_node = default();
        let _index_tp = default();
        Self { len, capacity, first_node, _index_tp }
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
    B: AllocationBehavior<T>,
{
    /// Deallocate nodes that are not used.
    pub fn shrink_to_fit(&mut self) {
        self.shrink_to_internal(self.len())
    }

    /// Shrink the capacity of the list with a lower bound.
    ///
    /// The capacity will remain at least as large as both the length and the supplied value.
    pub fn shrink_to(&mut self, capacity: usize) {
        self.shrink_to_internal(capacity);
    }

    /// This is the internal implementation of [`Self::shrink_to_fit`] and [`Self::shrink_to`]. It
    /// is defined separately to allow inlining in both methods.
    #[inline(always)]
    fn shrink_to_internal(&mut self, capacity: usize) {
        let new_capacity = self.len().max(capacity);
        if new_capacity == 0 {
            self.first_node.set_default();
        } else if let Some(first_node) = self.first_node.opt_item_mut() {
            let new_capacity = first_node.shrink_to_fit(new_capacity);
            self.capacity.set(new_capacity)
        }
    }

    /// Number of nodes used to store the items.
    #[inline(always)]
    pub fn node_count(&self) -> usize {
        self.first_node.opt_item().map_or_default(|t| t.node_count())
    }

    /// Get the element at the given index if the index is in bounds.
    #[inline(always)]
    pub fn get(&self, index: I) -> Option<&T> {
        let uindex: usize = index.into();
        (uindex < self.len()).then(|| &self[index])
    }

    /// Get the element at the given index if the index is in bounds.
    #[inline(always)]
    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        let uindex: usize = index.into();
        (uindex < self.len()).then(|| &mut self[index])
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

    /// Add a new item at the end and return its index. The exact new item shape depends on the
    /// allocation behavior (the [`Self::B`] parametrization). In case of [`prealloc::Disabled]` or
    /// [`prealloc::Default`], the new element will be initialized with its default value. in case
    /// of [`prealloc::Zeroed`], the new element will be initialized with zeroed memory.
    ///
    /// If the list uses preallocated memory and there is enough space, this operation is almost
    /// zero-cost.
    #[inline(always)]
    pub fn push_new(&self) -> I {
        self.push_new_multiple(1)
    }

    /// Add several new items at the end and return the index of the first added item. The exact
    /// mew item shape depends on the allocation behavior (the [`Self::B`] parametrization). In case
    /// of [`prealloc::Disabled]` or [`prealloc::Default`], the new element will be initialized
    /// with its default value. in case of [`prealloc::Zeroed`], the new element will be
    /// initialized with zeroed memory.
    ///
    /// If the list uses preallocated memory and there is enough space, this operation is almost
    /// zero-cost.
    #[inline(always)]
    pub fn push_new_multiple(&self, count: usize) -> I {
        AllocationBehavior::push_new_multiple(self, count).into()
    }

    /// Add a new element by increasing the element counter and assuming that the memory is already
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

    /// Retain only the elements that satisfy the predicate.
    pub fn retain<F>(&mut self, f: F)
    where F: FnMut(&T) -> bool {
        if let Some(first_node) = self.first_node.opt_item_mut() {
            let len = self.len.get();
            first_node.retain_stage1_retain_nodes_elements(len, f);
            let (new_size, new_capacity) = first_node.retain_stage2_merge_sparse_nodes();
            self.len.set(new_size);
            self.capacity.set(new_capacity);
        }
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
}

impl<T, const N: usize, I, B> Clearable for UnrolledLinkedList<T, N, I, B>
where B: AllocationBehavior<T>
{
    #[inline(always)]
    fn clear(&mut self) {
        self.len.set(0);
        if let Some(first_node) = self.first_node.opt_item_mut() {
            AllocationBehavior::clear_nodes(first_node);
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

type NodeItem<T> = UnsafeCell<T>;

/// A single node in the [`UnrolledLinkedList`]. It stores up to `N` items in an array and contains
/// a pointer to the next node, if any.
pub struct Node<T, const N: usize, B> {
    _allocation_behavior: ZST<B>,
    // Vec is used here instead of an array in order to re-use some of its functionality, such as
    // efficient retaining implementation. It is initialized like an array and never allocated or
    // deallocates memory.
    items:                UnsafeCell<Vec<NodeItem<T>>>,
    next:                 InitCell<Option<Box<Node<T, N, B>>>>,
}

impl<T: Debug, const N: usize, B> Debug for Node<T, N, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut items = vec![];
        for i in 0..self.items().len() {
            items.push(self.unchecked_index_this_node_only(i))
        }
        f.debug_struct("Node").field("items", &items).field("next", &self.next).finish()
    }
}

impl<T, const N: usize, B> Default for Node<T, N, B>
where B: AllocationBehavior<T>
{
    #[inline(always)]
    fn default() -> Self {
        AllocationBehavior::new_node()
    }
}

impl<T, const N: usize, B> Node<T, N, B> {
    /// Count of all nodes starting at this node, including this node.
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

    /// Get a mutable reference to items and the next node if it exists.
    #[inline(always)]
    fn items_and_next_mut(&mut self) -> (&mut Vec<NodeItem<T>>, Option<&mut Box<Self>>) {
        let next = self.next.opt_item_mut();
        // # Safety
        // Self is mutably borrowed, so this call is safe.
        #[allow(unsafe_code)]
        let items = unsafe { self.items.unchecked_borrow_mut() };
        (items, next)
    }

    #[inline(always)]
    fn items(&self) -> &Vec<NodeItem<T>> {
        // # Safety
        // The only function that does not require mutable self access which mutably borrows items
        // is [`Self::push_internal`], but it borrows newly created item only.
        #[allow(unsafe_code)]
        unsafe {
            self.items.unchecked_borrow()
        }
    }

    #[inline(always)]
    fn items_mut(&mut self) -> &mut Vec<NodeItem<T>> {
        // # Safety
        // The only function that does not require mutable self access which mutably borrows items
        // is [`Self::push_internal`], but it borrows newly created item only.
        #[allow(unsafe_code)]
        unsafe {
            self.items.unchecked_borrow_mut()
        }
    }
}

impl<T, const N: usize, B> Node<T, N, B>
where B: AllocationBehavior<T>
{
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
                // its new size here, as the value will be written below. This is safe with the
                // current Vec implementation, as it only changes its length (`usize`) field.
                #[allow(unsafe_code)]
                unsafe {
                    self.items.unchecked_borrow_mut().set_len(offset + 1);
                }
            }

            // # Safety
            // The item at `offset` either does not exist yet or was removed, so there are no
            // mutable references to it.
            #[allow(unsafe_code)]
            unsafe {
                *self.items()[offset].unchecked_borrow_mut() = elem
            };
            false
        } else {
            let next_node_added = self.next.init_default_if_empty();
            let next_node = self.next.opt_item().unwrap();
            let sub_node_added = next_node.push_internal(offset - N, elem, resize_items);
            next_node_added || sub_node_added
        }
    }

    #[inline(always)]
    #[allow(unconditional_recursion)]
    fn add_tail_nodes(&self, count: usize) {
        if count > 0 {
            let sub_count = if self.next.init_default_if_empty() { count - 1 } else { count };
            self.next.opt_item().unwrap().add_tail_nodes(sub_count);
        }
    }

    /// For each node, retain the items. Node lengths will be shortened if necessary, however,
    /// two adjacent not-full nodes will not be merged. Merging will be performed in stage 2.
    #[inline(always)]
    pub fn retain_stage1_retain_nodes_elements<F>(&mut self, len: usize, mut f: F)
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
        // mutably borrows the newly added item for its lifetime.
        #[allow(unsafe_code)]
        self.items_mut().retain(|t| f(unsafe { t.unchecked_borrow() }));
        if let Some(next) = self.next.opt_item_mut() {
            next.retain_stage1_retain_nodes_elements(len - N, f);
        }
    }

    /// For each node, merge two adjacent not-full nodes.
    #[inline(always)]
    pub fn retain_stage2_merge_sparse_nodes(&mut self) -> (usize, usize) {
        while self.items().len() < N {
            let new_next_node = {
                let (items, mut next_node) = self.items_and_next_mut();
                next_node.as_mut().and_then(|next| {
                    let end = next.items().len().min(N - items.len());
                    items.extend(next.items_mut().drain(0..end));
                    let empty_next_node = next.items().is_empty();
                    let next_next_node = next.next.opt_item_mut();
                    empty_next_node.then(|| next_next_node.map(mem::take))
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
                let (next_len, next_capacity) = next.retain_stage2_merge_sparse_nodes();
                (N + next_len, N + next_capacity)
            }
        }
    }
}

impl<T, const N: usize, B> Node<T, N, B> {
    /// Get reference to the element at `index` without checking bounds of the node.
    #[inline(always)]
    fn unchecked_index_this_node_only(&self, index: usize) -> &T {
        // # Safety
        // All mutable borrows of an item require `self` to be mutably borrowed as well, so
        // there are no other mutable borrows currently. The only exception is
        // [`Self::push`] which mutably borrows the newly added item.
        #[allow(unsafe_code)]
        unsafe {
            self.items()[index].unchecked_borrow()
        }
    }

    /// Get mutable reference to the element at `index` without checking bounds of the node.
    #[inline(always)]
    fn unchecked_index_this_node_only_mut(&mut self, index: usize) -> &mut T {
        // # Safety
        // All mutable borrows of an item require `self` to be mutably borrowed as well, so
        // there are no other mutable borrows currently. The only exception is
        // [`Self::push`] which mutably borrows the newly added item.
        #[allow(unsafe_code)]
        unsafe {
            self.items()[index].unchecked_borrow_mut()
        }
    }
}

impl<T, const N: usize, B> std::ops::Index<usize> for Node<T, N, B> {
    type Output = T;
    #[inline(always)]
    fn index(&self, offset: usize) -> &Self::Output {
        if offset < N {
            self.unchecked_index_this_node_only(offset)
        } else {
            self.next.opt_item().unwrap().index(offset - N)
        }
    }
}

impl<T, const N: usize, B> IndexMut<usize> for Node<T, N, B> {
    #[inline(always)]
    fn index_mut(&mut self, offset: usize) -> &mut Self::Output {
        if offset < N {
            self.unchecked_index_this_node_only_mut(offset)
        } else {
            self.next.opt_item_mut().unwrap().index_mut(offset - N)
        }
    }
}



// ==========================
// === AllocationBehavior ===
// ==========================

/// Operations that differ depending on the preallocation behavior.
pub trait AllocationBehavior<T>: Sized {
    /// Add new items at the end and return the index of the first added item. If the list uses
    /// preallocated memory, this operation is almost zero-cost.
    fn push_new_multiple<const N: usize, I: Index>(
        array: &UnrolledLinkedList<T, N, I, Self>,
        count: usize,
    ) -> usize;
    /// Create a new node.
    fn new_node<const N: usize>() -> Node<T, N, Self>;
    /// Push new item to the node. Return `true` if a new node was added.
    fn push_to_node<const N: usize>(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool;
    /// Clear the memory in all the nodes. If the allocation behavior pre-allocates items, the
    /// memory will be populated with new instances.
    fn clear_nodes<const N: usize>(node: &mut Node<T, N, Self>);
    /// Preallocate missing items in the node. If the list was configured not to preallocate
    /// items, this is a no-op.
    fn preallocate_missing_node_items<const N: usize>(node: &mut Node<T, N, Self>);
}

impl<T: Default> AllocationBehavior<T> for prealloc::Default {
    #[inline(always)]
    fn push_new_multiple<const N: usize, I: Index>(
        array: &UnrolledLinkedList<T, N, I, Self>,
        count: usize,
    ) -> usize {
        array.push_new_assume_initialized(count)
    }

    #[inline(always)]
    fn new_node<const N: usize>() -> Node<T, N, Self> {
        let mut items = Vec::with_capacity(N);
        for _ in 0..N {
            items.push(default());
        }
        let items = UnsafeCell::new(items);
        let next = default();
        let _allocation_behavior = ZST();
        Node { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_node<const N: usize>(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool {
        node.push_internal(offset, elem, false)
    }

    #[inline(always)]
    fn clear_nodes<const N: usize>(node: &mut Node<T, N, Self>) {
        node.items_mut().clear();
        for _ in 0..N {
            node.items_mut().push(default());
        }
        if let Some(next) = node.next.opt_item_mut() {
            Self::clear_nodes(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_node_items<const N: usize>(node: &mut Node<T, N, Self>) {
        let len = node.items().len();
        for _ in len..N {
            node.items_mut().push(default());
        }
    }
}

impl<T: Zeroable> AllocationBehavior<T> for prealloc::Zeroed {
    #[inline(always)]
    fn push_new_multiple<const N: usize, I: Index>(
        array: &UnrolledLinkedList<T, N, I, Self>,
        count: usize,
    ) -> usize {
        array.push_new_assume_initialized(count)
    }

    #[inline(always)]
    fn new_node<const N: usize>() -> Node<T, N, Self> {
        let items = {
            let layout = std::alloc::Layout::array::<NodeItem<T>>(N).unwrap();
            // # Safety
            // The bound `T: Zeroable` guarantees that `T` can be initialized with zeroed memory.
            #[allow(unsafe_code)]
            unsafe {
                Vec::from_raw_parts(std::alloc::alloc_zeroed(layout) as *mut _, N, N)
            }
        };
        let items = UnsafeCell::new(items);
        let next = default();
        let _allocation_behavior = ZST();
        Node { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_node<const N: usize>(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool {
        node.push_internal(offset, elem, false)
    }

    #[inline(always)]
    fn clear_nodes<const N: usize>(node: &mut Node<T, N, Self>) {
        // The following line is needed to call drop on the items.
        node.items_mut().clear();
        // # Safety
        // The bound `T: Zeroable` guarantees that `T` can be initialized with zeroed memory.
        // Usage of the `volatile_set_memory` intrinsic is used in order for the compiler not to
        // optimize it away and perform the zeroing always.
        //
        // Also, setting the Vec len to `N` is safe, as [`Vec::clear`] does not deallocate the
        // memory and the [`T: Zeroable`] bound guarantees that the memory is safe to be used
        #[allow(unsafe_code)]
        unsafe {
            std::intrinsics::volatile_set_memory(node.items_mut().as_mut_ptr(), 0, N);
            node.items_mut().set_len(N);
        }
        if let Some(next) = node.next.opt_item_mut() {
            Self::clear_nodes(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_node_items<const N: usize>(node: &mut Node<T, N, Self>) {
        let len = node.items().len();
        // TODO: This might be implemented more efficiently by zeroing the whole memory chunk.
        for _ in len..N {
            node.items_mut().push(Zeroable::zeroed());
        }
    }
}

impl<T: Default> AllocationBehavior<T> for prealloc::Disabled {
    #[inline(always)]
    fn push_new_multiple<const N: usize, I: Index>(
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
    fn new_node<const N: usize>() -> Node<T, N, Self> {
        let items = Vec::with_capacity(N);
        let items = UnsafeCell::new(items);
        let next = default();
        let _allocation_behavior = ZST();
        Node { _allocation_behavior, items, next }
    }

    #[inline(always)]
    fn push_to_node<const N: usize>(node: &Node<T, N, Self>, offset: usize, elem: T) -> bool {
        node.push_internal(offset, elem, true)
    }

    #[inline(always)]
    fn clear_nodes<const N: usize>(node: &mut Node<T, N, Self>) {
        node.items_mut().clear();
        if let Some(next) = node.next.opt_item_mut() {
            Self::clear_nodes(next);
        }
    }

    #[inline(always)]
    fn preallocate_missing_node_items<const N: usize>(_node: &mut Node<T, N, Self>) {}
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
    B: AllocationBehavior<T>,
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
    current_node: std::vec::IntoIter<NodeItem<T>>,
    next_offset:  usize,
    max_offset:   usize,
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

impl<T, const N: usize, I, B> IntoIterator for UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T>,
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



// ====================
// === FromIterator ===
// ====================

impl<T, const N: usize, I, B> FromIterator<T> for UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T>,
{
    #[inline(always)]
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        let mut list = Self::new();
        list.extend(iter);
        list
    }
}



// ==============
// === Extend ===
// ==============

impl<T, const N: usize, I, B> Extend<T> for UnrolledLinkedList<T, N, I, B>
where
    I: Index,
    B: AllocationBehavior<T>,
{
    fn extend<Iter: IntoIterator<Item = T>>(&mut self, iter: Iter) {
        for item in iter {
            self.push(item);
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    /// Testing different list configurations and initialization methods.
    macro_rules! test_multi_config {
        ($tp:ident, $f:ident) => {
            test_multi_config_with_cons! { $tp, $f, new }
            test_multi_config_with_cons! { $tp, $f, zeroed }
        };
    }

    macro_rules! test_multi_config_with_cons {
        ($tp:ident, $f:ident, $cons:ident) => {
            test_multi_config_with_cons_and_sizes! { $tp, $f, $cons, [1, 2, 3, 4, 7, 8, 16] }
        };
    }

    macro_rules! test_multi_config_with_cons_and_sizes {
        ($tp:ident, $f:ident, $cons:ident, [$($size:tt),*]) => {
            $(
                $f(&mut UnrolledLinkedList::<$tp, $size, usize, prealloc::Default>::$cons());
                $f(&mut UnrolledLinkedList::<$tp, $size, usize, prealloc::Zeroed>::$cons());
                $f(&mut UnrolledLinkedList::<$tp, $size, usize, prealloc::Disabled>::$cons());
            )*
        };
    }


    // === Node Structure ===

    #[test]
    fn test_node_structure() {
        test_node_structure_template(UnrolledLinkedList::new());
        test_node_structure_template(Zeroable::zeroed());
    }

    fn test_node_structure_template(
        mut list: UnrolledLinkedList<usize, 2, usize, prealloc::Zeroed>,
    ) {
        // []
        assert_eq!(list.len(), 0);
        assert_eq!(list.capacity(), 0);
        assert_eq!(list.node_count(), 0);
        list.push(1);
        // [1, _]
        assert_eq!(list.len(), 1);
        assert_eq!(list.capacity(), 2);
        assert_eq!(list.node_count(), 1);
        list.push(2);
        // [1, 2]
        assert_eq!(list.len(), 2);
        assert_eq!(list.capacity(), 2);
        assert_eq!(list.node_count(), 1);
        list.push(3);
        // [1, 2] -> [3, _]
        assert_eq!(list.len(), 3);
        assert_eq!(list.capacity(), 4);
        assert_eq!(list.node_count(), 2);
        list.retain(|t| *t < 2);
        // [1, _]
        assert_eq!(list.len(), 1);
        assert_eq!(list.capacity(), 2);
        assert_eq!(list.node_count(), 1);
        list.retain(|_| false);
        // [_, _]
        assert_eq!(list.len(), 0);
        assert_eq!(list.capacity(), 2);
        assert_eq!(list.node_count(), 1);
        list.push_new_multiple(9);
        // [0, 0] -> [0, 0] -> [0, 0] -> [0, 0] -> [0, _]
        assert_eq!(list.len(), 9);
        assert_eq!(list.capacity(), 10);
        assert_eq!(list.node_count(), 5);
        list.clear();
        // [_, _] -> [_, _] -> [_, _] -> [_, _] -> [_, _]
        assert_eq!(list.len(), 0);
        assert_eq!(list.capacity(), 10);
        assert_eq!(list.node_count(), 5);
        list.push(1);
        // [1, _] -> [_, _] -> [_, _] -> [_, _] -> [_, _]
        list.shrink_to_fit();
        // [1, _]
        assert_eq!(list.len(), 1);
        assert_eq!(list.capacity(), 2);
        assert_eq!(list.node_count(), 1);
        list.shrink_to(0);
        // [1, _]
        assert_eq!(list.len(), 1);
        assert_eq!(list.capacity(), 2);
        assert_eq!(list.node_count(), 1);
    }



    // === Push ===

    #[test]
    fn test_push() {
        test_multi_config!(usize, test_template_push);
    }

    fn test_template_push<const N: usize, I, B>(list: &mut UnrolledLinkedList<usize, N, I, B>)
    where
        I: Index,
        B: AllocationBehavior<usize>, {
        list.push(1);
        assert_eq!(&list.to_vec(), &[1]);
        list.push(2);
        assert_eq!(&list.to_vec(), &[1, 2]);
        list.push(3);
        assert_eq!(&list.to_vec(), &[1, 2, 3]);
        list.push(4);
        assert_eq!(&list.to_vec(), &[1, 2, 3, 4]);
        list.push(5);
        assert_eq!(&list.to_vec(), &[1, 2, 3, 4, 5]);
        list.push_new();
        list.push_new();
        assert_eq!(&list.to_vec(), &[1, 2, 3, 4, 5, 0, 0]);
        list.push_new_multiple(3);
        assert_eq!(&list.to_vec(), &[1, 2, 3, 4, 5, 0, 0, 0, 0, 0]);
    }


    // === Clear ===

    #[test]
    fn test_clear() {
        test_multi_config!(usize, test_template_clear);
    }

    fn test_template_clear<const N: usize, I, B>(list: &mut UnrolledLinkedList<usize, N, I, B>)
    where
        I: Index,
        B: AllocationBehavior<usize>, {
        list.extend([1, 2, 3, 4, 5].iter().copied());
        assert_eq!(&list.to_vec(), &[1, 2, 3, 4, 5]);
        list.clear();
        assert!(&list.is_empty());
        list.push_new();
        list.push_new();
        list.push_new();
        list.push_new();
        list.push_new();
        assert_eq!(&list.to_vec(), &[0, 0, 0, 0, 0]);
    }


    // === Retain ===

    #[test]
    fn test_retain() {
        test_multi_config!(usize, test_template_retain);
    }

    fn test_template_retain<const N: usize, I, B>(list: &mut UnrolledLinkedList<usize, N, I, B>)
    where
        I: Index,
        B: AllocationBehavior<usize>, {
        list.extend([1, 2, 3, 4, 5].iter().copied());
        assert_eq!(&list.to_vec(), &[1, 2, 3, 4, 5]);
        list.retain(|t| *t > 0);
        assert_eq!(&list.to_vec(), &[1, 2, 3, 4, 5]);
        list.retain(|t| *t > 3);
        assert_eq!(&list.to_vec(), &[4, 5]);
        list.extend([6, 7, 8].iter().copied());
        assert_eq!(&list.to_vec(), &[4, 5, 6, 7, 8]);
        list.retain(|t| t % 2 == 0);
        assert_eq!(&list.to_vec(), &[4, 6, 8]);
        list.retain(|_| false);
        assert!(&list.is_empty());
    }


    // === List of bools ===

    /// Although, we are not using uninitialized memory, we are performing a small sanity check
    /// here, as `bool` is one of the special types that can't be used with uninitialized memory,
    /// causing undefined behavior, as described here:
    /// https://doc.rust-lang.org/stable/std/mem/union.MaybeUninit.html.
    #[test]
    fn test_bool_ops() {
        test_multi_config!(bool, test_template_bool_ops);
    }

    fn test_template_bool_ops<const N: usize, I, B>(list: &mut UnrolledLinkedList<bool, N, I, B>)
    where
        I: Index,
        B: AllocationBehavior<bool>, {
        list.push(true);
        assert_eq!(&list.to_vec(), &[true]);
        list.push(false);
        assert_eq!(&list.to_vec(), &[true, false]);
        list.push(true);
        assert_eq!(&list.to_vec(), &[true, false, true]);
        list.push(false);
        assert_eq!(&list.to_vec(), &[true, false, true, false]);
        list.push(true);
        assert_eq!(&list.to_vec(), &[true, false, true, false, true]);
        list.push_new();
        list.push_new();
        assert_eq!(&list.to_vec(), &[true, false, true, false, true, false, false]);
        list.push_new_multiple(2);
        assert_eq!(&list.to_vec(), &[true, false, true, false, true, false, false, false, false]);
        list.retain(|t| *t);
        assert_eq!(&list.to_vec(), &[true, true, true]);
        list.retain(|t| !t);
        assert!(&list.is_empty());
    }
}
