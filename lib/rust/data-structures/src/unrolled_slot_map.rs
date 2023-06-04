//! A slot map based on [`UnrolledLinkedList`].

use crate::prelude::*;

use crate::unrolled_linked_list;
use unrolled_linked_list::AllocationBehavior;
use unrolled_linked_list::UnrolledLinkedList;



// ======================
// === VersionedIndex ===
// ======================

/// An index in the [`UnrolledSlotMap`]. The index is versioned, so it is invalid if the item
/// was removed from the map. The [`Kind`] parameter is used to introduce binding between indexes
/// and a map instance, so you can be sure that the index is not used with a different map instance.
///
/// The default parametrization of [`VersionedIndex`] is `()` which allows you to skip the extra
/// type-level security check described above.
#[derive(Derivative)]
#[derivative(
    Copy(bound = ""),
    Clone(bound = ""),
    Default(bound = ""),
    Debug(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = "")
)]
pub struct VersionedIndex<Kind = ()> {
    index:   usize,
    version: usize,
    _kind:   PhantomData<Kind>,
}

#[allow(unsafe_code)]
unsafe impl<Kind> Zeroable for VersionedIndex<Kind> {}

impl<Kind> VersionedIndex<Kind> {
    fn new(index: usize, version: usize) -> Self {
        Self { index, version, _kind: PhantomData }
    }
}



// ============
// === Slot ===
// ============

/// A slot, versioned value in the slot map.
#[derive(Debug, Default, Zeroable)]
pub struct Slot<Item> {
    value:   Item,
    /// Version of the slot. An even version (0, 2, 4, ...) means that the slot is occupied. An odd
    /// version (1, 3, 5, ...) means that the slot is free.
    version: Cell<usize>,
}

impl<Item> Slot<Item> {
    fn version(&self) -> usize {
        self.version.get()
    }

    fn is_occupied(&self) -> bool {
        self.version.get() % 2 == 0
    }
}



// =======================
// === UnrolledSlotMap ===
// =======================

/// A slot map based on [`UnrolledLinkedList`]. A slot map provides persistent unique keys to access
/// stored values, which means that you can remove a value from it without invalidating other keys.
/// Every key is versioned, so removing a value is simply altering the version of the slot, so the
/// existing key cannot be used to access it anymore.
///
/// It has several important properties:
/// - It allows invalidation of indexes without requiring mutable self-reference
///   ([Self::invalidate`]). This will not remove the value, but will invalidate the index, so it
///   will be re-used in the future.
/// - It allows reserving ([`Self::reserve`]) new indexes without requiring mutable self-reference.
///   Reserving an index will provide you with a new slot that will either be with a default value
///   (according to the allocation behavior, the [`B`] type parameter) or will re-use a slot of a
///   previously freed key. If the key was freed with [`Self::invalidate`], the slot will contain
///   the previous value.
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
pub struct UnrolledSlotMap<Item, const N: usize, Kind = (), B = prealloc::Disabled> {
    free_indexes: OptRefCell<Vec<usize>>,
    slots:        Box<UnrolledLinkedList<Slot<Item>, N, usize, B>>,
    _kind:        PhantomData<Kind>,
}

impl<T: Debug, const N: usize, Kind, B> Debug for UnrolledSlotMap<T, N, Kind, B>
where B: AllocationBehavior<Slot<T>>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.slots.iter().enumerate().filter_map(|(i, t)| {
                t.is_occupied().then(|| (format!("{i}v{}", t.version()), &t.value))
            }))
            .finish()
    }
}

impl<Item, const N: usize, Kind, B> UnrolledSlotMap<Item, N, Kind, B>
where B: AllocationBehavior<Slot<Item>>
{
    /// Constructor.
    #[inline(always)]
    pub fn new() -> Self {
        default()
    }

    /// Return the number of elements in the map. This will iterate over all elements, skipping
    /// freed slots, so it's cost is `O(n)`, where `n` is the number of occupied and freed slots.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    /// Check if the map is empty. This will iterate over all elements, skipping freed slots, so
    /// it's cost is `O(n)`, where `n` is the number of occupied and freed slots.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Check the capacity of the map. In case you insert more elements than the capacity, the map
    /// will allocate additional memory under the hood.
    #[inline(always)]
    pub fn capacity(&self) -> usize {
        self.slots.capacity()
    }

    /// Invalidate the index. This will not remove the value, but you will not be able to use this
    /// index to access the value anymore. The index will be re-used in the future.
    #[inline(always)]
    pub fn invalidate(&self, id: VersionedIndex<Kind>) -> bool {
        let ok = self.slot(id).map(|slot| slot.version.update(|v| v + 1)).is_some();
        if ok {
            self.free_indexes.borrow_mut().push(id.index);
        }
        ok
    }

    /// Reserve a new index. This will either initialize a new slot with a default value (according
    /// to the allocation behavior, the [`B`] type parameter) or will re-use a slot of a previously
    /// freed key. If the key was freed with [`Self::invalidate`], the slot will contain the
    /// previous value.
    #[inline(always)]
    pub fn reserve(&self) -> VersionedIndex<Kind> {
        let mut free_indexes = self.free_indexes.borrow_mut();
        if let Some(index) = free_indexes.pop() {
            let version = self.slots[index].version.update(|v| v + 1);
            VersionedIndex::new(index, version)
        } else {
            let index = self.slots.len();
            self.slots.push_new();
            VersionedIndex::new(index, 0)
        }
    }

    /// Insert a new value into the map.
    #[inline(always)]
    pub fn insert(&mut self, item: Item) -> VersionedIndex<Kind> {
        let index = self.reserve();
        self.slot_mut(index).unwrap().value = item;
        index
    }

    /// Insert a new value into the map.
    #[inline(always)]
    pub fn remove(&mut self, id: VersionedIndex<Kind>) -> Option<Item>
    where Item: Default {
        let out = self.slot_mut(id).map(|t| mem::take(&mut t.value));
        self.invalidate(id);
        out
    }

    /// Check whether the index is valid.
    #[inline(always)]
    pub fn exists(&self, vix: VersionedIndex<Kind>) -> bool {
        self.slot(vix).is_some()
    }

    /// Get an immutable reference to the value if the provided index is valid.
    #[inline(always)]
    pub fn get(&self, vix: VersionedIndex<Kind>) -> Option<&Item> {
        self.slot(vix).map(|slot| &slot.value)
    }

    /// Get a mutable reference to the value if the provided index is valid.
    #[inline(always)]
    pub fn get_mut(&mut self, vix: VersionedIndex<Kind>) -> Option<&mut Item> {
        self.slot_mut(vix).map(|slot| &mut slot.value)
    }

    #[inline(always)]
    fn slot(&self, vix: VersionedIndex<Kind>) -> Option<&Slot<Item>> {
        let slot = &self.slots[vix.index];
        (slot.version.get() == vix.version).as_some(slot)
    }

    #[inline(always)]
    fn slot_mut(&mut self, vix: VersionedIndex<Kind>) -> Option<&mut Slot<Item>> {
        let slot = &mut self.slots[vix.index];
        (slot.version.get() == vix.version).as_some(slot)
    }

    /// Return an iterator over the items.
    #[inline(always)]
    pub fn iter(&self) -> Iter<Item, N, B> {
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
    pub fn into_iter(self) -> IntoIter<Item, N, B> {
        IntoIterator::into_iter(self)
    }

    /// Clone all elements to a vector.
    pub fn to_vec(&self) -> Vec<Item>
    where Item: Clone {
        self.iter().cloned().collect_vec()
    }

    /// Convert the list into a vector.
    pub fn into_vec(self) -> Vec<Item> {
        self.into_iter().collect_vec()
    }
}

impl<Item, const N: usize, Kind, B> UnrolledSlotMap<Item, N, Kind, B>
where B: AllocationBehavior<Slot<Item>>
{
    /// Invalidate all keys in the map. This does not remove the values from the map, which might
    /// give performance benefits, as the values will be reused if you use [`Self::reserve`]. If you
    /// want to clear the memory and drop the values, use [`Self::clear`] instead.
    #[inline(always)]
    pub fn invalidate_all_keys(&self) {
        for slot in &*self.slots {
            if slot.is_occupied() {
                slot.version.update(|v| v + 1);
            }
        }
        let mut free_indexes = self.free_indexes.borrow_mut();
        free_indexes.clear();
        // We add the keys in reversed order to make the new reservation from the beginning of the
        // list.
        free_indexes.extend((0..self.slots.len()).rev());
    }
}

// FIXME: Uncomment after implementing mut iter for UnrolledLinkedList
// impl<T, const N: usize, Kind, B> Clearable for UnrolledSlotMap<T, N, Kind, B> {
//     #[inline(always)]
//     fn clear(&mut self) {
//         // We can't just use `self.slots.clear()` because we need to keep the slots versioning.
//         for slot in &mut *self.slots {
//             mem::take(&mut slot.value);
//             if slot.is_occupied() {
//                 slot.version.update(|v| v + 1);
//             }
//         }
//         let mut free_indexes = self.free_indexes.borrow_mut();
//         free_indexes.clear();
//         // We add the keys in reversed order to make the new reservation from the beginning of
// the         // list.
//         free_indexes.extend((0..self.slots.len()).rev());
//     }
// }



// =================
// === Iterators ===
// =================

// === Iter for &T ===

/// An iterator over immutable references to [`UnrolledSlotMap`] items.
#[derive(Debug)]
pub struct Iter<'a, T, const N: usize, B> {
    inner: unrolled_linked_list::Iter<'a, Slot<T>, N, B>,
}

impl<'a, T, const N: usize, B> Iterator for Iter<'a, T, N, B> {
    type Item = &'a T;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(slot) = self.inner.next() {
            if slot.is_occupied() {
                return Some(&slot.value);
            }
        }
        None
    }
}

impl<'a, T, const N: usize, Kind, B> IntoIterator for &'a UnrolledSlotMap<T, N, Kind, B>
where B: AllocationBehavior<Slot<T>>
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T, N, B>;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        Iter { inner: self.slots.iter() }
    }
}


// === Iter for T ===

/// An iterator over owned [`UnrolledSlotMap`] items.
#[derive(Debug)]
pub struct IntoIter<T, const N: usize, B> {
    inner: unrolled_linked_list::IntoIter<Slot<T>, N, B>,
}

impl<T, const N: usize, B> Iterator for IntoIter<T, N, B> {
    type Item = T;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(slot) = self.inner.next() {
            if slot.is_occupied() {
                return Some(slot.value);
            }
        }
        None
    }
}

impl<T, const N: usize, Kind, B> IntoIterator for UnrolledSlotMap<T, N, Kind, B>
where B: AllocationBehavior<Slot<T>>
{
    type Item = T;
    type IntoIter = IntoIter<T, N, B>;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter { inner: self.slots.into_iter() }
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ops() {
        let mut map = UnrolledSlotMap::<usize, 2>::new();
        assert!(map.is_empty());
        let key1 = map.insert(1);
        assert_eq!(map.to_vec(), vec![1]);
        let key2 = map.insert(2);
        assert_eq!(map.to_vec(), vec![1, 2]);
        assert_eq!(map.get(key1), Some(&1));
        assert_eq!(map.get(key2), Some(&2));
        map.invalidate(key1);
        assert_eq!(map.to_vec(), vec![2]);
        assert_eq!(map.get(key1), None);
        assert_eq!(map.get(key2), Some(&2));
        let key3 = map.reserve();
        assert_eq!(map.to_vec(), vec![1, 2]);
        assert_eq!(map.get(key1), None);
        assert_eq!(map.get(key2), Some(&2));
        assert_eq!(map.get(key3), Some(&1));
        assert_eq!(map.remove(key1), None);
        assert_eq!(map.remove(key2), Some(2));
        assert_eq!(map.to_vec(), vec![1]);
        assert_eq!(map.get(key1), None);
        assert_eq!(map.get(key2), None);
        assert_eq!(map.get(key3), Some(&1));
        map.invalidate_all_keys();
        assert!(map.is_empty());
        assert_eq!(map.get(key1), None);
        assert_eq!(map.get(key2), None);
        assert_eq!(map.get(key3), None);
        let key4 = map.reserve();
        let key5 = map.reserve();
        let key6 = map.reserve();
        let key7 = map.reserve();
        assert_eq!(map.to_vec(), vec![1, 0, 0, 0]);
        assert_eq!(map.get(key4), Some(&1));
        assert_eq!(map.get(key5), Some(&0));
        assert_eq!(map.get(key6), Some(&0));
        assert_eq!(map.get(key7), Some(&0));
        println!("{:#?}", map);
    }
}
