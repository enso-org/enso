//! A slot map based on [`UnrolledLinkedList`].

use crate::prelude::*;

use crate::unrolled_linked_list;

use unrolled_linked_list::AllocationBehavior;
use unrolled_linked_list::UnrolledLinkedList;



// ===============
// === Version ===
// ===============

/// Version of the slot. An even version (0, 2, 4, ...) means that the slot is occupied. An odd
/// version (1, 3, 5, ...) means that the slot is free.
#[derive(Clone, Copy, Debug, Display, Default, Deref, PartialEq, Eq, Zeroable)]
#[repr(transparent)]
struct Version(usize);

impl Version {
    #[inline(always)]
    fn new_not_occupied() -> Self {
        Self(1)
    }

    #[inline(always)]
    fn inc(self) -> Self {
        Self(*self + 1)
    }

    #[inline(always)]
    fn is_occupied(self) -> bool {
        *self % 2 == 0
    }
}



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
    version: Version,
    _kind:   ZST<Kind>,
}

#[allow(unsafe_code)]
unsafe impl<Kind> Zeroable for VersionedIndex<Kind> {}

impl<Kind> VersionedIndex<Kind> {
    #[inline(always)]
    fn new(index: usize, version: Version) -> Self {
        Self { index, version, _kind: ZST() }
    }

    /// Create a new index that is not occupied. you will not be able to use this index. It can be
    /// used to create an index in case of an error if you need to return an index.
    #[inline(always)]
    pub fn new_not_occupied() -> Self {
        Self::new(0, Version::new_not_occupied())
    }
}



// ============
// === Slot ===
// ============

/// A slot, versioned value in the slot map.
#[derive(Debug, Default, Zeroable)]
pub struct Slot<Item> {
    value:   Item,
    version: Cell<Version>,
}

impl<Item> Slot<Item> {
    #[inline(always)]
    fn version(&self) -> Version {
        self.version.get()
    }

    #[inline(always)]
    fn is_occupied(&self) -> bool {
        self.version.get().is_occupied()
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
    free_indexes: ZeroOverheadRefCell<Vec<usize>>,
    slots:        Box<UnrolledLinkedList<Slot<Item>, N, usize, B>>,
    _kind:        ZST<Kind>,
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

    /// Return the number of elements in the map.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.slots.len() - self.free_indexes.borrow().len()
    }

    /// Check if the map is empty.
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
        let ok = self.slot(id).map(|slot| slot.version.update(|v| v.inc())).is_some();
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
        #[allow(unused_mut)]
        let mut free_indexes = self.free_indexes.borrow_mut();
        if let Some(index) = free_indexes.pop() {
            let version = self.slots[index].version.update(|v| v.inc());
            VersionedIndex::new(index, version)
        } else {
            let index = self.slots.len();
            self.slots.push_new();
            VersionedIndex::new(index, Version::default())
        }
    }

    /// Reserve a new index and execute the provided function on the mutable reference to the value.
    /// See docs of [`Self::reserve`] to learn more.
    #[inline(always)]
    pub fn reserve_and_init<T>(
        &mut self,
        f: impl FnOnce(&mut Item) -> T,
    ) -> (VersionedIndex<Kind>, T) {
        let id = self.reserve();
        let out = f(self.get_mut(id).unwrap());
        (id, out)
    }

    /// Reserve a new index and execute the provided function on the reference to the value. See
    /// docs of [`Self::reserve`] to learn more.
    #[inline(always)]
    pub fn reserve_and_init_im<T>(&self, f: impl FnOnce(&Item) -> T) -> (VersionedIndex<Kind>, T) {
        let id = self.reserve();
        let out = f(self.get(id).unwrap());
        (id, out)
    }

    /// Wrapper for [`Self::reserve_and_init] that returns only the newly reserved index.
    #[inline(always)]
    pub fn reserve_and_init_<T>(&mut self, f: impl FnOnce(&mut Item) -> T) -> VersionedIndex<Kind> {
        self.reserve_and_init(f).0
    }

    /// Wrapper for [`Self::reserve_and_init_im`] that returns only the newly reserved index.
    #[inline(always)]
    pub fn reserve_and_init_im_<T>(&self, f: impl FnOnce(&Item) -> T) -> VersionedIndex<Kind> {
        self.reserve_and_init_im(f).0
    }

    /// Insert a new value into the map.
    #[inline(always)]
    pub fn insert(&mut self, item: Item) -> VersionedIndex<Kind> {
        let index = self.reserve();
        self.slot_mut(index).unwrap().value = item;
        index
    }

    /// Remove the value at the provided index and return it.
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
                slot.version.update(|v| v.inc());
            }
        }
        #[allow(unused_mut)]
        let mut free_indexes = self.free_indexes.borrow_mut();
        free_indexes.clear();
        // We add the keys in reversed order to make the new reservation from the beginning of the
        // list.
        free_indexes.extend((0..self.slots.len()).rev());
    }
}

// FIXME: Uncomment after implementing mut iter for UnrolledLinkedList in
//        https://github.com/enso-org/enso/issues/7043
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
        for slot in self.inner.by_ref() {
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
        for slot in self.inner.by_ref() {
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
        println!("{map:#?}");
    }
}
