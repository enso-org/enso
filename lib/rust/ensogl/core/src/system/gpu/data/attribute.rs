//! This module defines attributes and related utilities.

use crate::data::dirty::traits::*;
use crate::prelude::*;
use crate::system::gpu::types::*;

use crate::control::callback;
use crate::data::dirty;
use crate::data::OptVec;
use crate::debug::Stats;
use crate::system::gpu::Context;

use enso_shapely::newtype_prim;
use std::collections::BTreeSet;



// =============
// === Types ===
// =============

newtype_prim! {
    /// Index of a buffer.
    BufferIndex(usize);
}

/// Index of an attribute instance in a buffer.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display, CloneRef)]
pub struct InstanceIndex {
    /// Raw value.
    raw: usize,
}

/// Sentinel value used to identify references to deallocated instances. This value must not collide
/// with any actual index.
const FREED_INDEX: usize = usize::MAX;

impl InstanceIndex {
    /// Return the value, unless it is a freed-entry marker.
    #[inline(always)]
    pub fn raw(self) -> Option<usize> {
        self.is_valid().then_some(self.raw)
    }

    /// Return a reference to the value for mutation, unless it is a freed-entry marker.
    #[inline(always)]
    fn raw_mut(&mut self) -> Option<&mut usize> {
        self.is_valid().then_some(&mut self.raw)
    }

    /// Return true if this value is not the [`FREED_INDEX`] sentinel.
    #[inline(always)]
    fn is_valid(self) -> bool {
        self.raw != FREED_INDEX
    }

    /// Return the value; it must be live.
    #[inline(always)]
    pub fn valid_index(self) -> usize {
        debug_assert!(self.is_valid());
        if self.is_valid() {
            self.raw
        } else {
            #[cold]
            #[inline(never)]
            fn report_access_error() {
                error!("InstanceIndex used after free.");
            }
            report_access_error();
            0
        }
    }
}

impl From<Option<usize>> for InstanceIndex {
    fn from(value: Option<usize>) -> Self {
        let raw = value.unwrap_or(FREED_INDEX);
        Self { raw }
    }
}

/// Stable identifier of an instance in a buffer.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Display, CloneRef)]
pub struct InstanceId {
    /// Raw value.
    pub raw: usize,
}

/// Dirty flag collecting information which buffers were mutated.
pub type BufferDirty = dirty::SharedBitField<u64, Box<dyn FnMut()>>;

/// Dirty flag indicating that the shape of the attribute (all buffers) was changed.
pub type ShapeDirty = dirty::SharedBool<Box<dyn FnMut()>>;



// ======================
// === AttributeScope ===
// ======================

shared! { AttributeScope
/// [`AttributeScope`] is a set of named [`Buffer`]s of the same length. The buffers con contain any
/// WebGL specific information, like geometry information. The buffers are named for easy access.
/// For example, the current geometry implementation uses [`AttributeScope`] to define logical
/// attribute scopes named "point" and "instance". They contain information attached to points and
/// geometry instances respectively. Also, the "point" [`AttributeScope`] can contain such buffers
/// as "color", while "instance" [`AttributeScope`] can be created with a "position" buffer to allow
/// controlling placement of each instance separately.
#[derive(Debug)]
pub struct AttributeScopeData {
    buffers         : OptVec<AnyBuffer>,
    buffer_dirty    : BufferDirty,
    shape_dirty     : ShapeDirty,
    /// Number of elements currently allocated.
    used_size       : usize,
    buffer_name_map : HashMap<String,BufferIndex>,
    /// Mapping from stable external identifiers to physical locations in a buffer.
    indexes         : Rc<RefCell<Vec<InstanceIndex>>>,
    partitions      : Vec<Partition>,
    context         : Option<Context>,
    stats           : Stats,
}

impl {
    /// Create a new scope with the provided dirty callback.
    pub fn new<OnMut:callback::NoArgs+Clone>
    (stats:&Stats, on_mut:OnMut) -> Self {
        debug_span!("Initializing.").in_scope(|| {
            let stats = stats.clone_ref();
            let buffer_dirty = BufferDirty::new(Box::new(on_mut.clone()));
            let shape_dirty = ShapeDirty::new(Box::new(on_mut));
            Self {
                buffer_dirty,
                shape_dirty,
                stats,
                used_size: default(),
                buffers: default(),
                buffer_name_map: default(),
                indexes: default(),
                partitions: default(),
                context: default(),
            }
        })
    }

    /// Add a new named buffer to the scope.
    pub fn add_buffer<Name:Str, T:Storable>(&mut self, name:Name) -> Buffer<T>
    where AnyBuffer: From<Buffer<T>> {
        let name = name.as_ref().to_string();
        let buffer_dirty = self.buffer_dirty.clone();
        let shape_dirty = self.shape_dirty.clone();
        let ix = self.buffers.reserve_index();
        let indexes = Rc::clone(&self.indexes);
        debug_span!("Adding buffer '{name}' at index {ix}.").in_scope(|| {
            let on_set = Box::new(move || { buffer_dirty.set(ix) });
            let on_resize = Box::new(move || { shape_dirty.set() });
            let buffer = Buffer::new(&self.stats, on_set, on_resize, indexes);
            buffer.set_context(self.context.as_ref());
            let buffer_ref = buffer.clone();
            self.buffers.set(ix,AnyBuffer::from(buffer));
            self.buffer_name_map.insert(name,ix.into());
            self.shape_dirty.set();
            buffer_ref
        })
    }

    /// Lookup buffer by a given name.
    pub fn buffer(&self, name:&str) -> Option<AnyBuffer> {
        self.buffer_name_map.get(name).map(|i| self.buffers[(*i).into()].clone())
    }

    /// Checks if a buffer with the given name was created in this scope.
    pub fn contains<S:Str>(&self, name:S) -> bool {
        self.buffer_name_map.contains_key(name.as_ref())
    }

    /// Add a new instance to every buffer in the scope.
    pub fn add_instance(&mut self) -> InstanceId {
        self.add_instance_at(default())
    }

    /// Add a new instance to every buffer in the scope, allocated in the specified partition.
    pub fn add_instance_at(&mut self, buffer_partition: BufferPartitionId) -> InstanceId {
        debug_span!("Adding instance.").in_scope(|| {
            let index = self.alloc(buffer_partition);
            let mut indexes = self.indexes.borrow_mut();
            let id = InstanceId { raw: indexes.len() };
            indexes.push(index);
            id
        })
    }

    /// Dispose instance for reuse in the future. All data in all buffers at the provided `id` will
    /// be set to default.
    pub fn dispose(&mut self, id:InstanceId) {
        debug_span!("Disposing instance {id}.").in_scope(|| {
            let ix = self.indexes.borrow()[id.raw].raw();
            debug_assert_ne!(ix, None);
            if let Some(ix) = ix {
                for buffer in &self.buffers {
                    buffer.set_to_default(ix)
                }
                self.free(ix);
            }
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        debug_span!("Updating.").in_scope(|| {
            if self.used_size * 2 < self.size() {
                self.shrink_to_fit();
            }
            if self.shape_dirty.check() {
                for buffer in self.buffers.iter() {
                    buffer.update()
                }
            } else {
                let dirty_buffers = self.buffers.iter_enumerate().filter_map(|(i, buffer)|
                    self.buffer_dirty.check(&i).then_some(buffer));
                for buffer in dirty_buffers {
                    buffer.update()
                }
            }
            self.shape_dirty.unset();
            self.buffer_dirty.unset_all();
        })
    }

    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    pub(crate) fn set_context(&mut self, context:Option<&Context>) {
        self.context = context.cloned();
        for buffer in &self.buffers {
            buffer.set_context(context);
        }
    }


    // === Instance allocation ===

    /// Allocate an element in the specified [`Partition`]. Its depth-order within the partition is
    /// unspecified, except for one rule: If two elements are allocated in a partition, and no
    /// element in that [`AttributeScope`] is freed in between their allocations, then the element
    /// added later will be drawn later.
    ///
    /// Elements are allocated from a per-partition freelist, until a partition runs out of space.
    /// When that occurs, the partition must be grown. Growing a partition is stable: It doesn't
    /// affect the depth-order of existing elements. To make space within the buffers while
    /// maintaining a total order of elements, we must allocate larger buffers and shift all
    /// elements after the affected partition (in order to support moving elements, the [`indexes`]
    /// table decouples the stable identifier used by external references from the physical
    /// locations within the buffer). Since reallocating elements and shifting buffers is an
    /// expensive operation, partition growth is performed in exponential increments (doubling the
    /// size each time capacity is exceeded), ensuring a sequence of `N` allocations can be
    /// satisfied with `log N` partition-growth operations.
    fn alloc(&mut self, partition: BufferPartitionId) -> InstanceIndex {
        // Create the partition when it is first referenced.
        if partition.index >= self.partitions.len() {
            let start = self.size();
            let empty = Partition { start, len: default(), free: default() };
            self.partitions.resize(partition.index + 1, empty);
        }
        // Return a freelist slot, if available.
        let i = if let Some(i) = self.partitions[partition.index].free.pop_first() {
            i
        } else {
            // === Resize the partition ===
            let partition_start = self.partitions[partition.index].start;
            let partition_orig_len = self.partitions[partition.index].len;
            let partition_orig_end = partition_start + partition_orig_len;
            let partition_increment = std::cmp::max(partition_orig_len, 1);
            let partition_new_len = partition_orig_len + partition_increment;
            self.partitions[partition.index].len = partition_new_len;
            // Shift buffers to make space.
            for buffer in &self.buffers {
                buffer.insert_elements(partition_orig_end, partition_increment);
            }
            // Update shifted entries in the id->index map.
            for index in self.indexes.borrow_mut().iter_mut().filter_map(|index| index.raw_mut()) {
                if *index >= partition_orig_end {
                    *index += partition_increment;
                }
            }
            // Add new space to the freelist.
            let new_allocations = 1;
            self.partitions[partition.index].free.clear();
            self.partitions[partition.index].free.extend(
                partition_orig_len + new_allocations..partition_new_len
            );
            // Update the partition offsets.
            if let Some(following) = self.partitions.get_mut(partition.index + 1..) {
                for partition in following {
                    partition.start += partition_increment;
                }
            }
            partition_orig_len
        };
        self.used_size += 1;
        InstanceIndex { raw: self.partitions[partition.index].start + i }
    }

    /// Free the storage at the specified instance-index.
    fn free(&mut self, index: usize) {
        let partition = self.partitions.iter_mut().find(
            |partition| index < partition.start + partition.len).unwrap();
        partition.free.insert(index - partition.start);
        self.used_size -= 1;
    }

    /// Compact the buffers to the size of the used elements. This operation is stable: The order of
    /// elements will not be affected.
    fn shrink_to_fit(&mut self) {
        const NO_ALLOCATION: isize = -1;
        // Drain all the freelists, build relocation table; update starts and lengths.
        let mut relocations = Vec::new();
        relocations.resize(self.size(), 0);
        let mut new_locations = relocations.iter_mut();
        let mut next_slot = 0;
        for partition in &mut self.partitions {
            partition.start = next_slot;
            for i in 0..partition.len {
                *new_locations.next().unwrap() = if !partition.free.contains(&i) {
                    let current_slot = next_slot;
                    next_slot += 1;
                    current_slot as isize
                } else {
                    NO_ALLOCATION
                };
            }
            let orig_partition_len = partition.len;
            partition.len = next_slot - partition.start;
            debug_assert_eq!(partition.len, orig_partition_len - partition.free.len());
            partition.free.clear();
        }
        self.used_size = next_slot;
        // Vacuum the buffers.
        for buffer in &mut self.buffers {
            let mut is_free = relocations.iter().map(|i| *i == -1);
            buffer.retain(|| !is_free.next().unwrap());
        }
        // Update the id->index map.
        for index in &mut *self.indexes.borrow_mut() {
            if let Some(i) = index.raw() {
                *index = (match relocations.get(i).copied() {
                    None | Some(NO_ALLOCATION) => None,
                    Some(valid) => Some(valid as usize),
                }).into();
            }
        }
    }

    /// Return the size of buffers in this scope.
    pub fn size(&self) -> usize {
        self.partitions.last().map(|part| part.start + part.len).unwrap_or_default()
    }
}}


// === Partitions ===

/// A buffer partition.
///
/// Partitions support the management of depth-ordering within a single [`AttributeScope`]. Each
/// buffer is divided into one or more partitions; elements can be allocated and freed in any
/// partition, and elements in lower partitions will always be drawn below elements in upper
/// partitions.
///
/// Every [`AttributeScope`] has a table of partitions; this table is applied to all buffers in the
/// scope.
#[derive(Debug, Default, Clone)]
struct Partition {
    start: usize,
    len:   usize,
    /// Free indexes, relative to [`start`].
    free:  BTreeSet<usize>,
}

/// Identifies a partition in a set of buffers.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct BufferPartitionId {
    /// Index in the sequence of partitions.
    pub index: usize,
}



// =================
// === Attribute ===
// =================

/// Interface for a particular [`Buffer`] element. It allows reading and writing the buffer value.
/// Attributes are used to bind geometric specific, like sprite positions, to specific [`Buffer`]
/// indexes.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Attribute<T> {
    id:     InstanceId,
    buffer: Buffer<T>,
}

impl<T> Attribute<T> {
    /// Create a new variable as an indexed view over provided buffer.
    pub fn new(id: InstanceId, buffer: Buffer<T>) -> Self {
        Self { id, buffer }
    }
}

impl<T> HasItem for Attribute<T> {
    type Item = T;
}

impl<T: Storable> CellGetter for Attribute<T> {
    fn get(&self) -> Self::Item {
        self.buffer.get_at(self.id)
    }
}

impl<T: Storable> CellSetter for Attribute<T> {
    fn set(&self, value: Self::Item) {
        self.buffer.set_at(self.id, value);
    }
}

impl<T: Storable + Default> Erase for Attribute<T> {
    fn erase(&self) {
        self.set(default())
    }
}



// =============
// === Erase ===
// =============

/// Generalization for internally mutable structures which can be erased.
///
/// For now, it is placed here, as only [`Attribute`] uses it, but it might be refactored in the
/// future if it will be usable in more places.
#[allow(missing_docs)]
pub trait Erase {
    fn erase(&self);
}

/// The provided element will be erased whenever this structure is dropped. Please note that the
///provided element implements [`CloneRef`] it can still be referenced after this struct is
/// dropped.
#[derive(Debug, NoCloneBecauseOfCustomDrop)]
pub struct EraseOnDrop<T: Erase> {
    elem: T,
}

impl<T: Erase> EraseOnDrop<T> {
    /// Constructor.
    pub fn new(elem: T) -> Self {
        Self { elem }
    }
}

impl<T: Erase> Drop for EraseOnDrop<T> {
    fn drop(&mut self) {
        self.elem.erase()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod allocator_tests {
    use super::*;

    enum Op {
        Alloc { partition: usize, count: usize },
        Free { partition: usize, count: usize },
    }

    #[test]
    fn test_single_partition() {
        let steps = [
            Op::Alloc { partition: 0, count: 23 },
            Op::Free { partition: 0, count: 15 },
            Op::Alloc { partition: 0, count: 16 },
            Op::Free { partition: 0, count: 4 },
            Op::Alloc { partition: 0, count: 42 },
            Op::Free { partition: 0, count: 8 },
        ];
        check_allocator(1, &steps);
    }

    #[test]
    fn test_multiple_partitions() {
        let steps = [
            Op::Alloc { partition: 1, count: 23 },
            Op::Alloc { partition: 0, count: 23 },
            Op::Free { partition: 0, count: 23 },
            Op::Alloc { partition: 1, count: 2 },
            Op::Free { partition: 1, count: 2 },
            Op::Alloc { partition: 0, count: 23 },
            Op::Alloc { partition: 1, count: 23 },
            Op::Free { partition: 0, count: 4 },
        ];
        check_allocator(2, &steps);
    }

    /// Use a PRNG to generate a *deterministic* sequence of operations to test.
    #[test]
    fn test_multiple_partitions_random_ops() {
        use rand::prelude::*;
        use rand_chacha::ChaCha8Rng;
        let mut rng = ChaCha8Rng::seed_from_u64(0);
        let mut partition_counts = [0, 0];
        let random_valid_op = |_| {
            let partition = rng.gen::<bool>() as usize;
            let count = rng.gen::<u8>() as usize;
            match rng.gen() {
                false => {
                    partition_counts[partition] += count;
                    Op::Alloc { partition, count }
                }
                true => {
                    let count = std::cmp::min(count, partition_counts[partition]);
                    partition_counts[partition] -= count;
                    Op::Free { partition, count }
                }
            }
        };
        let steps = (0..1000).map(random_valid_op).collect_vec();
        check_allocator(2, &steps);
    }

    fn check_allocator(partitions: usize, steps: impl IntoIterator<Item = &Op>) {
        let stats = Stats::new();
        let scope = AttributeScope::new(&stats, || ());
        let mut instances: Vec<Vec<InstanceId>> = default();
        instances.resize(partitions, default());
        macro_rules! check_live_instances {
            () => {{
                let scope = scope.rc.borrow();
                let indexes = scope.indexes.borrow();
                for (_partition, instances) in instances.iter().enumerate() {
                    for instance in instances {
                        assert_ne!(indexes[instance.raw].raw, FREED_INDEX);
                    }
                }
            }};
        }
        macro_rules! check_unique_locations {
            () => {{
                let mut locations_used: HashSet<usize> = default();
                let scope = scope.rc.borrow();
                let indexes = scope.indexes.borrow();
                for (_partition, instances) in instances.iter().enumerate() {
                    for instance in instances {
                        assert_ne!(indexes[instance.raw].raw, FREED_INDEX);
                        let no_collision = locations_used.insert(indexes[instance.raw].raw);
                        assert!(no_collision);
                    }
                }
            }};
        }
        for step in steps {
            match step {
                Op::Alloc { partition, count } => {
                    for _ in 0..*count {
                        let instance =
                            scope.add_instance_at(BufferPartitionId { index: *partition });
                        instances[*partition].push(instance);
                        check_live_instances!();
                    }
                    check_unique_locations!();
                }
                Op::Free { partition, count } => {
                    for instance_ix in instances[*partition].splice(..count, None) {
                        scope.dispose(instance_ix);
                    }
                    check_live_instances!();
                    check_unique_locations!();
                    scope.shrink_to_fit();
                    check_live_instances!();
                    check_unique_locations!();
                }
            }
        }
        for partition_instances in instances {
            for instance_ix in partition_instances {
                scope.dispose(instance_ix);
            }
        }
        scope.shrink_to_fit();
    }
}
