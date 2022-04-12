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
    /// Index of an attribute instance.
    InstanceIndex(usize);

    /// Index of a buffer.
    BufferIndex(usize);
}

/// Dirty flag collecting information which buffers were mutated.
pub type BufferDirty = dirty::SharedBitField<u64, Box<dyn Fn()>>;

/// Dirty flag indicating that the shape of the attribute (all buffers) was changed.
pub type ShapeDirty = dirty::SharedBool<Box<dyn Fn()>>;



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
///
///
/// # Internal Design (Memory management and ID re-use)
/// TODO: The proper memory management should be implemented. Currently, after creating a lot of
///       instances and dropping them, the memory is not freed. This section explains why and
///       describes possible solutions.
///
/// Currently, the `free_ids` field keeps track of all instance indexes that are not used anymore.
/// It is stored in a sorted container in order to preserve the display order when creating new
/// instances (new instances appear on top of older instances if no dropping happened in the
/// meantime). Alternative sorting solutions are described in the docs for the [`Symbol`].
///
/// In order to properly re-use memory, we need to free it sometimes. The following solutions are
/// possible:
///
/// 1. Keeping track of all free indexes in a sorted container (like [`BTreeSet`] or the specialized
///    [`enso_data_structures::Diet`] and in case the biggest index is freed, iterating over the indexes and
///    freeing as much as possible. This solution has the downside that the indexes are stored in
///    order, so insertion and deletion is much slower than when using unordered [`Vec`]. Also, this
///    does not work well if a instance with a big ID is kept alive, as it will prevent memory of
///    all instances with smaller IDs from being cleaned. See benchmarks in the `enso_data_structures::diet`
///    module to learn more.
///
/// 2. Keeping track of all free indexes in an unordered container and in case the biggest index is
///    freed, sorting the container and freeing the memory. As an optimization, the sorting might
///    be performed after the frame (or several frames) was drawn. It's not obvious when this
///    solution will be slower / faster than the solution (1), but time differences may be big.
///    See benchmarks in the `enso_data_structures::diet` module to learn more.
///
/// 3. Keeping track of all free indexes and in case a lot of them are free, re-ordering the
///    instances and freeing the memory. This would require all instance-users (like [`Sprite`]s) to
///    keep instance IDs in some kind of `Rc<Cell<ID>>`, which may slow attrib read/write down.
///    However, this solution works well even if an instance with a big ID is kept alive. It's not
///    obvious when this solution will be slower / faster than other ones, but time differences may
///    be big. See benchmarks in the `enso_data_structures::diet` module to learn more.
///
/// To learn more about these mechanisms and connected design decisions, read the docs of
/// [`Symbol`], especially the "Changing attribute & GPU memory consumption" sections.

#[derive(Debug)]
pub struct AttributeScopeData {
    buffers         : OptVec<AnyBuffer>,
    buffer_dirty    : BufferDirty,
    shape_dirty     : ShapeDirty,
    buffer_name_map : HashMap<String,BufferIndex>,
    logger          : Logger,
    free_ids        : BTreeSet<InstanceIndex>,
    size            : usize,
    context         : Option<Context>,
    stats           : Stats,
}

impl {
    /// Create a new scope with the provided dirty callback.
    pub fn new<OnMut:callback::NoArgs+Clone>
    (lgr:Logger, stats:&Stats, on_mut:OnMut) -> Self {
        info!(lgr,"Initializing.",|| {
            let logger          = lgr.clone();
            let stats           = stats.clone_ref();
            let buffer_logger   = Logger::new_sub(&logger,"buffer_dirty");
            let shape_logger    = Logger::new_sub(&logger,"shape_dirty");
            let buffer_dirty    = BufferDirty::new(buffer_logger,Box::new(on_mut.clone()));
            let shape_dirty     = ShapeDirty::new(shape_logger,Box::new(on_mut));
            let buffers         = default();
            let buffer_name_map = default();
            let free_ids        = default();
            let size            = default();
            let context         = default();
            Self {buffers,buffer_dirty,shape_dirty,buffer_name_map,logger,free_ids,size,context
                 ,stats}
        })
    }

    /// Add a new named buffer to the scope.
    pub fn add_buffer<Name:Str, T:Storable>(&mut self, name:Name) -> Buffer<T>
    where AnyBuffer: From<Buffer<T>> {
        let name         = name.as_ref().to_string();
        let buffer_dirty = self.buffer_dirty.clone();
        let shape_dirty  = self.shape_dirty.clone();
        let ix           = self.buffers.reserve_index();
        debug!(self.logger, "Adding buffer '{name}' at index {ix}.", || {
            let on_set     = Box::new(move || { buffer_dirty.set(ix) });
            let on_resize  = Box::new(move || { shape_dirty.set() });
            let logger     = Logger::new_sub(&self.logger,&name);
            let buffer     = Buffer::new(logger,&self.stats,on_set,on_resize);
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
    pub fn add_instance(&mut self) -> InstanceIndex {
        let instance_count = 1;
        debug!(self.logger, "Adding {instance_count} instance(s).", || {
            match self.free_ids.iter().next().copied() {
                Some(ix) => {
                    self.free_ids.remove(&ix);
                    ix
                }
                None => {
                    let ix = self.size;
                    self.size += instance_count;
                    self.buffers.iter_mut().for_each(|t| t.add_element());
                    ix.into()
                }
            }
        })
    }

    /// Dispose instance for reuse in the future. All data in all buffers at the provided `id` will
    /// be set to default.
    pub fn dispose(&mut self, ix:InstanceIndex) {
        debug!(self.logger, "Disposing instance {ix}.", || {
            for buffer in &self.buffers {
                buffer.set_to_default(ix.into())
            }
            self.free_ids.insert(ix);
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        debug!(self.logger, "Updating.", || {
            if self.shape_dirty.check() {
                for i in 0..self.buffers.len() {
                    self.buffers[i].update()
                }
            } else {
                for i in 0..self.buffers.len() {
                    if self.buffer_dirty.check(&i) {
                        self.buffers[i].update()
                    }
                }
            }
            self.shape_dirty.unset();
            self.buffer_dirty.unset_all();
        })
    }

    /// Return the size of buffers in this scope.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    pub(crate) fn set_context(&mut self, context:Option<&Context>) {
        self.context = context.cloned();
        for buffer in &self.buffers {
            buffer.set_context(context);
        }
    }
}}



// =================
// === Attribute ===
// =================

/// Interface for a particular [`Buffer`] element. It allows reading and writing the buffer value.
/// Attributes are used to bind geometric specific, like sprite positions, to specific [`Buffer`]
/// indexes.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Attribute<T> {
    index:  InstanceIndex,
    buffer: Buffer<T>,
}

impl<T> Attribute<T> {
    /// Create a new variable as an indexed view over provided buffer.
    pub fn new(index: InstanceIndex, buffer: Buffer<T>) -> Self {
        Self { index, buffer }
    }
}

impl<T> HasItem for Attribute<T> {
    type Item = T;
}

impl<T: Storable> CellGetter for Attribute<T> {
    fn get(&self) -> Self::Item {
        self.buffer.get(self.index.into())
    }
}

impl<T: Storable> CellSetter for Attribute<T> {
    fn set(&self, value: Self::Item) {
        self.buffer.set(self.index.into(), value);
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
