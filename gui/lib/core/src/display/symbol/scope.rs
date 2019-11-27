use crate::prelude::*;

use crate::closure;
use crate::data::function::callback::*;
use crate::data::opt_vec::OptVec;
use crate::dirty;
use crate::dirty::traits::*;
use crate::display::symbol::buffer;
use crate::display::symbol::buffer::item::Item;
use crate::display::symbol::buffer::IsBuffer;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::promote;
use crate::promote_all;
use crate::promote_buffer_types;
use eval_tt::*;


// =============
// === Scope ===
// =============

// === Definition ===

/// Scope defines a view for geometry structure. For example, there is point 
/// scope or instance scope. Scope contains buffer of data for each item it 
/// describes.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Scope <OnDirty> {
    pub buffers      : OptVec<AnyBuffer<OnDirty>>,
    pub buffer_dirty : BufferDirty<OnDirty>,
    pub shape_dirty  : ShapeDirty<OnDirty>,
    pub name_map     : HashMap<BufferName, BufferIndex>,
    pub logger       : Logger,
    instance_count   : usize
}

// === Types ===

pub type BufferIndex           = usize;
pub type BufferName            = String;
pub type BufferDirty <OnDirty> = dirty::SharedBitField<u64, OnDirty>;
pub type ShapeDirty  <OnDirty> = dirty::SharedBool<OnDirty>;
promote_buffer_types! {[BufferOnSet, BufferOnResize] buffer}

#[macro_export]
macro_rules! promote_scope_types { ($callbacks:tt $module:ident) => {
    crate::promote_buffer_types! { $callbacks $module }
    promote! { $callbacks $module [Scope] }
};}

// === Callbacks ===

closure! {
fn buffer_on_set<C:Callback0> (dirty:BufferDirty<C>, ix:usize) ->
    BufferOnSet { || dirty.set(ix) }
}

closure! {
fn buffer_on_resize<C:Callback0> (dirty:ShapeDirty<C>) ->
    BufferOnResize { || dirty.set() }
}

// === Implementation ===

impl<OnDirty: Clone> Scope<OnDirty> {
    /// Create a new scope with the provided dirty callback.
    pub fn new(logger:Logger, on_dirty:OnDirty) -> Self {
        logger.info("Initializing.");
        let buffer_logger  = logger.sub("buffer_dirty");
        let shape_logger   = logger.sub("shape_dirty");
        let buffer_dirty   = BufferDirty::new(buffer_logger,on_dirty.clone());
        let shape_dirty    = ShapeDirty::new(shape_logger,on_dirty);
        let buffers        = default();
        let name_map       = default();
        let instance_count = default();
        Self {buffers,buffer_dirty,shape_dirty,name_map,logger,instance_count}
    }
}

impl<OnDirty: Callback0> Scope<OnDirty> {
    /// Adds a new named buffer to the scope.
    pub fn add_buffer<Name: Str, T: Item>
    (&mut self, name:Name) -> SharedBuffer<T,OnDirty>
    where AnyBuffer<OnDirty>: From<SharedBuffer<T,OnDirty>> {
        let name         = name.as_ref().to_string();
        let buffer_dirty = self.buffer_dirty.clone();
        let shape_dirty  = self.shape_dirty.clone();
        let ix           = self.buffers.reserve_ix();
        group!(self.logger, "Adding buffer '{}' at index {}.", name, ix, {
            let on_set     = buffer_on_set(buffer_dirty, ix);
            let on_resize  = buffer_on_resize(shape_dirty);
            let logger     = self.logger.sub(&name);
            let buffer     = SharedBuffer::new(logger,on_set,on_resize);
            let buffer_ref = buffer.clone();
            self.buffers.set(ix, AnyBuffer::from(buffer));
            self.name_map.insert(name, ix);
            self.shape_dirty.set();
            buffer_ref
        })
    }
    /// Adds a new instance to every buffer in the scope.
    pub fn add_instance(&mut self) -> usize {
        group!(self.logger, "Adding {} instance(s).", 1, {
            let ix = self.instance_count;
            self.instance_count += 1;
            self.buffers.iter_mut().for_each(|t| t.add_element());
            ix
        })
    }
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            for i in 0..self.buffers.len() {
                if self.buffer_dirty.check_for(&(i, )) {
                    self.buffers[i].update()
                }
            }
            self.buffer_dirty.unset()
        })
    }
}

