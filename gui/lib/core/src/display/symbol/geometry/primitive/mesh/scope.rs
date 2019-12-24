use crate::prelude::*;

use crate::display::render::webgl::Context;
use crate::closure;
use crate::data::function::callback::*;
use crate::data::opt_vec::OptVec;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::display::symbol::geometry::primitive::mesh::buffer;
use crate::display::symbol::geometry::primitive::mesh::buffer::item::Item;
use crate::display::symbol::geometry::primitive::mesh::buffer::IsBuffer;
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
pub struct Scope <OnMut> {
    pub buffers      : OptVec<AnyBuffer<OnMut>>,
    pub buffer_dirty : BufferDirty<OnMut>,
    pub shape_dirty  : ShapeDirty<OnMut>,
    pub name_map     : HashMap<BufferName, BufferIndex>,
    pub logger       : Logger,
    instance_count   : usize,
    context          : Context
}


// === Types ===

pub type BufferIndex           = usize;
pub type BufferName            = String;
pub type BufferDirty <OnMut> = dirty::SharedBitField<u64, OnMut>;
pub type ShapeDirty  <OnMut> = dirty::SharedBool<OnMut>;
promote_buffer_types! {[BufferOnSet, BufferOnResize] buffer}

#[macro_export]
macro_rules! promote_scope_types { ($callbacks:tt $module:ident) => {
    crate::promote_buffer_types! { $callbacks $module }
    promote! { $callbacks $module [Scope] }
};}


// === Callbacks ===

closure! {
fn buffer_on_set<C:Callback0> (dirty:BufferDirty<C>, ix:usize) -> BufferOnSet {
    || dirty.set(ix)
}}

closure! {
fn buffer_on_resize<C:Callback0> (dirty:ShapeDirty<C>) -> BufferOnResize {
    || dirty.set()
}}


// === Implementation ===

impl<OnMut: Clone> Scope<OnMut> {
    /// Create a new scope with the provided dirty callback.
    pub fn new(context:&Context, logger:Logger, on_mut:OnMut) -> Self {
        logger.info("Initializing.");
        let buffer_logger  = logger.sub("buffer_dirty");
        let shape_logger   = logger.sub("shape_dirty");
        let buffer_dirty   = BufferDirty::new(buffer_logger,on_mut.clone());
        let shape_dirty    = ShapeDirty::new(shape_logger,on_mut);
        let buffers        = default();
        let name_map       = default();
        let instance_count = default();
        let context        = context.clone();
        Self {context,buffers,buffer_dirty,shape_dirty,name_map,logger
            ,instance_count}
    }
}

impl<OnMut: Callback0> Scope<OnMut> {

    /// Adds a new named buffer to the scope.
    pub fn add_buffer<Name: Str, T: Item>
    (&mut self, name:Name) -> Buffer<T,OnMut>
        where AnyBuffer<OnMut>: From<Buffer<T,OnMut>> {
        let name         = name.as_ref().to_string();
        let buffer_dirty = self.buffer_dirty.clone();
        let shape_dirty  = self.shape_dirty.clone();
        let ix           = self.buffers.reserve_ix();
        group!(self.logger, "Adding buffer '{}' at index {}.", name, ix, {
            let on_set     = buffer_on_set(buffer_dirty, ix);
            let on_resize  = buffer_on_resize(shape_dirty);
            let logger     = self.logger.sub(&name);
            let context    = &self.context;
            let buffer     = Buffer::new(context,logger,on_set,on_resize);
            let buffer_ref = buffer.clone();
            self.buffers.set(ix, AnyBuffer::from(buffer));
            self.name_map.insert(name, ix);
            self.shape_dirty.set();
            buffer_ref
        })
    }

    /// Lookups buffer by a given name.
    pub fn buffer(&self, name:&str) -> Option<&AnyBuffer<OnMut>> {
        self.name_map.get(name).map(|i| &self.buffers[*i])
    }

    /// Checks if a buffer with the given name was created in this scope.
    pub fn contains<S:Str>(&self, name:S) -> bool {
        self.name_map.contains_key(name.as_ref())
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

    /// Returns the size of buffers in this scope.
    pub fn size(&self) -> usize {
        self.instance_count
    }
}
