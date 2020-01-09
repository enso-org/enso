#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod class;

use crate::prelude::*;

use crate::closure;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::data::function::callback::*;
use crate::debug::stats::Stats;
use crate::display::render::webgl::Context;
use crate::system::gpu::buffer::IsBuffer;
use crate::system::gpu::data::GpuData;
use crate::system::gpu::buffer;
use crate::promote;
use crate::promote_all;
use crate::promote_buffer_types;
use crate::system::web::group;
use crate::system::web::Logger;
use data::opt_vec::OptVec;
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
pub struct AttributeScope<OnMut> {
    pub buffers      : OptVec<AnyBuffer<OnMut>>,
    pub buffer_dirty : BufferDirty<OnMut>,
    pub shape_dirty  : ShapeDirty<OnMut>,
    pub name_map     : HashMap<BufferName, BufferIndex>,
    pub logger       : Logger,
    free_ids         : Vec<InstanceId>,
    size             : usize,
    context          : Context,
    stats            : Stats,
}


// === Types ===

pub type InstanceId            = usize;
pub type BufferIndex           = usize;
pub type BufferName            = String;
pub type BufferDirty   <OnMut> = dirty::SharedBitField<u64,OnMut>;
pub type ShapeDirty    <OnMut> = dirty::SharedBool<OnMut>;
pub type Attribute   <T,OnMut> = class::Attribute<T,BufferOnSet<OnMut>,BufferOnResize<OnMut>>;
promote_buffer_types! {[BufferOnSet,BufferOnResize] buffer}

#[macro_export]
/// Promote relevant types to parent scope. See `promote!` macro for more information.
macro_rules! promote_scope_types { ($callbacks:tt $module:ident) => {
    crate::promote_buffer_types! { $callbacks $module }
    promote! { $callbacks $module [Attribute<T>,AttributeScope] }
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

impl<OnMut:Clone> AttributeScope<OnMut> {
    /// Create a new scope with the provided dirty callback.
    pub fn new(logger:Logger, stats:&Stats, context:&Context, on_mut:OnMut) -> Self {
        logger.info("Initializing.");
        let stats         = stats.clone_ref();
        let buffer_logger = logger.sub("buffer_dirty");
        let shape_logger  = logger.sub("shape_dirty");
        let buffer_dirty  = BufferDirty::new(buffer_logger,on_mut.clone());
        let shape_dirty   = ShapeDirty::new(shape_logger,on_mut);
        let buffers       = default();
        let name_map      = default();
        let free_ids      = default();
        let size          = default();
        let context       = context.clone();
        Self {context,buffers,buffer_dirty,shape_dirty,name_map,logger,free_ids,size,stats}
    }
}

impl<OnMut: Callback0> AttributeScope<OnMut> {
    /// Adds a new named buffer to the scope.
    pub fn add_buffer<Name:Str, T:GpuData>(&mut self, name:Name) -> Buffer<T,OnMut>
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
            let buffer     = Buffer::new(logger,&self.stats,context,on_set,on_resize);
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
    pub fn add_instance(&mut self) -> InstanceId {
        group!(self.logger, "Adding {} instance(s).", 1, {
            match self.free_ids.pop() {
                Some(ix) => ix,
                None     => {
                    let ix = self.size;
                    self.size += 1;
                    self.buffers.iter_mut().for_each(|t| t.add_element());
                    ix
                }
            }
        })
    }

    /// Disposes instance for reuse in the future. Please note that the disposed data still
    /// exists in the buffer and will be used when rendering. It is yours responsibility to hide
    /// id, fo example by degenerating vertices.
    pub fn dispose(&mut self, id:InstanceId) {
        group!(self.logger, "Disposing instance {}.", id, {
            self.free_ids.push(id);
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
        self.size
    }
}
