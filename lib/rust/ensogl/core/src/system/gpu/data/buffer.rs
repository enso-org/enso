//! This module implements utilities for managing WebGL buffers.

use crate::data::dirty::traits::*;
use crate::prelude::*;
use crate::system::gpu::data::gl_enum::traits::*;
use crate::system::gpu::data::prim::*;

use crate::closure;
use crate::control::callback;
use crate::data::dirty;
use crate::data::seq::observable::Observable;
use crate::debug::stats::Stats;
use crate::system::gpu::data::attribute;
use crate::system::gpu::data::attribute::Attribute;
use crate::system::gpu::data::buffer::item::JsBufferView;
use crate::system::gpu::data::buffer::usage::BufferUsage;
use crate::system::gpu::data::default::gpu_default;
use crate::system::Context;

use enso_shapely::shared;
use std::iter::Extend;
use std::ops::RangeInclusive;
use web_sys::WebGlBuffer;


// ==============
// === Export ===
// ==============

pub mod item;
pub mod usage;

pub use crate::system::gpu::data::Storable;



// =============
// === Types ===
// =============

/// A vector which fires events whenever it is modified or resized.
pub type ObservableVec<T> = Observable<Vec<T>, OnMut, OnResize>;

/// Dirty flag keeping track of the range of modified elements.
pub type MutDirty = dirty::SharedRange<usize, Box<dyn callback::NoArgs>>;

/// Dirty flag keeping track of whether the buffer was resized.
pub type ResizeDirty = dirty::SharedBool<Box<dyn callback::NoArgs>>;

closure! {
fn on_resize_fn(dirty:ResizeDirty) -> OnResize {
    || dirty.set()
}}

closure! {
fn on_mut_fn(dirty:MutDirty) -> OnMut {
    |ix: usize| dirty.set(ix)
}}



// ==============
// === GlData ===
// ==============

/// The WebGL data of the buffer. This data is missing from buffer instance if the buffer is not
/// bound to WebGL context. See the main architecture docs of this library to learn more.
#[derive(Debug)]
pub struct GlData {
    context: Context,
    buffer:  WebGlBuffer,
}

impl GlData {
    /// Constructor.
    pub fn new(context: &Context) -> Self {
        let context = context.clone();
        let buffer = create_gl_buffer(&context);
        Self { context, buffer }
    }
}



// ==============
// === Buffer ===
// ==============

shared! { Buffer
/// CPU-counterpart of WebGL buffers. The buffer can contain all GPU related data like vertex
/// positions, colors, or shader parameters. Geometries use multiple buffers to store the required
/// display data. The buffer data is synchronised with GPU on demand, usually in the update stage
/// before drawing the frame.
///
/// # Design
/// Buffers use [`ObservableVec`] under the hood. After an element is modified, buffer stories the
/// information about modified indexes in its dirty flags. Only the information about smallest and
/// biggest index is stored currently, as sending a big chunk of data to the GPU is often way
/// cheaper than sending many small updates. However, this depends on many parameters, including
/// chunks size and type of the used hardware, so it may be further optimized in the future.
///
/// # Contextless Buffers
/// Buffers were designed to work without active WebGL context. In such a case, the data is stored
/// on CPU-side and will be uploaded to the GPU as soon as the context is bound or restored.
#[derive(Debug)]
pub struct BufferData<T> {
    logger        : Logger,
    gl            : Option<GlData>,
    buffer        : ObservableVec<T>,
    mut_dirty     : MutDirty,
    resize_dirty  : ResizeDirty,
    usage         : BufferUsage,
    stats         : Stats,
    gpu_mem_usage : u32,
}

impl<T:Storable> {
    /// Constructor.
    pub fn new<OnMut:callback::NoArgs, OnResize:callback::NoArgs>
    (logger:Logger, stats:&Stats, on_mut:OnMut, on_resize:OnResize) -> Self {
        info!(logger,"Creating new {T::type_display()} buffer.", || {
            stats.inc_buffer_count();
            let logger        = logger.clone();
            let sublogger     = Logger::new_sub(&logger,"mut_dirty");
            let mut_dirty     = MutDirty::new(sublogger,Box::new(on_mut));
            let sublogger     = Logger::new_sub(&logger,"resize_dirty");
            let resize_dirty  = ResizeDirty::new(sublogger, Box::new(on_resize));
            resize_dirty.set();
            let on_resize_fn  = on_resize_fn(resize_dirty.clone_ref());
            let on_mut_fn     = on_mut_fn(mut_dirty.clone_ref());
            let buffer        = ObservableVec::new(on_mut_fn,on_resize_fn);
            let usage         = default();
            let stats         = stats.clone_ref();
            let gpu_mem_usage = default();
            let gl            = default();
            Self {logger,gl,buffer,mut_dirty,resize_dirty,usage,stats,gpu_mem_usage}
        })
    }

    /// Return the number of elements in the buffer.
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    /// Check if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    /// Read the usage pattern of the buffer.
    pub fn usage(&self) -> BufferUsage {
        self.usage
    }

    /// Set the usage pattern of the buffer.
    pub fn set_usage(&mut self, usage:BufferUsage) {
        self.usage = usage;
        self.resize_dirty.set();
    }

    /// Get a copy of the data by its index.
    pub fn get(&self, index:usize) -> T {
        *self.buffer.index(index)
    }

    /// Set data value at the given index.
    pub fn set(&mut self, index:usize, value:T) {
        *self.buffer.index_mut(index) = value;
    }

    /// Set the data at the given index to a default value.
    pub fn set_to_default(&mut self, index:usize) {
        self.set(index,gpu_default());
    }

    /// Add a single new element initialized to default value.
    pub fn add_element(&mut self) {
        self.add_elements(1);
    }

    /// Add multiple new elements initialized to default values.
    pub fn add_elements(&mut self, elem_count:usize) {
        self.extend(iter::repeat(T::gpu_default()).take(elem_count));
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        info!(self.logger, "Updating.", || {
            if let Some(gl) = &self.gl {
                gl.context.bind_buffer(Context::ARRAY_BUFFER,Some(&gl.buffer));
                if self.resize_dirty.check() {
                    self.upload_data(&None);
                } else if self.mut_dirty.check_all() {
                    self.upload_data(&self.mut_dirty.take().range);
                } else {
                    warning!(self.logger,"Update requested but it was not needed.");
                }
                self.mut_dirty.unset_all();
                self.resize_dirty.unset();
            }
        })
    }

    /// Bind the underlying WebGLBuffer to a given target.
    /// https://developer.mozilla.org/docs/Web/API/WebGLRenderingContext/bindBuffer
    pub fn bind(&self, target:u32) {
        if let Some(gl) = &self.gl {
            gl.context.bind_buffer(target,Some(&gl.buffer));
        }
    }

    /// Bind the buffer currently bound to gl.ARRAY_BUFFER to a generic vertex attribute of the
    /// current vertex buffer object and specifies its layout. Please note that this function is
    /// more complex that a raw call to `WebGLRenderingContext.vertexAttribPointer`, as it correctly
    /// handles complex data types like `mat4`. See the following links to learn more:
    /// https://developer.mozilla.org/docs/Web/API/WebGLRenderingContext/vertexAttribPointer
    /// https://stackoverflow.com/questions/38853096/webgl-how-to-bind-values-to-a-mat4-attribute
    pub fn vertex_attrib_pointer(&self, loc:u32, instanced:bool) {
        if let Some(gl) = &self.gl {
            let item_byte_size = T::item_gpu_byte_size() as i32;
            let item_type      = T::item_gl_enum().into();
            let rows           = T::rows() as i32;
            let cols           = T::cols() as i32;
            let col_byte_size  = item_byte_size * rows;
            let stride         = col_byte_size  * cols;
            let normalize      = false;
            for col in 0..cols {
                let lloc = loc + col as u32;
                let off  = col * col_byte_size;
                gl.context.enable_vertex_attrib_array(lloc);
                gl.context.vertex_attrib_pointer_with_i32(lloc,rows,item_type,normalize,stride,off);
                if instanced {
                    let instance_count = 1;
                    gl.context.vertex_attrib_divisor(lloc,instance_count);
                }
            }
        }
    }

    /// Set the WebGL context. See the main architecture docs of this library to learn more.
    pub(crate) fn set_context(&mut self, context:Option<&Context>) {
        self.gl = context.map(|ctx| {
            self.resize_dirty.set();
            GlData::new(ctx)
        });
        if self.gl.is_none() {
            self.drop_stats()
        }
    }
}}


// === Private API ===

impl<T: Storable> BufferData<T> {
    /// View the data as slice of primitive elements.
    pub fn as_prim_slice(&self) -> &[item::Cell<T>] {
        <T as Storable>::slice_to_items(&self.buffer.data)
    }

    /// View the data as slice of elements.
    pub fn as_slice(&self) -> &[T] {
        &self.buffer.data
    }
}


// === Data Upload ===

// Note [Safety]
// =============
// Usage of `js_buffer_view` is somewhat dangerous. It is creating a raw view into the module's
// `WebAssembly.Memory` buffer, but if we allocate more pages (aka do a memory allocation in Rust)
// it'll cause the buffer to change, causing the resulting js array to be invalid.

impl<T: Storable> BufferData<T> {
    /// Upload the provided data range to the GPU buffer. In case the local buffer was resized,
    /// it will be re-created on the GPU.
    fn upload_data(&mut self, opt_range: &Option<RangeInclusive<usize>>) {
        info!(self.logger, "Uploading buffer data.", || {
            self.stats.inc_data_upload_count();
            match opt_range {
                None => self.replace_gpu_buffer(),
                Some(range) => self.update_gpu_sub_buffer(range),
            }
        });
    }

    /// Replace the whole GPU buffer by the local data.
    #[allow(unsafe_code)]
    fn replace_gpu_buffer(&mut self) {
        if let Some(gl) = &self.gl {
            let data = self.as_slice();
            let gl_enum = self.usage.to_gl_enum().into();
            unsafe {
                // Note [Safety]
                let js_array = data.js_buffer_view();
                gl.context.buffer_data_with_array_buffer_view(
                    Context::ARRAY_BUFFER,
                    &js_array,
                    gl_enum,
                );
            }
            crate::if_compiled_with_stats! {
                let item_byte_size    = T::item_gpu_byte_size() as u32;
                let item_count        = T::item_count() as u32;
                let new_gpu_mem_usage = self.len() as u32 * item_count * item_byte_size;
                self.stats.mod_gpu_memory_usage(|s| s - self.gpu_mem_usage);
                self.stats.mod_gpu_memory_usage(|s| s + new_gpu_mem_usage);
                self.stats.mod_data_upload_size(|s| s + new_gpu_mem_usage);
                self.gpu_mem_usage = new_gpu_mem_usage;
            }
        }
    }

    /// Update the GPU sub-buffer data by the provided index range.
    #[allow(unsafe_code)]
    fn update_gpu_sub_buffer(&mut self, range: &RangeInclusive<usize>) {
        if let Some(gl) = &self.gl {
            let data = self.as_slice();
            let item_byte_size = T::item_gpu_byte_size() as u32;
            let item_count = T::item_count() as u32;
            let start = *range.start() as u32;
            let end = *range.end() as u32;
            let start_item = start * item_count;
            let length = (end - start + 1) * item_count;
            let dst_byte_offset = (item_byte_size * item_count * start) as i32;
            unsafe {
                // Note [Safety]
                let js_array = data.js_buffer_view();
                gl.context.buffer_sub_data_with_i32_and_array_buffer_view_and_src_offset_and_length(
                    Context::ARRAY_BUFFER,
                    dst_byte_offset,
                    &js_array,
                    start_item,
                    length,
                )
            }
            self.stats.mod_data_upload_size(|s| s + length * item_byte_size);
        }
    }
}


// === Stats ===

impl<T> BufferData<T> {
    fn drop_stats(&self) {
        self.stats.mod_gpu_memory_usage(|s| s - self.gpu_mem_usage);
        self.stats.dec_buffer_count();
    }
}


// === Smart Accessors ===

impl<T: Storable> Buffer<T> {
    /// Get the attribute pointing to a given buffer index.
    pub fn at(&self, index: attribute::InstanceIndex) -> Attribute<T> {
        Attribute::new(index, self.clone_ref())
    }
}


// === Instances ===

impl<T> Deref for BufferData<T> {
    type Target = ObservableVec<T>;
    fn deref(&self) -> &Self::Target {
        &self.buffer
    }
}

impl<T> DerefMut for BufferData<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buffer
    }
}

impl<T> Drop for BufferData<T> {
    fn drop(&mut self) {
        if let Some(gl) = &self.gl {
            gl.context.delete_buffer(Some(&gl.buffer));
            self.drop_stats();
        }
    }
}


// === Utils ===

fn create_gl_buffer(context: &Context) -> WebGlBuffer {
    let buffer = context.create_buffer();
    buffer.ok_or("Failed to create WebGL buffer.").unwrap()
}



// =================
// === AnyBuffer ===
// =================

use enum_dispatch::*;


// === Macros ===

/// Variant mismatch error type.
#[derive(Clone, Copy, Debug)]
pub struct BadVariant;

macro_rules! define_any_buffer {
([] [$([$base:ident $param:ident])*]) => { paste::item! {

    /// An enum with a variant per possible buffer type (i32, f32, Vector<f32>,
    /// and many, many more). It provides a faster alternative to dyn trait one:
    /// `Buffer<dyn Storable, OnMut, OnResize>`.
    #[enum_dispatch(IsBuffer)]
    #[derive(Clone,Debug)]
    #[allow(missing_docs)]
    pub enum AnyBuffer {
        $([<Variant $base For $param>](Buffer<$base<$param>>)),*
    }

    $(
        impl<'t> TryFrom<&'t AnyBuffer> for &'t Buffer<$base<$param>> {
            type Error = BadVariant;
            fn try_from(t:&'t AnyBuffer) -> Result <&'t Buffer<$base<$param>>,Self::Error> {
                match t {
                    AnyBuffer::[<Variant $base For $param>](a) => Ok(a),
                    _ => Err(BadVariant)
                }
            }
        }

        impl<'t> TryFrom<&'t mut AnyBuffer> for &'t mut Buffer<$base<$param>> {
            type Error = BadVariant;
            fn try_from(t:&'t mut AnyBuffer) -> Result <&'t mut Buffer<$base<$param>>,Self::Error> {
                match t {
                    AnyBuffer::[<Variant $base For $param>](a) => Ok(a),
                    _ => Err(BadVariant)
                }
            }
        }
    )*
}}}


// === Definition ===

crate::with_all_prim_types!([[define_any_buffer] []]);

/// Collection of all methods common to every buffer variant.
#[enum_dispatch]
#[allow(missing_docs)]
pub trait IsBuffer {
    /// Set the WebGL context. See the main architecture docs of this library to learn more.
    fn set_context(&self, context: Option<&Context>);
    fn add_element(&self);
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn update(&self);
    fn bind(&self, target: u32);
    fn vertex_attrib_pointer(&self, index: u32, instanced: bool);
    fn set_to_default(&self, index: usize);
}

// Calls are not recursive, as inherent methods are preferred over methods provided by trait.
// This implementation is needed, because `enum_dispatch` library requires variant types to
// implement the trait, as it invokes trait methods explicitly on variant values.
//
// Thus we provide implementation that just redirects calls to methods defined in the Buffer itself.
impl<T: Storable> IsBuffer for Buffer<T> {
    fn set_context(&self, context: Option<&Context>) {
        self.set_context(context)
    }
    fn add_element(&self) {
        self.add_element()
    }
    fn len(&self) -> usize {
        self.len()
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
    fn update(&self) {
        self.update()
    }
    fn bind(&self, target: u32) {
        self.bind(target)
    }
    fn vertex_attrib_pointer(&self, index: u32, instanced: bool) {
        self.vertex_attrib_pointer(index, instanced)
    }
    fn set_to_default(&self, index: usize) {
        self.set_to_default(index)
    }
}
