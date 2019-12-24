pub mod item;

use crate::prelude::*;

use crate::display::render::webgl::Context;
use crate::closure;
use crate::data::function::callback::*;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::data::seq::observable::Observable;
use crate::system::web::fmt;
use crate::system::web::group;
use crate::system::web::Logger;
use item::Item;
use item::Prim;
use nalgebra::Vector2;
use nalgebra::Vector3;
use nalgebra::Vector4;
use nalgebra::Matrix4;
use std::iter::Extend;
use web_sys::WebGlBuffer;
use std::ops::RangeInclusive;



// ==================
// === BufferData ===
// ==================

// === Definition ===

/// Please refer to the 'Buffer management pipeline' doc to learn more about
/// attributes, scopes, geometries, meshes, scenes, and other relevant concepts.
///
/// Buffers are values stored in geometry. Under the hood they are stored in
/// vectors and are synchronised with GPU buffers on demand.
#[derive(Derivative,Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derivative(Debug(bound="T:Debug"))]
pub struct BufferData<T,OnMut,OnResize> {
    #[shrinkwrap(main_field)]
    pub buffer       : Data        <T,OnMut,OnResize>,
    pub buffer_dirty : BufferDirty <OnMut>,
    pub resize_dirty : ResizeDirty <OnResize>,
    pub logger       : Logger,
    pub gl_buffer    : WebGlBuffer,
    context          : Context
}


// === Types ===

pub type ObservableVec<T,OnMut,OnResize> = Observable<Vec<T>,OnMut,OnResize>;
pub type Data<T,OnMut,OnResize> = ObservableVec<T,DataOnSet<OnMut>,DataOnResize<OnResize>>;

#[macro_export]
macro_rules! promote_buffer_types { ($callbacks:tt $module:ident) => {
    promote! { $callbacks $module [Var<T>,BufferData<T>,Buffer<T>,AnyBuffer] }
};}


// === Callbacks ===

pub type BufferDirty <Callback> = dirty::SharedRange<usize,Callback>;
pub type ResizeDirty <Callback> = dirty::SharedBool<Callback>;

closure! {
fn buffer_on_resize<C:Callback0> (dirty:ResizeDirty<C>) -> DataOnResize {
    || dirty.set()
}}

closure! {
fn buffer_on_mut<C:Callback0> (dirty:BufferDirty<C>) -> DataOnSet {
    |ix: usize| dirty.set(ix)
}}


// === Instances ===

impl<T,OnMut:Callback0, OnResize:Callback0>
BufferData<T,OnMut,OnResize> {

    /// Creates a new empty buffer.
    pub fn new(context: &Context, logger:Logger, on_mut:OnMut, on_resize:OnResize)
    -> Self {
        logger.info(fmt!("Creating new {} buffer.", T::type_display()));
        let set_logger     = logger.sub("buffer_dirty");
        let resize_logger  = logger.sub("resize_dirty");
        let buffer_dirty   = BufferDirty::new(set_logger,on_mut);
        let resize_dirty   = ResizeDirty::new(resize_logger,on_resize);
        let buff_on_resize = buffer_on_resize(resize_dirty.clone_ref());
        let buff_on_mut    = buffer_on_mut(buffer_dirty.clone_ref());
        let buffer         = Data::new(buff_on_mut, buff_on_resize);
        let context        = context.clone();
        let gl_buffer      = create_gl_buffer(&context);
        Self {buffer,buffer_dirty,resize_dirty,logger,gl_buffer,context}
    }

    /// Build the buffer from the provider configuration builder.
    pub fn build(context:&Context, builder:Builder, on_mut:OnMut, on_resize:OnResize) -> Self {
        let logger = builder._logger.unwrap_or_else(default);
        Self::new(context,logger,on_mut,on_resize)
    }
}

impl<T:Item,OnMut,OnResize>
BufferData<T,OnMut,OnResize> {

    /// View the data as slice of primitive elements.
    pub fn as_prim_slice(&self) -> &[Prim<T>] {
        <T as Item>::convert_prim_buffer(&self.buffer.data)
    }

    /// View the data as slice of elements.
    pub fn as_slice(&self) -> &[T] {
        &self.buffer.data
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            let data = self.as_slice();
            self.context.bind_buffer(Context::ARRAY_BUFFER, Some(&self.gl_buffer));
            if self.resize_dirty.check() {
                self.upload_data(&self.context,data,&None);
            } else if self.buffer_dirty.check_all() {
                let range = &self.buffer_dirty.take().range;
                self.upload_data(&self.context,data,range);
            }
            self.buffer_dirty.unset_all();
            self.resize_dirty.unset();
        })
    }

    /// Uploads the provided data to the GPU buffer.
    fn upload_data(&self, context:&Context, data:&[T], opt_range:&Option<RangeInclusive<usize>>) {
        // Note that `js_buffer_view` is somewhat dangerous (hence the `unsafe`!). This is creating
        // a raw view into our module's `WebAssembly.Memory` buffer, but if we allocate more pages
        // for ourself (aka do a memory allocation in Rust) it'll cause the buffer to change,
        // causing the resulting js array to be invalid.
        //
        // As a result, after `js_buffer_view` we have to be very careful not to do any memory
        // allocations before it's dropped.

        self.logger.info("Setting buffer data.");

        match opt_range {
            None => unsafe {
                let js_array = data.js_buffer_view();
                context.buffer_data_with_array_buffer_view
                (Context::ARRAY_BUFFER, &js_array, Context::STATIC_DRAW);
            }
            Some(range) => {
                let item_byte_size  = <T as Item>::gpu_prim_byte_size() as u32;
                let item_count      = <T as Item>::item_count() as u32;
                let start           = *range.start() as u32;
                let end             = *range.end()   as u32;
                let length          = (end - start + 1) * item_count;
                let dst_byte_offset = (item_byte_size * start) as i32;
                unsafe {
                    let js_array = data.js_buffer_view();
                    context.buffer_sub_data_with_i32_and_array_buffer_view_and_src_offset_and_length
                    (Context::ARRAY_BUFFER,dst_byte_offset,&js_array,start,length)
                }
            }
        }
    }

    /// Binds the buffer currently bound to gl.ARRAY_BUFFER to a generic vertex attribute of the
    /// current vertex buffer object and specifies its layout. Please note that this function is
    /// more complex that a raw call to `WebGLRenderingContext.vertexAttribPointer`, as it correctly
    /// handles complex data types like `mat4`. See the following links to learn more:
    /// https://developer.mozilla.org/docs/Web/API/WebGLRenderingContext/vertexAttribPointer
    /// https://stackoverflow.com/questions/38853096/webgl-how-to-bind-values-to-a-mat4-attribute
    pub fn vertex_attrib_pointer(&self, loc:u32, instanced:bool) {
        let item_byte_size = <T as Item>::gpu_prim_byte_size() as i32;
        let item_type      = <T as Item>::gpu_item_type();
        let rows           = <T as Item>::rows() as i32;
        let cols           = <T as Item>::cols() as i32;
        let col_byte_size  = item_byte_size * rows;
        let stride         = col_byte_size  * cols;
        let normalize      = false;
        for col in 0..cols {
            let lloc = loc + col as u32;
            let off  = col * col_byte_size;
            self.context.enable_vertex_attrib_array(lloc);
            self.context.vertex_attrib_pointer_with_i32(lloc,rows,item_type,normalize,stride,off);
            if instanced {
                self.context.vertex_attrib_divisor(lloc, 1);
            }
        }
    }
}

impl<T,OnMut,OnResize>
BufferData<T,OnMut,OnResize> {
    /// Returns a new buffer `Builder` object.
    pub fn builder() -> Builder {
        default()
    }

    /// Returns the number of elements in the buffer.
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    /// Checks if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    /// Binds the underlying WebGLBuffer to a given target.
    /// https://developer.mozilla.org/docs/Web/API/WebGLRenderingContext/bindBuffer
    pub fn bind(&self, target:u32) {
        self.context.bind_buffer(target, Some(&self.gl_buffer));
    }
}

pub trait AddElementCtx<T,OnResize> = where
    T: Item + Clone,
    OnResize: Callback0;

impl<T,OnMut,OnResize>
BufferData<T,OnMut,OnResize> where Self: AddElementCtx<T,OnResize> {
    /// Adds a single new element initialized to default value.
    pub fn add_element(&mut self) {
        self.add_elements(1);
    }

    /// Adds multiple new elements initialized to default values.
    pub fn add_elements(&mut self, elem_count: usize) {
        self.extend(iter::repeat(T::empty()).take(elem_count));
    }
}

impl<T,OnMut,OnResize>
Index<usize> for BufferData<T,OnMut,OnResize> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.buffer.index(index)
    }
}

impl<T,OnMut:Callback0,OnResize>
IndexMut<usize> for BufferData<T,OnMut,OnResize> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.buffer.index_mut(index)
    }
}


// === Utils ===

fn create_gl_buffer(context:&Context) -> WebGlBuffer {
    let buffer = context.create_buffer();
    buffer.ok_or("failed to create buffer").unwrap()
}



// ==============
// === Buffer ===
// ==============

/// Shared view for `Buffer`.
#[derive(Derivative)]
#[derivative(Debug(bound="T:Debug"))]
#[derivative(Clone(bound=""))]
pub struct Buffer<T,OnMut,OnResize> {
    rc: Rc<RefCell<BufferData<T,OnMut,OnResize>>>
}

impl<T, OnMut:Callback0, OnResize:Callback0>
Buffer<T,OnMut,OnResize> {

    /// Creates a new empty buffer.
    pub fn new
    (context:&Context, logger:Logger, on_mut:OnMut, on_resize:OnResize)
     -> Self {
        let data = BufferData::new(context, logger, on_mut, on_resize);
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }

    /// Build the buffer from the provided configuration builder.
    pub fn build(context:&Context, builder:Builder, on_mut:OnMut, on_resize:OnResize) -> Self {
        let data = BufferData::build(context, builder, on_mut, on_resize);
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }
}

impl<T:Item,OnMut,OnResize>
Buffer<T,OnMut,OnResize> {
    /// Check dirty flags and update the state accordingly.
    pub fn update(&self) {
        self.rc.borrow_mut().update()
    }

    /// binds the buffer currently bound to gl.ARRAY_BUFFER to a generic vertex
    /// attribute of the current vertex buffer object and specifies its layout.
    /// https://developer.mozilla.org/docs/Web/API/WebGLRenderingContext/vertexAttribPointer
    pub fn vertex_attrib_pointer(&self, index:u32, instanced:bool) {
        self.rc.borrow().vertex_attrib_pointer(index,instanced)
    }
}

impl<T,OnMut,OnResize>
Buffer<T,OnMut,OnResize> {
    /// Get the variable by given index.
    pub fn get(&self, index:usize) -> Var<T,OnMut,OnResize> {
        Var::new(index,self.clone())
    }

    /// Returns the number of elements in the buffer.
    pub fn len(&self) -> usize {
        self.rc.borrow().len()
    }

    /// Checks if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.rc.borrow().is_empty()
    }

    /// Binds the underlying WebGLBuffer to a given target.
    /// https://developer.mozilla.org/docs/Web/API/WebGLRenderingContext/bindBuffer
    pub fn bind(&self, target:u32) {
        self.rc.borrow().bind(target)
    }
}

impl<T,OnMut,OnResize>
Buffer<T,OnMut,OnResize> where (): AddElementCtx<T,OnResize> {
    /// Adds a single new element initialized to default value.
    pub fn add_element(&self){
        self.rc.borrow_mut().add_element()
    }
}

impl <T,OnMut,OnResize>
From<Rc<RefCell<BufferData<T,OnMut,OnResize>>>> for Buffer<T,OnMut,OnResize> {
    fn from(rc: Rc<RefCell<BufferData<T, OnMut, OnResize>>>) -> Self {
        Self {rc}
    }
}



// ===========
// === Var ===
// ===========

/// View for a particular buffer. Allows reading and writing buffer data
/// via the internal mutability pattern. It is implemented as a view on
/// a selected `Buffer` element under the hood.
#[derive(Clone,Derivative)]
#[derivative(Debug(bound="T:Debug"))]
pub struct Var<T,OnMut,OnResize> {
    index  : usize,
    buffer : Buffer<T,OnMut,OnResize>
}

impl<T,OnMut,OnResize> Var<T,OnMut,OnResize> {
    /// Creates a new variable as an indexed view over provided buffer.
    pub fn new(index:usize, buffer: Buffer<T,OnMut,OnResize>) -> Self {
        Self {index, buffer}
    }
}

impl<T:Copy,OnMut:Callback0,OnResize> Var<T,OnMut,OnResize> {
    /// Gets immutable reference to the underlying data.
    pub fn get(&self) -> T {
        *self.buffer.rc.borrow().index(self.index)
    }

    /// Sets the variable to a new value.
    pub fn set(&self, value:T) {
        *self.buffer.rc.borrow_mut().index_mut(self.index) = value;
    }

    /// Modifies the underlying data by using the provided function.
    pub fn modify<F:FnOnce(&mut T)>(&self, f:F) {
        let mut value = self.get();
        f(&mut value);
        self.set(value);
    }
}



// ===============
// === Builder ===
// ===============

/// Buffer builder.
#[derive(Derivative)]
#[derivative(Default(bound=""))]
pub struct Builder {
    pub _logger : Option <Logger>
}

impl Builder {
    /// Creates a new builder object.
    pub fn new() -> Self {
        default()
    }

    /// Sets the logger.
    pub fn logger(self, val: Logger) -> Self {
        Self { _logger: Some(val) }
    }
}



// ========================
// === TO BE REFACTORED ===
// ========================

// TODO The following code should be refactored to use the new macro `eval-tt`
// TODO engine. Some utils, like `cartesian` macro should also be refactored
// TODO out.

macro_rules! cartesian_impl {
    ($out:tt [] $b:tt $init_b:tt, $f:ident) => {
        $f!{ $out }
    };
    ($out:tt [$a:ident, $($at:tt)*] [] $init_b:tt, $f:ident) => {
        cartesian_impl!{ $out [$($at)*] $init_b $init_b, $f }
    };
    ([$($out:tt)*] [$a:ident, $($at:tt)*] [$b:ident, $($bt:tt)*] $init_b:tt
    ,$f:ident) => {
        cartesian_impl!{
            [$($out)* ($a, $b),] [$a, $($at)*] [$($bt)*] $init_b, $f
        }
    };
}

macro_rules! cartesian {
    ([$($a:tt)*], [$($b:tt)*], $f:ident) => {
        cartesian_impl!{ [] [$($a)*,] [$($b)*,] [$($b)*,], $f }
    };
}



// =================
// === AnyBuffer ===
// =================

use enum_dispatch::*;

// === Macros ===

#[derive(Debug)]
pub struct BadVariant;

macro_rules! mk_any_buffer_impl {
([$(($base:ident, $param:ident)),*,]) => { paste::item! {

    /// An enum with a variant per possible buffer type (i32, f32, Vector<f32>,
    /// and many, many more). It provides a faster alternative to dyn trait one:
    /// `Buffer<dyn Item, OnMut, OnResize>`.
    #[enum_dispatch(IsBuffer)]
    #[derive(Derivative)]
    #[derivative(Debug(bound=""))]
    pub enum AnyBuffer<OnMut, OnResize> {
        $(  [<Variant $base For $param>]
                (Buffer<$base<$param>, OnMut, OnResize>),
        )*
    }

    $( // ======================================================================

    impl<'t, T, S>
    TryFrom<&'t AnyBuffer<T, S>>
    for &'t Buffer<$base<$param>, T, S> {
        type Error = BadVariant;
        fn try_from(v: &'t AnyBuffer<T, S>)
        -> Result <&'t Buffer<$base<$param>, T, S>, Self::Error> {
            match v {
                AnyBuffer::[<Variant $base For $param>](a) => Ok(a),
                _ => Err(BadVariant)
            }
        }
    }

    impl<'t, T, S>
    TryFrom<&'t mut AnyBuffer<T, S>>
    for &'t mut Buffer<$base<$param>, T, S> {
        type Error = BadVariant;
        fn try_from(v: &'t mut AnyBuffer<T, S>)
        -> Result <&'t mut Buffer<$base<$param>, T, S>, Self::Error> {
            match v {
                AnyBuffer::[<Variant $base For $param>](a) => Ok(a),
                _ => Err(BadVariant)
            }
        }
    }

    )* // ======================================================================
}
}}

macro_rules! mk_any_buffer {
    ($bases:tt, $params:tt) => {
        cartesian!($bases, $params, mk_any_buffer_impl);
    }
}


// === Definition ===

type Identity<T> = T;
mk_any_buffer!([Identity,Vector2,Vector3,Vector4,Matrix4], [f32]);

/// Collection of all methods common to every buffer variant.
#[enum_dispatch]
pub trait IsBuffer<OnMut: Callback0, OnResize: Callback0> {
    fn add_element(&self);
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
    fn update(&self);
    fn bind(&self, target:u32);
    fn vertex_attrib_pointer(&self, index:u32, instanced:bool);
}
