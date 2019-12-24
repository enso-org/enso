use crate::display::render::webgl::Context;
use nalgebra::*;
use web_sys::WebGlUniformLocation;



// =============
// === Types ===
// =============

pub trait MatrixCtx<T,R,C> = where
    T:Scalar, R:DimName, C:DimName,
    nalgebra::DefaultAllocator: nalgebra::allocator::Allocator<T, R, C>;



// ====================
// === JSBufferView ===
// ====================

pub trait JSBufferView {
    /// Creates a JS typed array which is a view into wasm's linear memory at the slice specified.
    ///
    /// This function returns a new typed array which is a view into wasm's memory. This view does
    /// not copy the underlying data.
    ///
    /// # Safety
    ///
    /// Views into WebAssembly memory are only valid so long as the backing buffer isn't resized in
    /// JS. Once this function is called any future calls to `Box::new` (or malloc of any form) may
    /// cause the returned value here to be invalidated. Use with caution!
    ///
    /// Additionally the returned object can be safely mutated but the input slice isn't guaranteed
    /// to be mutable.
    ///
    /// Finally, the returned object is disconnected from the input slice's lifetime, so there's no
    /// guarantee that the data is read at the right time.
    unsafe fn js_buffer_view(&self) -> js_sys::Object;
}



// =============
// === Empty ===
// =============

/// Trait for types which have empty value.
pub trait Empty {
    fn empty() -> Self;
}

impl Empty for i32          { fn empty() -> Self { 0   } }
impl Empty for f32          { fn empty() -> Self { 0.0 } }
impl Empty for Vector2<f32> { fn empty() -> Self { Self::new(0.0,0.0)         } }
impl Empty for Vector3<f32> { fn empty() -> Self { Self::new(0.0,0.0,0.0)     } }
impl Empty for Vector4<f32> { fn empty() -> Self { Self::new(0.0,0.0,0.0,1.0) } }
impl Empty for Matrix4<f32> { fn empty() -> Self { Self::identity()           } }
//impl<T,R,C> Empty for MatrixMN<T,R,C> where T:Default, Self:MatrixCtx<T,R,C> {
//    fn empty() -> Self { Self::repeat(default()) }
//}



// =================
// === IsUniform ===
// =================

pub type UniformLocation = WebGlUniformLocation;

pub trait ContextUniformOps<T> {
    fn set_uniform(&self, location:&UniformLocation, value:&T);
}

impl ContextUniformOps<i32> for Context {
    fn set_uniform(&self, location:&UniformLocation, value:&i32) {
        self.uniform1i(Some(location),*value);
    }
}

impl ContextUniformOps<f32> for Context {
    fn set_uniform(&self, location:&UniformLocation, value:&f32) {
        self.uniform1f(Some(location),*value);
    }
}

impl ContextUniformOps<Vector2<f32>> for Context {
    fn set_uniform(&self, location:&UniformLocation, value:&Vector2<f32>) {
        self.uniform_matrix2fv_with_f32_array(Some(location),false,value.data.as_slice());
    }
}

impl ContextUniformOps<Vector3<f32>> for Context {
    fn set_uniform(&self, location:&UniformLocation, value:&Vector3<f32>) {
        self.uniform_matrix3fv_with_f32_array(Some(location),false,value.data.as_slice());
    }
}

impl ContextUniformOps<Vector4<f32>> for Context {
    fn set_uniform(&self, location:&UniformLocation, value:&Vector4<f32>) {
        self.uniform_matrix4fv_with_f32_array(Some(location),false,value.data.as_slice());
    }
}

impl ContextUniformOps<Matrix4<f32>> for Context {
    fn set_uniform(&self, location:&UniformLocation, value:&Matrix4<f32>) {
        self.uniform_matrix4fv_with_f32_array(Some(location),false,value.data.as_slice());
    }
}



// ============
// === Item ===
// ============

// === Definition ===

pub trait JSBufferViewArr = Sized where [Self]:JSBufferView;

/// Class for buffer items, like `f32` or `Vector<f32>`. It defines utils
/// for mapping the item to WebGL buffer and vice versa.
pub trait Item : Empty + JSBufferViewArr {

    // === Types ===

    /// The primitive type which this type is build of. In case of the most primitive types, like
    /// `f32` this type may be set to itself.
    type Prim: Item;

    /// The number of rows of the type encoded as 2d matrix.
    type Rows: DimName;

    /// The number of columns of the type encoded as 2d matrix.
    type Cols: DimName;


    // === Size ===

    /// Returns the number of rows of the type encoded as 2d matrix.
    fn rows() -> usize {
        <Self::Rows as DimName>::dim()
    }

    /// Returns the number of columns of the type encoded as 2d matrix.
    fn cols() -> usize {
        <Self::Cols as DimName>::dim()
    }

    /// Count of primitives of the item. For example, `Vector3<f32>` contains
    /// three primitives (`f32` values).
    fn item_count() -> usize {
        Self::rows() * Self::cols()
    }

    /// Returns the size in bytes in GPU memory of the type.
    fn gpu_byte_size() -> usize {
        Self::gpu_prim_byte_size() * Self::item_count()
    }

    /// Returns the size in bytes in GPU memory of the primitive type of this type.
    fn gpu_prim_byte_size() -> usize {
        Self::Prim::gpu_byte_size()
    }


    // === Type Encoding ===

    /// Returns the WebGL enum representing the item type, like Context::FLOAT.
    fn gpu_item_type() -> u32 {
        Self::Prim::gpu_item_type()
    }


    // === Conversions ===

    /// Conversion from slice of a buffer to the item. Buffers contain primitive
    /// values only, so two `Vector3<f32>` are represented there as six `f32`
    /// values. This allows us to view the buffers using desired types.
    fn from_buffer(buffer: &[Self::Prim]) -> &[Self];

    /// Mutable conversion from slice of a buffer to the item. See the docs for
    /// `from_buffer` to learn more.
    fn from_buffer_mut(buffer: &mut [Self::Prim]) -> &mut [Self];

    // TODO
    // Simplify when this gets resolved: https://github.com/rustsim/nalgebra/issues/687
    fn convert_prim_buffer(buffer: &[Self]) -> &[Self::Prim];
    fn convert_prim_buffer_mut(buffer: &mut [Self]) -> &mut [Self::Prim];
}


// === Type Families ===

pub type Prim <T> = <T as Item>::Prim;
pub type Rows <T> = <T as Item>::Rows;
pub type Cols <T> = <T as Item>::Cols;


// === Instances ===

impl Item for i32 {
    type Prim = Self;
    type Rows = U1;
    type Cols = U1;

    fn gpu_byte_size           () -> usize { 4 }
    fn gpu_item_type           () -> u32 { Context::INT }
    fn from_buffer             (buffer: &    [Self::Prim]) -> &    [Self] { buffer }
    fn from_buffer_mut         (buffer: &mut [Self::Prim]) -> &mut [Self] { buffer }
    fn convert_prim_buffer     (buffer: &    [Self]) -> &    [Self::Prim] { buffer }
    fn convert_prim_buffer_mut (buffer: &mut [Self]) -> &mut [Self::Prim] { buffer }
}

impl JSBufferView for [i32] {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        js_sys::Int32Array::view(self).into()
    }
}

impl Item for f32 {
    type Prim = Self;
    type Rows = U1;
    type Cols = U1;

    fn gpu_byte_size          () -> usize { 4 }
    fn gpu_item_type          () -> u32 { Context::FLOAT }
    fn from_buffer            (buffer: &    [Self::Prim]) -> &    [Self] { buffer }
    fn from_buffer_mut        (buffer: &mut [Self::Prim]) -> &mut [Self] { buffer }
    fn convert_prim_buffer    (buffer: &    [Self]) -> &    [Self::Prim] { buffer }
    fn convert_prim_buffer_mut(buffer: &mut [Self]) -> &mut [Self::Prim] { buffer }
}

impl JSBufferView for [f32] {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        js_sys::Float32Array::view(self).into()
    }
}

impl<T:Item<Prim=T>,R,C> Item for MatrixMN<T,R,C>
    where T:Default, Self:MatrixCtx<T,R,C>, Self:Empty {
    type Prim = T;
    type Rows = R;
    type Cols = C;

    fn from_buffer(buffer: &[Self::Prim]) -> &[Self] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        unsafe {
            let len = buffer.len() / Self::item_count();
            std::slice::from_raw_parts(buffer.as_ptr().cast(), len)
        }
    }

    fn from_buffer_mut(buffer: &mut [Self::Prim]) -> &mut [Self] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        unsafe {
            let len = buffer.len() / Self::item_count();
            std::slice::from_raw_parts_mut(buffer.as_mut_ptr().cast(), len)
        }
    }

    fn convert_prim_buffer(buffer: &[Self]) -> &[Self::Prim] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        let len = buffer.len() * Self::item_count();
        unsafe { std::slice::from_raw_parts(buffer.as_ptr().cast(), len) }
    }

    fn convert_prim_buffer_mut(buffer: &mut [Self]) -> &mut [Self::Prim] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        unsafe {
            let len = buffer.len() * Self::item_count();
            std::slice::from_raw_parts_mut(buffer.as_mut_ptr().cast(), len)
        }
    }
}

impl<T:Item<Prim=T>,R,C> JSBufferView for [MatrixMN<T,R,C>]
where Self:MatrixCtx<T,R,C>, T:Default, MatrixMN<T,R,C>:Item, [Prim<MatrixMN<T,R,C>>]:JSBufferView {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        <MatrixMN<T,R,C> as Item>::convert_prim_buffer(self).js_buffer_view()
    }
}

impl<T:Item<Prim=T>,R,C> JSBufferView for MatrixMN<T,R,C>
where Self:MatrixCtx<T,R,C> {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        self.as_slice().js_buffer_view()
    }
}