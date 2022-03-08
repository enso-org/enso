//! This module defines abstraction for items in buffers stored on GPU.

use crate::prelude::*;
use crate::system::gpu::data::gl_enum::traits::*;
use nalgebra::*;

use crate::system::gpu::data::gl_enum::GlEnum;
use crate::system::gpu::data::sized::GpuKnownSize;
use crate::system::gpu::data::GpuDefault;
use crate::system::gpu::shader::glsl;
use crate::system::gpu::shader::glsl::Glsl;



// =============
// === Types ===
// =============

/// Common Matrix bounds used as super-bounds for many helpers in this module.
pub trait MatrixCtx<T, R, C> = where
    T: Scalar,
    R: DimName,
    C: DimName,
    DefaultAllocator: nalgebra::allocator::Allocator<T, R, C>,
    <DefaultAllocator as nalgebra::allocator::Allocator<T, R, C>>::Buffer: Copy;



// ============
// === Item ===
// ============

// === Definition ===

pub trait JsBufferViewArr = Sized where [Self]: JsBufferView;

/// Super bounds of the `Item::Item` type;
pub trait ItemBounds = Storable + PhantomInto<GlEnum>;

/// Super bounds of the `Item` trait.
pub trait BufferItemBounds =
    Copy + GpuDefault + JsBufferViewArr + PhantomInto<glsl::PrimType> + Into<Glsl> + GpuKnownSize;

/// GPU Buffer item.
pub trait Item {
    /// Storable representation of the item.
    type Storable: Storable;
}

impl<T: Storable> Item for T {
    type Storable = T;
}



/// Class for buffer items, like `f32` or `Vector<f32>`.
///
/// WebGL buffers contain primitive values only, so for example, two `Vector3<f32>` are represented
/// as six `f32` values. This trait defines fast conversions (views) for the underlying flat data
/// storage.
pub trait Storable: BufferItemBounds {
    // === Types ===

    /// The primitive type which this type is build of. In case of the most primitive types, like
    /// `f32` this type may be set to itself.
    type Cell: ItemBounds;

    /// The number of rows of the type encoded as 2d matrix.
    type Rows: DimName;

    /// The number of columns of the type encoded as 2d matrix.
    type Cols: DimName;


    // === Size ===

    /// Return the number of rows of the type encoded as 2d matrix.
    fn rows() -> usize {
        <Self::Rows as DimName>::dim()
    }

    /// Return the number of columns of the type encoded as 2d matrix.
    fn cols() -> usize {
        <Self::Cols as DimName>::dim()
    }

    /// Count of primitives of the item. For example, `Vector3<f32>` contains
    /// three primitives (`f32` values).
    fn item_count() -> usize {
        Self::rows() * Self::cols()
    }


    // === Conversions ===

    /// Conversion from a slice of items to a buffer slice.
    fn slice_from_items(buffer: &[Self::Cell]) -> &[Self];

    /// Conversion from a mutable slice of items to a mutable buffer slice.
    fn slice_from_items_mut(buffer: &mut [Self::Cell]) -> &mut [Self];

    /// Converts from a buffer slice to a slice of items.
    fn slice_to_items(buffer: &[Self]) -> &[Self::Cell];

    /// Converts from a mutable buffer slice to a mutable slice of items.
    fn slice_to_items_mut(buffer: &mut [Self]) -> &mut [Self::Cell];


    // === Temporary Helpers ===

    // TODO: Remove when it gets resolved: https://github.com/rust-lang/rust/issues/68210
    /// Returns the WebGL enum code representing the item type, like Context::FLOAT.
    fn item_gl_enum() -> GlEnum {
        Self::Cell::gl_enum()
    }

    // TODO: Remove when it gets resolved: https://github.com/rust-lang/rust/issues/68210
    /// Returns the size in bytes in GPU memory of the primitive type of this type.
    fn item_gpu_byte_size() -> usize {
        Self::Cell::gpu_byte_size()
    }
}


// === Type Families ===

/// Item accessor.
pub type Cell<T> = <T as Storable>::Cell;

/// Rows accessor.
pub type Rows<T> = <T as Storable>::Rows;

/// Cols accessor.
pub type Cols<T> = <T as Storable>::Cols;


// === Instances ===


impl Storable for bool {
    type Cell = Self;
    type Rows = U1;
    type Cols = U1;

    fn slice_from_items(buffer: &[Self::Cell]) -> &[Self] {
        buffer
    }
    fn slice_from_items_mut(buffer: &mut [Self::Cell]) -> &mut [Self] {
        buffer
    }
    fn slice_to_items(buffer: &[Self]) -> &[Self::Cell] {
        buffer
    }
    fn slice_to_items_mut(buffer: &mut [Self]) -> &mut [Self::Cell] {
        buffer
    }
}

impl Storable for i32 {
    type Cell = Self;
    type Rows = U1;
    type Cols = U1;

    fn slice_from_items(buffer: &[Self::Cell]) -> &[Self] {
        buffer
    }
    fn slice_from_items_mut(buffer: &mut [Self::Cell]) -> &mut [Self] {
        buffer
    }
    fn slice_to_items(buffer: &[Self]) -> &[Self::Cell] {
        buffer
    }
    fn slice_to_items_mut(buffer: &mut [Self]) -> &mut [Self::Cell] {
        buffer
    }
}

impl Storable for u32 {
    type Cell = Self;
    type Rows = U1;
    type Cols = U1;

    fn slice_from_items(buffer: &[Self::Cell]) -> &[Self] {
        buffer
    }
    fn slice_from_items_mut(buffer: &mut [Self::Cell]) -> &mut [Self] {
        buffer
    }
    fn slice_to_items(buffer: &[Self]) -> &[Self::Cell] {
        buffer
    }
    fn slice_to_items_mut(buffer: &mut [Self]) -> &mut [Self::Cell] {
        buffer
    }
}

impl Storable for f32 {
    type Cell = Self;
    type Rows = U1;
    type Cols = U1;

    fn slice_from_items(buffer: &[Self::Cell]) -> &[Self] {
        buffer
    }
    fn slice_from_items_mut(buffer: &mut [Self::Cell]) -> &mut [Self] {
        buffer
    }
    fn slice_to_items(buffer: &[Self]) -> &[Self::Cell] {
        buffer
    }
    fn slice_to_items_mut(buffer: &mut [Self]) -> &mut [Self::Cell] {
        buffer
    }
}

impl<T: Storable<Cell = T>, R, C> Storable for OMatrix<T, R, C>
where
    T: ItemBounds,
    Self: MatrixCtx<T, R, C>,
    Self: GpuDefault + PhantomInto<glsl::PrimType> + GpuKnownSize,
{
    type Cell = T;
    type Rows = R;
    type Cols = C;

    #[allow(unsafe_code)]
    fn slice_from_items(buffer: &[Self::Cell]) -> &[Self] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        let len = buffer.len() / Self::item_count();
        unsafe { std::slice::from_raw_parts(buffer.as_ptr().cast(), len) }
    }

    #[allow(unsafe_code)]
    fn slice_from_items_mut(buffer: &mut [Self::Cell]) -> &mut [Self] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        let len = buffer.len() / Self::item_count();
        unsafe { std::slice::from_raw_parts_mut(buffer.as_mut_ptr().cast(), len) }
    }

    #[allow(unsafe_code)]
    fn slice_to_items(buffer: &[Self]) -> &[Self::Cell] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        let len = buffer.len() * Self::item_count();
        unsafe { std::slice::from_raw_parts(buffer.as_ptr().cast(), len) }
    }

    #[allow(unsafe_code)]
    fn slice_to_items_mut(buffer: &mut [Self]) -> &mut [Self::Cell] {
        // This code casts slice to matrix. This is safe because `MatrixMN`
        // uses `nalgebra::Owned` allocator, which resolves to array defined as
        // `#[repr(C)]` under the hood.
        let len = buffer.len() * Self::item_count();
        unsafe { std::slice::from_raw_parts_mut(buffer.as_mut_ptr().cast(), len) }
    }
}



// ====================
// === JsBufferView ===
// ====================

/// Extension method for viewing into wasm's linear memory.
#[allow(unsafe_code)]
pub trait JsBufferView {
    /// Create a JS typed array which is a view into wasm's linear memory at the slice specified.
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


// === Instances ===

#[allow(unsafe_code)]
impl JsBufferView for [bool] {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        let i32arr = self.iter().cloned().map(|t| if t { 1 } else { 0 }).collect::<Vec<i32>>();
        js_sys::Int32Array::view(&i32arr).into()
    }
}

#[allow(unsafe_code)]
impl JsBufferView for [i32] {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        js_sys::Int32Array::view(self).into()
    }
}

#[allow(unsafe_code)]
impl JsBufferView for [u32] {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        js_sys::Uint32Array::view(self).into()
    }
}

#[allow(unsafe_code)]
impl JsBufferView for [f32] {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        js_sys::Float32Array::view(self).into()
    }
}

#[allow(unsafe_code)]
impl JsBufferView for [u8] {
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        js_sys::Uint8Array::view(self).into()
    }
}

#[allow(unsafe_code)]
impl<T: Storable<Cell = T>, R, C> JsBufferView for [OMatrix<T, R, C>]
where
    Self: MatrixCtx<T, R, C>,
    T: ItemBounds,
    OMatrix<T, R, C>: Storable,
    [Cell<OMatrix<T, R, C>>]: JsBufferView,
{
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        <OMatrix<T, R, C> as Storable>::slice_to_items(self).js_buffer_view()
    }
}

#[allow(unsafe_code)]
impl<T: Storable<Cell = T>, R, C> JsBufferView for OMatrix<T, R, C>
where
    Self: MatrixCtx<T, R, C>,
    T: ItemBounds,
{
    unsafe fn js_buffer_view(&self) -> js_sys::Object {
        self.as_slice().js_buffer_view()
    }
}
