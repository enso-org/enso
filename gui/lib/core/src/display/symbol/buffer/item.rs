use crate::prelude::*;

use crate::tp::debug::TypeDebugName;
use nalgebra::*;


// =============
// === Types ===
// =============

pub trait MatrixCtx<T,R,C> = where
    T:Scalar, R:DimName, C:DimName,
    nalgebra::DefaultAllocator: nalgebra::allocator::Allocator<T, R, C>;


// =============
// === Empty ===
// =============

/// Trait for types which have empty value.
pub trait Empty {
    fn empty() -> Self;
}

impl Empty for i32 { fn empty() -> Self { 0 } }
impl Empty for f32 { fn empty() -> Self { 0.0 } }
impl<T,R,C> Empty for MatrixMN<T,R,C> where T:Default, Self:MatrixCtx<T,R,C> {
    fn empty() -> Self { Self::repeat(default()) }
}


// ============
// === Item ===
// ============

// === Definition ===

/// Class for buffer items, like `f32` or `Vector<f32>`. It defines utils
/// for mapping the item to WebGL buffer and vice versa.
pub trait Item: Empty {
    type Prim;
    type Dim: DimName;

    /// Count of primitives of the item. For example, `Vector3<f32>` contains
    /// three primitives (`f32` values).
    fn item_count() -> usize {
        <Self::Dim as DimName>::dim()
    }

    /// Conversion from slice of a buffer to the item. Buffers contain primitive
    /// values only, so two `Vector3<f32>` are represented there as six `f32`
    /// values. This allows us to view the buffers using desired types.
    fn from_buffer(buffer: &[Self::Prim]) -> &[Self]
        where Self: std::marker::Sized;

    /// Mutable conversion from slice of a buffer to the item. See the docs for
    /// `from_buffer` to learn more.
    fn from_buffer_mut(buffer: &mut [Self::Prim]) -> &mut [Self]
        where Self: std::marker::Sized;
}

// === Type Families ===

pub type Prim <T> = <T as Item>::Prim;
pub type Dim  <T> = <T as Item>::Dim;

// === Instances ===

impl Item for i32 {
    type Prim = Self;
    type Dim  = U1;

    fn from_buffer     (buffer: &    [Self::Prim]) -> &    [Self] { buffer }
    fn from_buffer_mut (buffer: &mut [Self::Prim]) -> &mut [Self] { buffer }
}

impl Item for f32 {
    type Prim = Self;
    type Dim  = U1;

    fn from_buffer     (buffer: &    [Self::Prim]) -> &    [Self] { buffer }
    fn from_buffer_mut (buffer: &mut [Self::Prim]) -> &mut [Self] { buffer }
}

impl<T,R,C> Item for MatrixMN<T,R,C>
    where T:Default, Self:MatrixCtx<T,R,C> {

    type Prim = T;
    type Dim  = R;

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
}

impl <T,R,C> TypeDebugName for MatrixMN<T,R,C> where Self: MatrixCtx<T,R,C> {
    fn type_debug_name() -> String {
        let col  = <C as DimName>::dim();
        let row  = <R as DimName>::dim();
        let item = type_name::<T>();
        match col {
            1 => format!("Vector{}<{}>"    , row, item),
            _ => format!("Matrix{}x{}<{}>" , row, col, item)
        }
    }
}