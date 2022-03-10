//! This module implements type-level utils for checking the size of values for a given type.

use nalgebra::*;

use crate::system::gpu::data::buffer::item::MatrixCtx;



// ====================
// === GpuKnownSize ===
// ====================

/// Extension methods.
pub mod traits {
    use super::*;

    /// Type-level computation of byte size for types stored on GPU.
    pub trait GpuKnownSize {
        /// Byte size as type-level uint.
        type GpuByteSize: DimName;

        /// Byte size of the type.
        fn gpu_byte_size() -> usize {
            <Self::GpuByteSize as DimName>::dim()
        }
    }
}
pub use traits::*;

/// A nicer way to query type-level byte size for types stored on GPU.
pub type GpuByteSize<T> = <T as GpuKnownSize>::GpuByteSize;


// === Instances ===

impl GpuKnownSize for bool {
    type GpuByteSize = U4;
}
impl GpuKnownSize for i32 {
    type GpuByteSize = U4;
}
impl GpuKnownSize for u32 {
    type GpuByteSize = U4;
}
impl GpuKnownSize for f32 {
    type GpuByteSize = U4;
}

type Mul<A, B> = <A as DimMul<B>>::Output;
impl<T: GpuKnownSize, R: DimName, C: DimName> GpuKnownSize for OMatrix<T, R, C>
where
    Self: MatrixCtx<T, R, C>,
    R: DimMul<C>,
    Mul<R, C>: DimName + DimMul<GpuByteSize<T>>,
    Mul<Mul<R, C>, GpuByteSize<T>>: DimName,
{
    type GpuByteSize = Mul<Mul<R, C>, GpuByteSize<T>>;
}
