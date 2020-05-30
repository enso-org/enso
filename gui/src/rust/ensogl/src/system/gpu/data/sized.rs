//! This module implements type-level utils for checking the size of values for a given type.

use nalgebra::*;
use crate::math::types::*;
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

impl GpuKnownSize for bool { type GpuByteSize = U4; }
impl GpuKnownSize for i32  { type GpuByteSize = U4; }
impl GpuKnownSize for u32  { type GpuByteSize = U4; }
impl GpuKnownSize for f32  { type GpuByteSize = U4; }

impl<T> GpuKnownSize for V2<T>
where T:GpuKnownSize, U2:DimMul<GpuByteSize<T>>, Mul<U2,GpuByteSize<T>>:DimName {
    type GpuByteSize = Mul<U2,GpuByteSize<T>>;
}

impl<T> GpuKnownSize for V3<T>
where T:GpuKnownSize, U3:DimMul<GpuByteSize<T>>, Mul<U3,GpuByteSize<T>>:DimName {
    type GpuByteSize = Mul<U3,GpuByteSize<T>>;
}

impl<T> GpuKnownSize for V4<T>
where T:GpuKnownSize, U4:DimMul<GpuByteSize<T>>, Mul<U4,GpuByteSize<T>>:DimName {
    type GpuByteSize = Mul<U4,GpuByteSize<T>>;
}

type Mul<A,B> = <A as DimMul<B>>::Output;
impl<T:GpuKnownSize,R:DimName,C:DimName> GpuKnownSize for MatrixMN<T,R,C>
    where Self:MatrixCtx<T,R,C>,
          R:DimMul<C>,
          Mul<R,C>:DimName+DimMul<GpuByteSize<T>>,
          Mul<Mul<R,C>,GpuByteSize<T>>:DimName {
    type GpuByteSize = Mul<Mul<R,C>,GpuByteSize<T>>;
}
