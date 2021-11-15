//! Define abstraction for data types that have a default value when used as GPU values.

use crate::types::*;



// ==================
// === GpuDefault ===
// ==================

/// Trait for types which have a default value when used as GPU values.
pub trait GpuDefault {
    /// Default value for this type.
    fn gpu_default() -> Self;

    /// Check if the current value is the same as the default one.
    fn is_gpu_default(&self) -> bool
    where Self: Sized + PartialEq {
        *self == Self::gpu_default()
    }
}

/// Default value of a GPU-type.
pub fn gpu_default<T: GpuDefault>() -> T {
    <T as GpuDefault>::gpu_default()
}


// === Instances ===

macro_rules! define_gpu_defaults {
    ($($ty:ty = $val:expr),* $(,)?) => {$(
        impl GpuDefault for $ty { fn gpu_default() -> Self { $val } }
    )*}
}

define_gpu_defaults! {
    i32            = 0,
    u32            = 0,
    f32            = 0.0,
    bool           = false,

    Vector2<f32>   = Vector2::new(0.0,0.0),
    Vector3<f32>   = Vector3::new(0.0,0.0,0.0),
    Vector4<f32>   = Vector4::new(0.0,0.0,0.0,1.0),

    Vector2<i32>   = Vector2::new(0,0),
    Vector3<i32>   = Vector3::new(0,0,0),
    Vector4<i32>   = Vector4::new(0,0,0,1),

    Vector2<u32>   = Vector2::new(0,0),
    Vector3<u32>   = Vector3::new(0,0,0),
    Vector4<u32>   = Vector4::new(0,0,0,1),

    Vector2<bool>  = Vector2::new(false,false),
    Vector3<bool>  = Vector3::new(false,false,false),
    Vector4<bool>  = Vector4::new(false,false,false,false),

    Matrix2<f32>   = Matrix2::identity(),
    Matrix3<f32>   = Matrix3::identity(),
    Matrix4<f32>   = Matrix4::identity(),
    Matrix2x3<f32> = Matrix2x3::identity(),
    Matrix2x4<f32> = Matrix2x4::identity(),
    Matrix3x2<f32> = Matrix3x2::identity(),
    Matrix3x4<f32> = Matrix3x4::identity(),
    Matrix4x2<f32> = Matrix4x2::identity(),
    Matrix4x3<f32> = Matrix4x3::identity(),
}
