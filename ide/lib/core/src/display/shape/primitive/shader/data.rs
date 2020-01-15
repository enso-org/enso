//! This module defines an abstraction for all types which can be used as GLSL code values.

use crate::system::gpu::data::BufferItem;
use crate::system::gpu::data::GpuDefault;
use crate::system::gpu::shader::glsl::Glsl;



// ==================
// === ShaderData ===
// ==================

/// Trait describing all types which can be converted to GLSL expressions.
///
/// `ShaderData<T>` is implemented for both `T` as well as for all kind of string inputs. This
/// allows for dirty injection of GLSL code easily. For example, when moving a shape, you can write
/// `s1.translate("a","b")`, where `a` and `b` refer to variables defined in the GLSL shader. Such
/// operation is not checked during compilation, so be careful when using it, please.
pub trait ShaderData<T>: Into<Glsl> {
    /// Checks if the value is zero.
    fn is_zero (&self) -> bool;
}


// === Instances ===

impl<T> ShaderData<T> for Glsl {
    fn is_zero (&self) -> bool { self.str == "0" || self.str == "0.0" }
}

impl<T> ShaderData<T> for &Glsl {
    fn is_zero (&self) -> bool { (*self).str == "0" || (*self).str == "0.0" }
}

impl<T> ShaderData<T> for String {
    fn is_zero (&self) -> bool { self == "0" || self == "0.0" }
}

impl<T> ShaderData<T> for &String {
    fn is_zero (&self) -> bool { *self == "0" || *self == "0.0" }
}

impl<T> ShaderData<T> for &str {
    fn is_zero (&self) -> bool { *self == "0" || *self == "0.0" }
}

impl<T: BufferItem+PartialEq+Into<Glsl>> ShaderData<T> for T {
    fn is_zero (&self) -> bool { <T as GpuDefault> :: is_gpu_default(self) }
}
