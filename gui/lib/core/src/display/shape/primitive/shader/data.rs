//! This module defines an abstraction for all types which can be used as GLSL code values.

use crate::prelude::*;

use crate::system::gpu::data::GpuData;
use crate::system::gpu::data::Empty;



// ==================
// === ShaderData ===
// ==================

/// Trait describing all types which can be converted to GLSL expressions.
///
/// `ShaderData<T>` is implemented for both `T` as well as for all kind of string inputs. This
/// allows for dirty injection of GLSL code easily. For example, when moving a shape, you can write
/// `s1.translate("a","b")`, where `a` and `b` refer to variables defined in the GLSL shader. Such
/// operation is not checked during compilation, so be careful when using it, please.

pub trait ShaderData<T> {
    /// Checks if the value is zero.
    fn is_zero (&self) -> bool;

    /// Converts the value to GLSL code.
    fn to_glsl (&self) -> String;
}


// === Instances ===

impl<T> ShaderData<T> for String {
    fn is_zero (&self) -> bool   { self == "0" || self == "0.0" }
    fn to_glsl (&self) -> String { self.into() }
}

impl<T> ShaderData<T> for &String {
    fn is_zero (&self) -> bool   { *self == "0" || *self == "0.0" }
    fn to_glsl (&self) -> String { (*self).into() }
}

impl<T> ShaderData<T> for str {
    fn is_zero (&self) -> bool   { self == "0" || self == "0.0" }
    fn to_glsl (&self) -> String { self.into() }
}

impl<T> ShaderData<T> for &str {
    fn is_zero (&self) -> bool   { *self == "0" || *self == "0.0" }
    fn to_glsl (&self) -> String { (*self).into() }
}

impl<T:GpuData+PartialEq> ShaderData<T> for T {
    fn is_zero (&self) -> bool   { <T as Empty>   :: is_empty(self) }
    fn to_glsl (&self) -> String { <T as GpuData> :: to_glsl(self)  }
}
