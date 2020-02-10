//! This module defines an abstraction for all types which can be used as GLSL code values.

use crate::prelude::*;

use crate::system::gpu::shader::glsl::Glsl;



// ==================
// === ShaderData ===
// ==================

/// An overlapping marker trait describing all types which can be converted to GLSL expressions.
///
/// If an input is typed as `ShaderData<T>`, it accepts either `T` or any kind of string. This
/// allows for dirty injection of GLSL code easily. For example, when moving a shape, you can write
/// `s1.translate("a","b")`, where `a` and `b` refer to variables defined in the GLSL shader. Such
/// operation is not checked during compilation, so use it only when really needed.
pub trait ShaderData<T:?Sized>: Into<Glsl> {}


// === Instances ===

impl<T> ShaderData<T> for Glsl    {}
impl<T> ShaderData<T> for &Glsl   {}
impl<T> ShaderData<T> for String  {}
impl<T> ShaderData<T> for &String {}
impl<T> ShaderData<T> for &str    {}
impl<T> ShaderData<T> for  T where  T:Into<Glsl> {}
impl<T> ShaderData<T> for &T where for <'t> &'t T:Into<Glsl> {}


// === Any ===

/// A special version which allows for any input type.

impl<T> ShaderData<dyn Any> for  T where  T:Into<Glsl> {}
impl<T> ShaderData<dyn Any> for &T where for <'t> &'t T:Into<Glsl> {}
