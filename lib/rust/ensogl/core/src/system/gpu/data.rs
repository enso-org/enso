//! This module defines data types representing attributes and uniforms.
//!   - Uniforms are per-primitive parameters (constant during an entire draw call).
//!   - Attributes are per-vertex parameters (typically positions, normals, colors, UVs, ...).
//!
//! To learn more about these concepts, follow the link:
//! https://www.khronos.org/opengl/wiki/Type_Qualifier_(GLSL)


// ==============
// === Export ===
// ==============

pub mod attribute;
pub mod buffer;
pub mod default;
pub mod gl_enum;
pub mod prim;
pub mod sized;
pub mod texture;
pub mod uniform;

pub use attribute::*;
pub use buffer::item::*;
pub use default::*;
pub use uniform::*;



/// Common types.
pub mod types {
    pub use super::texture;
    use super::*;
    pub use attribute::Attribute;
    pub use attribute::AttributeScope;
    pub use buffer::AnyBuffer;
    pub use buffer::Buffer;
    pub use buffer::IsBuffer;
    pub use buffer::Storable;
    pub use default::GpuDefault;
    pub use gl_enum::traits::*;
    pub use gl_enum::GlEnum;
    pub use prim::*;
    pub use texture::Texture;
    pub use texture::TextureBindGuard;
    pub use uniform::AnyPrimUniform;
    pub use uniform::AnyTextureUniform;
    pub use uniform::AnyUniform;
    pub use uniform::Uniform;
    pub use uniform::UniformScope;
}
pub use types::*;
