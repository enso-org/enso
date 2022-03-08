//! GPU-specific types and related implementations.


// ==============
// === Export ===
// ==============

pub mod data;
pub mod shader;



/// Common types.
pub mod types {
    use web_sys::WebGl2RenderingContext;

    pub use super::data::types::*;
    pub use super::shader::types::*;

    pub use super::data::attribute;
    pub use super::data::uniform;

    /// Alias for WebGl2RenderingContext.
    pub type Context = WebGl2RenderingContext;
}
pub use types::*;
