//! This is a root module for shapes, 2-dimensional graphical elements.


// ==============
// === Export ===
// ==============

pub mod compound;
pub mod constants;
pub mod primitive;

pub use constants::*;
pub use primitive::*;



// We have two Shape and two ShapeOps traits. This one takes precedence.
mod precedence_resolver {
    use super::*;
    pub use primitive::def::class::ShapeOps;
}
pub use precedence_resolver::ShapeOps;
