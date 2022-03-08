//! This is a root module for shapes, 2-dimensional graphical elements.


// ==============
// === Export ===
// ==============

pub mod compound;
pub mod constants;
pub mod primitive;

pub use constants::*;
pub use primitive::def::class::ShapeOps;
pub use primitive::*;
