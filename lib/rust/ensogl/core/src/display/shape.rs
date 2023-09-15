//! This is a root module for shapes, 2-dimensional graphical elements.
//!
//! Please note that this module does not re-export `compound::rectangle::Circle` as the name
//! collides with `primitive::Circle`. It is imported in many places in the code and should be one
//! day refactored in such a way, that shape definitions would import `primitive::*` automatically,
//! while this module would not re-export `primitive::*` at all.


// ==============
// === Export ===
// ==============

pub mod compound;
pub mod constants;
pub mod primitive;

pub use compound::rectangle::Rectangle;
pub use compound::rectangle::RoundedRectangle;
pub use compound::rectangle::SimpleTriangle;
pub use constants::*;
pub use primitive::*;



// We have two Shape and two ShapeOps traits. This one takes precedence.
mod precedence_resolver {
    use super::*;
    pub use primitive::def::class::ShapeOps;
}
pub use precedence_resolver::ShapeOps;
