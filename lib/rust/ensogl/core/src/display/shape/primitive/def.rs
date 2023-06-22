//! This module is the root module for all primitive shapes and shape transform definitions.


// ==============
// === Export ===
// ==============

pub mod class;
pub mod modifier;
pub mod primitive;
mod unit;
pub mod var;



/// Common types.
mod export {
    pub use super::class::AnyShape;
    pub use super::class::ShapeOps;
    pub use super::modifier::*;
    pub use super::primitive::*;
    pub use super::unit::*;
    pub use super::var::*;
}

pub use export::*;
