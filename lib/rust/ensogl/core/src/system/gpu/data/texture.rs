//! This module implements GPU-based texture support. Proper texture handling is a complex topic.
//! Follow the link to learn more about many assumptions this module was built upon:
//! https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D


// ==============
// === Export ===
// ==============

pub mod class;
pub mod storage;
pub mod types;

pub use class::*;
pub use storage::*;
pub use types::*;



/// Provides smart scope for item types.
pub mod item_type {
    pub use super::types::item_type::AnyItemType::*;
}
