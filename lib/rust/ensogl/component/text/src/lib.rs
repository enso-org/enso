//! Ensogl text rendering implementation.
//!
//! To properly understand the implementation and its assumptions, please read the documentation
//! of [`enso_text`] crate carefully.

#![recursion_limit = "1024"]
// === Features ===
#![feature(const_trait_impl)]
#![feature(trait_alias)]
#![feature(type_ascription)]
#![feature(option_zip)]
#![feature(derive_default_enum)]
#![feature(generators)]
#![feature(btree_drain_filter)]
#![feature(allocator_api)]
#![feature(let_chains)]
#![feature(step_trait)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]



// ==============
// === Consts ===
// ==============

/// If set to true, animations will be running slow. This is useful for debugging purposes.
pub const DEBUG_ANIMATION_SLOWDOWN: bool = false;

/// Spring factor for animations. If [`DEBUG_ANIMATION_SLOWDOWN`] is set to true, this value will be
/// used for animation simulators.
pub const DEBUG_ANIMATION_SPRING_FACTOR: f32 = if DEBUG_ANIMATION_SLOWDOWN { 0.1 } else { 1.0 };



// ==============
// === Export ===
// ==============

pub mod buffer;
pub mod component;
pub mod font;



/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

pub use buffer::formatting;
pub use buffer::formatting::*;
pub use buffer::traits;
pub use buffer::*;
pub use component::Text;
pub use enso_text::unit::*;
pub use ensogl_core::data;
