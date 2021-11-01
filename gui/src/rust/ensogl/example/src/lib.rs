//! The Visual Part of IDE.
//!
//! This crate has the all code for displaying GUI of Enso IDE application. The views provides
//! the FRP endpoints to communicate with controllers. It also have a mocked debug scenes of IDE.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![allow(incomplete_features)] // To be removed, see: https://github.com/enso-org/ide/issues/1559
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

pub mod animation;
pub mod complex_shape_system;
pub mod dom_symbols;
pub mod drop_manager;
pub mod easing_animator;
pub mod glyph_system;
#[allow(clippy::option_map_unit_fn)]
mod leak;
pub mod list_view;
pub mod mouse_events;
pub mod scroll_area;
pub mod shape_system;
pub mod slider;
pub mod sprite_system;
pub mod sprite_system_benchmark;
pub mod text_area;

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use super::leak::*;
    pub use ensogl_core::prelude::*;
}
