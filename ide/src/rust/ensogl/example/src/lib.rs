//! The Visual Part of IDE.
//!
//! This crate has the all code for displaying GUI of Enso IDE application. The views provides
//! the FRP endpoints to communicate with controllers. It also have a mocked debug scenes of IDE.

#![feature(associated_type_defaults)]
#![feature(clamp)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(overlapping_marker_traits)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(vec_remove_item)]
#![feature(weak_into_raw)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

#![recursion_limit="1024"]

#[allow(clippy::option_map_unit_fn)]

pub mod animation;
pub mod dom_symbols;
pub mod easing_animator;
pub mod glyph_system;
pub mod list_view;
pub mod mouse_events;
pub mod shape_system;
pub mod complex_shape_system;
pub mod sprite_system;
pub mod sprite_system_benchmark;
pub mod text_area;

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}
