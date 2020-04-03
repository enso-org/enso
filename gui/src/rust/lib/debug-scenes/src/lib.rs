//! Root module for all example scenes.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(overlapping_marker_traits)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(weak_into_raw)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#[allow(clippy::option_map_unit_fn)]

pub mod dom_symbols;
pub mod easing_animator;
pub mod glyph_system;
pub mod ide;
pub mod shapes;
pub mod sprite_system;
pub mod text_field;
pub mod text_typing;

pub use enso_prelude as prelude;
