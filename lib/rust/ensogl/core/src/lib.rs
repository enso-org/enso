//! The core vector rendering engine for EnsoGL, a blazing fast 2D drawing library. This crate
//! contains the core utilities necessary for the rendering engine to run correctly. See the docs
//! of the `ensogl` crate to learn more.

#![recursion_limit = "512"]
// === Features ===
#![allow(incomplete_features)]
#![feature(negative_impls)]
#![feature(associated_type_defaults)]
#![feature(associated_type_bounds)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(marker_trait_attr)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(trace_macros)]
#![feature(const_trait_impl)]
#![feature(slice_as_chunks)]
#![feature(local_key_cell_methods)]
#![feature(auto_traits)]
#![feature(int_roundings)]
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod animation;
pub mod application;
pub mod control;
pub mod data;
pub mod debug;
pub mod display;
pub mod gui;
pub mod system;

pub use animation::Animation;
pub use animation::DEPRECATED_Animation;
pub use animation::Easing;
pub use display::event;
pub use enso_frp as frp;
pub use enso_types as types;



/// Commonly used utilities.
pub mod prelude {
    pub use super::display::layout::alignment;
    pub use super::display::traits::*;
    pub use super::display::world::scene;
    pub use super::frp;
    pub use super::types::*;
    pub use crate::application::command::FrpNetworkProvider;
    pub use crate::data::container::AddMut;
    pub use crate::shapes_order_dependencies;
    pub use crate::types::unit2::traits::*;
    pub use enso_data_structures as data;
    pub use enso_prelude::*;
    pub use enso_profiler as profiler;
    pub use enso_profiler::prelude::*;
    pub use enso_shapely::newtype_prim;
    pub use enso_shapely::newtype_prim_no_default;
    pub use enso_shapely::newtype_prim_no_default_no_display;
    pub use enso_shapely::newtype_prim_no_derives;
    pub use enso_shapely::shared;
    pub use enso_shapely::CloneRef;
}

/// Common traits.
pub mod traits {
    use super::*;
    pub use display::traits::*;
}



// ===================
// === Macro Debug ===
// ===================

// Uncomment the following lines in order to enable macro-expansion debugging during compilation.
//#![feature(trace_macros)]
//trace_macros!(true);
