//! The core vector rendering engine for EnsoGL, a blazing fast 2D drawing library. This crate
//! contains the core utilities necessary for the rendering engine to run correctly. See thr docs
//! of the `ensogl` crate to learn more.

#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![feature(associated_type_defaults)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(marker_trait_attr)]
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
#![recursion_limit = "512"]
// To be removed after this gets resolved: https://github.com/rust-lang/cargo/issues/5034
#![allow(clippy::option_map_unit_fn)]



// ===================
// === Macro Debug ===
// ===================

/// Uncomment the following lines in order to enable macro-expansion debugging during compilation.
//#![feature(trace_macros)]
//trace_macros!(true);

// =================================
// === Module Structure Reexport ===
// =================================
pub mod animation;
pub mod application;
pub mod control;
pub mod data;
pub mod debug;
pub mod display;
pub mod gui;
pub mod system;

pub use enso_frp as frp;
pub use enso_types as types;

pub use animation::Animation;
pub use animation::DEPRECATED_Animation;
pub use animation::DEPRECATED_Tween;
pub use animation::Easing;

/// Commonly used utilities.
pub mod prelude {
    pub use super::display::traits::*;
    pub use super::types::*;
    pub use crate::data::container::AddMut;
    pub use crate::shapes_order_dependencies;
    pub use enso_data_structures as data;
    pub use enso_logger as logger;
    pub use enso_logger::AnyLogger;
    pub use enso_logger::DefaultWarningLogger as Logger;
    pub use enso_logger::*;
    pub use enso_prelude::*;
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
