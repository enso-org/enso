//! ensogl is a blazing fast 2D vector rendering engine with a rich set of primitives and a GUI
//! component library. It is able to display millions of shapes 60 frames per second in a web
//! browser on a modern laptop hardware. This is the main entry point to the library.

#![allow(dead_code)]

#![deny(unconditional_recursion)]

#![feature(associated_type_defaults)]
#![feature(cell_update)]
#![feature(drain_filter)]
#![feature(overlapping_marker_traits)]
#![feature(specialization)]
#![feature(fn_traits)]
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
#![recursion_limit="512"]

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
pub mod control;
pub mod data;
pub mod debug;
pub mod display;
pub mod gui;
pub mod math;
pub mod system;

/// Prelude - commonly used utilities.
pub mod prelude {
    pub use enso_prelude::*;
    pub use logger::*;
    pub use shapely::CloneRef;
    pub use shapely::newtype_copy;
    pub use shapely::shared;
    pub use super::data::container::AddMut;
    pub use super::math::types::*;
}

/// Common traits.
pub mod traits {
    use super::*;
    pub use display::traits::*;
}
