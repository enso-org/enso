//! BaseGL is a blazing fast 2D vector rendering engine with a rich set of primitives and a GUI
//! component library. It is able to display millions of shapes 60 frames per second in a web
//! browser on a modern laptop hardware. This is the main entry point to the library.

#![allow(dead_code)]

#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
//#![warn(unsafe_code)]
//#![warn(missing_copy_implementations)]
//#![warn(missing_debug_implementations)]

#![feature(associated_type_defaults)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(weak_into_raw)]
#![feature(drain_filter)]
#![recursion_limit="512"]

// To be removed after this gets resolved: https://github.com/rust-lang/cargo/issues/5034
#![allow(clippy::option_map_unit_fn)]



// ===================
// === Macro Debug ===
// ===================

/// Uncomment the following lines in order to enable macro-expansion debugging during compilation.

//#![feature(trace_macros)]
//#![recursion_limit="256"]
//trace_macros!(true);



// =================================
// === Module Structure Reexport ===
// =================================

pub mod animation;
pub mod control;
pub mod data;
pub mod debug;
pub mod display;
pub mod examples;
pub mod system;

/// Prelude - commonly used utilities.
pub mod prelude {
    pub use basegl_prelude::*;
    pub use logger::*;
    pub use shapely::newtype_copy;
    pub use shapely::shared;
    pub use super::data::container::AddMut;
}
