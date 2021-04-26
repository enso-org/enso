//! The Visual Part of IDE.
//!
//! This crate has the all code for displaying GUI of Enso IDE application. The views provides
//! the FRP endpoints to communicate with controllers. It also have a mocked debug scenes of IDE.

#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(clamp)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(matches_macro)]
#![feature(option_result_contains)]
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

pub mod code_editor;
pub mod debug_scenes;
pub mod documentation;
pub mod project;
pub mod searcher;
pub mod status_bar;
pub mod window_control_buttons;

pub use ide_view_graph_editor as graph_editor;

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use ensogl::prelude::*;
}
