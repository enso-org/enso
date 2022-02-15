//! The Visual Part of IDE.
//!
//! This crate has the all code for displaying GUI of Enso IDE application. The views provides
//! the FRP endpoints to communicate with presenter. It also have a mocked debug scenes of IDE.

#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(option_result_contains)]
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

#[allow(clippy::option_map_unit_fn)]
pub mod code_editor;
pub mod debug_mode_popup;
pub mod documentation;
pub mod open_dialog;
pub mod project;
pub mod root;
pub mod searcher;
pub mod status_bar;
pub mod window_control_buttons;

pub use ide_view_graph_editor as graph_editor;
pub use welcome_screen;

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use ensogl::prelude::*;
}
