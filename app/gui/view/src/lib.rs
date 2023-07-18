//! The Visual Part of IDE.
//!
//! This crate has the all code for displaying GUI of Enso IDE application. The views provides
//! the FRP endpoints to communicate with presenter. It also have a mocked debug scenes of IDE.

// === Features ===
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(option_result_contains)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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
pub mod popup;
pub mod project;
pub mod project_list;
pub mod root;
pub mod searcher;
pub mod status_bar;

pub use ide_view_component_browser as component_browser;
pub use ide_view_documentation as documentation;
pub use ide_view_execution_environment_selector as execution_environment_selector;
pub use ide_view_graph_editor as graph_editor;
pub use ide_view_project_view_top_bar as project_view_top_bar;
pub use welcome_screen;

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use ensogl::prelude::*;
}
