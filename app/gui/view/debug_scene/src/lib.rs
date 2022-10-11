//! A crate consisting all debug scenes.
//!
//! Those scenes are designated to test visual components of our application isolated from the
//! controllers.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
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

pub use debug_scene_component_list_panel_view as new_component_list_panel_view;
pub use debug_scene_icons as icons;
pub use debug_scene_interface as interface;
pub use debug_scene_visualization as visualization;
