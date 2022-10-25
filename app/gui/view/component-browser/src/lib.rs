//! A crate with Component Browser View.
//!
//! Currently, this crate gathers the related panels. The Component Browser View itself is defined
//! in `ide_view` crate, because the Documentation panel is used by old node searcher as well, and
//! we need to avoid crates' circular dependencies.

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

pub use ide_view_component_list_panel as component_list_panel;
pub use ide_view_component_list_panel_breadcrumbs as breadcrumbs;
