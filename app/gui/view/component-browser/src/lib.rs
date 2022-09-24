//! A crate with Component Browser View.
//!
//! Currently, this crate gathers the related panels. The Component Browser View itself is defined
//! in `ide_view` crate, because the Documentation panel is used by old node searcher as well, and
//! we need to avoid crates' circular dependencies.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
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

pub use ide_view_breadcrumbs as breadcrumbs;
pub use ide_view_component_group as component_group;
pub use ide_view_component_list_panel as list_panel;
