//! EnsoGL Components.
//!
//! A collection of useful standard UI components for EnsoGL.

#![feature(option_result_contains)]
#![feature(trait_alias)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "512"]

pub mod component;
pub mod drop_down_menu;
pub mod file_browser;
pub mod label;
pub mod list_view;
pub mod scroll_area;
pub mod scrollbar;
pub mod selector;
pub mod shadow;
pub mod toggle_button;

/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}
