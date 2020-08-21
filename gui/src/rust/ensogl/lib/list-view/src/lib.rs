//! ListView EnsoGL Component.
//!
//! ListView a displayed list of entries with possibility of selecting one and "chosing" by
//! clicking or pressing enter - similar to the HTML `<select>`.

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

#![recursion_limit="512"]

pub mod component;
pub mod entry;

/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

pub use component::ListView;
