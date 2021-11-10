//! This crate is a part of the EnsoGL library, containing the web-based functionalities.

#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

pub mod drop;

/// Commonly used utilities.
pub mod prelude {
    pub use enso_logger::DefaultWarningLogger as Logger;
    pub use enso_logger::*;
    pub use enso_prelude::*;
}


pub use ensogl_system_web::*;
