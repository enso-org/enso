//! Main library crate for IDE. It includes implementation of
//! controllers, view logic and code that wraps them all together.

#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

pub mod controller;
#[allow(unused)]
pub mod todo;
pub mod view;

/// Common types that should be visible across the whole IDE crate.
pub mod prelude {
    pub use enso_prelude::*;

    pub use futures::Future;
    pub use futures::FutureExt;
    pub use futures::Stream;
    pub use futures::StreamExt;
    pub use futures::task::LocalSpawnExt;

    pub use crate::constants;
}

/// Global constants used across whole application.
pub mod constants {
    /// A name of language this IDE supports
    pub const LANGUAGE_NAME           : &str = "Enso";

    /// A file extension of modules of language this IDE supports
    pub const LANGUAGE_FILE_EXTENSION : &str = "enso";
}

use view::project::ProjectView;

/// This function is the IDE entry point responsible for setting up all views and controllers.
pub fn run_ide() {
    ProjectView::new().forget();
}