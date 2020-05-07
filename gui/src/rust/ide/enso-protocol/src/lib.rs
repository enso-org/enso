//! Client side implementation of Enso protocol.

#![feature(associated_type_bounds)]
#![feature(coerce_unsized)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

pub mod types;
pub mod language_server;
pub mod project_manager;

pub use enso_prelude as prelude;

/// Module gathering all traits which may be used by crate's users.
pub mod traits {
    pub use crate::language_server::API as TRAIT_LanguageServerAPI;
    pub use crate::project_manager::API as TRAIT_ProjectManagerAPI;
}
