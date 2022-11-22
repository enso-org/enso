//! Client side implementation of the Enso Protocol
//!
//! See https://enso.org/docs/developer/enso/language-server/protocol-architecture.html.

// === Features ===
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(coerce_unsized)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]


// ==============
// === Export ===
// ==============

pub mod binary;
pub mod common;
pub mod generated;
pub mod handler;
pub mod language_server;
pub mod project_manager;
pub mod types;



#[allow(missing_docs)]
pub mod prelude {
    pub use crate::traits::*;
    pub use enso_logger::*;
    pub use enso_prelude::*;
    pub use json_rpc::prelude::*;

    pub use enso_logger::DefaultWarningLogger as Logger;
    /// We always use local futures in our single-threaded environment
    pub use futures::future::LocalBoxFuture as BoxFuture;
    pub use futures::FutureExt;
    pub use futures::Stream;
    pub use futures::StreamExt;
    pub use std::future::Future;
    pub use uuid::Uuid;

    /// We want most our futures to be static. Otherwise, they would automatically inherit
    /// lifetime of the client, which is not the desired behavior.
    pub type StaticBoxFuture<T> = futures::future::LocalBoxFuture<'static, T>;

    /// We want all our streams to be static. Otherwise, the would automatically inherit
    /// lifetime of the client, which is not the desired behavior.
    pub type StaticBoxStream<T> = futures::stream::LocalBoxStream<'static, T>;
}

/// Module gathering all traits which may be used by crate's users.
pub mod traits {
    pub use crate::binary::client::API;
    pub use crate::binary::serialization::DeserializableRoot;
    pub use crate::binary::serialization::DeserializableUnionField;
    pub use crate::binary::serialization::SerializableRoot;
    pub use crate::binary::serialization::SerializableUnion;
    pub use crate::binary::uuid::UuidExt;

    pub use crate::language_server::API as TRAIT_LanguageServerAPI;
    pub use crate::project_manager::API as TRAIT_ProjectManagerAPI;
}
