//! Client side implementation of the Enso Protocol
//!
//! See https://enso.org/docs/developer/enso/language-server/protocol-architecture.html.

#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(coerce_unsized)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

pub mod binary;
pub mod common;
pub mod generated;
pub mod types;
pub mod language_server;
pub mod project_manager;
pub mod handler;

#[allow(missing_docs)]
pub mod prelude {
    pub use enso_prelude::*;
    pub use json_rpc::prelude::*;
    pub use enso_logger::*;
    pub use crate::traits::*;

    pub use enso_logger::DefaultWarningLogger as Logger;
    /// We always use local futures in our single-threaded environment
    pub use futures::future::LocalBoxFuture as BoxFuture;
    pub use futures::FutureExt;
    pub use futures::Stream;
    pub use futures::StreamExt;
    pub use utils::fail::FallibleResult;
    pub use uuid::Uuid;
    pub use std::future::Future;

    /// We want most our futures to be static. Otherwise, they would automatically inherit
    /// lifetime of the client, which is not the desired behavior.
    pub type StaticBoxFuture<T> = futures::future::LocalBoxFuture<'static,T>;

    /// We want all our streams to be static. Otherwise, the would automatically inherit
    /// lifetime of the client, which is not the desired behavior.
    pub type StaticBoxStream<T> = futures::stream::LocalBoxStream<'static,T>;

    #[cfg(test)] pub use utils::test::traits::*;
}

/// Module gathering all traits which may be used by crate's users.
pub mod traits {
    pub use crate::binary::uuid::UuidExt;
    pub use crate::binary::client::API;
    pub use crate::binary::serialization::DeserializableUnionField;
    pub use crate::binary::serialization::DeserializableRoot;
    pub use crate::binary::serialization::SerializableRoot;
    pub use crate::binary::serialization::SerializableUnion;

    pub use crate::language_server::API as TRAIT_LanguageServerAPI;
    pub use crate::project_manager::API as TRAIT_ProjectManagerAPI;
}
