//! This crate is meant to provide a foundational set of utilities and reexports, that should be
//! common for the whole Enso codebase. Eventually both WASM and native code should use this crate.
//!
//! Currently it is employed by the native build scripts code.

// === Features ===
#![feature(pin_macro)]
#![feature(default_free_fn)]
#![feature(result_flattening)]
#![feature(associated_type_bounds)]
#![feature(extend_one)]
#![feature(option_result_contains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]


// ==============
// === Export ===
// ==============

pub mod extensions;
pub mod fs;



pub mod prelude {
    //! This module contains all the reexports of the most common traits and types used in the
    //! Enso codebase.

    /// anyhow-based result type.
    pub type Result<T = ()> = anyhow::Result<T>;

    pub use std::borrow::Borrow;
    pub use std::borrow::BorrowMut;
    pub use std::borrow::Cow;
    pub use std::collections::BTreeMap;
    pub use std::collections::BTreeSet;
    pub use std::collections::HashMap;
    pub use std::collections::HashSet;
    pub use std::default::default;
    pub use std::ffi::OsStr;
    pub use std::ffi::OsString;
    pub use std::fmt::Debug;
    pub use std::fmt::Display;
    pub use std::fmt::Formatter;
    pub use std::future::ready;
    pub use std::future::Future;
    pub use std::hash::Hash;
    pub use std::io::Read;
    pub use std::io::Seek;
    pub use std::iter::once;
    pub use std::iter::FromIterator;
    pub use std::marker::PhantomData;
    pub use std::ops::Deref;
    pub use std::ops::DerefMut;
    pub use std::ops::Range;
    pub use std::path::Path;
    pub use std::path::PathBuf;
    pub use std::pin::pin;
    pub use std::pin::Pin;
    pub use std::sync::Arc;

    pub use crate::extensions::from_string::FromString;
    pub use crate::extensions::future::FutureExt as _;
    pub use crate::extensions::future::TryFutureExt as _;
    pub use crate::extensions::iterator::IteratorExt as _;
    pub use crate::extensions::iterator::TryIteratorExt as _;
    pub use crate::extensions::option::OptionExt as _;
    pub use crate::extensions::os_str::OsStrExt as _;
    pub use crate::extensions::path::PathExt as _;
    pub use crate::extensions::pathbuf::PathBufExt as _;
    pub use crate::extensions::result::ResultExt as _;
    pub use crate::extensions::str::StrLikeExt as _;

    pub use anyhow::anyhow;
    pub use anyhow::bail;
    pub use anyhow::ensure;
    pub use anyhow::Context as _;
    pub use fn_error_context::context;
    pub use futures_util::future::BoxFuture;
    pub use futures_util::select;
    pub use futures_util::stream::BoxStream;
    pub use futures_util::try_join;
    pub use futures_util::AsyncWrite;
    pub use futures_util::FutureExt as _;
    pub use futures_util::Stream;
    pub use futures_util::StreamExt as _;
    pub use futures_util::TryFuture;
    pub use futures_util::TryFutureExt as _;
    pub use futures_util::TryStream;
    pub use futures_util::TryStreamExt as _;
    pub use serde::de::DeserializeOwned;
    pub use serde::Deserialize;
    pub use serde::Serialize;
    pub use tracing::debug;
    pub use tracing::debug_span;
    pub use tracing::error;
    pub use tracing::error_span;
    pub use tracing::info;
    pub use tracing::info_span;
    pub use tracing::instrument;
    pub use tracing::span;
    pub use tracing::trace;
    pub use tracing::trace_span;
    pub use tracing::warn;
    pub use tracing::warn_span;
    pub use tracing::Instrument;
}
