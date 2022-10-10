// === Features ===
#![feature(result_flattening)]
#![feature(const_fmt_arguments_new)]
#![feature(hash_set_entry)]
#![feature(let_chains)]
#![feature(min_specialization)]
#![feature(exit_status_error)]
#![feature(option_result_contains)]
#![feature(associated_type_defaults)]
#![feature(associated_type_bounds)]
#![feature(exact_size_is_empty)]
#![feature(async_closure)]
#![feature(type_alias_impl_trait)]
#![feature(default_free_fn)]
#![feature(trait_alias)]
#![feature(io_error_other)]
#![feature(string_remove_matches)]
#![feature(once_cell)]
#![feature(duration_constants)]
#![feature(const_trait_impl)]
#![feature(is_some_with)]
#![feature(pin_macro)]
#![feature(result_option_inspect)]
#![feature(extend_one)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod actions;
pub mod anyhow;
pub mod archive;
pub mod buffer;
pub mod cache;
pub mod ci;
pub mod deploy;
pub mod env;
pub mod extensions;
pub mod fmt;
pub mod fs;
pub mod future;
pub mod github;
pub mod global;
pub mod goodie;
pub mod goodies;
pub mod io;
pub mod log;
pub mod models;
pub mod os;
pub mod path;
pub mod paths;
pub mod platform;
pub mod program;
pub mod programs;
pub mod reqwest;
pub mod serde;



pub mod prelude {

    pub type Result<T = ()> = anyhow::Result<T>;
    pub use anyhow::anyhow;
    pub use anyhow::bail;
    pub use anyhow::ensure;
    pub use anyhow::Context as _;
    pub use async_trait::async_trait;
    pub use bytes::Bytes;
    pub use derivative::Derivative;
    pub use derive_more::Display;
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
    pub use ifmt::iformat;
    pub use itertools::Itertools;
    pub use lazy_static::lazy_static;
    pub use octocrab::Octocrab;
    pub use path_absolutize::*;
    pub use platforms::target::Arch;
    pub use platforms::target::OS;
    pub use semver::Version;
    pub use serde::de::DeserializeOwned;
    pub use serde::Deserialize;
    pub use serde::Serialize;
    pub use shrinkwraprs::Shrinkwrap;
    pub use snafu::Snafu;
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
    pub use tokio::io::AsyncWriteExt as _;
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
    pub use url::Url;
    pub use uuid::Uuid;

    pub use crate::EMPTY_REQUEST_BODY;

    pub use crate::anyhow::ResultExt;
    pub use crate::env::Variable as EnvironmentVariable;
    pub use crate::extensions::str::StrLikeExt;
    pub use crate::github::RepoPointer;
    pub use crate::goodie::Goodie;
    pub use crate::log::setup_logging;
    pub use crate::os::target::TARGET_ARCH;
    pub use crate::os::target::TARGET_OS;
    pub use crate::program::command::provider::CommandProvider;
    pub use crate::program::command::Command;
    pub use crate::program::command::IsCommandWrapper;
    pub use crate::program::command::MyCommand;
    pub use crate::program::Program;
    pub use crate::program::Shell;

    pub use crate::cache::goodie::GoodieExt as _;
    pub use crate::env::new::RawVariable as _;
    pub use crate::env::new::TypedVariable as _;
    pub use crate::extensions::clap::ArgExt as _;
    pub use crate::extensions::command::CommandExt as _;
    pub use crate::extensions::from_string::FromString;
    pub use crate::extensions::future::FutureExt as _;
    pub use crate::extensions::future::TryFutureExt as _;
    pub use crate::extensions::iterator::IteratorExt;
    pub use crate::extensions::iterator::TryIteratorExt;
    pub use crate::extensions::output::OutputExt as _;
    pub use crate::extensions::path::PathExt as _;
    pub use crate::extensions::result::ResultExt as _;
    pub use crate::program::command::provider::CommandProviderExt as _;
    pub use crate::program::version::IsVersion as _;
    pub use crate::program::ProgramExt as _;

    pub fn into<T, U>(u: U) -> T
    where U: Into<T> {
        u.into()
    }
}

use prelude::*;
use std::net::Ipv4Addr;
use std::net::SocketAddrV4;
use std::net::TcpListener;

use ::anyhow::Context;

/// `None` that is used to represent an empty request body in calls `octocrab`.
pub const EMPTY_REQUEST_BODY: Option<&()> = None;

/// The user agent string name used by our HTTP clients.
pub const USER_AGENT: &str = "enso-build";

pub const UNREGISTERED_PORTS: Range<u16> = 49152..65535;

/// Looks up a free port in the IANA private or dynamic port range.
pub fn get_free_port() -> Result<u16> {
    let port_range = UNREGISTERED_PORTS;
    port_range
        .into_iter()
        .find(|port| {
            // Note that we must use Ipv4Addr::UNSPECIFIED. Ipv4Addr::LOCALHOST would not be enough,
            // as it misses e.g. services spawned by docker subnetworks.
            // This also makes us write this by hand, rather than use a crate.
            let ipv4 = SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, *port);
            // FIXME investigate? this can show firewall dialog on windows
            TcpListener::bind(ipv4).is_ok()
        })
        .context("Failed to find a free local port.")
}

pub fn ok_ready_boxed<'a, T: 'a + Send>(t: T) -> BoxFuture<'a, Result<T>> {
    ready(Ok(t)).boxed()
}

pub fn ready_boxed<'a, T: 'a + Send>(t: T) -> BoxFuture<'a, T> {
    ready(t).boxed()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    pub fn get_free_port_test() {
        debug!("{:?}", get_free_port());
    }
}
