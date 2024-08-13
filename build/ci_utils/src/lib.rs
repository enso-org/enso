// === Non-Standard Linter Configuration ===
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod actions;
pub mod archive;
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
pub mod io;
pub mod log;
pub mod models;
pub mod os;
pub mod packaging;
pub mod path;
pub mod platform;
pub mod process;
pub mod program;
pub mod programs;
pub mod reqwest;
pub mod serde;



pub mod prelude {
    pub use derive_more::*;
    pub use enso_build_base::prelude::*;
    pub use enso_zst::*;

    pub use async_trait::async_trait;
    pub use bytes::Bytes;
    pub use derive_more::Display;
    pub use derive_where::derive_where;
    pub use itertools::Itertools;
    pub use lazy_static::lazy_static;
    pub use octocrab::Octocrab;
    pub use path_absolutize::*;
    pub use semver::Version;
    pub use tokio::io::AsyncWriteExt as _;
    pub use url::Url;
    pub use uuid::Uuid;

    pub use crate::EMPTY_REQUEST_BODY;

    pub use crate::cache::goodie::Goodie;
    pub use crate::github::release::IsRelease;
    pub use crate::github::repo::IsRepo;
    pub use crate::log::setup_logging;
    pub use crate::os::target::Arch;
    pub use crate::os::target::OS;
    pub use crate::os::target::TARGET_ARCH;
    pub use crate::os::target::TARGET_OS;
    pub use crate::program::command::default_status_checker;
    pub use crate::program::command::provider::CommandProvider;
    pub use crate::program::command::Command;
    pub use crate::program::command::IsCommandWrapper;
    pub use crate::program::command::MyCommand;
    pub use crate::program::Program;
    pub use crate::program::Shell;

    pub use crate::cache::goodie::GoodieExt as _;
    pub use crate::env::accessor::RawVariable as _;
    pub use crate::env::accessor::TypedVariable as _;
    pub use crate::extensions::clap::ArgExt as _;
    pub use crate::extensions::command::CommandExt as _;
    pub use crate::extensions::output::OutputExt as _;
    pub use crate::extensions::version::PrereleaseExt as _;
    pub use crate::extensions::version::VersionExt as _;
    pub use crate::github::release::IsReleaseExt as _;
    pub use crate::program::command::provider::CommandProviderExt as _;
    pub use crate::program::version::IsVersion as _;
    pub use crate::program::ProgramExt as _;

    pub fn into<T, U>(u: U) -> T
    where U: Into<T> {
        u.into()
    }
}

use prelude::*;

use ::anyhow::Context;

/// `None` that is used to represent an empty request body in calls `octocrab`.
pub const EMPTY_REQUEST_BODY: Option<&()> = None;

/// The user agent string name used by our HTTP clients.
pub const USER_AGENT: &str = "enso-build";

pub const RECORD_SEPARATOR: &str = "\u{1E}";

/// Looks up a free port.
pub fn get_free_port() -> Result<u16> {
    portpicker::pick_unused_port().context("Failed to find a free available port.")
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
