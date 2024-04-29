//! This crate implements the Windows installer for the Enso IDE.

// === Features ===
#![feature(lazy_cell)]

use enso_install::access_built_time_env;
use enso_install::prelude::*;


// ==============
// === Export ===
// ==============

#[cfg(windows)]
pub mod win;

pub use enso_install::prelude;


#[derive(Debug)]
pub enum InstallerUpdate {
    Progress(f64),
    Stage(String),
    Finished(Result),
}

#[derive(Copy, Clone, Debug)]
pub struct Payload {
    pub data:     &'static [u8],
    pub metadata: &'static enso_install_config::payload::Metadata,
}

pub fn access_payload_metadata() -> &'static enso_install_config::payload::Metadata {
    access_built_time_env!(
        ENSO_INSTALL_METADATA_PATH,
        enso_install_config::payload::Metadata,
        "payload metadata"
    )
}
