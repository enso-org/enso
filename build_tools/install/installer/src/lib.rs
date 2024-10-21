//! This crate implements the Windows installer for the Enso IDE.

use enso_install::prelude::*;

use enso_install::access_built_time_env;


// ==============
// === Export ===
// ==============

#[cfg(windows)]
pub mod win;

pub use enso_install::prelude;



/// Update message sent by the installer logic thread to the UI.
///
/// These are used to communicate the progress of the installation process to the user.
#[derive(Debug)]
pub enum InstallerUpdate {
    /// Update the overall progress of the installation. The value is a number between 0 and 1.
    Progress(f64),
    /// Describe the current stage of the installation process.
    ///
    /// The value will be displayed to the user, so it should be a human-readable string.
    Stage(String),
    /// The installation has finished.
    ///
    /// If the Result is an error, the installation has failed and the error will be displayed to
    /// the user.
    Finished(Result),
}

/// Handle to the compiled-in installer payload.
#[derive(Copy, Clone, Debug)]
pub struct Payload {
    /// The binary data of the payload.
    pub data:     &'static [u8],
    /// The metadata of the payload.
    pub metadata: &'static enso_install_config::payload::Metadata,
}

/// Retrieve the compiled-in installer payload metadata.
pub fn access_payload_metadata() -> &'static enso_install_config::payload::Metadata {
    access_built_time_env!(
        ENSO_INSTALL_METADATA_PATH,
        enso_install_config::payload::Metadata,
        "payload metadata"
    )
}
