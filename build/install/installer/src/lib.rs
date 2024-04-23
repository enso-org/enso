//! This crate implements the Windows installer for the Enso IDE.

// === Features ===
#![feature(lazy_cell)]

use enso_install::prelude::*;


// ==============
// === Export ===
// ==============

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

/// A macro that uses an environment variable (defined build-time) with path to compile-in a JSON
/// file.
macro_rules! access_built_time_env {
    ($env:ident, $typename:ty, $pretty_name:expr) => {
        {
        static DATA: std::sync::LazyLock<$typename> = std::sync::LazyLock::new(|| {
            let crate_name = env!("CARGO_PKG_NAME");
            let pretty = $pretty_name;
            let path = env!(stringify!($env));
            let data = include_str!(env!(stringify!($env)));
            if path.is_empty() {
                panic!("The path to the {pretty} is empty. The {crate_name} was built without `{}` environment variable set.", stringify!($env));
            }
            if data.is_empty() {
                panic!("The {pretty} file is empty. Likely the stub was provided to enable compilation. Investigate the build logs warnings.");
            }
            serde_json::from_str(data).expect("Failed to parse the {pretty}.")
        });
        &DATA
        }
    };
}

pub fn access_payload_metadata() -> &'static enso_install_config::payload::Metadata {
    access_built_time_env!(
        ENSO_INSTALL_METADATA_PATH,
        enso_install_config::payload::Metadata,
        "payload metadata"
    )
}
