//! Binary to prepare installer payload for Bazel builds.
//!
//! This binary uses pure Rust to create the installer payload archive and metadata files,
//! without requiring external tools like `tar`.
//!
//! Usage: prepare_payload <unpacked_dir> <output_archive> <output_metadata>

use enso_install_config::payload;
use enso_install_config::prelude::*;

fn main() -> Result {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 4 {
        bail!(
            "Usage: {} <unpacked_dir> <output_archive> <output_metadata>",
            args.first().map(String::as_str).unwrap_or("prepare_payload")
        );
    }

    let unpacked_dir = PathBuf::from(&args[1]);
    let output_archive = PathBuf::from(&args[2]);
    let output_metadata = PathBuf::from(&args[3]);

    // Use the synchronous, pure-Rust version that doesn't shell out to external tools
    payload::prepare_payload_sync(&unpacked_dir, &output_archive, &output_metadata)
}
