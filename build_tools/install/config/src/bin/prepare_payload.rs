//! Binary to prepare installer payload for Bazel builds.
//!
//! This binary uses `ide_ci` to create the installer payload archive (via external tar)
//! and metadata files.
//!
//! Usage: prepare_payload <unpacked_dir> <output_archive> <output_metadata>

use enso_install_config::payload;
use enso_install_config::prelude::*;

#[tokio::main]
async fn main() -> Result {
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

    payload::prepare_payload(&unpacked_dir, &output_archive, &output_metadata).await
}
