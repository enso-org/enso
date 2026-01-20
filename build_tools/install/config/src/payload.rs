//! Installer has compiled-in binary payload and metadata about it.

use crate::prelude::*;

use flate2::Compression;
use flate2::write::GzEncoder;
use std::fs::File;
use std::io::BufWriter;

/// Information about the archive payload of the installer.
///
/// This information is used to display progress information to the user. While it could be
/// generated at runtime by inspecting the archive, it is more efficient to generate it at build
/// time and embed it into the installer binary.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Metadata {
    /// Number of files in the archive.
    pub total_files: u64,
    /// Total size of the extracted files in bytes.
    pub total_bytes: u64,
}

impl Metadata {
    /// Scans the given directory and calculates the payload information.
    pub fn from_directory(unpacked_directory: &Path) -> Result<Self> {
        let mut total_files = 0;
        let mut total_bytes = 0;
        for entry in walkdir::WalkDir::new(unpacked_directory) {
            let entry = entry?;
            let metadata = entry.metadata()?;
            total_files += 1;
            // We treat directories as empty files.
            total_bytes += if metadata.is_dir() { 0 } else { metadata.len() };
        }
        Ok(Self { total_files, total_bytes })
    }
}

/// Create a tar.gz archive from a directory using pure Rust (no external tools).
pub fn compress_directory_to_tar_gz(source_dir: &Path, output_archive: &Path) -> Result {
    let file = File::create(output_archive)
        .with_context(|| format!("Failed to create archive file: {}", output_archive.display()))?;
    let encoder = GzEncoder::new(BufWriter::new(file), Compression::default());
    let mut archive = tar::Builder::new(encoder);

    // Walk the directory and add all files/directories to the archive
    for entry in walkdir::WalkDir::new(source_dir) {
        let entry = entry?;
        let full_path = entry.path();
        let relative_path = full_path
            .strip_prefix(source_dir)
            .with_context(|| format!("Failed to strip prefix from {}", full_path.display()))?;

        // Skip the root directory itself
        if relative_path.as_os_str().is_empty() {
            continue;
        }

        let file_metadata = entry.metadata()?;
        if file_metadata.is_dir() {
            archive.append_dir(relative_path, full_path)?;
        } else {
            archive.append_path_with_name(full_path, relative_path)?;
        }
    }

    // Finish writing the archive
    let encoder = archive.into_inner()?;
    encoder.finish()?;

    Ok(())
}

/// Take the electron-builder output and prepare the payload files for the installer.
///
/// It should be used for Bazel builds where `tar` may not be available in the sandbox.
pub fn prepare_payload_sync(
    unpacked_directory: &Path,
    output_archive: &Path,
    output_metadata: &Path,
) -> Result {
    let metadata = Metadata::from_directory(unpacked_directory)?;
    compress_directory_to_tar_gz(unpacked_directory, output_archive)?;
    let metadata_json = serde_json::to_string_pretty(&metadata)?;
    std::fs::write(output_metadata, metadata_json)?;
    Ok(())
}

/// Take the electron-builder output and prepare the payload files for the installer.
///
/// These files need to be provided to the installer at build time, so they can be embedded into
/// the installer binary
pub async fn prepare_payload(
    unpacked_directory: &Path,
    output_archive: &Path,
    output_metadata: &Path,
) -> Result {
    let metadata = Metadata::from_directory(unpacked_directory)?;
    ide_ci::archive::compress_directory_contents(&output_archive, &unpacked_directory).await?;
    let metadata_json = serde_json::to_string_pretty(&metadata)?;
    ide_ci::fs::write_if_different(output_metadata, metadata_json)?;
    Ok(())
}
