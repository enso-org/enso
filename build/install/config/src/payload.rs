use crate::prelude::*;



/// Information about the archive payload of the installer.
///
/// This information is used to display progress information to the user. While it could be
/// generated at runtime by inspecting the archive, it is more efficient to generate it at build
/// time and embed it into the installer binary.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Metadata {
    /// Number of files in the archive.
    pub total_files: usize,
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
            total_bytes += metadata.len();
        }
        Ok(Self { total_files, total_bytes })
    }
}

pub async fn prepare_payload(
    unpacked_directory: &Path,
    output_archive: &Path,
    output_metadata: &Path,
) -> Result {
    let metadata = Metadata::from_directory(unpacked_directory)?;
    ide_ci::archive::compress_directory_contents(&output_archive, &unpacked_directory).await?;
    let metadata_json = serde_json::to_string_pretty(&metadata)?;
    ide_ci::fs::write_if_different(&output_metadata, metadata_json)?;
    Ok(())
}
