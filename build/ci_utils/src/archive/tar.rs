use crate::prelude::*;

use crate::archive::extract_files::ExtractFiles;

use flate2::read::GzDecoder;
use std::fs::File;



// ===============
// === Archive ===
// ===============

/// A `tar` archive.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Archive {
    /// The path that the `file` originated from. This is stored for error reporting.
    path: Box<Path>,
    #[derivative(Debug = "ignore")]
    file: tar::Archive<GzDecoder<File>>,
}

impl Archive {
    /// Open a gzip-compressed tar archive.
    #[context("Failed to open archive: {}", path.as_ref().display())]
    pub async fn open_tar_gz(path: impl AsRef<Path>) -> Result<Self> {
        let file = crate::fs::tokio::open(&path).await?;
        let file = file
            .try_into_std()
            .map_err(|_| anyhow!("Failed to convert tokio::fs::File to std::fs::File"))?;
        let tar_stream = flate2::read::GzDecoder::new(file);
        let archive = tar::Archive::new(tar_stream);
        let path = path.as_ref().to_owned().into_boxed_path();
        Ok(Self { path, file: archive })
    }

    /// Synchronous version of [`extract_files`].
    #[context("Failed to extract files from archive {}", self.path.display())]
    pub fn extract_files_sync(
        mut self,
        mut filter: impl FnMut(&Path) -> Option<PathBuf>,
    ) -> Result {
        let entries = self.file.entries()?;
        for entry in entries {
            let mut entry = entry?;
            let path_in_archive = entry.path()?;
            if let Some(output_path) = filter(&path_in_archive) {
                let entry_type = entry.header().entry_type();
                let make_message = |prefix, path: Cow<Path>| {
                    format!(
                        "{} {:?} entry: {} => {}",
                        prefix,
                        entry_type,
                        path.display(),
                        output_path.display()
                    )
                };

                trace!("{}", make_message("Extracting", path_in_archive));
                entry.unpack(&output_path).with_context(|| {
                    make_message("Failed to extract", entry.path().unwrap_or_default())
                })?;
            }
        }
        Ok(())
    }

    /// Extract all files from the specified subtree in the archive, placing them in the specified
    /// output directory.
    pub async fn extract_subtree(
        self,
        prefix: impl AsRef<Path>,
        output: impl AsRef<Path>,
    ) -> Result {
        let path = self.path.clone();
        debug!(
            "Extracting subtree '{}' from archive {} to {}",
            prefix.as_ref().display(),
            self.path.display(),
            output.as_ref().display()
        );
        self.extract_files(|path_in_archive| {
            path_in_archive
                .strip_prefix(&prefix)
                .ok()
                .map(|relative_path| output.as_ref().join(relative_path))
        })
        .await
        .with_context(|| {
            format!(
                "Failed to extract subtree '{}' from archive {} to {}",
                prefix.as_ref().display(),
                path.display(),
                output.as_ref().display()
            )
        })
    }
}

impl ExtractFiles for Archive {
    async fn extract_files(self, filter: impl FnMut(&Path) -> Option<PathBuf>) -> Result {
        let job = move || self.extract_files_sync(filter);
        tokio::task::block_in_place(job)
    }
}
