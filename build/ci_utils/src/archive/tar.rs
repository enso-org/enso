use crate::prelude::*;

use async_compression::tokio::bufread::GzipDecoder;
use tokio::io::AsyncRead;
use tokio::io::BufReader;
use tokio_tar::Archive as Tar;



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
    file: Tar<Box<dyn AsyncRead + Unpin + Send>>,
}

impl Archive {
    /// Open a gzip-compressed tar archive.
    pub async fn open_tar_gz(path: impl AsRef<Path>) -> Result<Self> {
        let file = crate::fs::tokio::open(&path).await?;
        let file = BufReader::new(file);
        let file = GzipDecoder::new(file);
        let file: Box<dyn AsyncRead + Unpin + Send> = Box::new(file);
        let file = Tar::new(file);
        let path = path.as_ref().to_owned().into_boxed_path();
        Ok(Self { path, file })
    }

    /// The given function will be called with the path of each file within the archive. For each
    /// input path, if it returns a path the file will be extracted to the returned path.
    ///
    /// IMPORTANT: If the function uses its input path to generate an output path, care must be
    /// taken that the output path is not in an unexpected location, especially if coming from an
    /// untrusted archive.
    #[context("Failed to extract files from archive: {}", self.path.display())]
    pub async fn extract_files(
        mut self,
        mut filter: impl FnMut(&Path) -> Option<PathBuf>,
    ) -> Result {
        let mut entries = self.file.entries()?;
        while let Some(entry) = entries.next().await {
            let mut entry = entry?;
            let path_in_archive = entry.path()?;
            if let Some(output_path) = filter(&path_in_archive) {
                trace!("Extracting {}", output_path.display());
                entry.unpack(&output_path).await?;
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
        self.extract_files(|path_in_archive| {
            path_in_archive
                .strip_prefix(&prefix)
                .ok()
                .map(|relative_path| output.as_ref().join(relative_path))
        })
        .await
    }
}
