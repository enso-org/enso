use crate::prelude::*;

pub trait ExtractFiles {
    /// The given function will be called with the path of each file within the archive. For each
    /// input path, if it returns a path the file will be extracted to the returned path.
    ///
    /// IMPORTANT: If the function uses its input path to generate an output path, care must be
    /// taken that the output path is not in an unexpected location, especially if coming from an
    /// untrusted archive.
    async fn extract_files(self, filter: impl FnMut(&Path) -> Option<PathBuf>) -> Result;
}
