use crate::prelude::*;

use flate2::read::GzDecoder;
use std::fs::File;
use tar::Archive;



pub fn open_tar_gz(path: impl AsRef<Path>) -> Result<Archive<GzDecoder<File>>> {
    let file = crate::fs::open(&path)?;
    let tar_stream = flate2::read::GzDecoder::new(file);
    Ok(tar::Archive::new(tar_stream))
}

pub fn extract_subtree<R: Read>(
    archive: &mut Archive<R>,
    prefix: impl AsRef<Path>,
    output: impl AsRef<Path>,
) -> Result {
    for entry in archive.entries()? {
        let mut entry = entry?;
        let path_in_archive = entry.path()?;
        if let Ok(relative_path) = path_in_archive.strip_prefix(&prefix) {
            let output = output.as_ref().join(relative_path);
            trace!("Extracting {}", output.display());
            entry.unpack(output)?;
        }
    }
    Ok(())
}
