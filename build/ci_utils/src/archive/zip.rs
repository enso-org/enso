use crate::prelude::*;

use anyhow::Context;
use std::io::Cursor;
use zip::read::ZipFile;



pub use ::zip::*;

pub fn open(path: impl AsRef<Path>) -> Result<ZipArchive<std::fs::File>> {
    ZipArchive::new(crate::fs::open(path)?).anyhow_err()
}

#[context("Failed to extract in-memory archive to {}.", output_dir.as_ref().display())]
pub fn extract_bytes(bytes: Bytes, output_dir: impl AsRef<Path>) -> Result {
    let mut archive = zip::ZipArchive::new(Cursor::new(&bytes))?;
    archive.extract(&output_dir)?;
    Ok(())
}

pub fn extract_file(file: &mut ZipFile, output: impl AsRef<Path>) -> Result {
    if file.is_dir() {
        crate::fs::create_dir_if_missing(&output)?;
    } else {
        let mut output_file = crate::fs::create(&output)?;
        std::io::copy(file, &mut output_file)?;
    }

    // We could consider setting file modification time, but the header data is not really reliable.
    // Leaving as-is for now. See: https://github.com/zip-rs/zip/issues/156#issuecomment-652981904

    // Get and Set permissions
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        if let Some(mode) = file.unix_mode() {
            std::fs::set_permissions(&output, std::fs::Permissions::from_mode(mode))?;
        }
    }
    Ok(())
}


#[tracing::instrument(
    name="Extracting subtree from archive.",
    skip_all,
    fields(
        prefix = %prefix.as_ref().display(),
        dest   = %output.as_ref().display()),
        err)]
pub fn extract_subtree(
    archive: &mut ZipArchive<impl Read + Seek>,
    prefix: impl AsRef<Path>,
    output: impl AsRef<Path>,
) -> Result {
    // let bar = crate::global::new_spinner("Extracting archive.");
    for index in 0..archive.len() {
        let mut file = archive.by_index(index)?;
        let path_in_archive = file
            .enclosed_name()
            .context(format!("Illegal path in the archive: {}", file.name()))?;
        if let Ok(relative_path) = path_in_archive.strip_prefix(&prefix) {
            let output = output.as_ref().join(relative_path);
            trace!("Extracting {}", output.display());
            extract_file(&mut file, output)?;
        }
    }
    Ok(())
}
