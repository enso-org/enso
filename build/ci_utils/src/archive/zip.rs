use crate::prelude::*;

use crate::archive::extract_files::ExtractFiles;
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

/// Synchronous version of [`extract_files`].
#[context("Failed to extract files from ZIP archive")]
pub fn extract_files_sync(
    archive: &mut ZipArchive<std::fs::File>,
    mut filter: impl FnMut(&Path) -> Option<PathBuf>,
) -> Result {
    for i in 0..archive.len() {
        let mut entry = archive.by_index(i).with_context(|| "Error getting ZIP archive entry")?;
        let path_in_archive = entry
            .enclosed_name()
            .with_context(|| "Could not get file path of ZIP archive entry")?;
        if let Some(output_path) = filter(path_in_archive) {
            let entry_type = if entry.is_dir() { "directory" } else { "file" };
            let make_message = |prefix, path: &Path| {
                format!(
                    "{} {:?} entry: {} => {}",
                    prefix,
                    entry_type,
                    path.display(),
                    output_path.display()
                )
            };

            trace!("{}", make_message("Extracting", path_in_archive));
            let mut output = std::fs::File::create(&output_path)
                .with_context(|| make_message("Could not extract file", path_in_archive))?;
            std::io::copy(&mut entry, &mut output)
                .with_context(|| format!("Could not copy file to {}", output_path.display()))?;
        }
    }
    Ok(())
}

impl ExtractFiles for &mut ZipArchive<std::fs::File> {
    async fn extract_files(self, filter: impl FnMut(&Path) -> Option<PathBuf>) -> Result {
        let job = move || extract_files_sync(self, filter);
        tokio::task::block_in_place(job)
    }
}
