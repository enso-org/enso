use crate::prelude::*;

use async_compression::tokio::bufread::GzipEncoder;
use async_compression::Level;
use fs_extra::dir::CopyOptions;
use std::fs::File;


// ==============
// === Export ===
// ==============

pub mod tokio;
pub mod wrappers;

pub use wrappers::*;



/////////////////////////////

/// Like the standard version but will create any missing parent directories from the path.
#[context("Failed to write path: {}", path.as_ref().display())]
pub fn write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    create_parent_dir_if_missing(&path)?;
    wrappers::write(&path, &contents)
}

/// Serialize the data to JSON text and write it to the file.
///
/// See [`write`].
#[context("Failed to write path: {}", path.as_ref().display())]
pub fn write_json(path: impl AsRef<Path>, contents: &impl Serialize) -> Result {
    let contents = serde_json::to_string(contents)?;
    write(&path, &contents)
}

/// Like the standard version but will create any missing parent directories from the path.
#[context("Failed to open path for writing: {}", path.as_ref().display())]
pub fn create(path: impl AsRef<Path>) -> Result<File> {
    create_parent_dir_if_missing(&path)?;
    wrappers::create(&path)
}

///////////////////////////

#[context("Failed to read the file: {}", path.as_ref().display())]
pub fn read_string_into<T: FromString>(path: impl AsRef<Path>) -> Result<T> {
    read_to_string(&path)?.parse2()
}

/// Create a directory (and all missing parent directories),
///
/// Does not fail when a directory already exists.
#[context("Failed to create directory {}", path.as_ref().display())]
pub fn create_dir_if_missing(path: impl AsRef<Path>) -> Result {
    let result = std::fs::create_dir_all(&path);
    match result {
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => Ok(()),
        result => result.anyhow_err(),
    }
}

/// Create a parent directory for path (and all missing parent directories),
///
/// Does not fail when a directory already exists.
#[context("Failed to create parent directory for {}", path.as_ref().display())]
pub fn create_parent_dir_if_missing(path: impl AsRef<Path>) -> Result<PathBuf> {
    if let Some(parent) = path.as_ref().parent() {
        create_dir_if_missing(parent)?;
        Ok(parent.into())
    } else {
        bail!("No parent directory for path {}.", path.as_ref().display())
    }
}

/// Remove a directory with all its subtree.
///
/// Does not fail if the directory is not found.
#[tracing::instrument(fields(path = %path.as_ref().display()))]
#[context("Failed to remove directory {}", path.as_ref().display())]
pub fn remove_dir_if_exists(path: impl AsRef<Path>) -> Result {
    let result = std::fs::remove_dir_all(&path);
    match result {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        result => result.anyhow_err(),
    }
}

/// Remove a regular file.
///
/// Does not fail if the file is not found.
#[tracing::instrument(fields(path = %path.as_ref().display()))]
#[context("Failed to remove file {}", path.as_ref().display())]
pub fn remove_file_if_exists(path: impl AsRef<Path>) -> Result<()> {
    let result = std::fs::remove_file(&path);
    match result {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        result => result.anyhow_err(),
    }
}

/// Remove a file being either directory or regular file..
///
/// Does not fail if the file is not found.
#[context("Failed to remove entry {} (if exists)", path.as_ref().display())]
pub fn remove_if_exists(path: impl AsRef<Path>) -> Result {
    let path = path.as_ref();
    if path.is_dir() {
        remove_dir_if_exists(path)
    } else {
        remove_file_if_exists(path)
    }
}

#[context("Failed to create symlink {} => {}", src.as_ref().display(), dst.as_ref().display())]
pub fn symlink_auto(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result {
    create_parent_dir_if_missing(&dst)?;
    symlink::symlink_auto(&src, &dst).anyhow_err()
}

/// Recreate directory, so it exists and is empty.
pub fn reset_dir(path: impl AsRef<Path>) -> Result {
    let path = path.as_ref();
    debug!("Will reset directory {}", path.display());
    remove_dir_if_exists(path)?;
    create_dir_if_missing(path)?;
    Ok(())
}

pub fn require_exist(path: impl AsRef<Path>) -> Result {
    if path.as_ref().exists() {
        trace!("{} does exist.", path.as_ref().display());
        Ok(())
    } else {
        bail!("{} does not exist.", path.as_ref().display())
    }
}

#[tracing::instrument(skip_all, fields(
    src  = %source_file.as_ref().display(),
    dest = %dest_dir.as_ref().display()),
    err)]
pub fn copy_to(source_file: impl AsRef<Path>, dest_dir: impl AsRef<Path>) -> Result {
    require_exist(&source_file)?;
    create_dir_if_missing(dest_dir.as_ref())?;
    debug!("Will copy {} to {}", source_file.as_ref().display(), dest_dir.as_ref().display());
    let mut options = CopyOptions::new();
    options.overwrite = true;
    fs_extra::copy_items(&[source_file], dest_dir, &options)?;
    Ok(())
}


#[tracing::instrument(skip_all, fields(
    src  = %source_file.as_ref().display(),
    dest = %destination_file.as_ref().display()),
    err)]
pub fn copy(source_file: impl AsRef<Path>, destination_file: impl AsRef<Path>) -> Result {
    let source_file = source_file.as_ref();
    let destination_file = destination_file.as_ref();
    debug!("Will copy {} => {}", source_file.display(), destination_file.display());
    if let Some(parent) = destination_file.parent() {
        create_dir_if_missing(parent)?;
        if source_file.is_dir() {
            let mut options = fs_extra::dir::CopyOptions::new();
            options.overwrite = true;
            options.content_only = true;
            fs_extra::dir::copy(source_file, destination_file, &options)?;
        } else {
            wrappers::copy(source_file, destination_file)?;
        }
    } else {
        bail!("Cannot copy to the root path: {}", destination_file.display());
    }
    Ok(())
}

pub fn same_existing_path(source: impl AsRef<Path>, destination: impl AsRef<Path>) -> Result<bool> {
    Ok(canonicalize(source)? == canonicalize(destination)?)
}

pub async fn mirror_directory(source: impl AsRef<Path>, destination: impl AsRef<Path>) -> Result {
    create_dir_if_missing(destination.as_ref())?;

    // Robocopy seems to waste much time when running with the same path as source and destination.
    if same_existing_path(&source, &destination)? {
        return Ok(());
    }

    if TARGET_OS == OS::Windows {
        crate::programs::robocopy::mirror_directory(source, destination).await
    } else {
        crate::programs::rsync::mirror_directory(source, destination).await
    }
}

#[context("Failed because the path does not point to a directory: {}", path.as_ref().display())]
pub fn expect_dir(path: impl AsRef<Path>) -> Result {
    let filetype = metadata(&path)?.file_type();
    if filetype.is_dir() {
        Ok(())
    } else {
        bail!("File is not directory, its type is: {filetype:?}")
    }
}


#[context("Failed because the path does not point to a regular file: {}", path.as_ref().display())]
pub fn expect_file(path: impl AsRef<Path>) -> Result {
    let filetype = metadata(&path)?.file_type();
    if filetype.is_file() {
        Ok(())
    } else {
        bail!("File is not a regular file, its type is: {filetype:?}")
    }
}

#[cfg(not(target_os = "windows"))]
#[context("Failed to update permissions on `{}`", path.as_ref().display())]
pub fn allow_owner_execute(path: impl AsRef<Path>) -> Result {
    use crate::anyhow::ResultExt;
    use std::os::unix::prelude::*;
    debug!("Setting executable permission on {}", path.as_ref().display());
    let metadata = path.as_ref().metadata()?;
    let mut permissions = metadata.permissions();
    let mode = permissions.mode();
    let owner_can_execute = 0o0100;
    permissions.set_mode(mode | owner_can_execute);
    std::fs::set_permissions(path.as_ref(), permissions).anyhow_err()
}

#[cfg(target_os = "windows")]
#[context("Failed to update permissions on `{}`", path.as_ref().display())]
pub fn allow_owner_execute(path: impl AsRef<Path>) -> Result {
    // No-op on Windows.
    Ok(())
}

/// Get the size of a file after gzip compression.
pub async fn compressed_size(path: impl AsRef<Path>) -> Result<byte_unit::Byte> {
    let file = ::tokio::io::BufReader::new(crate::fs::tokio::open(&path).await?);
    let encoded_stream = GzipEncoder::with_quality(file, Level::Best);
    crate::io::read_length(encoded_stream).await.map(into)
}

pub fn check_if_identical(source: impl AsRef<Path>, target: impl AsRef<Path>) -> bool {
    (|| -> Result<bool> {
        #[allow(clippy::if_same_then_else)] // should be different after TODO
        if metadata(&source)?.len() == metadata(&target)?.len() {
            Ok(true)
        } else if read(&source)? == read(&target)? {
            // TODO: Not good for large files, should process them chunk by chunk.
            Ok(true)
        } else {
            Ok(false)
        }
    })()
    .unwrap_or(false)
}

pub fn copy_file_if_different(source: impl AsRef<Path>, target: impl AsRef<Path>) -> Result {
    if !check_if_identical(&source, &target) {
        trace!(
            "Modified, will copy {} to {}.",
            source.as_ref().display(),
            target.as_ref().display()
        );
        copy(&source, &target)?;
    } else {
        trace!("No changes, skipping {}.", source.as_ref().display())
    }
    Ok(())
}

#[tracing::instrument(skip_all, fields(
    src  = %source.as_ref().display(),
    dest = %target.as_ref().display()),
    err)]
pub async fn copy_if_different(source: impl AsRef<Path>, target: impl AsRef<Path>) -> Result {
    if tokio::metadata(&source).await?.is_file() {
        return copy_file_if_different(source, target);
    }

    let walkdir = walkdir::WalkDir::new(&source);
    let entries = walkdir.into_iter().try_collect_vec()?;
    for entry in entries.into_iter().filter(|e| e.file_type().is_file()) {
        let entry_path = entry.path();
        let relative_path = pathdiff::diff_paths(entry_path, &source)
            .context(format!("Failed to relativize path {}.", entry_path.display()))?;
        copy_file_if_different(entry_path, target.as_ref().join(relative_path))?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::log::setup_logging;
    use ::tokio;

    #[tokio::test]
    #[ignore]
    async fn copy_if_different_test() -> Result {
        setup_logging()?;
        copy_if_different("../../..", r"C:\temp\out").await?;
        Ok(())
    }
}
