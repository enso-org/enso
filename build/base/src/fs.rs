//! Module meant as a replacement for [std::fs] module.
//!
//! Functionality and API-s are basically the same, but the functions are enriched with logging and
//! improved diagnostics. Most importantly, file operation failures will display the relevant path.

use crate::prelude::*;

use std::io::Write;


// ==============
// === Export ===
// ==============

pub mod wrappers;

pub use wrappers::*;



/// Like the standard version but will create any missing parent directories from the path.
#[context("Failed to write path: {}", path.as_ref().display())]
pub fn write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    create_parent_dir_if_missing(&path)?;
    wrappers::write(&path, &contents)
}

/// Read the file file, deserializing JSON text into the data.
#[context("Failed to deserialize JSON from path: {}", path.as_ref().display())]
pub fn read_json<T: DeserializeOwned>(path: impl AsRef<Path>) -> Result<T> {
    let contents = read_to_string(&path)?;
    serde_json::from_str(&contents)
        .with_context(|| format!("Failed to parse JSON from {}", path.as_ref().display()))
}

/// Serialize the data to JSON text and write it to the file.
///
/// See [`write()`].
#[context("Failed to write path: {}", path.as_ref().display())]
pub fn write_json(path: impl AsRef<Path>, contents: &impl Serialize) -> Result {
    let contents = serde_json::to_string(contents)?;
    write(&path, contents)
}

/// Like the standard version but will create any missing parent directories from the path.
#[context("Failed to open path for writing: {}", path.as_ref().display())]
pub fn create(path: impl AsRef<Path>) -> Result<std::fs::File> {
    create_parent_dir_if_missing(&path)?;
    wrappers::create(&path)
}

/// Read the file content and parse it using [`FromString`].
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

/// Recreate directory, so it exists and is empty.
pub fn reset_dir(path: impl AsRef<Path>) -> Result {
    let path = path.as_ref();
    debug!("Will reset directory {}", path.display());
    remove_dir_if_exists(path)?;
    create_dir_if_missing(path)?;
    Ok(())
}

/// Fail if the given path does not exist.
pub fn require_exist(path: impl AsRef<Path>) -> Result {
    if path.as_ref().exists() {
        trace!("{} does exist.", path.as_ref().display());
        Ok(())
    } else {
        bail!("{} does not exist.", path.as_ref().display())
    }
}

/// Check if the both path are equal.
///
/// This performs canonicalization of the paths before comparing them. As such, it requires that the
/// both paths exist.
pub fn same_existing_path(source: impl AsRef<Path>, destination: impl AsRef<Path>) -> Result<bool> {
    Ok(canonicalize(source)? == canonicalize(destination)?)
}

/// Fail if the given path is not an existing directory.
#[context("Failed because the path does not point to a directory: {}", path.as_ref().display())]
pub fn expect_dir(path: impl AsRef<Path>) -> Result {
    let filetype = metadata(&path)?.file_type();
    if filetype.is_dir() {
        Ok(())
    } else {
        bail!("File is not directory, its type is: {filetype:?}")
    }
}


/// Fail if the given path is not an existing file.
#[context("Failed because the path does not point to a regular file: {}", path.as_ref().display())]
pub fn expect_file(path: impl AsRef<Path>) -> Result {
    let filetype = metadata(&path)?.file_type();
    if filetype.is_file() {
        Ok(())
    } else {
        bail!("File is not a regular file, its type is: {filetype:?}")
    }
}

/// Change the file permissions, so the owner can execute it.
#[context("Failed to update permissions on `{}`", path.as_ref().display())]
pub fn allow_owner_execute(path: impl AsRef<Path>) -> Result {
    #[cfg(not(target_os = "windows"))]
    {
        use std::os::unix::prelude::*;
        debug!("Setting executable permission on {}", path.as_ref().display());
        let metadata = path.as_ref().metadata()?;
        let mut permissions = metadata.permissions();
        let mode = permissions.mode();
        let owner_can_execute = 0o0100;
        permissions.set_mode(mode | owner_can_execute);
        set_permissions(path.as_ref(), permissions)?;
    }
    Ok(())
}

/// Check if the files are of identical content.
pub fn check_if_identical(source: impl AsRef<Path>, target: impl AsRef<Path>) -> Result<bool> {
    // Different length means different content, no need to read.
    if metadata(&source)?.len() != metadata(&target)?.len() {
        return Ok(false);
    }

    // TODO: Not good for large files, should process them chunk by chunk.
    Ok(read(&source)? == read(&target)?)
}

/// Copy a file from source to target, unless they are already identical.
///
/// This is meant not really as an optimization for copying, but rather as a way to avoid
/// unnecessary file changes, which would trigger unnecessary rebuilds.
pub fn copy_file_if_different(source: impl AsRef<Path>, target: impl AsRef<Path>) -> Result {
    if check_if_identical(&source, &target).contains(&true) {
        trace!("Files are identical, not copying from {}.", source.as_ref().display());
    } else {
        trace!(
            "Modified, will copy {} to {}.",
            source.as_ref().display(),
            target.as_ref().display()
        );
        copy(&source, &target)?;
    }
    Ok(())
}

/// Append contents to the file.
///
/// If the file does not exist, it will be created.
pub fn append(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(&path)
        .context(format!("Failed to open {} for writing.", path.as_ref().display()))?
        .write_all(contents.as_ref())
        .context(format!("Failed to write to {}.", path.as_ref().display()))
}
