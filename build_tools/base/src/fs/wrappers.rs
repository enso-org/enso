//! Wrappers over [`std::fs`] functions that provide sensible error messages, i.e. explaining what
//! operation was attempted and what was the relevant path.
//!
//! Unless there is a specific reason to use the standard library functions, you should use these.

use crate::prelude::*;

use std::fs::DirEntry;
use std::fs::File;
use std::fs::Metadata;



// ==============
// === Export ===
// ==============

/// See [std::fs::metadata].
#[context("Failed to obtain metadata for file: {}", path.as_ref().display())]
pub fn metadata<P: AsRef<Path>>(path: P) -> Result<Metadata> {
    Ok(std::fs::metadata(&path)?)
}

/// See [std::fs::symlink_metadata].
#[context("Failed to obtain symlink metadata for file: {}", path.as_ref().display())]
pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> Result<Metadata> {
    Ok(std::fs::symlink_metadata(&path)?)
}

/// See [std::fs::copy].
#[context("Failed to copy file from {} to {}", from.as_ref().display(), to.as_ref().display())]
pub fn copy(from: impl AsRef<Path>, to: impl AsRef<Path>) -> Result<u64> {
    Ok(std::fs::copy(&from, &to)?)
}

/// See [std::fs::rename].
#[context("Failed to rename file from {} to {}", from.as_ref().display(), to.as_ref().display())]
pub fn rename(from: impl AsRef<Path>, to: impl AsRef<Path>) -> Result {
    Ok(std::fs::rename(&from, &to)?)
}

/// See [std::fs::read].
#[context("Failed to read the file: {}", path.as_ref().display())]
pub fn read(path: impl AsRef<Path>) -> Result<Vec<u8>> {
    Ok(std::fs::read(&path)?)
}

/// See [std::fs::read_dir].
pub fn read_dir(path: impl AsRef<Path>) -> Result<impl Iterator<Item = Result<DirEntry>>> {
    let path = path.as_ref().to_path_buf();
    let read_dir = std::fs::read_dir(&path)
        .map_err(|e| anyhow!("Failed to read the directory: '{}'. Error: {}", path.display(), e))?;
    Ok(read_dir.into_iter().map(move |elem_result| {
        elem_result.map_err(|e| {
            anyhow!("Failed to read sub-item from the directory '{}'. Error: {}", path.display(), e)
        })
    }))
}

/// See [std::fs::read_to_string].
#[context("Failed to read the file: {}", path.as_ref().display())]
pub fn read_to_string(path: impl AsRef<Path>) -> Result<String> {
    Ok(std::fs::read_to_string(&path)?)
}

/// See [std::fs::write].
#[context("Failed to write path: {}", path.as_ref().display())]
pub fn write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    Ok(std::fs::write(&path, contents)?)
}

/// See [std::fs::File::open].
#[context("Failed to open path for reading: {}", path.as_ref().display())]
pub fn open(path: impl AsRef<Path>) -> Result<File> {
    Ok(File::open(&path)?)
}

/// See [std::fs::File::create].
#[context("Failed to open path for writing: {}", path.as_ref().display())]
pub fn create(path: impl AsRef<Path>) -> Result<File> {
    Ok(File::create(&path)?)
}

/// See [std::fs::canonicalize].
#[context("Failed to canonicalize path: {}", path.as_ref().display())]
pub fn canonicalize(path: impl AsRef<Path>) -> Result<PathBuf> {
    Ok(std::fs::canonicalize(&path)?)
}

/// See [std::fs::create_dir_all].
#[context("Failed to create missing directories no path: {}", path.as_ref().display())]
pub fn create_dir_all(path: impl AsRef<Path>) -> Result {
    Ok(std::fs::create_dir_all(&path)?)
}

/// See [std::fs::set_permissions].
#[context("Failed to permissions on file: {}", path.as_ref().display())]
pub fn set_permissions(path: impl AsRef<Path>, perm: std::fs::Permissions) -> Result {
    Ok(std::fs::set_permissions(&path, perm)?)
}
