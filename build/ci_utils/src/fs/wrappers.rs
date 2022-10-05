//! Wrappers over [`std::fs`] functions that provide sensible error messages, i.e. explaining what
//! operation was attempted and what was the relevant path.

use crate::prelude::*;

use std::fs::File;
use std::fs::Metadata;
use std::io::Write;


// ==============
// === Export ===
// ==============

pub mod tokio;



#[context("Failed to obtain metadata for file: {}", path.as_ref().display())]
pub fn metadata<P: AsRef<Path>>(path: P) -> Result<Metadata> {
    std::fs::metadata(&path).anyhow_err()
}

#[context("Failed to copy file from {} to {}", from.as_ref().display(), to.as_ref().display())]
pub fn copy(from: impl AsRef<Path>, to: impl AsRef<Path>) -> Result<u64> {
    std::fs::copy(&from, &to).anyhow_err()
}

#[context("Failed to rename file from {} to {}", from.as_ref().display(), to.as_ref().display())]
pub fn rename(from: impl AsRef<Path>, to: impl AsRef<Path>) -> Result {
    std::fs::rename(&from, &to).anyhow_err()
}

#[context("Failed to read the file: {}", path.as_ref().display())]
pub fn read(path: impl AsRef<Path>) -> Result<Vec<u8>> {
    std::fs::read(&path).anyhow_err()
}

#[context("Failed to read the directory: {}", path.as_ref().display())]
pub fn read_dir(path: impl AsRef<Path>) -> Result<std::fs::ReadDir> {
    std::fs::read_dir(&path).anyhow_err()
}

#[context("Failed to read the file: {}", path.as_ref().display())]
pub fn read_to_string(path: impl AsRef<Path>) -> Result<String> {
    std::fs::read_to_string(&path).anyhow_err()
}

#[context("Failed to write path: {}", path.as_ref().display())]
pub fn write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    std::fs::write(&path, contents).anyhow_err()
}

pub fn append(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(&path)
        .context(format!("Failed to open {} for writing.", path.as_ref().display()))?
        .write_all(contents.as_ref())
        .context(format!("Failed to write to {}.", path.as_ref().display()))
}

#[context("Failed to open path for reading: {}", path.as_ref().display())]
pub fn open(path: impl AsRef<Path>) -> Result<File> {
    File::open(&path).anyhow_err()
}

#[context("Failed to open path for writing: {}", path.as_ref().display())]
pub fn create(path: impl AsRef<Path>) -> Result<File> {
    File::create(&path).anyhow_err()
}

#[context("Failed to canonicalize path: {}", path.as_ref().display())]
pub fn canonicalize(path: impl AsRef<Path>) -> Result<PathBuf> {
    std::fs::canonicalize(&path).anyhow_err()
}

#[context("Failed to create missing directories no path: {}", path.as_ref().display())]
pub fn create_dir_all(path: impl AsRef<Path>) -> Result {
    std::fs::create_dir_all(&path).anyhow_err()
}
