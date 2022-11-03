use crate::prelude::*;

use tokio::fs::File;
use tokio::io::AsyncRead;


// ==============
// === Export ===
// ==============

pub use crate::fs::wrappers::tokio::*;



/// Like the standard version but will create any missing parent directories from the path.
#[context("Failed to open path for writing: {}", path.as_ref().display())]
pub async fn create(path: impl AsRef<Path>) -> Result<File> {
    create_parent_dir_if_missing(&path).await?;
    crate::fs::wrappers::tokio::create(&path).await
}

/// Create a directory (and all missing parent directories),
///
/// Does not fail when a directory already exists.
#[context("Failed to create directory {}", path.as_ref().display())]
pub async fn create_dir_if_missing(path: impl AsRef<Path>) -> Result {
    let result = tokio::fs::create_dir_all(&path).await;
    match result {
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
            trace!("Directory already exists: {}", path.as_ref().display());
            Ok(())
        }
        result => {
            trace!("Created directory: {}", path.as_ref().display());
            result.anyhow_err()
        }
    }
}

/// Create a parent directory for path (and all missing parent directories),
///
/// Does not fail when a directory already exists.
#[context("Failed to create parent directory for {}", path.as_ref().display())]
pub async fn create_parent_dir_if_missing(path: impl AsRef<Path>) -> Result<PathBuf> {
    if let Some(parent) = path.as_ref().parent() {
        create_dir_if_missing(parent).await?;
        Ok(parent.into())
    } else {
        bail!("No parent directory for path {}.", path.as_ref().display())
    }
}

#[context("Failed to write file: {}", path.as_ref().display())]
pub async fn write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    create_parent_dir_if_missing(&path).await?;
    crate::fs::wrappers::tokio::write(&path, &contents).await.anyhow_err()
}

pub async fn copy_to_file(
    mut content: impl AsyncRead + Unpin,
    output_path: impl AsRef<Path>,
) -> Result<u64> {
    let mut output = create(output_path).await?;
    tokio::io::copy(&mut content, &mut output).await.anyhow_err()
}

/// Remove a directory with all its subtree.
///
/// Does not fail if the directory is not found.
#[instrument(fields(path = %path.as_ref().display()), err, level = "trace")]
pub async fn remove_dir_if_exists(path: impl AsRef<Path>) -> Result {
    let path = path.as_ref();
    let result = tokio::fs::remove_dir_all(&path).await;
    match result {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        result => result.context(format!("Failed to remove directory {}.", path.display())),
    }
}

pub async fn perhaps_remove_dir_if_exists(dry_run: bool, path: impl AsRef<Path>) -> Result {
    if dry_run {
        info!("Would remove directory {}.", path.as_ref().display());
        Ok(())
    } else {
        remove_dir_if_exists(path).await
    }
}

/// Recreate directory, so it exists and is empty.
pub async fn reset_dir(path: impl AsRef<Path>) -> Result {
    let path = path.as_ref();
    remove_dir_if_exists(&path).await?;
    create_dir_if_missing(&path).await?;
    Ok(())
}

pub async fn write_iter(
    path: impl AsRef<Path>,
    iter: impl IntoIterator<Item = impl AsRef<[u8]>>,
) -> Result {
    let mut file = create(&path).await?;
    for line in iter {
        file.write_all(line.as_ref())
            .await
            .with_context(|| format!("Failed to write to file {}.", path.as_ref().display()))?;
    }
    file.flush().await.with_context(|| {
        format!("Failed to flush file {} after writing.", path.as_ref().display())
    })?;
    Ok(())
}

/// Append contents to the file.
///
/// If the file does not exist, it will be created.
pub async fn append(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    tokio::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(&path)
        .await
        .with_context(|| format!("Failed to open file {} for appending.", path.as_ref().display()))?
        .write_all(contents.as_ref())
        .await
        .with_context(|| format!("Failed to write to file {}.", path.as_ref().display()))
}
