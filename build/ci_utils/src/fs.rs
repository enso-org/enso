use crate::prelude::*;

use async_compression::tokio::bufread::GzipEncoder;
use async_compression::Level;
use fs_extra::dir::CopyOptions;


// ==============
// === Export ===
// ==============

pub mod tokio;
pub mod wrappers;

pub use enso_build_base::fs::*;



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
            enso_build_base::fs::wrappers::copy(source_file, destination_file)?;
        }
    } else {
        bail!("Cannot copy to the root path: {}", destination_file.display());
    }
    Ok(())
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


/// Get the size of a file after gzip compression.
pub async fn compressed_size(path: impl AsRef<Path>) -> Result<byte_unit::Byte> {
    let file = ::tokio::io::BufReader::new(crate::fs::tokio::open(&path).await?);
    let encoded_stream = GzipEncoder::with_quality(file, Level::Best);
    crate::io::read_length(encoded_stream).await.map(into)
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

#[context("Failed to create symlink {} => {}", src.as_ref().display(), dst.as_ref().display())]
pub fn symlink_auto(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result {
    create_parent_dir_if_missing(&dst)?;
    symlink::symlink_auto(&src, &dst).anyhow_err()
}
