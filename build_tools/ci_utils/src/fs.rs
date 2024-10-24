//! Wrappers around the `std::fs` module, which provide better error messages and avoid some
//! typical pitfalls.

use crate::prelude::*;

use fs_extra::dir::CopyOptions;
use fs_extra::error::ErrorKind;


// ==============
// === Export ===
// ==============

pub mod tokio;
pub mod wrappers;

pub use enso_build_base::fs::*;



/// Copy source item (file or a directory) to a destination directory, preserving the filename.
#[tracing::instrument(skip_all, fields(
src  = %source_file.as_ref().display(),
dest = %dest_dir.as_ref().display()),
err)]
pub fn copy_to(source_file: impl AsRef<Path>, dest_dir: impl AsRef<Path>) -> Result<PathBuf> {
    require_exist(&source_file)?;
    create_dir_if_missing(dest_dir.as_ref())?;
    debug!("Will copy {} to {}", source_file.as_ref().display(), dest_dir.as_ref().display());
    let mut options = CopyOptions::new();
    options.overwrite = true;
    fs_extra::copy_items(&[&source_file], &dest_dir, &options).map_err(handle_fs_extra_error)?;
    Ok(dest_dir.as_ref().join(source_file.as_ref().try_file_name()?))
}

/// Copy the item (file or a directory) to a destination path.
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
            let mut options = CopyOptions::new();
            options.overwrite = true;
            options.content_only = true;
            fs_extra::dir::copy(source_file, destination_file, &options)
                .map_err(handle_fs_extra_error)?;
        } else {
            enso_build_base::fs::wrappers::copy(source_file, destination_file)?;
        }
    } else {
        bail!("Cannot copy to the root path: {}", destination_file.display());
    }
    Ok(())
}

/// Mirrors the directory (like `rsync`).
///
/// All files and directories from the source directory will be copied to the destination directory,
/// unless they are already present and have the same content.
/// Any files or directories that are present in the destination directory, but not in the source
/// directory, will be removed.
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

/// Copy the file to the destination path, unless the file already exists and has the same content.
///
/// If the directory is passed as the source, it will be copied recursively.
#[tracing::instrument(skip_all, fields(
src  = %source.as_ref().display(),
dest = %target.as_ref().display()),
err)]
pub async fn copy_if_different(source: impl AsRef<Path>, target: impl AsRef<Path>) -> Result {
    if tokio::metadata(&source).await?.is_file() {
        return copy_file_if_different(source, target);
    }

    let walkdir = walkdir::WalkDir::new(&source);
    let entries: Vec<_> = walkdir.into_iter().try_collect()?;
    for entry in entries.into_iter().filter(|e| e.file_type().is_file()) {
        let entry_path = entry.path();
        let relative_path = pathdiff::diff_paths(entry_path, &source)
            .context(format!("Failed to relativize path {}.", entry_path.display()))?;
        copy_file_if_different(entry_path, target.as_ref().join(relative_path))?;
    }
    Ok(())
}

/// Create a symlink.
///
/// This function hides the platform differences between Windows and Unix.
#[context("Failed to create symlink {} => {}", src.as_ref().display(), dst.as_ref().display())]
pub fn symlink_auto(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result {
    create_parent_dir_if_missing(&dst)?;
    debug!("Creating symlink {} <= {}", src.as_ref().display(), dst.as_ref().display());
    Ok(symlink::symlink_auto(&src, &dst)?)
}

/// Remove a symlink to a directory if it exists.
#[context("Failed to remove symlink {}", path.as_ref().display())]
pub fn remove_symlink_dir_if_exists(path: impl AsRef<Path>) -> Result {
    let result = symlink::remove_symlink_dir(&path);
    match result {
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(()),
        ret => Ok(ret?),
    }
}

/// Create a symlink to a directory, or remove and recreate it if it already exists.
pub fn create_or_update_symlink_dir(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> Result {
    remove_symlink_dir_if_exists(&dst)?;
    symlink_auto(&src, &dst)
}

/// `fs_extra`'s error type is not friendly to `anyhow`, so we need to convert it manually.
///
/// Otherwise, we get just the message to look into the error kind, but the kind information is
/// lost.
pub fn handle_fs_extra_error(error: fs_extra::error::Error) -> anyhow::Error {
    let message = error.to_string();
    match error.kind {
        ErrorKind::Io(inner) => anyhow::Error::new(inner),
        ErrorKind::StripPrefix(inner) => anyhow::Error::new(inner),
        ErrorKind::OsString(inner) => anyhow::Error::msg(inner.to_string_lossy().to_string()),
        _ => return error.into(),
    }
    .context(message)
}

/// Remove files using [glob patterns](https://docs.rs/glob/latest/glob/struct.Pattern.html).
#[tracing::instrument(skip_all, fields(pattern = %glob_pattern), err)]
#[context("Failed to remove files using glob pattern `{}`.", glob_pattern)]
pub fn remove_glob(glob_pattern: &str) -> Result {
    for entry in glob::glob(glob_pattern)? {
        let path = entry?;
        remove_if_exists(&path)?;
    }
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn remove_glob_test() -> Result {
        let temp = tempfile::tempdir()?;
        crate::env::try_with_current_dir(&temp, || {
            let pattern_to_remove = "**/file1.txt";
            write("file1.txt", "file1")?;
            write("file2.txt", "file2")?;
            write("dir1/file1.txt", "file1")?;
            write("dir1/file2.txt", "file2")?;

            remove_glob(pattern_to_remove)?;
            assert!(!Path::new("file1.txt").exists());
            assert!(Path::new("file2.txt").exists());
            assert!(!Path::new("dir1/file1.txt").exists());
            assert!(Path::new("dir1/file2.txt").exists());
            Ok(())
        })
    }

    #[test]
    fn remove_glob_test2() -> Result {
        // Make sure that `**` can expand to nothing outside the current directory.
        let temp = tempfile::tempdir()?;
        let pattern_to_remove = "**/file1.txt";
        let file1 = temp.path().join("file1.txt");

        write(&file1, "file1")?;
        remove_glob(temp.path().join(pattern_to_remove).as_str())?;
        assert!(!file1.exists());
        Ok(())
    }
}
