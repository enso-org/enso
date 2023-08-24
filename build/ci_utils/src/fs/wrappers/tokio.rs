use crate::prelude::*;

use tokio::fs::File;
use tokio::io::AsyncReadExt;
use tokio_util::io::ReaderStream;



pub fn metadata<P: AsRef<Path>>(path: P) -> BoxFuture<'static, Result<std::fs::Metadata>> {
    let path = path.as_ref().to_owned();
    tokio::fs::metadata(path).anyhow_err().boxed()
}

/// See [tokio::fs::symlink_metadata].
pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> BoxFuture<'static, Result<std::fs::Metadata>> {
    let path = path.as_ref().to_owned();
    tokio::fs::symlink_metadata(path.clone())
        .with_context(move || {
            format!("Failed to obtain symlink metadata for file: {}", path.display())
        })
        .boxed()
}

#[context("Failed to open path for reading: {}", path.as_ref().display())]
pub async fn open(path: impl AsRef<Path>) -> Result<File> {
    File::open(&path).await.anyhow_err()
}

pub fn open_stream(path: impl AsRef<Path>) -> BoxFuture<'static, Result<ReaderStream<File>>> {
    let error_message = format!("Failed to open path for reading: {}", path.as_ref().display());
    let path = path.as_ref().to_owned();
    let file = open(path);
    async move {
        let file = file.await?;
        Result::Ok(ReaderStream::new(file))
    }
    .context(error_message)
    .boxed()
}

#[context("Failed to open path for writing: {}", path.as_ref().display())]
pub async fn create(path: impl AsRef<Path>) -> Result<File> {
    File::create(&path).await.anyhow_err()
}

#[context("Failed to create missing directories no path: {}", path.as_ref().display())]
pub async fn create_dir_all(path: impl AsRef<Path>) -> Result {
    tokio::fs::create_dir_all(&path).await.anyhow_err()
}

pub async fn read_dir(
    path: impl AsRef<Path>,
) -> Result<impl Stream<Item = Result<tokio::fs::DirEntry>>> {
    let path = path.as_ref().to_path_buf();
    let read_dir = tokio::fs::read_dir(&path)
        .await
        .with_context(|| format!("Failed to read the directory: {}", path.display()))?;

    let stream = tokio_stream::wrappers::ReadDirStream::new(read_dir);
    Ok(stream.map(move |entry| {
        entry.with_context(|| {
            format!("Failed to get entry when reading the directory: {}", path.display())
        })
    }))
}

#[context("Failed to remove directory with the subtree: {}", path.as_ref().display())]
pub async fn remove_dir_all(path: impl AsRef<Path>) -> Result {
    tokio::fs::remove_dir_all(&path).await.anyhow_err()
}

#[context("Failed to write file: {}", path.as_ref().display())]
pub async fn write(path: impl AsRef<Path>, contents: impl AsRef<[u8]>) -> Result {
    tokio::fs::write(&path, &contents).await.anyhow_err()
}

#[context("Failed to read file: {}", path.as_ref().display())]
pub async fn read<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    let mut file = File::open(&path).await?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).await?;
    Ok(contents)
}

#[context("Failed to read the file: {}", path.as_ref().display())]
pub async fn read_to_string(path: impl AsRef<Path>) -> Result<String> {
    tokio::fs::read_to_string(&path).await.anyhow_err()
}

/// See [`tokio::fs::set_permissions`].
#[context("Failed to set permissions {:?} for file: {}", permissions, path.as_ref().display())]
pub async fn set_permissions(path: impl AsRef<Path>, permissions: std::fs::Permissions) -> Result {
    tokio::fs::set_permissions(&path, permissions.clone()).await.anyhow_err()
}
