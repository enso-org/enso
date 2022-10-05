use crate::prelude::*;

use crate::io::web::filename_from_response;
use crate::io::web::stream_response_to_file;

use reqwest::IntoUrl;
use tokio::io::AsyncRead;
use web::client;


// ==============
// === Export ===
// ==============

pub mod web;



/// Read the whole input and return its length.
///
/// Inputs content is discarded.
pub async fn read_length(mut read: impl AsyncRead + Unpin) -> Result<u64> {
    let mut sink = tokio::io::sink();
    tokio::io::copy(&mut read, &mut sink).anyhow_err().await
}

/// Get the the response body as a byte stream.
pub async fn download(url: impl IntoUrl) -> Result<impl Stream<Item = reqwest::Result<Bytes>>> {
    client::download(&default(), url).await
}

pub async fn download_to_dir(url: impl IntoUrl, dir: impl AsRef<Path>) -> Result<PathBuf> {
    let url = url.into_url()?;
    let response = web::client::get(&default(), url.clone()).await?;
    let filename = filename_from_response(&response)
        .map(ToOwned::to_owned)
        .or_else(|_| filename_from_url(&url))
        .unwrap_or_else(|_| Uuid::new_v4().to_string().into());

    trace!("Filename for {url} download shall be {filename}", filename = filename.display());
    let output = dir.as_ref().join(&filename);
    stream_response_to_file(response, &output).await?;
    Ok(output)
}

/// Get the full response body from URL as bytes.
pub async fn download_all(url: impl IntoUrl) -> anyhow::Result<Bytes> {
    client::download_all(&default(), url).await
}

/// Take the trailing filename from URL path.
///
/// ```
/// use std::path::PathBuf;
/// use url::Url;
/// use ide_ci::io::filename_from_url;
/// let url = Url::parse("https://github.com/enso-org/ide/releases/download/v2.0.0-alpha.18/enso-win-2.0.0-alpha.18.exe").unwrap();
/// assert_eq!(filename_from_url(&url).unwrap(), PathBuf::from("enso-win-2.0.0-alpha.18.exe"));
/// ```
pub fn filename_from_url(url: &Url) -> anyhow::Result<PathBuf> {
    url.path_segments()
        .ok_or_else(|| anyhow!("Cannot split URL '{}' into path segments!", url))?
        .last()
        .ok_or_else(|| anyhow!("No segments in path for URL '{}'", url))
        .map(PathBuf::from)
        .map_err(Into::into)
}

/// Downloads archive from URL and extracts it into an output path.
pub async fn download_and_extract(
    url: impl IntoUrl,
    output_dir: impl AsRef<Path>,
) -> anyhow::Result<()> {
    client::download_and_extract(&default(), url, output_dir).await
}

// pub async fn stream_to_file<E: Into<Box<dyn std::error::Error + Send + Sync>>>(
//     stream: impl Stream<Item = std::result::Result<Bytes, E>> + Unpin,
//     output_path: impl AsRef<Path>,
// ) -> Result {
//     let mut reader = tokio_util::io::StreamReader::new(stream.map_err(std::io::Error::other));
//     let mut output = crate::fs::tokio::create(output_path).await?;
//     tokio::io::copy(&mut reader, &mut output).await?;
//     Ok(())
// }


#[cfg(test)]
mod tests {
    use super::*;
    use crate::fs::copy;
    use crate::fs::create_parent_dir_if_missing;
    use crate::fs::mirror_directory;
    use tempfile::tempdir;

    #[tokio::test]
    #[ignore]
    async fn test_download() -> Result {
        debug!("Hello world!");
        let url = "https://speed.hetzner.de/100MB.bin";
        download_all(url).await?;
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn copy_dir_with_symlink() -> Result {
        let dir = tempdir()?;
        let foo = dir.join_iter(["src", "foo.txt"]);
        crate::env::set_current_dir(&dir)?;
        create_parent_dir_if_missing(&foo)?;
        std::fs::write(&foo, "foo")?;

        let bar = foo.with_file_name("bar");

        // Command::new("ls").arg("-laR").run_ok().await?;
        #[cfg(not(target_os = "windows"))]
        std::os::unix::fs::symlink(foo.file_name().unwrap(), &bar)?;
        #[cfg(target_os = "windows")]
        std::os::windows::fs::symlink_file(foo.file_name().unwrap(), &bar)?;

        copy(foo.parent().unwrap(), foo.parent().unwrap().with_file_name("dest"))?;

        mirror_directory(foo.parent().unwrap(), foo.parent().unwrap().with_file_name("dest2"))
            .await?;

        tokio::process::Command::new(r"C:\msys64\usr\bin\ls.exe")
            .arg("-laR")
            .status()
            .await?
            .exit_ok()?;

        Ok(())
    }
}
