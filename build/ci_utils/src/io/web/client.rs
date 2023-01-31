use crate::prelude::*;

use crate::archive::Format;
use crate::global::progress_bar;
use crate::io::web;

use reqwest::Client;
use reqwest::IntoUrl;
use reqwest::Response;
use std::time::Duration;



pub async fn get(client: &Client, url: impl IntoUrl) -> Result<Response> {
    let url = url.into_url()?;
    web::execute(client.get(url.clone())).await.with_context(|| format!("Failed to get {url}"))
}

/// Get the the response body as a byte stream.
pub async fn download(
    client: &Client,
    url: impl IntoUrl,
) -> Result<impl Stream<Item = reqwest::Result<Bytes>>> {
    let url = url.into_url()?;
    debug!("Downloading {url}.");
    Ok(client.get(url).send().await?.error_for_status()?.bytes_stream())
}

/// Get the full response body from URL as bytes.
pub async fn download_all(client: &Client, url: impl IntoUrl) -> Result<Bytes> {
    let url = url.into_url()?;
    let bar = progress_bar(indicatif::ProgressBar::new_spinner);
    bar.enable_steady_tick(Duration::from_millis(100));
    bar.set_message(format!("Downloading {url}"));
    let response = web::execute(client.get(url.clone())).await?;
    response.bytes().await.with_context(|| format!("Failed to download body of {url}"))
}

/// Downloads archive from URL and extracts it into an output path.
pub async fn download_and_extract(
    client: &Client,
    url: impl IntoUrl,
    output_dir: impl AsRef<Path>,
) -> anyhow::Result<()> {
    let url = url.into_url()?;
    let url_text = url.to_string();
    let filename = crate::io::filename_from_url(&url)?;
    let format = Format::from_filename(&filename)?;

    debug!("Downloading {}", url_text);
    // FIXME: dont keep the whole download in the memory.
    let contents = download_all(client, url).await?;
    let buffer = std::io::Cursor::new(contents);

    debug!("Extracting {} to {}", filename.display(), output_dir.as_ref().display());
    format.extract(buffer, output_dir.as_ref()).with_context(|| {
        format!("Failed to extract data from {} to {}.", url_text, output_dir.as_ref().display(),)
    })
}

/// Download file at base_url/subpath to output_dir_base/subpath.
pub async fn download_relative(
    client: &Client,
    base_url: &Url,
    output_dir_base: impl AsRef<Path>,
    subpath: &Path,
) -> Result<PathBuf> {
    let url_to_get = base_url.join(&subpath.display().to_string())?;
    let output_path = output_dir_base.as_ref().join(subpath);

    debug!("Will download {} => {}", url_to_get, output_path.display());
    let response = client.get(url_to_get).send().await?.error_for_status()?;

    if let Some(parent_dir) = output_path.parent() {
        crate::fs::create_dir_if_missing(parent_dir)?;
    }

    crate::io::web::stream_to_file(response.bytes_stream(), &output_path).await?;
    debug!("Download finished: {}", output_path.display());
    Ok(output_path)
}
