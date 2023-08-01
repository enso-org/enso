//! Support for downloading files.

use crate::prelude::*;

use ide_ci::cache::Cache;
use ide_ci::cache::Storable;
use reqwest::IntoUrl;
use std::fs::File;



// ===========================
// === Download with cache ===
// ===========================

/// Check whether the specified URL has been downloaded and cached. If not, download it using the
/// specified downloader, and store it in the cache. Opens the file in the cache and returns it.
/// The `filename` parameter may be any path-safe string; it will only be seen when inspecting the
/// cache.
pub async fn get_file_from_cache_or_download(
    filename: impl AsRef<Path>,
    cache: &Cache,
    downloader: impl Downloader,
    url: impl IntoUrl,
) -> Result<File> {
    let url = url.into_url()?;
    let filename = filename.as_ref().into();
    let downloader = Arc::new(downloader);
    let cache_path = cache.get(FixedContentUrl { url, filename, downloader }).await?;
    Ok(File::open(&cache_path)?)
}

/// A URL that can provide a file which can be assumed to be constant.
#[derive(Debug)]
struct FixedContentUrl<D> {
    url:        Url,
    filename:   Box<Path>,
    downloader: Arc<D>,
}

impl<D: Downloader> Storable for FixedContentUrl<D> {
    /// Relative path to the file within its cache directory.
    type Metadata = Box<Path>;
    /// Relative path to the file within its cache directory.
    type Output = Box<Path>;
    /// The URL from which the file is obtained.
    type Key = Url;

    fn generate(
        &self,
        _cache: Cache,
        store: PathBuf,
    ) -> BoxFuture<'static, Result<Self::Metadata>> {
        let url = self.url.clone();
        let filename = store.join(&self.filename).into_boxed_path();
        let downloader = self.downloader.clone();
        async move {
            downloader.download(url, filename.clone()).await?;
            Ok(filename)
        }
        .boxed()
    }

    fn adapt(
        &self,
        cache: PathBuf,
        metadata: Self::Metadata,
    ) -> BoxFuture<'static, Result<Self::Output>> {
        let path = cache.join(&metadata).into();
        async move { Ok(path) }.boxed()
    }

    fn key(&self) -> Self::Key {
        self.url.clone()
    }
}



// ==================
// === Downloader ===
// ==================

/// Represents the capability of obtaining a file from a URL.
pub trait Downloader: Debug + Send + Sync + 'static {
    fn download(&self, url: impl IntoUrl, path: Box<Path>) -> BoxFuture<'static, Result<()>>;
}


// === Implementations ===

impl Downloader for Octocrab {
    fn download(&self, url: impl IntoUrl, path: Box<Path>) -> BoxFuture<'static, Result<()>> {
        let url = url.into_url();
        let client = self.client.clone();
        async move {
            let reply = ide_ci::io::web::client::download(&client, url?).await?;
            ide_ci::io::web::stream_to_file(reply, path).await?;
            Ok(())
        }
        .boxed()
    }
}
