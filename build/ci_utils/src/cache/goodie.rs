use crate::prelude::*;

use crate::cache;
use crate::cache::Cache;


// ==============
// === Export ===
// ==============

pub mod binaryen;
pub mod graalpy;
pub mod graalvm;
pub mod sbt;



/// Something that can be obtained (with IO) and, after that, enabled by modifying global state.
pub trait Goodie: Debug + Clone + Send + Sync + 'static {
    /// Obtain and place this in the cache.
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>>;

    /// Check whether this is already present.
    fn is_active(&self) -> BoxFuture<'static, Result<bool>>;

    /// Changes to the environment to activate this.
    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>>;

    /// Apply the activation environment changes.
    fn activate(&self, package_path: &Path) -> Result {
        for modification in self.activation_env_changes(package_path)? {
            modification.apply()?;
        }
        Ok(())
    }
}

pub trait GoodieExt: Goodie {
    /// Check if this goodie is active. If not, download it and activate it.
    ///
    /// If the goodie was already active, returns Ok(None). If it was not active, returns
    /// Ok(Some(path)), where path is the path to the downloaded goodie within the cache.
    /// Usually it should not be necessary to use the returned path, as the goodie should have been
    /// activated and the global state modified accordingly.
    fn install_if_missing(&self, cache: &Cache) -> BoxFuture<'static, Result<Option<PathBuf>>> {
        let this = self.clone();
        let cache = cache.clone();
        async move {
            if this.is_active().await.unwrap_or(false) {
                trace!("Skipping activation of {this:?} because it already present.",);
                Result::Ok(None)
            } else {
                let package = this.install(&cache).await?;
                Result::Ok(Some(package))
            }
        }
        .boxed()
    }

    // Download it and activate this goodie. Does not check if it is already active.
    fn install(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        let this = self.clone();
        let cache = cache.clone();
        async move {
            let package = this.get(&cache).await?;
            this.activate(&package)?;
            Result::Ok(package)
        }
        .boxed()
    }
}

impl<T: Goodie> GoodieExt for T {}


pub fn download_url(url: Url, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
    download_try_url(Ok(url), cache)
}


pub fn download_try_url(url: Result<Url>, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
    download_try_future_url(ready(url), cache)
}


pub fn download_try_future_url(
    url: impl Future<Output = Result<Url>> + Send + 'static,
    cache: &Cache,
) -> BoxFuture<'static, Result<PathBuf>> {
    let cache = cache.clone();
    async move {
        let url = url.await?;
        let archive_source = cache::download::DownloadFile::new(url)?;
        let package = cache::archive::ExtractedArchive { archive_source, path_to_extract: None };
        cache.get(package).await
    }
    .boxed()
}
