use crate::prelude::*;

use crate::cache;
use crate::cache::Cache;


// ==============
// === Export ===
// ==============

pub mod binaryen;
pub mod graalvm;
pub mod sbt;



/// Something that can be downloaded and, after that, enabled by modifying global state.
pub trait Goodie: Debug + Clone + Send + Sync + 'static {
    fn url(&self) -> BoxFuture<'static, Result<Url>>;
    fn is_active(&self) -> BoxFuture<'static, Result<bool>>;
    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>>;
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
    /// If the goodie was already ective, returns Ok(None). If it was not active, returns
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
                let package = this.download(&cache).await?;
                this.activate(&package)?;
                Result::Ok(Some(package))
            }
        }
        .boxed()
    }


    fn package(
        &self,
    ) -> BoxFuture<'static, Result<cache::archive::ExtractedArchive<cache::download::DownloadFile>>>
    {
        let url_fut = self.url();
        async move {
            let url = url_fut.await?;
            let archive_source = cache::download::DownloadFile::new(url)?;
            let path_to_extract = None;
            Ok(cache::archive::ExtractedArchive { archive_source, path_to_extract })
        }
        .boxed()
    }

    fn download(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        let package = self.package();
        let cache = cache.clone();
        async move { cache.get(package.await?).await }.boxed()
    }
}

impl<T: Goodie> GoodieExt for T {}
//
// /// Whoever owns a token, can assume that the Goodie is available.
// #[derive(Clone, Debug, Display)]
// pub struct Token<G>(G);
//
// #[derive(Clone, Debug, Display)]
// pub struct PotentialFutureGoodie<G>(Box<dyn FnOnce() -> BoxFuture<'static, Result<Token<G>>>>);
//
// impl<G> PotentialFutureGoodie<G> {
//     pub fn new<F, Fut>(f: F) -> Self
//     where
//         F: FnOnce() -> Fut + 'static,
//         Fut: Future<Output = Result<Token<G>>> + Send + 'static, {
//         Self(Box::new(move || f().boxed()))
//     }
// }
//
// // pub type GoodieGenerator<G: Goodie> =
// //     dyn FnOnce(Cache, G) -> BoxFuture<'static, Result<Token<G>>> + Send + Sync + 'static;
// //
// // pub type PotentialFutureGoodie<G: Goodie> =
// //     dyn FnOnce(Cache) -> BoxFuture<'static, Result<Token<G>>> + Send + Sync + 'static;
