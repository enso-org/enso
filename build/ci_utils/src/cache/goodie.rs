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
    fn activate(&self, package_path: PathBuf) -> Result;
}

pub trait GoodieExt: Goodie {
    fn install_if_missing(&self, cache: &Cache) -> BoxFuture<'static, Result> {
        let this = self.clone();
        let cache = cache.clone();
        async move {
            if this.is_active().await.unwrap_or(false) {
                trace!("Skipping activation of {this:?} because it already present.",);
            } else {
                let package = this.download(&cache).await?;
                this.activate(package)?;
            }
            Result::Ok(())
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
