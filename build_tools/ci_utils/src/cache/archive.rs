use crate::prelude::*;

use crate::cache::Cache;
use crate::cache::Storable;



#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Key<S> {
    pub archive_source_key: S,
    pub path_to_extract:    Option<PathBuf>,
}

#[derive(Clone, Debug)]
pub struct ExtractedArchive<S> {
    pub archive_source:  S,
    pub path_to_extract: Option<PathBuf>,
}

impl<S: Storable<Output = PathBuf> + Clone> Storable for ExtractedArchive<S> {
    type Metadata = ();
    type Output = PathBuf;
    type Key = Key<S::Key>;

    fn generate(&self, cache: Cache, store: PathBuf) -> BoxFuture<'static, Result<Self::Metadata>> {
        let Self { path_to_extract, archive_source } = self.clone();
        let get_archive_job = cache.get(archive_source);
        async move {
            let archive_path = get_archive_job.await?;
            if let Some(path_to_extract) = path_to_extract {
                crate::archive::extract_item(&archive_path, path_to_extract, &store).await
            } else {
                crate::archive::extract_to(&archive_path, &store).await
            }
        }
        .boxed()
    }

    fn adapt(&self, cache: PathBuf, _: Self::Metadata) -> BoxFuture<'static, Result<Self::Output>> {
        async move { Ok(cache) }.boxed()
    }

    fn key(&self) -> Self::Key {
        Key {
            archive_source_key: self.archive_source.key(),
            path_to_extract:    self.path_to_extract.clone(),
        }
    }
}
