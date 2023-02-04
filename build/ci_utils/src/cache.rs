use crate::prelude::*;

use anyhow::Context;
use serde::de::DeserializeOwned;
use sha2::Digest;
use std::hash::Hasher;


// ==============
// === Export ===
// ==============

pub mod archive;
pub mod artifact;
pub mod asset;
pub mod download;
pub mod goodie;

pub use goodie::Goodie;



/// Format of the hashing scheme.
///
/// This value can be bumped to invalidate all the hashes.
pub const VERSION: u8 = 2;

/// Default location of the cache root.
pub fn default_path() -> Result<PathBuf> {
    Ok(dirs::data_local_dir()
        .context("Cannot locate user's local data directory.")?
        .join_iter([".enso-ci", "cache"]))
}

/// Description of the entity that can be cached.
pub trait Storable: Debug + Send + Sync + 'static {
    /// Data necessary to construct output from a disk storage.
    type Metadata: Clone + Debug + Serialize + DeserializeOwned + Send + Sync + 'static;
    /// An instance of the cached entity.
    type Output: Clone + Send + Sync + 'static;
    /// A key used to generate a hash.
    type Key: Clone + Debug + Serialize + DeserializeOwned + Send + Sync + 'static;

    /// Fill the cache store with this entity.
    ///
    /// The cache `store` parameter is an existing, writable, empty directory. The store path should
    /// not be assumed to be constant for this entry, metadata should not include it in any way.
    fn generate(&self, cache: Cache, store: PathBuf) -> BoxFuture<'static, Result<Self::Metadata>>;

    fn adapt(
        &self,
        cache: PathBuf,
        metadata: Self::Metadata,
    ) -> BoxFuture<'static, Result<Self::Output>>;

    fn key(&self) -> Self::Key;
}

/// The required metadata for a cache entry. Used when reading the cache.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EntryIndexRequired<S: Storable> {
    pub metadata: S::Metadata,
}

/// The metadata for a cache entry with additional information to help debugging.
#[derive(Clone, Debug, Serialize, Deserialize, derive_more::Deref, derive_more::DerefMut)]
#[serde(bound = "S:")]
pub struct EntryIndexExtended<S: Storable> {
    #[serde(flatten)]
    #[deref]
    #[deref_mut]
    pub inner:          EntryIndexRequired<S>,
    pub key:            Option<S::Key>,
    pub r#type:         Option<String>,
    pub key_type:       Option<String>,
    pub schema_version: Option<u8>,
}

impl<S: Storable> EntryIndexExtended<S> {
    pub fn new(metadata: S::Metadata, key: S::Key) -> Self {
        Self {
            inner:          EntryIndexRequired { metadata },
            key:            Some(key),
            r#type:         Some(std::any::type_name::<S>().into()),
            key_type:       Some(std::any::type_name::<S::Key>().into()),
            schema_version: Some(VERSION),
        }
    }
}

#[derive(Debug)]
pub struct HashToDigest<'a, D: Digest>(&'a mut D);
impl<'a, D: Digest> Hasher for HashToDigest<'a, D> {
    fn finish(&self) -> u64 {
        todo!()
    }

    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes)
    }
}

pub fn digest<S: Storable>(storable: &S) -> Result<String> {
    let key = storable.key();
    let key_serialized = bincode::serialize(&key)?;

    let mut digest = sha2::Sha224::default();
    sha2::Digest::update(&mut digest, [VERSION]);
    sha2::Digest::update(&mut digest, key_serialized);
    std::any::TypeId::of::<S::Key>().hash(&mut HashToDigest(&mut digest));
    std::any::TypeId::of::<S>().hash(&mut HashToDigest(&mut digest));
    let digest = digest.finalize();
    Ok(data_encoding::BASE64URL_NOPAD.encode(&digest))
}

#[derive(Clone, Debug)]
pub struct Cache {
    root: PathBuf,
}

impl Cache {
    pub async fn new_default() -> Result<Self> {
        Self::new(default_path()?).await
    }

    /// Path to the cache root.
    pub fn path(&self) -> &Path {
        &self.root
    }

    pub async fn new(path: impl Into<PathBuf>) -> Result<Self> {
        let root = path.into();
        crate::fs::tokio::create_dir_if_missing(&root).await?;
        debug!("Prepared cache in {}", root.display());
        Ok(Self { root })
    }

    pub fn get<S>(&self, storable: S) -> BoxFuture<'static, Result<S::Output>>
    where S: Storable {
        let this = self.clone();
        async move {
            let digest = digest(&storable)?;
            tracing::Span::current().record("digest", digest.as_str());
            let entry_dir = this.root.join(&digest);
            let entry_meta = entry_dir.with_appended_extension("json");

            let retrieve = async {
                let info = entry_meta.read_to_json::<EntryIndexRequired<S>>()?;
                crate::fs::require_exist(&entry_dir)?;
                storable.adapt(entry_dir.clone(), info.metadata).await
            };

            match retrieve.await {
                Ok(out) => {
                    trace!("Found in cache, skipping generation.");
                    Ok(out)
                }
                Err(e) => {
                    trace!("Value cannot be retrieved from cache because: {e}");
                    crate::fs::reset_dir(&entry_dir)?;
                    let key = storable.key();
                    tracing::Span::current().record("key", tracing::field::debug(&key));
                    let metadata = storable
                        .generate(this, entry_dir.clone())
                        .instrument(info_span!("Generating value to fill the cache."))
                        .await?;
                    let info = EntryIndexExtended::<S>::new(metadata, key);
                    entry_meta.write_as_json(&info)?;
                    storable.adapt(entry_dir, info.inner.metadata).await
                }
            }
        }
        .instrument(trace_span!(
            "Getting a value from cache.",
            digest = tracing::field::Empty,
            key = tracing::field::Empty
        ))
        .boxed()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::cache::download::DownloadFile;
    use crate::log::setup_logging;

    #[tokio::test]
    #[ignore]
    async fn cache_test() -> Result {
        setup_logging()?;
        let download_task = DownloadFile::new("https://store.akamai.steamstatic.com/public/shared/images/header/logo_steam.svg?t=962016")?;

        let cache = Cache::new("C:/temp/enso-cache").await?;
        cache.get(download_task).await?;
        Ok(())
    }
}
