use crate::prelude::*;

use crate::cache::Cache;
use crate::cache::Storable;
use crate::io::filename_from_url;
use crate::io::web::filename_from_response;
use crate::io::web::handle_error_response;
use crate::io::web::stream_response_to_file;

use derivative::Derivative;
use headers::HeaderMap;
use reqwest::Client;
use reqwest::ClientBuilder;
use reqwest::IntoUrl;
use reqwest::Response;



#[derive(Clone, Derivative, Serialize, Deserialize)]
#[derivative(Debug)]
pub struct Key {
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub url: Url,

    /// We keep this as part of the key, as some GitHub API endpoints change their meaning based on
    /// the headers set.
    #[serde(with = "http_serde::header_map")]
    pub additional_headers: HeaderMap,
}

#[derive(Clone, Debug)]
pub struct DownloadFile {
    pub key:    Key,
    pub client: Client,
}

impl DownloadFile {
    pub fn new(url: impl IntoUrl) -> Result<Self> {
        Ok(Self {
            key:    Key { url: url.into_url()?, additional_headers: default() },
            client: ClientBuilder::new().user_agent("enso-build").build()?,
        })
    }


    pub fn send_request(&self) -> BoxFuture<'static, Result<Response>> {
        let response = self
            .client
            .get(self.key.url.clone())
            .headers(self.key.additional_headers.clone())
            .send();

        let span = info_span!("Downloading a file.", url = %self.key.url);
        async move { handle_error_response(response.await?).await }.instrument(span).boxed()
    }
}

impl Storable for DownloadFile {
    type Metadata = PathBuf;
    type Output = PathBuf;
    type Key = Key;

    fn generate(
        &self,
        _cache: Cache,
        store: PathBuf,
    ) -> BoxFuture<'static, Result<Self::Metadata>> {
        // FIXME use `download_to_dir`
        let response = self.send_request();
        let filename = filename_from_url(&self.key.url);
        async move {
            let response = response.await?;
            let last_fallback_name = PathBuf::from("data");
            let filename = filename_from_response(&response)
                .map(ToOwned::to_owned)
                .or(filename)
                .unwrap_or(last_fallback_name);
            let output = store.join(&filename);
            stream_response_to_file(response, &output).await?;
            Ok(filename) // We don't store absolute paths to keep cache relocatable.
        }
        .boxed()
    }

    fn adapt(
        &self,
        store: PathBuf,
        metadata: Self::Metadata,
    ) -> BoxFuture<'static, Result<Self::Output>> {
        ready(Ok(store.join(metadata))).boxed()
    }

    fn key(&self) -> Self::Key {
        self.key.clone()
    }
}

pub async fn download(cache: Cache, url: impl IntoUrl) -> Result<PathBuf> {
    let download = DownloadFile::new(url)?;
    cache.get(download).await
}
