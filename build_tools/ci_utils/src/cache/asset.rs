// use crate::prelude::*;
//
// use crate::cache::Cache;
// use crate::cache::Storable;
// use crate::github;
// use octocrab::models::AssetId;
// use reqwest::header::HeaderMap;
// use reqwest::header::HeaderValue;
//
// #[derive(Clone, Debug, Serialize, Deserialize)]
// pub struct Key {
//     pub repository: github::Repo,
//     pub asset_id:   AssetId,
// }
//
// #[derive(Clone, Debug)]
// pub struct Asset {
//     pub key:      Key,
//     pub octocrab: Octocrab,
// }
//
// impl Storable for Asset {
//     type Metadata = ();
//     type Output = PathBuf;
//     type Key = Key;
//
//     fn generate(&self, cache: Cache, store: PathBuf) -> BoxFuture<'static,
// Result<Self::Metadata>> {         let this = self.clone();
//         async move {
//             let Asset { octocrab, key: Key { asset_id, repository } } = this;
//             let url =
//                 format!("https://api.github.com/repos/{repository}/releases/assets/{asset_id}");
//             let url = Url::parse(&url)?;
//             let job = crate::cache::download::DownloadFile {
//                 client: octocrab.client.clone(),
//                 key:    crate::cache::download::Key {
//                     url:                url.clone(),
//                     additional_headers: HeaderMap::from_iter([(
//                         reqwest::header::ACCEPT,
//                         HeaderValue::from_static(mime::APPLICATION_OCTET_STREAM.as_ref()),
//                     )]),
//                 },
//             };
//             cache.get(job).await.map(|_| ())
//         }
//         .boxed()
//     }
//
//     fn adapt(
//         &self,
//         cache: PathBuf,
//         _metadata: Self::Metadata,
//     ) -> BoxFuture<'static, Result<Self::Output>> {
//         ready(Result::Ok(cache)).boxed()
//     }
//
//     fn key(&self) -> Self::Key {
//         self.key.clone()
//     }
// }

// pub struct DownloadAsset {
//     pub octocrab: Octocrab,
//     pub repo:     RepoContext,
// }
//
// impl DownloadAsset {
//     fn download_asset_request(&self) -> RequestBuilder {
//         self.octocrab
//             .client
//             .get(self.url.clone())
//             .header(reqwest::header::ACCEPT, mime::APPLICATION_OCTET_STREAM.as_ref())
//     }
//
//     async fn cached(&self, cache: &Cache) -> Result<PathBuf> {
//         let job = crate::cache::download::DownloadFile {
//             client: self.octocrab.client.clone(),
//             key:    crate::cache::download::Key {
//                 url:                self.url.clone(),
//                 additional_headers: HeaderMap::from_iter([(
//                     reqwest::header::ACCEPT,
//                     HeaderValue::from_static(mime::APPLICATION_OCTET_STREAM.as_ref()),
//                 )]),
//             },
//         };
//         cache.get(job).await
//     }
//
//     async fn get(&self) -> Result<Response> {
//         let request = self.download_asset_request();
//         let response = request.send().await?;
//         let response = crate::io::web::handle_error_response(response).await?;
//         Ok(response)
//     }
// }
