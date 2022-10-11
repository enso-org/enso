// use crate::prelude::*;
//
// use crate::models::config::RepoContext;
// use octocrab::repos::RepoHandler;
// use reqwest::RequestBuilder;
//
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
