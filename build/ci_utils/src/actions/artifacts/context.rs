use crate::prelude::*;

use crate::actions::artifacts::API_VERSION;
use crate::env::expect_var;
use crate::extensions::reqwest::ClientBuilderExt;

use mime::Mime;
use reqwest::header::HeaderMap;
use reqwest::header::HeaderValue;
use reqwest::header::ACCEPT_ENCODING;
use reqwest::Client;
use reqwest::ClientBuilder;



#[derive(Clone, Debug)]
pub struct Context {
    pub runtime_url:   Url,
    pub runtime_token: String,
    pub run_id:        String,
    pub api_version:   String,
}

impl Context {
    pub fn new_from_env() -> Result<Self> {
        let runtime_url = expect_var("ACTIONS_RUNTIME_URL")?.parse()?;
        let runtime_token = expect_var("ACTIONS_RUNTIME_TOKEN")?;
        let run_id = expect_var("GITHUB_RUN_ID")?;
        let api_version = API_VERSION.to_string();
        Ok(Context { runtime_url, runtime_token, run_id, api_version })
    }

    pub fn artifact_url(&self) -> Result<Url> {
        let Context { runtime_url, run_id, api_version, .. } = self;
        let url_text = format!(
            "{runtime_url}_apis/pipelines/workflows/{run_id}/artifacts?api-version={api_version}"
        );
        Url::parse(&url_text).anyhow_err()
    }

    pub fn prepare_client(&self, accept_mime: Mime) -> Result<ClientBuilder> {
        let mut headers = HeaderMap::new();
        headers.insert(
            reqwest::header::ACCEPT,
            iformat!("{accept_mime};api-version={self.api_version}").parse()?,
        );
        headers.insert(
            reqwest::header::AUTHORIZATION,
            format!("Bearer {}", self.runtime_token).parse()?,
        );

        Ok(ClientBuilder::new().default_headers(headers).user_agent(crate::USER_AGENT))
    }

    pub fn json_client(&self) -> Result<Client> {
        self.prepare_client(mime::APPLICATION_JSON)?
            .default_content_type(mime::APPLICATION_JSON)
            .build()
            .anyhow_err()
    }

    pub fn upload_client(&self) -> Result<Client> {
        let keep_alive_seconds = 3;

        let mut headers = HeaderMap::new();
        headers.insert(reqwest::header::CONNECTION, HeaderValue::from_static("Keep-Alive"));
        headers.insert("Keep-Alive", keep_alive_seconds.into());
        self.prepare_client(mime::APPLICATION_OCTET_STREAM)?
            .default_content_type(mime::APPLICATION_JSON)
            .default_headers(headers)
            .build()
            .anyhow_err()
    }

    pub fn download_client(&self) -> Result<Client> {
        self.prepare_client(mime::APPLICATION_OCTET_STREAM)?
            .default_content_type(mime::APPLICATION_JSON)
            .keep_alive(10)
            .default_header(ACCEPT_ENCODING, HeaderValue::try_from("gzip").unwrap())
            .build()
            .anyhow_err()
    }
}
