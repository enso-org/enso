use crate::prelude::*;

use crate::actions::artifacts::context::Context;
use crate::actions::artifacts::models::ArtifactResponse;
use crate::actions::artifacts::models::ContainerEntry;
use crate::actions::artifacts::models::CreateArtifactResponse;
use crate::actions::artifacts::models::PatchArtifactSizeResponse;
use crate::actions::artifacts::raw;

use reqwest::Client;
use tokio::io::AsyncRead;



#[derive(Clone, Debug)]
pub struct SessionClient {
    pub json_client:     Client,
    pub upload_client:   Client,
    pub download_client: Client,
    pub artifact_url:    Url,
}

impl SessionClient {
    pub async fn create_container(
        &self,
        artifact_name: impl AsRef<str>,
    ) -> Result<CreateArtifactResponse> {
        raw::endpoints::create_container(
            &self.json_client,
            self.artifact_url.clone(),
            artifact_name,
        )
        .await
    }

    pub async fn list_artifacts(&self) -> Result<Vec<ArtifactResponse>> {
        raw::endpoints::list_artifacts(&self.json_client, self.artifact_url.clone()).await
    }

    pub fn new(context: &Context) -> Result<Self> {
        Ok(Self {
            json_client:     context.json_client()?,
            upload_client:   context.upload_client()?,
            artifact_url:    context.artifact_url()?,
            download_client: context.download_client()?,
        })
    }

    pub fn new_from_env() -> Result<Self> {
        Self::new(&Context::new_from_env()?)
    }

    pub async fn patch_artifact_size(
        &self,
        artifact_name: &str,
        total_size: usize,
    ) -> Result<PatchArtifactSizeResponse> {
        raw::endpoints::patch_artifact_size(
            &self.json_client,
            self.artifact_url.clone(),
            artifact_name,
            total_size,
        )
        .await
    }

    pub async fn get_container_items(
        &self,
        artifact: &ArtifactResponse,
    ) -> Result<Vec<ContainerEntry>> {
        Ok(crate::actions::artifacts::raw::endpoints::get_container_items(
            &self.json_client,
            artifact.file_container_resource_url.clone(),
            &artifact.name,
        )
        .await?
        .value)
    }

    pub async fn download_container_item(
        &self,
        content_location: Url,
    ) -> Result<impl AsyncRead + Send> {
        raw::endpoints::download_item(&self.download_client, content_location).await
    }
}
