use crate::prelude::*;

use crate::actions::artifacts::models::ArtifactResponse;
use crate::actions::artifacts::models::ContainerEntry;
use crate::actions::artifacts::models::ItemType;
use crate::actions::artifacts::run_session::SessionClient;
use crate::actions::artifacts::API_VERSION;

use reqwest::header::HeaderMap;
use reqwest::header::HeaderValue;
use reqwest::header::ACCEPT;
use reqwest::header::ACCEPT_ENCODING;
use tokio::fs::create_dir_all;



#[derive(Clone, Debug)]
pub struct ArtifactDownloader {
    pub client:        SessionClient,
    pub artifact_name: String,
    pub info:          ArtifactResponse,
    pub items:         Vec<ContainerEntry>,
}

impl ArtifactDownloader {
    pub async fn new(client: SessionClient, artifact_name: impl Into<String>) -> Result<Self> {
        let artifact_name = artifact_name.into();
        let list = client.list_artifacts().await?;

        let relevant_entry = list
            .iter()
            .find(|artifact| artifact.name == artifact_name)
            .ok_or_else(|| anyhow!("Failed to find artifact by name {artifact_name}."))?;

        let items = client.get_container_items(relevant_entry).await?;
        dbg!(&items);
        Ok(Self { client, artifact_name, info: relevant_entry.clone(), items })
    }

    pub async fn download_file_item(&self, file: &FileToDownload) -> Result {
        let span = info_span!("Downloading file from artifact", url = %file.remote_source_location, target = %file.target.display());
        async move {
            let stream =
                self.client.download_container_item(file.remote_source_location.clone()).await?;
            crate::fs::tokio::copy_to_file(stream, &file.target).await?;
            Ok(())
        }
        .instrument(span)
        .await
    }

    pub async fn download_all_to(&self, root_path: &Path) -> Result {
        for item in &self.items {
            match item.item_type {
                ItemType::File => {
                    let file = FileToDownload::new_to_subtree(root_path, item)?;
                    self.download_file_item(&file).await?;
                }
                ItemType::Folder => {
                    create_dir_all(root_path.join(item.relative_path())).await?;
                }
            }
        }
        Ok(())
    }

    pub fn file_items(&self) -> impl Iterator<Item = &ContainerEntry> {
        self.items.iter().filter(|entry| entry.item_type == ItemType::File)
    }
}


#[derive(Clone, Debug)]
pub struct FileToDownload {
    /// Absolute path in the local filesystem.
    pub target:                 PathBuf,
    /// Relative path within the artifact container. Does not include the leading segment with the
    /// artifact name.
    pub remote_source_location: Url,
}

impl FileToDownload {
    #[context("Failed to process entry {} from the artifact container.", entry.path.display())]
    pub fn new_to_subtree(target_root: impl AsRef<Path>, entry: &ContainerEntry) -> Result<Self> {
        Ok(Self {
            target:                 target_root.as_ref().join(entry.relative_path()),
            remote_source_location: entry.content_location.clone(),
        })
    }
}

pub fn headers() -> HeaderMap {
    let mut header = HeaderMap::new();
    // We can safely unwrap, because we know that all mime types are in format that can be used
    // as HTTP header value.
    header.insert(ACCEPT_ENCODING, HeaderValue::from_static("gzip"));
    header.insert(
        ACCEPT,
        HeaderValue::try_from(format!(
            "{};api-version={}",
            mime::APPLICATION_OCTET_STREAM,
            API_VERSION
        ))
        .unwrap(),
    );
    header
}
