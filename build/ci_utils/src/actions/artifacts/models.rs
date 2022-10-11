use crate::prelude::*;

use chrono::DateTime;
use chrono::Utc;



#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")] // Sic!
pub struct CreateArtifactRequest {
    r#type:         String,
    name:           String,
    // GH Actions server does not support deserializing optional fields that are described as
    // `null`.
    #[serde(skip_serializing_if = "Option::is_none")]
    retention_days: Option<u32>,
}

impl CreateArtifactRequest {
    pub fn new(name: impl Into<String>, retention_days: Option<u32>) -> Self {
        CreateArtifactRequest {
            r#type: "actions_storage".to_string(),
            name: name.into(),
            retention_days,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")] // Sic!
pub struct CreateArtifactResponse {
    pub container_id: u64,
    pub size: i64, // must be signed, as -1 is used as a placeholder
    pub signed_content: Option<String>,
    pub file_container_resource_url: Url,
    pub r#type: String,
    pub name: String,
    pub url: Url,
    pub expires_on: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")] // Sic!
pub struct UploadFileQuery {
    pub file:              String,
    pub resource_url:      Url,
    pub max_chunk_size:    i64,
    pub continue_on_error: bool,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")] // Sic!
pub struct PatchArtifactSize {
    pub size: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")] // Sic!
pub struct PatchArtifactSizeResponse {
    pub container_id:   u64,
    pub size:           i64,
    pub signed_content: Option<String>,
    pub r#type:         String,
    pub name:           String,
    pub url:            Url,
    // This is not actually present, despite what GH sources say.
    // pub upload_url:     Url,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ListArtifactsResponse {
    pub count: i64,
    pub value: Vec<ArtifactResponse>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ArtifactResponse {
    pub container_id: u64,
    pub size: i64,
    pub signed_content: Option<String>,
    pub file_container_resource_url: Url,
    pub r#type: String,
    pub name: String,
    pub url: Url,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QueryArtifactResponse {
    pub count: i64,
    pub value: Vec<ContainerEntry>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ContainerEntry {
    pub container_id:       u64,
    pub scope_identifier:   Uuid,
    pub path:               PathBuf,
    pub item_type:          ItemType,
    pub status:             EntryStatus,
    pub file_length:        Option<i64>,
    pub file_encoding:      Option<i64>,
    pub file_type:          Option<i64>,
    pub date_created:       DateTime<Utc>,
    pub date_last_modified: DateTime<Utc>,
    pub created_by:         Uuid,
    pub last_modified_by:   Uuid,
    pub item_location:      Url,
    pub content_location:   Url,
    pub file_id:            Option<usize>,
    pub content_id:         String,
}

impl ContainerEntry {
    pub fn relative_path(&self) -> PathBuf {
        //ensure!(self.path.is_relative(), "Path {} is not relative.", self.path.display());
        // First part is artifact name.
        let path_iter = self.path.iter().skip(1);
        // ensure!(
        //     path_iter.next() == Some(&OsStr::new(artifact_name)),
        //     "Entry path does not start with an artifact name."
        // );
        PathBuf::from_iter(path_iter)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum EntryStatus {
    Created,
    PendingUpload,
    // No other values known at this point.
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ItemType {
    File,
    Folder,
}
