//! Helper structures wrapping RPC method result types.
use super::*;
use crate::language_server::SuggestionsDatabaseEntry;

/// Response of `init_protocol_connection` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitProtocolConnection {
    /// List of Root IDs.
    pub content_roots: Vec<ContentRoot>,
}

/// Response of `file_read` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Read {
    #[allow(missing_docs)]
    pub contents: String,
}

/// Response of `file_exists` method.
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileExists {
    #[allow(missing_docs)]
    pub exists: bool,
}

/// Response of `file_lst` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileList {
    #[allow(missing_docs)]
    pub paths: Vec<FileSystemObject>,
}

/// Response of `file_info` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileInfo {
    #[allow(missing_docs)]
    pub attributes: FileAttributes,
}

/// Response of `file_checksum` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileChecksum {
    #[allow(missing_docs)]
    pub checksum: Sha3_224,
}

/// Response of `open_text_file` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct OpenTextFile {
    pub write_capability: Option<CapabilityRegistration>,
    pub content:          String,
    pub current_version:  Sha3_224,
}

/// Response of `create_execution_context` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct CreateExecutionContext {
    pub context_id:       ContextId,
    pub can_modify:       CapabilityRegistration,
    pub receives_updates: CapabilityRegistration,
}

/// Response of `get_suggestions_database` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct GetSuggestionDatabase {
    pub entries:         Vec<SuggestionsDatabaseEntry>,
    pub current_version: SuggestionsDatabaseVersion,
}

/// Response of `get_suggestions_database_version` method.
#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct GetSuggestionDatabaseVersion {
    pub current_version: SuggestionsDatabaseVersion,
}

/// Response of `completion` method.
#[derive(Hash, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct Completion {
    pub results:         Vec<SuggestionId>,
    pub current_version: SuggestionsDatabaseVersion,
}
