//! Helper structures wrapping RPC method result types.
use super::*;

/// Response of `init_protocol_connection` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitProtocolConnection {
    /// List of Root IDs.
    pub content_roots:Vec<Uuid>,
}

/// Response of `file_read` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct Read {
    #[allow(missing_docs)]
    pub contents:String,
}

/// Response of `file_exists` method.
#[derive(Hash,Debug,Clone,Copy,PartialEq,Eq,Serialize,Deserialize)]
pub struct FileExists {
    #[allow(missing_docs)]
    pub exists:bool,
}

/// Response of `file_lst` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct FileList {
    #[allow(missing_docs)]
    pub paths:Vec<FileSystemObject>,
}

/// Response of `file_info` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
pub struct FileInfo {
    #[allow(missing_docs)]
    pub attributes: FileAttributes,
}

/// Response of `open_text_file` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct OpenTextFile {
    pub write_capability : Option<CapabilityRegistration>,
    pub content          : String,
    pub current_version  : Sha3_224
}

/// Response of `create_execution_context` method.
#[derive(Hash,Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct CreateExecutionContext {
    pub context_id       : ContextId,
    pub can_modify       : CapabilityRegistration,
    pub receives_updates : CapabilityRegistration
}
