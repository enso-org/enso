//! Client library for the Language Server part of the Enso Protocol.
//!
//! Please refer to https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md---language-server
//! for the full protocol documentation and discussion on the types and terms used here.
//!
//! Also, the Enso Protocol specification is source for many names and comments used here.
//! This file tries to follow the scheme of the protocol specification.


// ==============
// === Export ===
// ==============

pub mod connection;
pub mod constants;
pub mod response;



#[cfg(test)]
mod tests;
pub mod types;

pub use connection::Connection;
pub use types::*;

use crate::prelude::*;

use crate::types::Sha3_224;
use crate::types::UTCDateTime;

use json_rpc::api::Result;
use json_rpc::make_rpc_methods;
use json_rpc::Handler;
use serde::Deserialize;
use serde::Serialize;
use std::future::Future;
use uuid::Uuid;



// ====================
// === API & Client ===
// ====================

make_rpc_methods! {
/// An interface containing all the available file management operations.
trait API {
    /// Initialize the connection used to send the textual protocol messages. This initialisation
    /// is important such that the client identifier can be correlated between the textual and data
    /// connections.
    #[MethodInput=InitProtocolInput, rpc_name="session/initProtocolConnection"]
    fn init_protocol_connection(&self, client_id: Uuid) -> response::InitProtocolConnection;

    /// Copy a specified file system object to another location.
    #[MethodInput=CopyFileInput, rpc_name="file/copy"]
    fn copy_file(&self, from: Path, to: Path) -> ();

    /// Delete the specified file system object.
    #[MethodInput=DeleteFileInput, rpc_name="file/delete"]
    fn delete_file(&self, path: Path) -> ();

    /// Check if file system object exists.
    #[MethodInput=FileExistsInput, rpc_name="file/exists"]
    fn file_exists(&self, path: Path) -> response::FileExists;

    /// List all file-system objects in the specified path.
    #[MethodInput=FileListInput, rpc_name="file/list"]
    fn file_list(&self, path: Path) -> response::FileList;

    /// Move file system object to another location.
    #[MethodInput=MoveFileInput, rpc_name="file/move"]
    fn move_file(&self, from: Path, to: Path) -> ();

    /// Reads file's content as a String.
    #[MethodInput=ReadFileInput, rpc_name="file/read"]
    fn read_file(&self, path: Path) -> response::Read;

    /// Gets file system object's attributes information.
    #[MethodInput=FileInfoInput, rpc_name="file/info"]
    fn file_info(&self, path: Path) -> response::FileInfo;

    /// Requests that the language server provide the checksum of the provided file.
    #[MethodInput=FileChecksumInput, rpc_name="file/checksum"]
    fn file_checksum(&self, path: Path) -> response::FileChecksum;

    /// Creates the specified file system object.
    #[MethodInput=CreateInput, rpc_name="file/create"]
    fn create_file(&self, object: FileSystemObject) -> ();

    /// Writes String contents to a file in the specified path.
    #[MethodInput=FileWriteInput, rpc_name="file/write"]
    fn write_file(&self, path: Path, contents: String) -> ();

    /// Acquire capability permission.
    #[MethodInput=AcquireCapabilityInput, rpc_name="capability/acquire"]
    fn acquire_capability(&self, method: String, register_options: RegisterOptions) -> ();

    /// Open the specified file. If no user has write lock on the opened file, the write lock
    /// capability is granted to the caller.
    #[MethodInput=OpenTextFileInput, rpc_name="text/openFile"]
    fn open_text_file(&self, path: Path) -> response::OpenTextFile;

    /// Informs the language server that a client has closed the specified file.
    #[MethodInput=CloseTextFileInput, rpc_name="text/closeFile"]
    fn close_text_file(&self, path: Path) -> ();

    /// Save the specified file. It may fail if the user does not have permission to edit that file.
    #[MethodInput=SaveTextFileInput, rpc_name="text/save"]
    fn save_text_file(&self, path: Path, current_version:Sha3_224) -> ();

    /// Apply edits to the specified text file. This operation may fail if the user does not
    /// have permission to edit the resources for which edits are sent. This failure may be partial,
    /// in that some edits are applied and others are not.
    #[MethodInput=ApplyTextFileEditInput, rpc_name="text/applyEdit"]
    fn apply_text_file_edit(&self, edit: FileEdit) -> ();

    /// Create a new execution context. Return capabilities executionContext/canModify and
    /// executionContext/receivesUpdates containing freshly created ContextId
    #[MethodInput=CreateExecutionContextInput, rpc_name="executionContext/create"]
    fn create_execution_context(&self) -> response::CreateExecutionContext;

    /// Destroy an execution context and free its resources.
    #[MethodInput=DestroyExecutionContextInput, rpc_name="executionContext/destroy"]
    fn destroy_execution_context(&self, context_id: ContextId) -> ();

    /// Move the execution context to a new location deeper down the stack.
    #[MethodInput=PushToExecutionContextInput, rpc_name="executionContext/push"]
    fn push_to_execution_context(&self, context_id: ContextId, stack_item: StackItem) -> ();

    /// Move the execution context up the stack.
    #[MethodInput=PopFromExecutionContextInput, rpc_name="executionContext/pop"]
    fn pop_from_execution_context(&self, context_id: ContextId) -> ();

    /// Attach a visualisation, potentially preprocessed by some arbitrary Enso code, to a given
    /// node in the program.
    #[MethodInput=AttachVisualisationInput, rpc_name="executionContext/attachVisualisation"]
    fn attach_visualisation
    ( &self
    , visualisation_id     : Uuid
    , expression_id        : Uuid
    , visualisation_config : VisualisationConfiguration) -> ();

    /// Detach a visualisation from the executing code.
    #[MethodInput=DetachVisualisationInput, rpc_name="executionContext/detachVisualisation"]
    fn detach_visualisation
    (&self, context_id: Uuid, visualisation_id: Uuid, expression_id: Uuid) -> ();

    /// Modify the configuration for an existing visualisation.
    #[MethodInput=ModifyVisualisationInput, rpc_name="executionContext/modifyVisualisation"]
    fn modify_visualisation
    (&self, visualisation_id: Uuid, visualisation_config: VisualisationConfiguration) -> ();

    /// Interrupt the program execution.
    #[MethodInput=InterruptInput, rpc_name="executionContext/interrupt"]
    fn interrupt(&self, context_id: ContextId) -> ();

    /// Restart the program execution.
    #[MethodInput=RecomputeInput, rpc_name="executionContext/recompute"]
    fn recompute(&self, context_id: ContextId) -> ();

    /// Obtain the full suggestions database.
    #[MethodInput=GetSuggestionsDatabaseInput, rpc_name="search/getSuggestionsDatabase"]
    fn get_suggestions_database(&self) -> response::GetSuggestionDatabase;

    /// Receive the current version of the suggestions database.
    #[MethodInput=GetSuggestionsDatabaseVersionInput,
        rpc_name="search/getSuggestionsDatabaseVersion"]
    fn get_suggestions_database_version(&self) -> response::GetSuggestionDatabaseVersion;

    /// Receive the autocomplete suggestion.
    #[MethodInput=CompletionInput,rpc_name="search/completion"]
    fn completion
    ( &self
    , file        : Path
    , position    : Position
    , self_type   : Option<String>
    , return_type : Option<String>
    , tags        : Option<Vec<SuggestionEntryType>>
    ) -> response::Completion;

    /// Get the list of component groups available in runtime.
    #[MethodInput=GetComponentGroups, rpc_name="executionContext/getComponentGroups"]
    fn get_component_groups(&self, context_id: ContextId) -> response::GetComponentGroups;

    /// Initialize VCS at the specified root.
    #[MethodInput=VcsInitInput, rpc_name="vcs/init"]
    fn init_vcs(&self, root: Path) -> ();

    /// Save project to VCS at the specified root.
    #[MethodInput=VcsWriteInput, rpc_name="vcs/save"]
    fn save_vcs(&self, root: Path, name: Option<String>) -> response::SaveVcs;

    /// Return a list of all project states that are saved to the VCS.
    #[MethodInput=VcsListInput, rpc_name="vcs/list"]
    fn list_vcs(&self, root: Path, limit: Option<usize>) -> response::ListVcs;

    /// Returns the current status of the changes made to the project.
    #[MethodInput=VcsStatusInput, rpc_name="vcs/status"]
    fn vcs_status(&self, root: Path) -> response::VcsStatus;
}}



// ==============
// === Errors ===
// ==============

/// Check if the given `Error` value corresponds to an RPC call timeout.
///
/// Recognizes both client- and server-side timeouts.
#[rustfmt::skip]
pub fn is_timeout_error(error: &failure::Error) -> bool {
    use json_rpc::messages;
    use json_rpc::RpcError;
    use json_rpc::RpcError::*;
    const TIMEOUT: i64 = constants::ErrorCodes::Timeout as i64;
    matches!(error.downcast_ref::<RpcError>()
      , Some(TimeoutError{..})
      | Some(RemoteError(messages::Error{code:TIMEOUT,..})))
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn recognize_timeout_errors() {
        type RpcError = json_rpc::RpcError<serde_json::Value>;

        // Server-side errors.
        let text = r#"{"code":11,"message":"Request timeout"}"#;
        let msg = serde_json::from_str::<json_rpc::messages::Error>(text).unwrap();
        let error = RpcError::RemoteError(msg).into();
        assert!(is_timeout_error(&error));

        let text = r#"{"code":2007,"message":"Evaluation of the visualisation expression failed"}"#;
        let msg = serde_json::from_str::<json_rpc::messages::Error>(text).unwrap();
        let error = RpcError::RemoteError(msg).into();
        assert!(!is_timeout_error(&error));


        // Client-side errors.
        let error = RpcError::TimeoutError { millis: 500 }.into();
        assert!(is_timeout_error(&error));

        let error = RpcError::LostConnection.into();
        assert!(!is_timeout_error(&error));
    }
}
