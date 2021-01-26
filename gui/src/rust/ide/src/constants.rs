//! Global constants used across whole application.

/// A name of language this IDE supports
pub const LANGUAGE_NAME:&str = "Enso";

/// A file extension of modules of language this IDE supports without leading dot.
pub const LANGUAGE_FILE_EXTENSION:&str = "enso";

/// The directory in the project that contains all the source files.
pub const SOURCE_DIRECTORY:&str = "src";

/// An invocable language expression that serialize given input into JSON.
pub const SERIALIZE_TO_JSON_EXPRESSION:&str = "x -> x.to_json.to_text";

/// Endpoint used by default by a locally run Project Manager.
pub const PROJECT_MANAGER_ENDPOINT:&str = "ws://127.0.0.1:30535";

/// Default project name used by IDE on startup.
pub const DEFAULT_PROJECT_NAME:&str = "Unnamed";

/// Visualization folder where IDE can look for user-defined visualizations per project.
pub const VISUALIZATION_DIRECTORY:&str = "visualization";

/// A module with language-specific constants.
pub mod keywords {
    /// A keyword indicating current module.
    pub const HERE:&str = "here";

    /// The "void" atom returned by function meant to not return any argument.
    pub const NOTHING:&str = "Nothing";
}
