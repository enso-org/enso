//! Global constants used across whole application.

/// A name of language this IDE supports
pub const LANGUAGE_NAME: &str = "Enso";

/// A file extension of modules of language this IDE supports without leading dot.
pub const LANGUAGE_FILE_EXTENSION: &str = "enso";

/// The directory in the project that contains all the source files.
pub const SOURCE_DIRECTORY: &str = "src";

/// Endpoint used by default by a locally run Project Manager.
pub const PROJECT_MANAGER_ENDPOINT: &str = "ws://127.0.0.1:30535";

/// Default project name used by IDE on startup.
pub const DEFAULT_PROJECT_NAME: &str = "Unnamed";

/// The default namespace used when opening a project.
pub const DEFAULT_PROJECT_NAMESPACE: &str = "local";

/// The name of the main module in each project. The main module is explicitly imported when the
/// import statement has the project name only.
pub const PROJECTS_MAIN_MODULE: &str = "Main";

/// Visualization folder where IDE can look for user-defined visualizations per project.
pub const VISUALIZATION_DIRECTORY: &str = "visualization";

/// How many times IDE will try attaching visualization when there is a timeout error.
///
/// Timeout error suggests that there might be nothing wrong with the request, just that the backend
/// is currently too busy to reply or that there is some connectivity hiccup. Thus, it makes sense
/// to give it a few more tries.
pub const ATTACHING_TIMEOUT_RETRIES: usize = 50;

/// A module with language-specific constants.
pub mod keywords {
    /// A keyword indicating current module.
    pub const HERE: &str = "here";

    /// The "void" atom returned by function meant to not return any argument.
    pub const NOTHING: &str = "Nothing";
}
