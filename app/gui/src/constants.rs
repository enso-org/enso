//! Global constants used across whole application.

pub use ast::constants;

/// Endpoint used by default by a locally run Project Manager.
pub const PROJECT_MANAGER_ENDPOINT: &str = "ws://127.0.0.1:30535";

/// Default project name used by IDE on startup.
pub const DEFAULT_PROJECT_NAME: &str = "Unnamed";

/// The default namespace used when opening a project.
pub const DEFAULT_PROJECT_NAMESPACE: &str = "local";

/// Visualization folder where IDE can look for user-defined visualizations per project.
pub const VISUALIZATION_DIRECTORY: &str = "visualization";

/// How many times IDE will try attaching visualization when there is a timeout error.
///
/// Timeout error suggests that there might be nothing wrong with the request, just that the backend
/// is currently too busy to reply or that there is some connectivity hiccup. Thus, it makes sense
/// to give it a few more tries.
pub const ATTACHING_TIMEOUT_RETRIES: usize = 50;
