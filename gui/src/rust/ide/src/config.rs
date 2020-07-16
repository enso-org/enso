//! This module provides IDE configuration structures.

use crate::constants;

use enso_protocol::project_manager::ProjectName;

/// Configuration data necessary to initialize IDE.
///
/// We will eventually want to load it from a configuration file.
#[derive(Clone,Debug)]
pub struct Startup {
    /// WebSocket endpoint of the project manager service.
    pub project_manager_endpoint : String,
    /// The project name we want to open on startup passed from the optional `--project` argument
    pub project_name : ProjectName
}

impl Startup {
    /// Provisional initial configuration that can be used during local deployments.
    pub fn new_local() -> Startup {
        let arguments = ensogl::system::web::Arguments::new();
        let project_manager_endpoint   = constants::PROJECT_MANAGER_ENDPOINT.into();
        let project_name = arguments.get("project").map(ProjectName::new);
        let project_name = project_name.unwrap_or_else(|| {
            ProjectName::new(constants::DEFAULT_PROJECT_NAME)
        });
        Startup{project_manager_endpoint,project_name}
    }
}
