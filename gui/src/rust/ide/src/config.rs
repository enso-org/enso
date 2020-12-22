//! This module provides IDE configuration structures.
use crate::prelude::*;

use crate::constants;

use enso_protocol::project_manager::ProjectName;
use ensogl::system::web;



// =================
// === Constants ===
// =================

mod connection_arguments {
    pub const PROJECT_MANAGER        : &str = "project_manager";
    pub const LANGUAGE_SERVER_JSON   : &str = "language_server_rpc";
    pub const LANGUAGE_SERVER_BINARY : &str = "language_server_data";
}



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Missing program option: {}.",name)]
pub struct MissingOption {name:&'static str}

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Provided options for both project manager and language server connection.")]
pub struct MutuallyExclusiveOptions;



// ==================
// === Connection ===
// ==================

/// A Configuration defining to what backend service should IDE connect.
#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub enum BackendService {
    /// Connect to the project manager. Using the project manager IDE will open or create a specific
    /// project and connect to its Language Server.
    ProjectManager {endpoint:String},
    /// Connect to the language server of some project. The project managing operations will be
    /// unavailable.
    LanguageServer {
        json_endpoint   : String,
        binary_endpoint : String,
    }
}

impl Default for BackendService {
    fn default() -> Self {
        Self::ProjectManager {endpoint:constants::PROJECT_MANAGER_ENDPOINT.into()}
    }
}

impl BackendService {
    /// Read backend configuration from the web arguments. See also [`web::Arguments`]
    /// documentation.
    pub fn from_web_arguments(arguments:&web::Arguments) -> FallibleResult<Self> {
        let pm_endpoint      = arguments.get(connection_arguments::PROJECT_MANAGER).cloned();
        let ls_json_endpoint = arguments.get(connection_arguments::LANGUAGE_SERVER_JSON).cloned();
        let ls_bin_endpoint  = arguments.get(connection_arguments::LANGUAGE_SERVER_BINARY).cloned();
        if let Some(endpoint) = pm_endpoint {
            if ls_json_endpoint.is_some() || ls_bin_endpoint.is_some() {
                Err(MutuallyExclusiveOptions.into())
            } else {
                Ok(Self::ProjectManager {endpoint})
            }
        } else {
            match (ls_json_endpoint,ls_bin_endpoint) {
                (Some(json_endpoint),Some(binary_endpoint)) =>
                    Ok(Self::LanguageServer {json_endpoint,binary_endpoint}),
                (None,None) =>
                    Ok(default()),
                (Some(_),None) =>
                    Err(MissingOption{name:connection_arguments::LANGUAGE_SERVER_BINARY}.into()),
                (None,Some(_)) =>
                    Err(MissingOption{name:connection_arguments::LANGUAGE_SERVER_JSON  }.into())
            }
        }
    }
}

/// Configuration data necessary to initialize IDE.
#[derive(Clone,Debug)]
pub struct Startup {
    /// The configuration of connection to the backend service.
    pub backend:BackendService,
    /// The project name we want to open on startup.
    pub project_name : ProjectName
}

impl Default for Startup {
    fn default() -> Self {
        Self {
            backend      : default(),
            project_name : ProjectName(constants::DEFAULT_PROJECT_NAME.to_owned())
        }
    }
}

impl Startup {
    /// Read configuration from the web arguments. See also [`web::Arguments`] documentation.
    pub fn from_web_arguments() -> FallibleResult<Startup> {
        let arguments    = ensogl::system::web::Arguments::new();
        let backend      = BackendService::from_web_arguments(&arguments)?;
        let project_name = arguments.get("project").map(ProjectName::new);
        let project_name = project_name.unwrap_or_else(|| {
            ProjectName::new(constants::DEFAULT_PROJECT_NAME)
        });
        Ok(Startup{backend,project_name})
    }
}
