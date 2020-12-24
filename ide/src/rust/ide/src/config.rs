//! This module provides IDE configuration structures.
use crate::prelude::*;

use crate::constants;

use enso_protocol::project_manager::ProjectName;
use ensogl::system::web;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Missing program option: {}.",0)]
pub struct MissingOption (&'static str);

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="Provided options for both project manager and language server connection.")]
pub struct MutuallyExclusiveOptions;



// ======================
// === BackendService ===
// ======================

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
    pub fn from_web_arguments(config:&ConfigReader) -> FallibleResult<Self> {
        if let Some(endpoint) = &config.project_manager {
            if config.language_server_rpc.is_some() || config.language_server_data.is_some() {
                Err(MutuallyExclusiveOptions.into())
            } else {
                let endpoint = endpoint.clone();
                Ok(Self::ProjectManager {endpoint})
            }
        } else {
            match (&config.language_server_rpc,&config.language_server_data) {
                (Some(json_endpoint),Some(binary_endpoint)) => {
                    let json_endpoint   = json_endpoint.clone();
                    let binary_endpoint = binary_endpoint.clone();
                    Ok(Self::LanguageServer {json_endpoint,binary_endpoint})
                }
                (None,None)    => Ok(default()),
                (Some(_),None) => Err(MissingOption(config.names().language_server_data).into()),
                (None,Some(_)) => Err(MissingOption(config.names().language_server_rpc).into())
            }
        }
    }
}



// ==============
// === Config ===
// ==============

/// The path at which the config is accessible. This needs to be synchronised with the
/// `src/config.yaml` configuration file. In the future, we could write a procedural macro, which
/// loads the configuration and splits Rust variables from it during compilation time. This is not
/// possible by using macro rules, as there is no way to plug in the output of `include_str!` macro
/// to another macro input.
const WINDOW_CFG_PATH : &[&str] = &["enso","config"];

/// Defines a new config structure. The provided fields are converted to optional fields. The config
/// constructor queries JavaScript configuration for the keys defined in this structure. For each
/// resulting string value, it converts it to the defined type. It also reports warnings for all
/// config options that were provided, but were not matched this definition.
macro_rules! define_config {
    ($name:ident { $($field:ident : $field_type:ty),* $(,)? }) => {

        /// Reflection mechanism containing string representation of option names.
        #[derive(Clone,Copy,Debug)]
        pub struct Names {
            $($field : &'static str),*
        }

        impl Default for Names {
            fn default() -> Self {
                $(let $field = stringify!{$field};)*
                Self {$($field),*}
            }
        }

        /// The structure containing application configs.
        #[derive(Clone,Debug,Default)]
        pub struct $name {
            __names__ : Names,
            $($field : Option<$field_type>),*
        }

        impl $name {
            /// Constructor.
            pub fn new() -> Self {
                let logger = Logger::new(stringify!{$name});
                let window = web::window();
                match web::reflect_get_nested_object(&window,WINDOW_CFG_PATH).ok() {
                    None => {
                        let path = WINDOW_CFG_PATH.join(".");
                        error!(&logger,"The config path '{path}' is invalid.");
                        default()
                    }
                    Some(cfg) => {
                        let __names__ = default();
                        let keys      = web::object_keys(&cfg);
                        let mut keys  = keys.into_iter().collect::<HashSet<String>>();
                        $(
                            let name   = stringify!{$field};
                            let $field = web::reflect_get_nested_string(&cfg,&[name]).ok();
                            let $field = $field.map(|t|t.into());
                            keys.remove(name);
                        )*
                        for key in keys {
                            warning!(&logger,"Unknown config option provided '{key}'.");
                        }
                        Self {__names__,$($field),*}
                    }
                }
            }

            /// Reflection mechanism to get string representation of option names.
            pub fn names(&self) -> &Names {
                &self.__names__
            }
        }
    };
}

define_config! {
    ConfigReader {
        entry                : String,
        project              : ProjectName,
        project_manager      : String,
        language_server_rpc  : String,
        language_server_data : String,
    }
}



// ===============
// === Startup ===
// ===============

/// Configuration data necessary to initialize IDE.
#[derive(Clone,Debug)]
pub struct Startup {
    /// The configuration of connection to the backend service.
    pub backend : BackendService,
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
        let config       = ConfigReader::new();
        let backend      = BackendService::from_web_arguments(&config)?;
        let project_name = config.project.unwrap_or_else(||
            ProjectName::new(constants::DEFAULT_PROJECT_NAME)
        );
        Ok(Startup{backend,project_name})
    }
}
