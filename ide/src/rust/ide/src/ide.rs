//! This module contains the IDE object implementation.

use crate::prelude::*;

use crate::transport::web::ConnectingError;
use crate::transport::web::WebSocket;
use crate::config;

use enso_protocol::binary;
use enso_protocol::language_server;
use enso_protocol::project_manager;
use enso_protocol::project_manager::ProjectMetadata;
use enso_protocol::project_manager::ProjectName;
use uuid::Uuid;
use crate::view::View;


// ==============
// === Errors ===
// ==============

/// Error raised when project with given name was not found.
#[derive(Clone,Debug,Fail)]
#[fail(display="Project with nae {} was not found.", name)]
pub struct ProjectNotFound {
    name : String
}



// ===========
// === Ide ===
// ===========

/// The IDE structure containing its configuration and its components instances.
#[derive(Debug)]
pub struct Ide {
    view : View
}



// ======================
// === IdeInitializer ===
// ======================

/// The IDE initializer.
#[derive(Debug)]
pub struct IdeInitializer {
    logger : Logger
}

impl Default for IdeInitializer {
    fn default() -> Self {
        let logger = Logger::new("IdeInitializer");
        Self {logger}
    }
}

impl IdeInitializer {
    /// Creates a new IDE instance.
    pub fn new() -> Self {
        default()
    }

    /// Establishes transport to the file manager server websocket endpoint.
    pub async fn connect_to_project_manager(&self, config:&config::Startup) -> Result<WebSocket,ConnectingError> {
        WebSocket::new_opened(self.logger.clone_ref(),&config.project_manager_endpoint).await
    }

    /// Wraps the transport to the project manager server into the client type and registers it
    /// within the global executor.
    pub fn setup_project_manager
    (transport:impl json_rpc::Transport + 'static) -> project_manager::Client {
        let project_manager = project_manager::Client::new(transport);
        executor::global::spawn(project_manager.runner());
        project_manager
    }

    /// Connect to language server.
    pub async fn open_project
    ( logger           : &Logger
    , project_manager  : Rc<dyn project_manager::API>
    , project_metadata : ProjectMetadata
    ) -> FallibleResult<model::Project> {
        let endpoints       = project_manager.open_project(&project_metadata.id).await?;
        let json_endpoint   = endpoints.language_server_json_address;
        let binary_endpoint = endpoints.language_server_binary_address;
        info!(logger, "Establishing Language Server connection.");
        let client_id     = Uuid::new_v4();
        let json_ws       = new_opened_ws(logger.clone_ref(), json_endpoint).await?;
        let binary_ws     = new_opened_ws(logger.clone_ref(), binary_endpoint).await?;
        let client_json   = language_server::Client::new(json_ws);
        let client_binary = binary::Client::new(logger,binary_ws);
        crate::executor::global::spawn(client_json.runner());
        crate::executor::global::spawn(client_binary.runner());
        let connection_json   = language_server::Connection::new(client_json,client_id).await?;
        let connection_binary = binary::Connection::new(client_binary,client_id).await?;
        let project_id        = project_metadata.id;
        let ProjectName(name) = project_metadata.name;
        let project           = model::project::Synchronized::from_connections(logger,
            project_manager,connection_json,connection_binary,project_id,name).await?;
        Ok(Rc::new(project))
    }

    /// Creates a new project and returns its metadata, so the newly connected project can be
    /// opened.
    pub async fn create_project
    ( logger          : &Logger
    , project_manager : &impl project_manager::API
    , name            : &str
    ) -> FallibleResult<ProjectMetadata> {
        info!(logger, "Creating a new project named '{name}'.");
        let id          = project_manager.create_project(&name.to_string()).await?.project_id;
        let name        = name.to_string();
        let name        = ProjectName::new(name);
        let last_opened = default();
        Ok(ProjectMetadata{id,name,last_opened})
    }

    async fn lookup_project
    ( project_manager : &impl project_manager::API
    , project_name    : &str) -> FallibleResult<ProjectMetadata> {
        let name         = project_name.to_string();
        let project_name = ProjectName::new(&name);
        let response     = project_manager.list_projects(&None).await?;
        let mut projects = response.projects.iter();
        projects.find(|project_metadata| {
            project_metadata.name == project_name
        }).cloned().ok_or_else(|| ProjectNotFound{name}.into())
    }

    /// Returns project with `project_name` or returns a newly created one if it doesn't exist.
    pub async fn get_project_or_create_new
    ( logger          : &Logger
    , project_manager : &impl project_manager::API
    , project_name    : &str) -> FallibleResult<ProjectMetadata> {
        let project = Self::lookup_project(project_manager,project_name).await;
        let project_metadata = if let Ok(project) = project {
            project
        } else {
            info!(logger, "Attempting to create {project_name}");
            Self::create_project(logger,project_manager,project_name).await?
        };
        Ok(project_metadata)
    }

    /// Returns the most recent opened project or returns a newly created one if the user doesn't
    /// have any project.
    pub async fn get_most_recent_project_or_create_new
    ( logger          : &Logger
    , project_manager : &impl project_manager::API
    , project_name    : &str) -> FallibleResult<ProjectMetadata> {
        let projects_to_list = Some(1);
        let mut response     = project_manager.list_projects(&projects_to_list).await?;
        let project_metadata = if let Some(project) = response.projects.pop() {
            project
        } else {
            Self::create_project(logger,project_manager,project_name).await?
        };
        Ok(project_metadata)
    }

    async fn initialize_project_manager
    (&mut self, config:&config::Startup) -> FallibleResult<project_manager::Client> {
        let transport        = self.connect_to_project_manager(config).await?;
        Ok(Self::setup_project_manager(transport))
    }

    /// Initialize the project view, including the controller it uses.
    pub async fn initialize_project_view
    ( &self
    , config          : &config::Startup
    , project_manager : project_manager::Client
    ) -> FallibleResult<View> {
        let logger           = &self.logger;
        let project_name     = config.project_name.to_string();
        let project_metadata = Self::get_project_or_create_new
            (logger,&project_manager,&project_name).await?;
        let project_manager = Rc::new(project_manager);
        let project         = Self::open_project(logger,project_manager,project_metadata).await?;
        Ok(View::new(logger,project).await?)
    }

    /// This function initializes the project manager, creates the project view and forget IDE
    /// to indefinitely keep it alive.
    pub fn start_and_forget(mut self) {
        let executor = setup_global_executor();
        let config   = config::Startup::new_local();
        info!(self.logger, "Starting IDE with the following config: {config:?}");
        executor::global::spawn(async move {
            // TODO [mwu] Once IDE gets some well-defined mechanism of reporting
            //      issues to user, such information should be properly passed
            //      in case of setup failure.
            let project_manager = self.initialize_project_manager(&config).await;
            let project_manager = project_manager.expect("Failed to initialize Project Manager.");
            let view            = self.initialize_project_view(&config,project_manager).await;
            let view            = view.expect("Failed to setup initial project view.");
            info!(self.logger,"Setup done.");
            let ide = Ide{view};
            std::mem::forget(ide);
            std::mem::forget(executor);
        });
    }
}



// =============
// === Utils ===
// =============

/// Creates a new running executor with its own event loop. Registers them as a global executor.
pub fn setup_global_executor() -> executor::web::EventLoopExecutor {
    let executor = executor::web::EventLoopExecutor::new_running();
    executor::global::set_spawner(executor.spawner.clone());
    executor
}

/// Creates a new websocket transport and waits until the connection is properly opened.
pub async fn new_opened_ws
(logger:Logger, address:project_manager::IpWithSocket) -> Result<WebSocket,ConnectingError> {
    let endpoint = format!("ws://{}:{}", address.host, address.port);
    WebSocket::new_opened(logger,endpoint).await
}
