//! Main library crate for IDE. It includes implementation of
//! controllers, view logic and code that wraps them all together.
//!
#![feature(associated_type_bounds)]
#![feature(bool_to_option)]
#![feature(cell_update)]
#![feature(drain_filter)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![recursion_limit="256"]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

pub mod controller;
pub mod double_representation;
pub mod executor;
pub mod model;
pub mod notification;
pub mod transport;
pub mod view;

/// Common types that should be visible across the whole IDE crate.
pub mod prelude {
    pub use ensogl::prelude::*;
    pub use enso_prelude::*;
    pub use ast::prelude::*;
    pub use wasm_bindgen::prelude::*;

    pub use crate::constants;
    pub use crate::controller;
    pub use crate::double_representation;
    pub use crate::executor;
    pub use crate::model;

    pub use futures::Future;
    pub use futures::FutureExt;
    pub use futures::Stream;
    pub use futures::StreamExt;
    pub use futures::task::LocalSpawnExt;

    pub use std::ops::Range;

    pub use utils::fail::FallibleResult;
    pub use utils::option::OptionExt;
    pub use utils::vec::VecExt;

    #[cfg(test)] pub use wasm_bindgen_test::wasm_bindgen_test;
    #[cfg(test)] pub use wasm_bindgen_test::wasm_bindgen_test_configure;
}

use crate::prelude::*;

use crate::transport::web::ConnectingError;
use crate::transport::web::WebSocket;
use crate::view::project::ProjectView;

use enso_protocol::language_server;
use enso_protocol::project_manager;



// =================
// === Constants ===
// =================

/// Global constants used across whole application.
pub mod constants {
    /// A name of language this IDE supports
    pub const LANGUAGE_NAME           : &str = "Enso";

    /// A file extension of modules of language this IDE supports
    pub const LANGUAGE_FILE_EXTENSION : &str = "enso";
}



// ===================
// === SetupConfig ===
// ===================

/// Endpoint used by default by a locally run Project Manager.
const PROJECT_MANAGER_ENDPOINT:&str = "ws://127.0.0.1:30535";

/// Configuration data necessary to initialize IDE.
///
/// Eventually we expect it to be passed to IDE from an external source.
#[derive(Clone,Debug)]
pub struct SetupConfig {
    /// WebSocket endpoint of the project manager service.
    pub project_manager_endpoint:String
}

impl SetupConfig {
    /// Provisional initial configuration that can be used during local deployments.
    pub fn new_local() -> SetupConfig {
        SetupConfig {
            project_manager_endpoint:PROJECT_MANAGER_ENDPOINT.into()
        }
    }
}



// =================
// === IDE Setup ===
// =================

const DEFAULT_PROJECT_NAME:&str = "Project";

/// Creates a new running executor with its own event loop. Registers them
/// as a global executor.
///
/// Note: Caller should store or leak this `JsExecutor` so the global
/// spawner won't be dangling.
pub fn setup_global_executor() -> executor::web::EventLoopExecutor {
    let executor   = executor::web::EventLoopExecutor::new_running();
    executor::global::set_spawner(executor.spawner.clone());
    executor
}

/// Establishes connection with file manager server websocket endpoint.
pub async fn connect_to_project_manager
(config:SetupConfig) -> Result<project_manager::Client,ConnectingError> {
    let transport       = WebSocket::new_opened(config.project_manager_endpoint).await?;
    let project_manager = project_manager::Client::new(transport);
    executor::global::spawn(project_manager.runner());
    Ok(project_manager)
}

/// Connect to language server.
pub async fn open_project
(address:project_manager::IpWithSocket) -> FallibleResult<controller::Project> {
    let endpoint   = format!("ws://{}:{}",address.host,address.port);
    let transport  = WebSocket::new_opened(endpoint).await?;
    let client     = language_server::Client::new(transport);
    crate::executor::global::spawn(client.runner());
    let connection = language_server::Connection::new(client).await?;
    Ok(controller::Project::new(connection))
}

/// Open most recent project or create a new project if none exists.
pub async fn open_most_recent_project_or_create_new
(project_manager:&impl project_manager::API) -> FallibleResult<controller::Project> {
    let mut response = project_manager.list_recent_projects(1).await?;
    let project_id = if let Some(project) = response.projects.pop() {
        project.id
    } else {
        project_manager.create_project(DEFAULT_PROJECT_NAME.into()).await?.project_id
    };
    let address = project_manager.open_project(project_id).await?.language_server_rpc_address;
    open_project(address).await
}

/// Sets up the project view, including the controller it uses.
pub async fn setup_project_view(logger:&Logger,config:SetupConfig)
-> Result<ProjectView,failure::Error> {
    let pm           = connect_to_project_manager(config).await?;
    let project      = open_most_recent_project_or_create_new(&pm).await?;
    let project_view = ProjectView::new(logger,project).await?;
    Ok(project_view)
}

/// This function is the IDE entry point responsible for setting up all views and controllers.
pub fn run_ide() {
    let logger          = Logger::new("IDE");
    let global_executor = setup_global_executor();
    // We want global executor to live indefinitely.
    std::mem::forget(global_executor);

    let config = SetupConfig::new_local();
    executor::global::spawn(async move {
        let error_msg = "Failed to setup initial project view.";
        // TODO [mwu] Once IDE gets some well-defined mechanism of reporting
        //      issues to user, such information should be properly passed
        //      in case of setup failure.
        let project_view = setup_project_view(&logger,config).await.expect(error_msg);
        logger.info("Setup done.");
        project_view.forget();
    });
}
