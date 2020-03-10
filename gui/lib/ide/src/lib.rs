//! Main library crate for IDE. It includes implementation of
//! controllers, view logic and code that wraps them all together.

#![feature(bool_to_option)]
#![feature(drain_filter)]
#![feature(trait_alias)]
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
pub mod transport;
pub mod view;

/// Common types that should be visible across the whole IDE crate.
pub mod prelude {
    pub use basegl::prelude::*;
    pub use enso_prelude::*;
    pub use wasm_bindgen::prelude::*;

    pub use crate::constants;
    pub use crate::controller;
    pub use crate::double_representation;
    pub use crate::executor;

    pub use futures::Future;
    pub use futures::FutureExt;
    pub use futures::Stream;
    pub use futures::StreamExt;
    pub use futures::task::LocalSpawnExt;
}

use crate::prelude::*;

use crate::transport::web::ConnectingError;
use crate::transport::web::WebSocket;
use crate::view::project::ProjectView;



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

/// Endpoint used by default by a locally run mock file manager server.
const MOCK_FILE_MANAGER_ENDPOINT:&str = "ws://127.0.0.1:30616";

/// Configuration data necessary to initialize IDE.
///
/// Eventually we expect it to be passed to IDE from an external source.
#[derive(Clone,Debug)]
pub struct SetupConfig {
    /// WebSocket endpoint of the file manager service.
    pub file_manager_endpoint:String
}

impl SetupConfig {
    /// Provisional initial configuration that can be used during mock
    /// deployments (manually run mock file manager server).
    pub fn new_mock() -> SetupConfig {
        SetupConfig {
            file_manager_endpoint: MOCK_FILE_MANAGER_ENDPOINT.into()
        }
    }
}



// ==================
// === IDE Setup ===
// ==================

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
pub async fn connect_to_file_manager(config:SetupConfig) -> Result<WebSocket,ConnectingError> {
    WebSocket::new_opened(config.file_manager_endpoint).await
}

/// Sets up the project view, including the controller it uses.
pub async fn setup_project_view(logger:&Logger,config:SetupConfig)
-> Result<ProjectView,failure::Error> {
    let fm_transport = connect_to_file_manager(config).await?;
    let controller   = controller::project::Handle::new_running(fm_transport);
    let project_view = ProjectView::new(logger,controller).await?;
    Ok(project_view)
}

/// This function is the IDE entry point responsible for setting up all views and controllers.
pub fn run_ide() {
    let logger          = Logger::new("IDE");
    let global_executor = setup_global_executor();
    // We want global executor to live indefinitely.
    std::mem::forget(global_executor);

    let config = SetupConfig::new_mock();
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
