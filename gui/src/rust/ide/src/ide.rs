//! This module contains the IDE object implementation.
pub mod initializer;
pub mod integration;

use crate::prelude::*;

use crate::controller::project::INITIAL_MODULE_NAME;
use crate::ide::integration::Integration;

use ensogl::application::Application;

pub use initializer::Initializer;



// =================
// === Constants ===
// =================

/// Text that shows up in the statusbar when any of the backend connections is lost.
pub const BACKEND_DISCONNECTED_MESSAGE:&str =
    "Connection to the backend has been lost. Please try restarting IDE.";



// ===========
// === Ide ===
// ===========

/// The main Ide structure.
///
/// This structure is a root of all objects in our application. It includes both layers:
/// Controllers and Views, and an integration between them.
#[derive(Debug)]
pub struct Ide {
    application : Application,
    integration : Integration,
}

impl Ide {
    /// Constructor.
    pub async fn new
    (application:Application, view:ide_view::project::View, controller:controller::Ide)
    -> Self {
        let integration = integration::Integration::new(controller,view);
        Ide {application,integration}
    }
}

/// The Path of the module initially opened after opening project in IDE.
pub fn initial_module_path(project:&model::Project) -> FallibleResult<model::module::Path> {
    model::module::Path::from_name_segments(project.content_root_id(),&[INITIAL_MODULE_NAME])
}
