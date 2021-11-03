//! This module contains the IDE object implementation.
pub mod initializer;
pub mod integration;

use crate::prelude::*;

use crate::controller::project::INITIAL_MODULE_NAME;
use crate::ide::integration::Integration;

use analytics::AnonymousData;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::system::web::sleep;
use std::time::Duration;

pub use initializer::Initializer;



// =================
// === Constants ===
// =================

/// Text that shows up in the statusbar when any of the backend connections is lost.
pub const BACKEND_DISCONNECTED_MESSAGE: &str =
    "Connection to the backend has been lost. Please try restarting IDE.";

const ALIVE_LOG_INTERVAL_SEC: u64 = 60;



// ===========
// === Ide ===
// ===========

/// The main Ide structure.
///
/// This structure is a root of all objects in our application. It includes both layers:
/// Controllers and Views, and an integration between them.
#[derive(Debug)]
pub struct Ide {
    application: Application,
    #[allow(dead_code)]
    /// The integration layer is never directly accessed, but needs to be kept alive to keep
    /// performing its function.
    integration: Integration,
    network:     frp::Network,
}

impl Ide {
    /// Constructor.
    pub async fn new(
        application: Application,
        view: ide_view::project::View,
        controller: controller::Ide,
    ) -> Self {
        let integration = integration::Integration::new(controller, view);
        let network = frp::Network::new("Ide");
        Ide { application, integration, network }.init()
    }

    fn init(self) -> Self {
        executor::global::spawn(self.alive_log_sending_loop());
        self
    }

    fn alive_log_sending_loop(&self) -> impl Future<Output = ()> + 'static {
        let network = &self.network;
        let scene = self.application.display.scene();
        let mouse = &scene.mouse.frp;
        let keyboard = &scene.keyboard.frp;

        enso_frp::extend! { network
            on_log_sent          <- source::<()>();
            mouse_moved          <- mouse.position.constant(());
            any_mouse_press      <- any(mouse.up,mouse.down).constant(());
            any_mouse_event      <- any(any_mouse_press,mouse_moved,mouse.wheel);
            any_keyboard_event   <- any(keyboard.down,keyboard.up).constant(());
            any_input_event      <- any(any_mouse_event,any_keyboard_event);
            // True if any input event was captured since the last "alive" log sending.
            input_event_received <- bool(&on_log_sent,&any_input_event).sampler();
        }
        async move {
            loop {
                let value = AnonymousData(input_event_received.value());
                analytics::remote_log_value("alive", "input_event_received", value);
                on_log_sent.emit(());
                sleep(Duration::from_secs(ALIVE_LOG_INTERVAL_SEC)).await;
            }
        }
    }
}

/// The Path of the module initially opened after opening project in IDE.
pub fn initial_module_path(project: &model::Project) -> FallibleResult<model::module::Path> {
    model::module::Path::from_name_segments(project.project_content_root_id(), &[
        INITIAL_MODULE_NAME,
    ])
}
