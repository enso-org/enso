//! This module contains the IDE object implementation.

use crate::prelude::*;

use crate::presenter::Presenter;

use analytics::AnonymousData;
use enso_frp as frp;
use ensogl::system::web::sleep;
use std::time::Duration;


// ==============
// === Export ===
// ==============

pub mod initializer;

pub use initializer::Initializer;



// =================
// === Constants ===
// =================

/// Constant notification ID, so we can reuse the same notification.
pub const BACKEND_DISCONNECTED_NOTIFICATION_ID: &str = "backend-disconnected-toast";
/// Text that shows up in the statusbar when any of the backend connections is lost.
pub const BACKEND_DISCONNECTED_MESSAGE: &str =
    "Connection to the backend has been lost. Please try restarting IDE.";
/// Text that shows up in the statusbar when backend reports a failed execution.
pub const EXECUTION_FAILED_MESSAGE: &str =
    "Execution failed. Please try restarting project or IDE and report this problem at support@enso.org.";

const ALIVE_LOG_INTERVAL_SEC: u64 = 60;



// ===========
// === Ide ===
// ===========

/// The main Ide structure.
///
/// This structure is a root of all objects in our application. It includes both layers:
/// Controllers and Views, and an integration between them.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Ide {
    pub ensogl_app: ensogl::application::Application,
    pub presenter:  Presenter,
    network:        frp::Network,
}

impl Ide {
    /// Constructor.
    pub fn new(
        ensogl_app: ensogl::application::Application,
        view: ide_view::root::View,
        controller: controller::Ide,
    ) -> Self {
        let presenter = Presenter::new(controller, view);
        let network = frp::Network::new("Ide");
        Ide { ensogl_app, presenter, network }.init()
    }

    fn init(self) -> Self {
        executor::global::spawn(self.alive_log_sending_loop());
        self
    }

    fn alive_log_sending_loop(&self) -> impl Future<Output = ()> + 'static {
        let network = &self.network;
        let scene = &self.ensogl_app.display.default_scene;
        let mouse = &scene.mouse.frp_deprecated;
        let keyboard = &scene.global_keyboard.frp;

        enso_frp::extend! { network
            on_log_sent <- source::<()>();
            mouse_moved <- mouse.position.constant(()).profile();
            any_mouse_press <- any(mouse.up, mouse.down).constant(()).profile();
            any_mouse_event <- any(any_mouse_press, mouse_moved, mouse.wheel).profile();
            any_keyboard_event <- keyboard.any_event.profile();
            any_input_event <- any(any_mouse_event, any_keyboard_event).profile();
            // True if any input event was captured since the last "alive" log sending.
            input_event_received <- bool(&on_log_sent, &any_input_event).profile().sampler();
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

/// A reduced version of [`Ide`] structure, representing an application which failed to initialize.
///
/// It contains only the view displaying the error. No connection to the backend is maintained.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct FailedIde {
    pub view: ide_view::root::View,
}


/// The Path of the module initially opened after opening project in IDE.
pub fn initial_module_path(project: &model::Project) -> model::module::Path {
    project.main_module_path()
}
