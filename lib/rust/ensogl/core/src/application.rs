//! Application top-level structure definition. Handles views, keyboard shortcuts and more.

use crate::prelude::*;
use enso_web::traits::*;

use crate::application::command::FrpNetworkProvider;
use crate::control::callback;
use crate::display;
use crate::display::scene::DomPath;
use crate::display::style::theme;
use crate::display::world::World;
use crate::gui::cursor::Cursor;
use crate::system::web;



// ==============
// === Export ===
// ==============

pub mod args;
pub mod command;
pub mod frp;
pub mod shortcut;
pub mod tooltip;
pub mod view;
pub use view::View;

/// A module with commonly used traits to mass import.
pub mod traits {
    pub use crate::application::view::View as TRAIT_View;
}


// ===========
// === Frp ===
// ===========

crate::define_endpoints_2! {
    Input {
        set_tooltip(tooltip::Style),
    }
    Output {
        tooltip(tooltip::Style)
    }
}



// ===================
// === Application ===
// ===================

/// A top level structure for an application. It combines a view, keyboard shortcut manager, and is
/// intended to also manage layout of visible panes.
#[derive(Debug, Clone, CloneRef)]
#[allow(missing_docs)]
pub struct Application {
    pub logger:           Logger,
    pub cursor:           Cursor,
    pub display:          World,
    pub commands:         command::Registry,
    pub shortcuts:        shortcut::Registry,
    pub views:            view::Registry,
    pub themes:           theme::Manager,
    pub frp:              Frp,
    update_themes_handle: callback::Handle,
}

impl Application {
    /// Constructor.
    pub fn new(dom: impl DomPath) -> Self {
        let logger = Logger::new("Application");
        let display = World::new();
        let scene = &display.default_scene;
        scene.display_in(dom);
        let commands = command::Registry::create(&logger);
        let shortcuts =
            shortcut::Registry::new(&logger, &scene.mouse.frp, &scene.keyboard.frp, &commands);
        let views = view::Registry::create(&logger, &display, &commands, &shortcuts);
        let themes = theme::Manager::from(&display.default_scene.style_sheet);
        let cursor = Cursor::new(&display.default_scene);
        display.add_child(&cursor);
        web::document.body_or_panic().set_style_or_warn("cursor", "none");
        let update_themes_handle = display.on.before_frame.add(f_!(themes.update()));
        let frp = Frp::new();
        let _network = frp.network();
        enso_frp::extend! { _network
            frp.private.output.tooltip <+ frp.private.input.set_tooltip;
        }

        Self {
            logger,
            cursor,
            display,
            commands,
            shortcuts,
            views,
            themes,
            update_themes_handle,
            frp,
        }
    }

    /// Create a new instance of a view.
    pub fn new_view<T: View>(&self) -> T {
        self.views.new_view(self)
    }
}

impl display::Object for Application {
    fn display_object(&self) -> &display::object::Instance {
        self.display.display_object()
    }
}

impl AsRef<theme::Manager> for Application {
    fn as_ref(&self) -> &theme::Manager {
        &self.themes
    }
}



// ==================
// === Test Utils ===
// ==================

/// Test-specific API.
pub mod test_utils {
    use super::*;

    /// Screen size for unit and integration tests.
    const TEST_SCREEN_SIZE: (f32, f32) = (1920.0, 1080.0);

    /// Extended API for tests.
    pub trait ApplicationExt {
        /// Set "fake" screen dimensions for unit and integration tests. This is important for a lot
        /// of position and screen size related computations in the IDE.
        fn set_screen_size_for_tests(&self);
    }

    impl ApplicationExt for Application {
        fn set_screen_size_for_tests(&self) {
            let (screen_width, screen_height) = TEST_SCREEN_SIZE;
            let scene = &self.display.default_scene;
            scene.layers.iter_sublayers_and_masks_nested(|layer| {
                let camera = layer.camera();
                camera.set_screen(screen_width, screen_height);
                camera.reset_zoom();
                camera.update(scene);
            });
        }
    }
}

// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_compilation_in_test_mode() {
        let _app = Application::new("root");
    }
}
