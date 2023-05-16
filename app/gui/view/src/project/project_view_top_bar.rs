//! Defines a UI container for the window control buttons and the "go to dashboard" button. This is
//! merely here to make use of the auto-layout functionality.

use ensogl::prelude::*;

use enso_config::ARGS;
use ensogl::application::Application;
use ensogl::display;



mod go_to_dashboard_button;
pub mod window_control_buttons;



// =================
// === Constants ===
// =================

/// The gap in pixels between the various components of the project view top bar.
const GAP: f32 = 18.0;
/// The padding around the project view top bar.
const PADDING: f32 = 18.0;



// ============================
// === Project View Top Bar ===
// ============================

/// Defines a UI container for the window control buttons and the "go to dashboard" button. This is
/// merely here to make use of the auto-layout functionality.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct ProjectViewTopBar {
    root: display::object::Instance,
    /// These buttons are only visible in a cloud environment.
    pub window_control_buttons: window_control_buttons::View,
    pub go_to_dashboard_button: go_to_dashboard_button::View,
}

impl ProjectViewTopBar {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new_named("ProjectViewTopBar");
        let window_control_buttons = app.new_view::<window_control_buttons::View>();
        let go_to_dashboard_button = go_to_dashboard_button::View::new(app);

        if ARGS.groups.startup.options.platform.value == "web" {
            root.add_child(&window_control_buttons);
        }
        root.add_child(&go_to_dashboard_button);
        root.use_auto_layout()
            .set_gap((GAP, 0.0))
            .set_padding_all(PADDING)
            .set_children_alignment_center();

        app.display.default_scene.layers.panel.add(&root);

        Self { root, window_control_buttons, go_to_dashboard_button }
    }
}

impl display::Object for ProjectViewTopBar {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}
