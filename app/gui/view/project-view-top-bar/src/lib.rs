//! Defines a UI container for the window control buttons and the "go to dashboard" button. This is
//! merely here to make use of the auto-layout functionality.

#![recursion_limit = "512"]
use ensogl::prelude::*;

use enso_config::ARGS;
use ensogl::application::Application;
use ensogl::display;



mod breadcrumbs;
mod go_to_dashboard_button;
pub mod window_control_buttons;
use breadcrumbs::project_name::ProjectName;



// =================
// === Constants ===
// =================

/// The gap in pixels between the various components of the project view top bar.
const GAP: f32 = 16.0;
/// The padding left of the project view top bar.
const PADDING_LEFT: f32 = 19.0;

// TODO: Read only setup for breadcrumbs.


#[derive(Clone, CloneRef, Debug)]
pub struct ProjectNameWithEnvironmentSelector {
    root:             display::object::Instance,
    pub project_name: ProjectName,
    pub selector:     ide_view_execution_environment_selector::ExecutionEnvironmentSelector,
}

impl ProjectNameWithEnvironmentSelector {
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new();
        let project_name = app.new_view();
        let selector =
            ide_view_execution_environment_selector::ExecutionEnvironmentSelector::new(app);

        root.use_auto_layout().set_children_alignment_center();
        root.add_child(&project_name);
        root.add_child(&selector);
        Self { root, project_name, selector }
    }
}

impl display::Object for ProjectNameWithEnvironmentSelector {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}

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
    pub breadcrumbs: breadcrumbs::Breadcrumbs,
    pub project_name_with_environment_selector: ProjectNameWithEnvironmentSelector,
}

impl ProjectViewTopBar {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new_named("ProjectViewTopBar");
        let window_control_buttons = app.new_view::<window_control_buttons::View>();
        let go_to_dashboard_button = go_to_dashboard_button::View::new(app);
        let breadcrumbs = breadcrumbs::Breadcrumbs::new(app);
        let project_name_with_environment_selector = ProjectNameWithEnvironmentSelector::new(app);

        if ARGS.groups.startup.options.platform.value == "web" {
            root.add_child(&window_control_buttons);
        }
        root.add_child(&go_to_dashboard_button);
        root.add_child(&project_name_with_environment_selector);
        root.add_child(&breadcrumbs);
        root.use_auto_layout()
            .set_gap((GAP, 0.0))
            .set_padding_left(PADDING_LEFT)
            // We use `GAP` as the right padding since it delimits the space to the part of the top
            // bar that's defined in the graph editor.
            .set_padding_right(GAP)
            .set_children_alignment_center();

        app.display.default_scene.layers.panel.add(&root);

        Self {
            root,
            window_control_buttons,
            go_to_dashboard_button,
            breadcrumbs,
            project_name_with_environment_selector,
        }
    }
}

impl display::Object for ProjectViewTopBar {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}
