//! Defines a UI container for the window control buttons and the "go to dashboard" button. This is
//! merely here to make use of the auto-layout functionality.

#![recursion_limit = "512"]
use ensogl::display::shape::StyleWatchFrp;
use ensogl::prelude::*;

use enso_config::ARGS;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::shape::compound::rectangle::Rectangle;



mod breadcrumbs;
mod go_to_dashboard_button;
pub mod window_control_buttons;
use breadcrumbs::project_name::ProjectName;
use ensogl_hardcoded_theme::application::top_bar as theme;



// ==========================================
// === ProjectNameWithEnvironmentSelector ===
// ==========================================

#[derive(Clone, CloneRef, Debug)]
pub struct ProjectNameWithEnvironmentSelector {
    // root:             display::object::Instance,
    // content:          display::object::Instance,
    pub project_name: ProjectName,
    pub selector:     ide_view_execution_environment_selector::ExecutionEnvironmentSelector,
    background:       Rectangle,
    network:          frp::Network,
}

impl ProjectNameWithEnvironmentSelector {
    pub fn new(app: &Application) -> Self {
        // let root = display::object::Instance::new();
        // let content = display::object::Instance::new();
        let project_name = app.new_view::<ProjectName>();
        let selector =
            ide_view_execution_environment_selector::ExecutionEnvironmentSelector::new(app);
        let background = Rectangle::new();

        scene().layers.panel_background.add(&background);
        background.add_child(&background);
        background
            .use_auto_layout()
            .set_children_alignment_left_center()
            .justify_content_center_y();
        background.add_child(&project_name);
        background.add_child(&selector);
        let network = frp::Network::new("ProjectNameWithEnvironmentSelector");

        Self { project_name, selector, background, network }.init()
    }

    fn init(self) -> Self {
        let style_watch = StyleWatchFrp::new(&scene().style_sheet);
        self.background.set_style(&theme::background::HERE, &style_watch);
        let network = &self.network;
        let background = &self.background;
        frp::extend! { network
            init <- source_();
            let height = style_watch.get_number(theme::project_name_with_environment_selector::background::height);
            let gap = style_watch.get_number(theme::project_name_with_environment_selector::gap);
            let padding_left = style_watch.get_number(theme::project_name_with_environment_selector::background::padding_left);
            let padding_right = style_watch.get_number(theme::project_name_with_environment_selector::background::padding_right);
            height <- all(init, height)._1();
            gap <- all(init, gap)._1();
            padding_left <- all(init, padding_left)._1();
            padding_right <- all(init, padding_right)._1();
            eval height([background](h) { background.set_size_y(*h); });
            eval gap([background](g) { background.set_gap((*g, 0.0)); });
            eval padding_left([background](p) { background.set_padding_left(*p); });
            eval padding_right([background](p) { background.set_padding_right(*p); });
        }
        init.emit(());

        self
    }
}

impl display::Object for ProjectNameWithEnvironmentSelector {
    fn display_object(&self) -> &display::object::Instance {
        self.background.display_object()
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
    network: frp::Network,
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
        root.use_auto_layout().set_children_alignment_center();

        app.display.default_scene.layers.panel.add(&root);

        let network = frp::Network::new("ProjectViewTopBar");

        Self {
            root,
            window_control_buttons,
            go_to_dashboard_button,
            breadcrumbs,
            project_name_with_environment_selector,
            network,
        }
        .init()
    }

    fn init(self) -> Self {
        let network = &self.network;
        let style_watch = StyleWatchFrp::new(&scene().style_sheet);
        let root = &self.root;

        frp::extend! { network
            init <- source_();
            let gap = style_watch.get_number(theme::gap);
            let padding_left = style_watch.get_number(theme::padding_left);
            gap <- all(init, gap)._1();
            padding_left <- all(init, padding_left)._1();
            eval gap([root](g) { root.set_gap((*g, 0.0)); });
            eval padding_left([root](p) { root.set_padding_left(*p); });
        }
        init.emit(());
        self
    }
}

impl display::Object for ProjectViewTopBar {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}
