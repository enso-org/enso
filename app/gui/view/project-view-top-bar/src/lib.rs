//! Defines a top bar component for the project view. It contains window control buttons, a "go to
//! dashboard" button, project name with execution environment selector, and project breadcrumbs.

#![recursion_limit = "512"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use ensogl::prelude::*;

use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::compound::rectangle::Rectangle;
use ensogl::display::shape::StyleWatchFrp;
use ensogl_hardcoded_theme::application::top_bar as theme;
use project_name::ProjectName;


// ==============
// === Export ===
// ==============

pub mod project_name;

pub use breadcrumbs::LocalCall;



mod breadcrumbs;



// ==========================================
// === ProjectNameWithEnvironmentSelector ===
// ==========================================

/// A container for the project name and execution environment selector.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct ProjectNameWithEnvironmentSelector {
    pub project_name: ProjectName,
    pub selector:     ide_view_execution_environment_selector::ExecutionEnvironmentSelector,
    #[display_object]
    background:       Rectangle,
    network:          frp::Network,
}

impl ProjectNameWithEnvironmentSelector {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
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
        self.background.set_style(theme::background::HERE, &style_watch);
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



// ============================
// === Project View Top Bar ===
// ============================

#[derive(Clone, CloneRef, Debug, display::Object)]
#[allow(missing_docs)]
pub struct ProjectViewTopBar {
    #[display_object]
    root: display::object::Instance,
    pub breadcrumbs: breadcrumbs::Breadcrumbs,
    pub project_name_with_environment_selector: ProjectNameWithEnvironmentSelector,
    network: frp::Network,
}

impl ProjectViewTopBar {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new_named("ProjectViewTopBar");
        let breadcrumbs = breadcrumbs::Breadcrumbs::new(app);
        let project_name_with_environment_selector = ProjectNameWithEnvironmentSelector::new(app);

        root.add_child(&project_name_with_environment_selector);
        root.add_child(&breadcrumbs);
        root.use_auto_layout().set_children_alignment_center();

        app.display.default_scene.layers.panel.add(&root);

        let network = frp::Network::new("ProjectViewTopBar");

        Self { root, breadcrumbs, project_name_with_environment_selector, network }.init()
    }

    fn init(self) -> Self {
        let network = &self.network;
        let style_watch = StyleWatchFrp::new(&scene().style_sheet);
        let root = &self.root;

        frp::extend! { network
            init <- source_();
            let gap = style_watch.get_number(theme::gap);
            let padding_left = style_watch.get_number(theme::padding_left);
            let padding_top = style_watch.get_number(theme::padding_top);
            gap <- all(init, gap)._1();
            padding_left <- all(init, padding_left)._1();
            padding_top <- all(init, padding_top)._1();
            eval gap([root](g) { root.set_gap((*g, 0.0)); });
            eval padding_left([root](p) { root.set_padding_left(*p); });
            eval padding_top([root](p) { root.set_padding_top(*p); });
        }
        init.emit(());
        self
    }

    /// Access the project name view directly.
    pub fn project_name(&self) -> &ProjectName {
        &self.project_name_with_environment_selector.project_name
    }
}
