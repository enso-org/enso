//! This module contains ProjectView, the main view, responsible for managing TextEditor and
//! GraphEditor.

use crate::prelude::*;

use crate::double_representation::definition::DefinitionName;
use crate::view::layout::ViewLayout;

use ensogl::control::callback;
use ensogl::control::io::keyboard::listener::KeyboardFrpBindings;
use ensogl::data::color;
use ensogl::display::shape::text::glyph::font;
use ensogl::display::style::theme;
use ensogl::system::web;
use enso_frp::io::keyboard::Keyboard;
use enso_frp::io::keyboard;
use nalgebra::Vector2;
use shapely::shared;
use ensogl::application::Application;



// =================
// === Constants ===
// =================

/// Path of the file that is initially opened in project view.
///
/// Currently this path is hardcoded in the engine services and is populated for each project
/// created using engine's Project Picker service.
///
/// TODO [mwu] Path of the file that will be initially opened in the text editor.
///      Provisionally the Project View is hardcoded to open with a single text
///      editor and it will be connected with a file under this path.
///      To be replaced with better mechanism once we decide how to describe
///      default initial layout for the project.
const INITIAL_FILE_PATH: [&str;2] = ["src","Main.enso"];

/// Name of the main definition.
///
/// This is the definition whose graph will be opened on IDE start.
const MAIN_DEFINITION_NAME:&str = "main";



// ===================
// === ProjectView ===
// ===================

shared! { ProjectView

    /// ProjectView is the main view of the project, holding instances of TextEditor and
    /// GraphEditor.
    #[derive(Debug)]
    pub struct ProjectViewData {
        application       : Application,
        layout            : ViewLayout,
        resize_callback   : Option<callback::Handle>,
        controller        : controller::Project,
        keyboard          : Keyboard,
        keyboard_bindings : KeyboardFrpBindings,
        keyboard_actions  : keyboard::Actions
    }

    impl {
        /// Set view size.
        pub fn set_size(&mut self, size:Vector2<f32>) {
            self.layout.set_size(size);
        }
    }
}

impl ProjectView {
    /// Create a new ProjectView.
    pub async fn new(logger:&Logger, controller:controller::Project)
    -> FallibleResult<Self> {
        let root_id              = controller.language_server_rpc.content_root();
        let path                 = controller::FilePath::new(root_id,&INITIAL_FILE_PATH);
        let text_controller      = controller.text_controller(path.clone()).await?;
        let main_name            = DefinitionName::new_plain(MAIN_DEFINITION_NAME);
        let graph_id             = controller::graph::Id::new_single_crumb(main_name);
        let module_controller    = controller.module_controller(path.try_into()?).await?;
        let graph_controller     = module_controller.executed_graph_controller_unchecked(graph_id);
        let graph_controller     = graph_controller.await?;
        let application          = Application::new(&web::get_html_element_by_id("root").unwrap());
        Self::setup_components(&application);
        Self::setup_theme(&application);
        let _world               = &application.display;
        // graph::register_shapes(&world);
        let logger               = logger.sub("ProjectView");
        let keyboard             = Keyboard::default();
        let keyboard_bindings    = KeyboardFrpBindings::new(&logger,&keyboard);
        let mut keyboard_actions = keyboard::Actions::new(&keyboard);
        let resize_callback      = None;
        let mut fonts            = font::Registry::new();
        let layout               = ViewLayout::new(&logger,&mut keyboard_actions,&application,
            text_controller,graph_controller,&mut fonts);
        let data = ProjectViewData {application,layout,resize_callback,controller,keyboard,
            keyboard_bindings,keyboard_actions};
        Ok(Self::new_from_data(data).init())
    }

    fn init(self) -> Self {
        let scene = self.with_borrowed(|data| data.application.display.scene().clone_ref());
        let weak  = self.downgrade();
        let resize_callback = scene.camera().add_screen_update_callback(
            move |size:&Vector2<f32>| {
                if let Some(this) = weak.upgrade() {
                    this.set_size(*size)
                }
            }
        );
        self.with_borrowed(move |data| data.resize_callback = Some(resize_callback));
        self
    }

    fn setup_components(app:&Application) {
        app.views.register::<graph_editor::GraphEditor>();
    }

    fn setup_theme(app:&Application) {
        let mut dark = theme::Theme::new();
        dark.insert("application.background.color", color::Lcha::new(0.13,0.013,0.18,1.0));
        dark.insert("graph_editor.node.background.color", color::Lcha::new(0.2,0.013,0.18,1.0));
        dark.insert("graph_editor.node.selection.color", color::Lcha::new(0.72,0.5,0.22,1.0));
        dark.insert("graph_editor.node.selection.size", 7.0);
        //    dark.insert("graph_editor.node.selection.color", color::Lcha::new(0.7,0.59,0.18,1.0));
        dark.insert("animation.duration", 0.5);
        dark.insert("graph.node.shadow.color", 5.0);
        dark.insert("graph.node.shadow.size", 5.0);
        dark.insert("mouse.pointer.color", color::Rgba::new(0.3,0.3,0.3,1.0));

        app.themes.register("dark",dark);
        app.themes.set_enabled(&["dark"]);
    }

    /// Forgets ProjectView, so it won't get dropped when it goes out of scope.
    pub fn forget(self) {
        std::mem::forget(self)
    }
}
