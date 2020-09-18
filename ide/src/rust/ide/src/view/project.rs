//! This module contains ProjectView, the main view, responsible for managing TextEditor and
//! GraphEditor.

use crate::prelude::*;

use crate::controller::FilePath;
use crate::model::module::Path as ModulePath;
use crate::view::layout::ViewLayout;

use ensogl::application::Application;
use ensogl::control::callback;
use ensogl::control::io::keyboard::listener::KeyboardFrpBindings;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::display::shape::text::glyph::font;
use ensogl::system::web;
use enso_frp::io::keyboard::Keyboard;
use enso_frp::io::keyboard;
use enso_shapely::shared;
use ensogl_theme;
use ide_view::graph_editor;
use nalgebra::Vector2;



// =================
// === Constants ===
// =================

/// The name of the module initially opened in the project view.
///
/// Currently this name is hardcoded in the engine services and is populated for each project
/// created using engine's Project Picker service.
///
/// TODO [mwu] Name of the moduke that will be initially opened in the text editor.
///      Provisionally the Project View is hardcoded to open with a single text
///      editor and it will be connected with a file with module of this name.
///      To be replaced with better mechanism once we decide how to describe
///      default initial layout for the project.
pub const INITIAL_MODULE_NAME:&str = "Main";

/// Name of the main definition.
///
/// This is the definition whose graph will be opened on IDE start.
pub const MAIN_DEFINITION_NAME:&str = "main";

/// The default content of the newly created initial module file.
pub const DEFAULT_MAIN_CONTENT:&str = r#"main = IO.println "Hello, World!""#;



// ===================
// === ProjectView ===
// ===================

shared! { ProjectView

    /// ProjectView is the main view of the project, holding instances of TextEditor and
    /// GraphEditor.
    #[derive(Debug)]
    pub struct ProjectViewData {
        application       : Application,
        navigator         : Navigator,
        layout            : ViewLayout,
        resize_callback   : Option<callback::Handle>,
        model             : model::Project,
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

/// Returns the path to the initially opened module in the given project.
pub fn initial_module_path(project:&model::Project) -> FallibleResult<ModulePath> {
    model::module::Path::from_name_segments(project.content_root_id(),&[INITIAL_MODULE_NAME])
}

/// Create a file with default content if it does not already exist.
pub async fn recreate_if_missing(project:&model::Project, path:&FilePath, default_content:String)
-> FallibleResult<()> {
    let rpc = project.json_rpc();
    if !rpc.file_exists(path).await?.exists {
        rpc.write_file(path,&default_content).await?;
    }
    Ok(())
}

impl ProjectView {
    /// Create a new ProjectView.
    pub async fn new(logger:impl AnyLogger, model:model::Project)
    -> FallibleResult<Self> {
        let logger      = Logger::sub(logger,"ProjectView");
        let module_path = initial_module_path(&model)?;
        let file_path   = module_path.file_path().clone();
        // TODO [mwu] This solution to recreate missing main file should be considered provisional
        //   until proper decision is made. See: https://github.com/enso-org/enso/issues/1050
        recreate_if_missing(&model,&file_path,DEFAULT_MAIN_CONTENT.into()).await?;
        let text_controller   = controller::Text::new(&logger,&model,file_path).await?;
        let method            = module_path.method_pointer(model.name(),MAIN_DEFINITION_NAME);
        let graph_controller  = controller::ExecutedGraph::new(&logger,model.clone_ref(),method);
        let graph_controller  = graph_controller.await?;
        let application       = Application::new(&web::get_html_element_by_id("root").unwrap());
        let scene             = application.display.scene();
        let camera            = scene.camera();
        let navigator         = Navigator::new(&scene,&camera);
        Self::setup_components(&application);
        ensogl_theme::dark::setup(&application);
        ensogl_theme::light::setup(&application);
        let _world = &application.display;
        // graph::register_shapes(&world);
        let keyboard                 = Keyboard::default();
        let keyboard_bindings        = KeyboardFrpBindings::new(&logger,&keyboard);
        let mut keyboard_actions     = keyboard::Actions::new(&keyboard);
        let resize_callback          = None;
        let mut fonts                = font::Registry::new();
        let visualization_controller = model.visualization().clone();
        let layout = ViewLayout::new(&logger,&mut keyboard_actions,&application, text_controller,
            graph_controller,visualization_controller,model.clone_ref(),&mut fonts).await?;
        let data = ProjectViewData {application,layout,resize_callback,model,keyboard,
            keyboard_bindings,keyboard_actions,navigator};
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

    /// Forgets ProjectView, so it won't get dropped when it goes out of scope.
    pub fn forget(self) {
        std::mem::forget(self)
    }
}
