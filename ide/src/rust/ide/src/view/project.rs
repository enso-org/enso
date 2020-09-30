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
use enso_protocol::language_server::MethodPointer;
use enso_shapely::shared;
use ensogl_theme;
use ide_view::graph_editor;
use nalgebra::Vector2;
use parser::Parser;



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

/// The code with definition of the default `main` method.
pub fn default_main_method_code() -> String {
    format!(r#"{} = "Hello, World!""#, MAIN_DEFINITION_NAME)
}

/// The default content of the newly created initial main module file.
pub fn default_main_module_code() -> String {
    default_main_method_code()
}

/// Method pointer that described the main method, i.e. the method that project view wants to open
/// and which presence is currently required.
pub fn main_method_ptr(project_name:impl Str, module_path:&model::module::Path) -> MethodPointer {
    module_path.method_pointer(project_name,MAIN_DEFINITION_NAME)
}



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

/// Add main method definition to the given module, if the method is not already defined.
///
/// The lookup will be done using the given `main_ptr` value.
pub fn add_main_if_missing
(module:&model::Module, main_ptr:&MethodPointer, parser:&Parser) -> FallibleResult<()> {
    if module.lookup_method(main_ptr).is_err() {
        let mut info  = module.info();
        let main_code = default_main_method_code();
        let main_ast  = parser.parse_line(main_code)?;
        info.add_ast(main_ast,double_representation::module::Placement::End)?;
        module.update_ast(info.ast);

        // TODO [mwu] Add imports required by the default main definition (when needed).
    }
    Ok(())
}

impl ProjectView {
    /// Create a new ProjectView.
    pub async fn new(logger:impl AnyLogger, project:model::Project) -> FallibleResult<Self> {
        let logger      = Logger::sub(logger,"ProjectView");
        let module_path = initial_module_path(&project)?;
        let file_path   = module_path.file_path().clone();
        // TODO [mwu] This solution to recreate missing main file should be considered provisional
        //   until proper decision is made. See: https://github.com/enso-org/enso/issues/1050
        recreate_if_missing(&project, &file_path, default_main_method_code()).await?;

        let method = main_method_ptr(project.name(),&module_path);
        let module = project.module(module_path).await?;
        add_main_if_missing(&module,&method,&project.parser())?;

        // Here, we should be relatively certain (except race conditions in case of multiple clients
        // that we currently do not support) that main module exists and contains main method.
        // Thus, we should be able to successfully create a graph controller for it.
        let graph_controller  = controller::ExecutedGraph::new(&logger,project.clone_ref(),method);
        let graph_controller  = graph_controller.await?;
        let text_controller   = controller::Text::new(&logger, &project, file_path).await?;
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
        let visualization_controller = project.visualization().clone();
        let layout = ViewLayout::new(&logger,&mut keyboard_actions,&application,text_controller,
            graph_controller,visualization_controller,project.clone_ref(),&mut fonts).await?;
        let model = project;
        let data = ProjectViewData {model,application,layout,resize_callback,keyboard,
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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    #[wasm_bindgen_test]
    fn adding_missing_main() {
        let _ctx        = TestWithLocalPoolExecutor::set_up();
        let parser      = parser::Parser::new_or_panic();
        let mut data    = crate::test::mock::Unified::new();
        let module_name = data.module_path.module_name();
        let main_ptr    = main_method_ptr(&data.project_name,&data.module_path);

        // Check that module without main gets it after the call.
        let empty_module_code = "";
        data.set_code(empty_module_code);
        let module = data.module();
        assert!(module.lookup_method(&main_ptr).is_err());
        add_main_if_missing(&module,&main_ptr,&parser).unwrap();
        assert!(module.lookup_method(&main_ptr).is_ok());

        // Now check that modules that have main already defined won't get modified.
        let mut expect_intact = move |code:&str| {
            data.set_code(code);
            let module = data.module();
            add_main_if_missing(&module,&main_ptr,&parser).unwrap();
            assert_eq!(code,module.ast().repr());
        };
        expect_intact("main = 5");
        expect_intact("here.main = 5");
        expect_intact(&format!("{}.main = 5",module_name));
    }
}
