//! Enso Integration Tests
//!
//! This crate contains all integration tests of Enso applications: the IDE and the backend
//! (engine). All of those are placed in `tests` directory. The sources (`src`) contain only
//! fixtures and helpers.
//!
//! These tests require the Project Manager for working.

#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use crate::prelude::*;

use enso_frp::future::EventOutputExt;
use enso_gui::executor::web::EventLoopExecutor;
use enso_gui::initializer::setup_global_executor;
use enso_gui::Ide;
use enso_web::HtmlDivElement;
use enso_web::NodeInserter;
use enso_web::StyleSetter;
use ensogl::application::Application;

/// Reexports of commonly-used structures, methods and traits.
pub mod prelude {
    pub use crate::IntegrationTest;
    pub use crate::IntegrationTestOnNewProject;

    pub use enso_frp::future::EventOutputExt;
    pub use enso_gui::prelude::*;
    pub use wasm_bindgen_test::wasm_bindgen_test;
}



// =======================
// === IntegrationTest ===
// =======================

/// A fixture for each IDE integration tests. During setup, the executor and [`Ide`] structure are
/// initialized.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct IntegrationTest {
    pub executor: EventLoopExecutor,
    pub ide:      Ide,
    pub root_div: HtmlDivElement,
}

impl IntegrationTest {
    const SCREEN_SIZE: (f32, f32) = (1920.0, 1000.0);

    /// Initializes the executor and `Ide` structure and returns new Fixture.
    pub async fn setup() -> Self {
        enso_web::forward_panic_hook_to_error();
        let executor = setup_global_executor();
        let root_div = enso_web::create_div();
        root_div.set_id("root");
        root_div.set_style_or_panic("display", "none");
        enso_web::body().append_or_panic(&root_div);

        let initializer = enso_gui::ide::Initializer::new(default());
        let ide = initializer.start().await.expect("Failed to initialize the application.");
        Self::set_screen_size(&ide.ensogl_app);
        Self { executor, ide, root_div }
    }

    fn set_screen_size(app: &Application) {
        let (screen_width, screen_height) = Self::SCREEN_SIZE;
        app.display.scene().layers.iter_sublayers_and_masks_nested(|layer| {
            layer.camera().set_screen(screen_width, screen_height)
        });
    }
}

impl Drop for IntegrationTest {
    fn drop(&mut self) {
        self.root_div.remove();
    }
}



// ===================================
// === IntegrationTestOnNewProject ===
// ===================================

/// A fixture for IDE integration tests on created project. It is derived from [`IntegrationTest`].
/// During setup, the Ide initialization is performed, then new project is created, and we wait till
/// the prompt for user will be displayed (thus informing us, that the project is ready to work).
#[derive(Debug)]
pub struct IntegrationTestOnNewProject {
    parent: IntegrationTest,
}

impl Deref for IntegrationTestOnNewProject {
    type Target = IntegrationTest;

    fn deref(&self) -> &Self::Target {
        &self.parent
    }
}

impl IntegrationTestOnNewProject {
    /// Test initialization. After returning, the IDE is in state with new project opened and ready
    /// to work (after libraries' compilation).
    pub async fn setup() -> Self {
        let parent = IntegrationTest::setup().await;
        let ide = &parent.ide;
        let project = ide.presenter.view().project();
        let controller = ide.presenter.controller();
        let project_management =
            controller.manage_projects().expect("Cannot access Managing Project API");

        let expect_prompt = project.show_prompt.next_event();
        project_management.create_new_project(None).await.expect("Failed to create new project");
        expect_prompt.await;
        Self { parent }
    }

    /// Get the Project View.
    pub fn project_view(&self) -> enso_gui::view::project::View {
        self.ide.presenter.view().project()
    }

    /// Get the Graph Editor.
    pub fn graph_editor(&self) -> enso_gui::view::graph_editor::GraphEditor {
        self.project_view().graph().clone_ref()
    }
}
