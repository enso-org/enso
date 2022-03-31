//! Enso Integration Tests
//!
//! This crate contains all integration tests of Enso applications: the IDE and the backend
//! (engine). All of those are placed in `tests` directory. The sources (`src`) contain only
//! fixtures and helpers.
//!
//! These tests require the Project Manager for working.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use crate::prelude::*;
use enso_web::traits::*;

use enso_frp::future::EventOutputExt;
use enso_gui::executor::web::EventLoopExecutor;
use enso_gui::initializer::setup_global_executor;
use enso_gui::Ide;
use enso_web::Closure;
use enso_web::HtmlDivElement;
use ensogl::application::test_utils::ApplicationExt;
use std::pin::Pin;



/// Reexports of commonly-used structures, methods and traits.
pub mod prelude {
    pub use crate::IntegrationTest;
    pub use crate::IntegrationTestOnNewProject;

    pub use crate::wait_a_frame;
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
    /// Initializes the executor and `Ide` structure and returns new Fixture.
    pub async fn setup() -> Self {
        let executor = setup_global_executor();
        let root_div = enso_web::document.create_div_or_panic();
        root_div.set_id("root");
        root_div.set_style_or_warn("display", "none");
        enso_web::document.body_or_panic().append_or_warn(&root_div);

        let initializer = enso_gui::ide::Initializer::new(default());
        let ide = initializer.start().await.expect("Failed to initialize the application.");
        ide.ensogl_app.set_screen_size_for_tests();
        Self { executor, ide, root_div }
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



// ==================
// === WaitAFrame ===
// ==================

/// A future that resolves after one frame.
#[derive(Default, Debug)]
#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
pub struct WaitAFrame {
    frame_passed: Rc<Cell<bool>>,
    closure:      Option<Closure<dyn FnMut(f64)>>,
}

impl Future for WaitAFrame {
    type Output = ();

    #[cfg(not(target_arch = "wasm32"))]
    fn poll(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        std::task::Poll::Ready(())
    }

    #[cfg(target_arch = "wasm32")]
    fn poll(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        if self.frame_passed.get() {
            std::task::Poll::Ready(())
        } else {
            let waker = cx.waker().clone();
            let frame_passed = self.frame_passed.clone_ref();
            let closure = Closure::once(move |_| {
                frame_passed.set(true);
                waker.wake()
            });
            enso_web::window.request_animation_frame_with_closure_or_panic(&closure);
            self.closure = Some(closure);
            std::task::Poll::Pending
        }
    }
}

/// Return a future that resolves after one frame.
pub fn wait_a_frame() -> impl Future<Output = ()> {
    WaitAFrame::default()
}
