//! Support structures for high-level testing crates that must connect to the Project Manager.
//! This includes integration tests and batch-mode application profiling.

use crate::prelude::*;
use enso_web::traits::*;

use crate::config::InitialView;
use crate::executor::web::EventLoopExecutor;
use crate::initializer::setup_global_executor;
use crate::Ide;
use enso_frp::future::EventOutputExt;
use enso_web::Closure;
use enso_web::HtmlDivElement;
use ensogl::application::test_utils::ApplicationExt;
use std::pin::Pin;



/// Reexports of commonly-used structures, methods and traits.
pub mod prelude {
    pub use super::Fixture;

    pub use super::wait_a_frame;
    pub use crate::prelude::*;
    pub use enso_frp::future::EventOutputExt;
}



// ===============
// === Fixture ===
// ===============

/// A root object for tests; contains the IDE, and objects that support it.
#[derive(Debug)]
pub struct Fixture {
    /// The top-level object of the IDE.
    pub ide:      Ide,
    /// Runs the IDE's async tasks.
    pub executor: EventLoopExecutor,
    /// Container for the IDE's HTML elements.
    pub root_div: HtmlDivElement,
}

impl Fixture {
    /// Initializes the executor and `Ide` structure and returns new Fixture.
    pub async fn new(initial_view: InitialView) -> Self {
        let config = crate::config::Startup { initial_view, ..default() };
        let executor = setup_global_executor();
        let root_div = enso_web::document.create_div_or_panic();
        root_div.set_id(config.dom_parent_id());
        root_div.set_style_or_warn("display", "none");
        enso_web::document.body_or_panic().append_or_warn(&root_div);
        let initializer = crate::ide::Initializer::new(config);
        let ide = initializer.start().await.expect("Failed to initialize the application.");
        ide.ensogl_app.set_screen_size_for_tests();
        Self { executor, ide, root_div }
    }

    /// Initializes the executor and `Ide` structure (loading the welcome screen),
    /// and returns the new Fixture.
    pub async fn setup() -> Self {
        Self::new(InitialView::WelcomeScreen).await
    }

    /// Create a fixture for testing in a newly-created project.
    ///
    /// Initializes the IDE, creates a new project, and waits until the project is ready.
    pub async fn setup_new_project() -> Self {
        let fixture = Self::new(InitialView::Project).await;
        fixture.new_project().await;
        fixture
    }

    /// After returning, the IDE is in a state with new project opened and ready to work
    /// (after libraries' compilation).
    pub async fn new_project(&self) {
        let project = self.ide.presenter.view().project();
        let controller = self.ide.presenter.controller();
        let project_management =
            controller.manage_projects().expect("Cannot access Managing Project API");

        let expect_prompt = project.show_prompt.next_event();
        project_management.create_new_project(None).await.expect("Failed to create new project");
        expect_prompt.await;
    }

    /// Get the Project View.
    pub fn project_view(&self) -> crate::view::project::View {
        self.ide.presenter.view().project()
    }

    /// Get the Graph Editor.
    pub fn graph_editor(&self) -> crate::view::graph_editor::GraphEditor {
        self.project_view().graph().clone_ref()
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        self.root_div.remove();
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
