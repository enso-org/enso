//! Support structures for high-level testing crates that must connect to the Project Manager.
//! This includes integration tests and batch-mode application profiling.

use crate::prelude::*;
use enso_web::traits::*;

use crate::config::InitialView;
use crate::executor::setup_global_executor;
use crate::executor::web::EventLoopExecutor;
use crate::Ide;

use enso_web::Closure;
use enso_web::HtmlDivElement;
use ensogl::application::test_utils::ApplicationExt;
use std::pin::Pin;



/// Reexports of commonly-used structures, methods and traits.
pub mod prelude {
    pub use super::Fixture;

    pub use super::wait_a_frame;
    pub use crate::prelude::*;
    pub use crate::view::graph_editor::automation::*;
    pub use enso_frp::future::EventOutputExt;
}

use prelude::*;



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
    #[profile(Debug)]
    pub async fn setup_new_project() -> Self {
        let fixture = Self::new(InitialView::Project).await;
        fixture.new_project().await;
        fixture
    }

    /// After returning, the IDE is in a state with new project opened and ready to work
    /// (after libraries' compilation).
    pub async fn new_project(&self) {
        let controller = self.ide.presenter.controller();
        let project_management =
            controller.manage_projects().expect("Cannot access Managing Project API");

        project_management
            .create_new_project(None, None)
            .await
            .expect("Failed to create new project");
    }

    /// After returning, the IDE is in a state with the project opened and ready to work
    /// (after libraries' compilation).
    pub async fn load_project(&self, name: String) {
        let controller = self.ide.presenter.controller();
        let project_management =
            controller.manage_projects().expect("Cannot access Managing Project API");

        project_management.open_project_by_name(name).await.expect("Failed to open project");
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


// === Profiling objectives ===

impl Fixture {
    /// Create a new project; doesn't complete until the project is ready to render.
    #[profile(Objective)]
    pub async fn create_project() -> Self {
        let test = Self::setup_new_project().await;
        test.compile_new_shaders().await;
        test
    }

    /// Open the "Orders" project; doesn't complete until the project is ready to render.
    #[profile(Objective)]
    pub async fn open_project_orders() -> Self {
        let template_project = "Orders";
        let self_ = Self::new(InitialView::Project).await;
        self_.load_project(template_project.to_owned()).await;
        self_.compile_new_shaders().await;
        self_
    }

    /// Create a new node; doesn't complete until the new node is ready to render.
    #[profile(Objective)]
    pub async fn create_node(&self, expression: &str) {
        let graph_editor = self.graph_editor();
        add_node_with_add_node_button(&graph_editor, expression).await;
        profiler::join(self.compile_new_shaders(), self.backend_execution()).await;
    }

    /// Collapse the selected nodes; doesn't complete until the action is completed and the graph
    /// has been recolored.
    #[profile(Objective)]
    pub async fn collapse_selected_nodes(&self) -> crate::view::graph_editor::NodeId {
        let expect_node_added = self.graph_editor().node_added.next_event();
        self.graph_editor().collapse_selected_nodes();
        profiler::join(self.compile_new_shaders(), self.backend_execution()).await;
        expect_node_added.expect().0
    }

    /// Enter the selected node; doesn't complete until the action is completed and the graph
    /// is ready to render.
    #[profile(Objective)]
    pub async fn enter_selected_node(&self) {
        self.graph_editor().enter_selected_node();
        profiler::join(self.compile_new_shaders(), self.backend_execution()).await;
    }

    /// Enable the visualizations for the selected nodes; doesn't complete until the visualizations
    /// are ready to render.
    #[profile(Objective)]
    pub async fn visualize_selected_nodes(&self) {
        self.graph_editor().press_visualization_visibility();
        self.compile_new_shaders().await;
    }
}


// === Profiling helpers ===

impl Fixture {
    /// Wait until the renderer has compiled any new/changed shaders.
    ///
    /// Note that if there are no shaders currently being compiled, this will wait until new shaders
    /// are enqueued and then wait until the compiler is idle again.
    #[profile(Detail)]
    pub async fn compile_new_shaders(&self) {
        let (mut idle_notifier, mut idle_notifications) = futures::channel::mpsc::channel(32);
        let notify_idle = move || idle_notifier.try_send(()).unwrap();

        let compiler = &self.ide.ensogl_app.display.default_scene.shader_compiler;
        let _handle = compiler.on_idle(notify_idle);
        idle_notifications.next().await.unwrap();
    }

    /// Wait until the backend has executed the graph, and we've processed the results.
    #[profile(Detail)]
    pub async fn backend_execution(&self) {
        self.ide.presenter.view().project().values_updated.next_event().await;
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
