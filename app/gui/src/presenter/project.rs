//! The module with the [`Project`] presenter. See [`crate::presenter`] documentation to know more
//! about presenters in general.

use crate::prelude::*;

use crate::presenter;

use ide_view as view;



// =============
// === Model ===
// =============

// Those fields will be probably used when Searcher and Breadcrumbs integration will be implemented.
#[allow(unused)]
#[derive(Debug)]
struct Model {
    controller: controller::Project,
    view:       view::project::View,
    graph:      presenter::Graph,
}



// ===============
// === Project ===
// ===============

/// The Project Presenter, synchronizing state between project controller and project view.
#[derive(Clone, CloneRef, Debug)]
pub struct Project {
    model: Rc<Model>,
}

impl Project {
    /// Construct new project presenter, basing of the project initialization result.
    ///
    /// The returned presenter will be already working: it will display the initial main graph, and
    /// react to all notifications.
    pub fn new(
        controller: controller::Project,
        init_result: controller::project::InitializationResult,
        view: view::project::View,
    ) -> Self {
        let graph_controller = init_result.main_graph;
        let graph = presenter::Graph::new(graph_controller, view.graph().clone_ref());
        let model = Model { controller, view, graph };
        Self { model: Rc::new(model) }
    }

    /// Initialize project and return working presenter.
    ///
    /// This calls the [`controller::Project::initialize`] method and use the initialization result
    /// to construct working presenter.
    pub async fn initialize(
        controller: controller::Project,
        view: view::project::View,
    ) -> FallibleResult<Self> {
        let init_result = controller.initialize().await?;
        Ok(Self::new(controller, init_result, view))
    }
}
