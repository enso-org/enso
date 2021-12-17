//! The module with the [`Project`] presenter. See [`crate::presenter`] documentation to know more
//! about presenters in general.

use crate::prelude::*;

use crate::presenter;

use crate::presenter::graph::ViewNodeId;
use enso_frp as frp;
use ide_view as view;


// =============
// === Model ===
// =============

// Those fields will be probably used when Searcher and Breadcrumbs integration will be implemented.
#[allow(unused)]
#[derive(Debug)]
struct Model {
    logger:           Logger,
    controller:       controller::Project,
    module_model:     model::Module,
    graph_controller: controller::ExecutedGraph,
    ide_controller:   controller::Ide,
    view:             view::project::View,
    graph:            presenter::Graph,
    searcher:         RefCell<Option<presenter::Searcher>>,
}

impl Model {
    fn new(
        ide_controller: controller::Ide,
        controller: controller::Project,
        init_result: controller::project::InitializationResult,
        view: view::project::View,
    ) -> Self {
        let logger = Logger::new("presenter::Project");
        let graph_controller = init_result.main_graph;
        let module_model = init_result.main_module_model;
        let graph = presenter::Graph::new(
            controller.model.clone_ref(),
            graph_controller.clone_ref(),
            view.graph().clone_ref(),
        );
        let searcher = default();
        Model {
            logger,
            controller,
            module_model,
            graph_controller,
            ide_controller,
            view,
            graph,
            searcher,
        }
    }

    fn setup_searcher_presenter(&self, node_view: ViewNodeId) {
        let new_presenter = presenter::Searcher::setup_controller(
            &self.logger,
            self.ide_controller.clone_ref(),
            self.controller.clone_ref(),
            self.graph_controller.clone_ref(),
            &self.graph,
            self.view.clone_ref(),
            node_view,
        );
        match new_presenter {
            Ok(searcher) => {
                *self.searcher.borrow_mut() = Some(searcher);
            }
            Err(err) => {
                error!(self.logger, "Error while creating searcher integration: {err}");
            }
        }
    }

    fn editing_committed(
        &self,
        node: ViewNodeId,
        entry_id: Option<view::searcher::entry::Id>,
    ) -> bool {
        let searcher = self.searcher.take();
        if let Some(searcher) = searcher {
            if let Some(created_node) = searcher.editing_committed(entry_id) {
                DEBUG!("Created node {created_node}");
                self.graph.assign_node_view_explicitly(node, created_node);
                false
            } else {
                true
            }
        } else {
            false
        }
    }

    fn editing_aborted(&self) {
        let searcher = self.searcher.take();
        if let Some(searcher) = searcher {
            searcher.editing_aborted();
        } else {
            warning!(self.logger, "Editing aborted without searcher controller.");
        }
    }
}



// ===============
// === Project ===
// ===============

/// The Project Presenter, synchronizing state between project controller and project view.
#[derive(Clone, CloneRef, Debug)]
pub struct Project {
    network: frp::Network,
    model:   Rc<Model>,
}

impl Project {
    /// Construct new project presenter, basing of the project initialization result.
    ///
    /// The returned presenter will be already working: it will display the initial main graph, and
    /// react to all notifications.
    pub fn new(
        ide_controller: controller::Ide,
        controller: controller::Project,
        init_result: controller::project::InitializationResult,
        view: view::project::View,
    ) -> Self {
        let network = frp::Network::new("presenter::Project");
        let model = Model::new(ide_controller, controller, init_result, view);
        Self { network, model: Rc::new(model) }.init_frp()
    }

    fn init_frp(self) -> Self {
        let model = &self.model;
        let network = &self.network;

        let view = &model.view.frp;
        let graph_view = &model.view.graph().frp;

        frp::extend! { network
            searcher_input <- view.searcher_input.filter_map(|view| *view);
            trace searcher_input;
            eval searcher_input ((node_view) {
                model.setup_searcher_presenter(*node_view)
            });

            trace view.editing_committed;
            trace view.editing_aborted;

            graph_view.remove_node <+ view.editing_committed.filter_map(f!([model]((node_view, entry)) {
                model.editing_committed(*node_view, *entry).as_some(*node_view)
            }));
            trace graph_view.remove_node;
            eval_ view.editing_aborted(model.editing_aborted());
        }

        self
    }

    /// Initialize project and return working presenter.
    ///
    /// This calls the [`controller::Project::initialize`] method and use the initialization result
    /// to construct working presenter.
    pub async fn initialize(
        ide_controller: controller::Ide,
        controller: controller::Project,
        view: view::project::View,
    ) -> FallibleResult<Self> {
        let init_result = controller.initialize().await?;
        Ok(Self::new(ide_controller, controller, init_result, view))
    }
}
