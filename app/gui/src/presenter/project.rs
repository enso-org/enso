//! The module with the [`Project`] presenter. See [`crate::presenter`] documentation to know more
//! about presenters in general.

use crate::prelude::*;

use crate::executor::global::spawn_stream_handler;
use crate::presenter;
use crate::presenter::graph::ViewNodeId;

use enso_frp as frp;
use ide_view as view;
use ide_view::project::SearcherParams;



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
    status_bar:       view::status_bar::View,
    graph:            presenter::Graph,
    code:             presenter::Code,
    searcher:         RefCell<Option<presenter::Searcher>>,
}

impl Model {
    #[profile(Task)]
    fn new(
        ide_controller: controller::Ide,
        controller: controller::Project,
        init_result: controller::project::InitializationResult,
        view: view::project::View,
        status_bar: view::status_bar::View,
    ) -> Self {
        let logger = Logger::new("presenter::Project");
        let graph_controller = init_result.main_graph;
        let text_controller = init_result.main_module_text;
        let module_model = init_result.main_module_model;
        let graph = presenter::Graph::new(
            controller.model.clone_ref(),
            graph_controller.clone_ref(),
            &view,
        );
        let code = presenter::Code::new(text_controller, &view);
        let searcher = default();
        Model {
            logger,
            controller,
            module_model,
            graph_controller,
            ide_controller,
            view,
            status_bar,
            graph,
            code,
            searcher,
        }
    }

    fn setup_searcher_presenter(&self, params: SearcherParams) {
        let new_presenter = presenter::Searcher::setup_controller(
            &self.logger,
            self.ide_controller.clone_ref(),
            self.controller.clone_ref(),
            self.graph_controller.clone_ref(),
            &self.graph,
            self.view.clone_ref(),
            params,
        );
        match new_presenter {
            Ok(searcher) => {
                *self.searcher.borrow_mut() = Some(searcher);
            }
            Err(err) => {
                error!("Error while creating searcher integration: {err}");
            }
        }
    }

    fn editing_committed_old_searcher(
        &self,
        node: ViewNodeId,
        entry_id: Option<view::searcher::entry::Id>,
    ) -> bool {
        let searcher = self.searcher.take();
        if let Some(searcher) = searcher {
            let is_example = entry_id.map_or(false, |i| searcher.is_entry_an_example(i));
            if let Some(created_node) = searcher.commit_editing(entry_id) {
                self.graph.allow_expression_auto_updates(created_node, true);
                if is_example {
                    self.view.graph().enable_visualization(node);
                }
                false
            } else {
                true
            }
        } else {
            false
        }
    }

    fn editing_committed(
        &self,
        entry_id: Option<view::component_browser::component_list_panel::grid::GroupEntryId>,
    ) -> bool {
        let searcher = self.searcher.take();
        if let Some(searcher) = searcher {
            if let Some(created_node) = searcher.expression_accepted(entry_id) {
                self.graph.allow_expression_auto_updates(created_node, true);
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
            let input_node_view = searcher.input_view();
            if let Some(node) = self.graph.ast_node_of_view(input_node_view) {
                self.graph.allow_expression_auto_updates(node, true);
                searcher.abort_editing();
            } else {
                tracing::warn!("When porting editing the AST node of the node view {input_node_view} could not be found.");
            }
        } else {
            tracing::warn!("Editing aborted without searcher controller.");
        }
    }

    fn rename_project(&self, name: impl Str) {
        if self.controller.model.name() != name.as_ref() {
            let project = self.controller.model.clone_ref();
            let breadcrumbs = self.view.graph().model.breadcrumbs.clone_ref();
            let name = name.into();
            executor::global::spawn(async move {
                if let Err(e) = project.rename_project(name).await {
                    error!("The project couldn't be renamed: {e}");
                    breadcrumbs.cancel_project_name_editing.emit(());
                }
            });
        }
    }

    fn undo(&self) {
        debug!("Undo triggered in UI.");
        if let Err(e) = self.controller.model.urm().undo() {
            error!("Undo failed: {e}");
        }
    }

    fn redo(&self) {
        debug!("Redo triggered in UI.");
        if let Err(e) = self.controller.model.urm().redo() {
            error!("Redo failed: {e}");
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
    #[profile(Task)]
    pub fn new(
        ide_controller: controller::Ide,
        controller: controller::Project,
        init_result: controller::project::InitializationResult,
        view: view::project::View,
        status_bar: view::status_bar::View,
    ) -> Self {
        let network = frp::Network::new("presenter::Project");
        let model = Model::new(ide_controller, controller, init_result, view, status_bar);
        let model = Rc::new(model);
        Self { network, model }.init()
    }

    #[profile(Detail)]
    fn init(self) -> Self {
        let model = &self.model;
        let network = &self.network;

        let view = &model.view.frp;
        let breadcrumbs = &model.view.graph().model.breadcrumbs;
        let graph_view = &model.view.graph().frp;

        frp::extend! { network
            eval view.searcher ([model](params) {
                if let Some(params) = params {
                    model.setup_searcher_presenter(*params)
                }
            });

            graph_view.remove_node <+ view.editing_committed_old_searcher.filter_map(f!([model]((node_view, entry)) {
                model.editing_committed_old_searcher(*node_view, *entry).as_some(*node_view)
            }));
            graph_view.remove_node <+ view.editing_committed.filter_map(f!([model]((node_view, entry)) {
                model.editing_committed(*entry).as_some(*node_view)
            }));
            eval_ view.editing_aborted(model.editing_aborted());

            eval breadcrumbs.output.project_name((name) {model.rename_project(name);});

            eval_ view.undo (model.undo());
            eval_ view.redo (model.redo());

            values_computed <- source::<()>();
            values_computed_first_time <- values_computed.constant(true).on_change().constant(());
            view.show_prompt <+ values_computed_first_time;
            view.values_updated <+ values_computed;
        }

        let graph_controller = self.model.graph_controller.clone_ref();

        self.init_analytics()
            .setup_notification_handler()
            .attach_frp_to_values_computed_notifications(graph_controller, values_computed)
    }

    fn init_analytics(self) -> Self {
        let network = &self.network;
        let project = &self.model.view;
        let graph = self.model.view.graph();
        frp::extend! { network
            eval_ graph.node_editing_started([]analytics::remote_log_event("graph_editor::node_editing_started"));
            eval_ graph.node_editing_finished([]analytics::remote_log_event("graph_editor::node_editing_finished"));
            eval_ graph.node_added([]analytics::remote_log_event("graph_editor::node_added"));
            eval_ graph.node_removed([]analytics::remote_log_event("graph_editor::node_removed"));
            eval_ graph.nodes_collapsed([]analytics::remote_log_event("graph_editor::nodes_collapsed"));
            eval_ graph.node_entered([]analytics::remote_log_event("graph_editor::node_enter_request"));
            eval_ graph.node_exited([]analytics::remote_log_event("graph_editor::node_exit_request"));
            eval_ graph.on_edge_endpoints_set([]analytics::remote_log_event("graph_editor::edge_endpoints_set"));
            eval_ graph.visualization_shown([]analytics::remote_log_event("graph_editor::visualization_shown"));
            eval_ graph.visualization_hidden([]analytics::remote_log_event("graph_editor::visualization_hidden"));
            eval_ graph.on_edge_endpoint_unset([]analytics::remote_log_event("graph_editor::connection_removed"));
            eval_ project.editing_committed([]analytics::remote_log_event("project::editing_committed"));
        }
        self
    }

    fn setup_notification_handler(self) -> Self {
        let notifications = self.model.controller.model.subscribe();
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, notifications, |notification, model| {
            info!("Processing notification {notification:?}");
            let message = match notification {
                model::project::Notification::ConnectionLost(_) =>
                    crate::BACKEND_DISCONNECTED_MESSAGE,
            };
            let message = view::status_bar::event::Label::from(message);
            model.status_bar.add_event(message);
            std::future::ready(())
        });
        self
    }

    fn attach_frp_to_values_computed_notifications(
        self,
        graph: controller::ExecutedGraph,
        values_computed: frp::Source<()>,
    ) -> Self {
        let weak = Rc::downgrade(&self.model);
        let notifications = graph.subscribe();
        spawn_stream_handler(weak, notifications, move |notification, _| {
            if let controller::graph::executed::Notification::ComputedValueInfo(_) = notification {
                values_computed.emit(());
            }
            std::future::ready(())
        });
        self
    }

    /// Initialize project and return working presenter.
    ///
    /// This calls the [`controller::Project::initialize`] method and use the initialization result
    /// to construct working presenter.
    #[profile(Task)]
    pub async fn initialize(
        ide_controller: controller::Ide,
        controller: controller::Project,
        view: view::project::View,
        status_bar: view::status_bar::View,
    ) -> FallibleResult<Self> {
        let init_result = controller.initialize().await?;
        Ok(Self::new(ide_controller, controller, init_result, view, status_bar))
    }
}
