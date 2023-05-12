//! The module with the [`Visualization`] presenter. See [`crate::presenter`] documentation to know
//! more about presenters in general.

use crate::prelude::*;

use crate::controller::visualization::manager;
use crate::controller::visualization::manager::Manager;
use crate::executor::global::spawn_stream_handler;
use crate::model::execution_context::VisualizationUpdateData;
use crate::presenter::graph;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;

use enso_frp as frp;
use ide_view as view;
use ide_view::graph_editor::component::node as node_view;
use ide_view::graph_editor::component::visualization as visualization_view;



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
struct Model {
    controller:    controller::Visualization,
    graph_view:    view::graph_editor::GraphEditor,
    manager:       Rc<Manager>,
    error_manager: Rc<Manager>,
    state:         Rc<graph::state::State>,
}

impl Model {
    /// Handle the showing visualization UI.
    fn visualization_shown(&self, node_id: ViewNodeId, metadata: visualization_view::Metadata) {
        self.update_visualization(node_id, &self.manager, Some(metadata));
    }

    /// Handle the hiding in UI.
    fn visualization_hidden(&self, node_id: view::graph_editor::NodeId) {
        self.update_visualization(node_id, &self.manager, None);
    }

    /// Handle the node removal in UI.
    fn node_removed(&self, node_id: view::graph_editor::NodeId) {
        if self.state.ast_node_id_of_view(node_id).is_some() {
            self.update_visualization(node_id, &self.manager, None);
            self.update_visualization(node_id, &self.error_manager, None);
        }
    }

    /// Handle the preprocessor change requested by visualization.
    fn visualization_preprocessor_changed(
        &self,
        node_id: ViewNodeId,
        preprocessor: visualization_view::instance::PreprocessorConfiguration,
    ) {
        let metadata = visualization_view::Metadata { preprocessor };
        self.update_visualization(node_id, &self.manager, Some(metadata))
    }

    /// Handle the error change on given node: attach/detach the error visualization if needed.
    fn error_on_node_changed(&self, node_id: ViewNodeId, error: &Option<node_view::Error>) {
        use view::graph_editor::builtin::visualization::native::error as error_visualization;
        let error_kind = error.as_ref().map(|error| *error.kind);
        let needs_error_vis = error_kind.contains(&node_view::error::Kind::Dataflow);
        let metadata = needs_error_vis.then(error_visualization::metadata);
        self.update_visualization(node_id, &self.error_manager, metadata);
    }

    /// Route the metadata description as a desired visualization state to the Manager.
    fn update_visualization(
        &self,
        node_id: ViewNodeId,
        manager: &Rc<Manager>,
        metadata: Option<visualization_view::Metadata>,
    ) {
        if let Some(target_id) = self.state.ast_node_id_of_view(node_id) {
            manager.set_visualization(target_id, metadata);
        } else {
            error!("Failed to update visualization: {node_id:?} does not represent any AST code.")
        }
    }

    /// Pass the value update received from controllers to the Graph view appropriate endpoint.
    ///
    /// The `update_endpoint` should be `set_visualization_data` or `set_error_visualization_data`,
    /// of [`ide_view::graph_editor::GraphEditor`].
    #[profile(Debug)]
    fn handle_value_update(
        &self,
        update_endpoint: &frp::Source<(ViewNodeId, visualization_view::Data)>,
        target: AstNodeId,
        data: VisualizationUpdateData,
    ) {
        if let Some(view_id) = self.state.view_id_of_ast_node(target) {
            match deserialize_visualization_data(data) {
                Ok(data) => update_endpoint.emit((view_id, data)),
                Err(err) => {
                    // TODO [mwu]: We should consider having the visualization also accept error
                    //     input.
                    error!("Failed to deserialize visualization update: {err}");
                }
            }
        }
    }

    /// Handle the visualization update failure by passing the affected view in the given FPR
    /// endpoint.
    fn handle_controller_failure(
        &self,
        failure_endpoint: &frp::Source<ViewNodeId>,
        node: AstNodeId,
    ) {
        if let Some(node_view) = self.state.view_id_of_ast_node(node) {
            failure_endpoint.emit(node_view);
        }
    }

    /// Load the available visualizations to the view.
    ///
    /// See also [`controller::Visualization`] for information about loaded visualizations.
    #[profile(Detail)]
    fn load_visualizations(&self) {
        self.graph_view.reset_visualization_registry();
        let controller = self.controller.clone_ref();
        let graph_editor = self.graph_view.clone_ref();
        executor::global::spawn(async move {
            let identifiers = controller.list_visualizations().await;
            let identifiers = identifiers.unwrap_or_default();
            for identifier in identifiers {
                match controller.load_visualization(&identifier).await {
                    Ok(visualization) => {
                        graph_editor.frp.register_visualization.emit(Some(visualization));
                    }
                    Err(err) => {
                        error!("Error while loading visualization {identifier}: {err:?}");
                    }
                }
            }
            info!("Visualizations Initialized.");
        });
    }
}



// =====================
// === Visualization ===
// =====================

/// Visualization Presenter, synchronizing the visualization attached in the Engine with the
/// visualization shown in the view, including the error visualizations.
#[derive(Debug)]
pub struct Visualization {
    _network: frp::Network,
    model:    Rc<Model>,
}

impl Visualization {
    /// Constructor. The returned structure is does not require any further initialization.
    pub fn new(
        project: model::Project,
        graph: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
        state: Rc<graph::state::State>,
    ) -> Self {
        let network = frp::Network::new("presenter::graph::Visualization");

        let controller = project.visualization().clone_ref();
        let (manager, notifications) = Manager::new(graph.clone_ref());
        let (error_manager, error_notifications) = Manager::new(graph.clone_ref());
        let model = Rc::new(Model {
            controller,
            graph_view: view.clone_ref(),
            manager: manager.clone_ref(),
            error_manager: error_manager.clone_ref(),
            state,
        });

        frp::extend! { network
            eval view.visualization_shown (((node, metadata)) model.visualization_shown(*node, metadata.clone()));
            eval view.visualization_hidden ((node) model.visualization_hidden(*node));
            eval view.node_removed ((node) model.node_removed(*node));
            eval view.visualization_preprocessor_changed (((node, preprocessor)) model.visualization_preprocessor_changed(*node, preprocessor.clone_ref()));
            eval view.set_node_error_status (((node, error)) model.error_on_node_changed(*node, error));

            update <- source::<(ViewNodeId, visualization_view::Data)>();
            error_update <- source::<(ViewNodeId, visualization_view::Data)>();
            visualization_failure <- source::<ViewNodeId>();
            error_vis_failure <- source::<ViewNodeId>();

            view.set_visualization_data <+ update;
            view.set_error_visualization_data <+ error_update;
            view.disable_visualization <+ visualization_failure;

            eval_ view.visualization_registry_reload_requested (model.load_visualizations());
        }

        Self { model, _network: network }
            .spawn_visualization_handler(notifications, manager, update, visualization_failure)
            .spawn_visualization_handler(
                error_notifications,
                error_manager,
                error_update,
                error_vis_failure,
            )
            .setup_graph_listener(graph)
    }

    fn spawn_visualization_handler(
        self,
        notifier: impl Stream<Item = manager::Notification> + Unpin + 'static,
        manager: Rc<Manager>,
        update_endpoint: frp::Source<(ViewNodeId, visualization_view::Data)>,
        failure_endpoint: frp::Source<ViewNodeId>,
    ) -> Self {
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, notifier, move |notification, model| {
            info!("Received update for visualization: {notification:?}");
            match notification {
                manager::Notification::ValueUpdate { target, data, .. } => {
                    model.handle_value_update(&update_endpoint, target, data);
                }
                manager::Notification::FailedToAttach { visualization, error } => {
                    error!("Visualization {} failed to attach: {error}.", visualization.id);
                    model.handle_controller_failure(&failure_endpoint, visualization.expression_id);
                }
                manager::Notification::FailedToDetach { visualization, error } => {
                    error!("Visualization {} failed to detach: {error}.", visualization.id);
                    // Here we cannot really do much. Failing to detach might mean that
                    // visualization was already detached, that we detached it
                    // but failed to observe this (e.g. due to a connectivity
                    // issue) or that we did something really wrong. For now, we
                    // will just forget about this visualization. Better to unlikely "leak"
                    // it rather than likely break visualizations on the node altogether.
                    let forgotten = manager.forget_visualization(visualization.expression_id);
                    if let Some(forgotten) = forgotten {
                        error!("The visualization will be forgotten: {forgotten:?}")
                    }
                }
                manager::Notification::FailedToModify { desired, error } => {
                    error!(
                        "Visualization {} failed to be modified: {error}. Will hide it in GUI.",
                        desired.id
                    );
                    // Actually it would likely have more sense if we had just restored the previous
                    // visualization, as its LS state should be preserved. However, we already
                    // scrapped it on the GUI side and we don't even know its
                    // path anymore.
                    model.handle_controller_failure(&failure_endpoint, desired.expression_id);
                }
            }
            std::future::ready(())
        });
        self
    }

    fn setup_graph_listener(self, graph_controller: controller::ExecutedGraph) -> Self {
        use controller::graph;
        use controller::graph::executed::Notification;
        let notifications = graph_controller.subscribe();
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, notifications, move |notification, model| {
            match notification {
                Notification::Graph(graph::Notification::Invalidate)
                | Notification::EnteredStack(_)
                | Notification::ExitedStack(_) => match graph_controller.graph().nodes() {
                    Ok(nodes) => {
                        let nodes_set = nodes.into_iter().map(|n| n.id()).collect();
                        model.manager.retain_visualizations(&nodes_set);
                        model.error_manager.retain_visualizations(&nodes_set);
                    }
                    Err(err) => {
                        error!("Cannot update visualization after graph change: {err}");
                    }
                },
                _ => {}
            }
            std::future::ready(())
        });
        self
    }
}



// ========================
// === Helper Functions ===
// ========================

fn deserialize_visualization_data(
    data: VisualizationUpdateData,
) -> FallibleResult<visualization_view::Data> {
    let binary = data.as_ref();
    let as_text = std::str::from_utf8(binary)?;
    let as_json: serde_json::Value = serde_json::from_str(as_text)?;
    Ok(visualization_view::Data::from(as_json))
}
