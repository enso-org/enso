mod manager;

use crate::prelude::*;

use crate::presenter::graph;
use crate::presenter::graph::visualization::manager::Manager;

use crate::executor::global::spawn_stream_handler;
use crate::model::execution_context::VisualizationUpdateData;
use crate::presenter::graph::AstNodeId;
use crate::presenter::graph::ViewNodeId;

use enso_frp as frp;
use ide_view as view;
use ide_view::graph_editor::component::node as node_view;
use ide_view::graph_editor::component::visualization as visualization_view;
use ide_view::graph_editor::GraphEditor;


fn deserialize_visualization_data(
    data: VisualizationUpdateData,
) -> FallibleResult<visualization_view::Data> {
    let binary = data.as_ref();
    let as_text = std::str::from_utf8(binary)?;
    let as_json: serde_json::Value = serde_json::from_str(as_text)?;
    Ok(visualization_view::Data::from(as_json))
}

#[derive(Clone, CloneRef, Debug)]
struct Model {
    logger:        Logger,
    manager:       Rc<Manager>,
    error_manager: Rc<Manager>,
    state:         Rc<graph::state::State>,
}

impl Model {
    fn visualization_shown(&self, node_id: ViewNodeId, metadata: visualization_view::Metadata) {
        self.update_visualization(node_id, &self.manager, Some(metadata));
    }

    fn visualization_hidden(&self, node_id: view::graph_editor::NodeId) {
        self.update_visualization(node_id, &self.manager, None);
    }

    fn visualization_preprocessor_changed(
        &self,
        node_id: ViewNodeId,
        preprocessor: visualization_view::instance::PreprocessorConfiguration,
    ) {
        let metadata = visualization_view::Metadata { preprocessor };
        self.update_visualization(node_id, &self.manager, Some(metadata))
    }

    pub fn error_on_node_changed(&self, node_id: ViewNodeId, error: &Option<node_view::Error>) {
        use view::graph_editor::builtin::visualization::native::error as error_visualization;
        let error_kind = error.as_ref().map(|error| *error.kind);
        let needs_error_vis = error_kind.contains(&node_view::error::Kind::Dataflow);
        let metadata = needs_error_vis.then(error_visualization::metadata);
        DEBUG!("New errror no node {node_id:?}: {metadata:?}");
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
            error!(
                self.logger,
                "Failed to update visualization: {node_id:?} does not represent any AST code."
            )
        }
    }

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
                    error!(self.logger, "Failed to deserialize visualization update: {err}");
                }
            }
        }
    }

    fn handle_controller_failure(
        &self,
        failure_endpoint: &frp::Source<ViewNodeId>,
        node: AstNodeId,
    ) {
        if let Some(node_view) = self.state.view_id_of_ast_node(node) {
            failure_endpoint.emit(node_view);
        }
    }
}

#[derive(Debug)]
pub struct Visualization {
    network: frp::Network,
    model:   Rc<Model>,
}

impl Visualization {
    pub fn new(
        project: model::Project,
        graph: controller::ExecutedGraph,
        view: GraphEditor,
        state: Rc<graph::state::State>,
    ) -> Self {
        let logger = Logger::new("presenter::graph::Visualization");
        let network = frp::Network::new("presenter::graph::Visualization");

        let (manager, notifications) =
            Manager::new(&logger, graph.clone_ref(), project.clone_ref());
        let (error_manager, error_notifications) = Manager::new(&logger, graph, project);
        let model = Rc::new(Model {
            logger,
            manager: manager.clone_ref(),
            error_manager: error_manager.clone_ref(),
            state,
        });

        frp::extend! { network
            eval view.visualization_shown (((node, metadata)) model.visualization_shown(*node, metadata.clone()));
            eval view.visualization_hidden ((node) model.visualization_hidden(*node));
            eval view.visualization_preprocessor_changed (((node, preprocessor)) model.visualization_preprocessor_changed(*node, preprocessor.clone_ref()));
            eval view.set_node_error_status (((node, error)) model.error_on_node_changed(*node, error));

            update <- source::<(ViewNodeId, visualization_view::Data)>();
            error_update <- source::<(ViewNodeId, visualization_view::Data)>();
            visualization_failure <- source::<ViewNodeId>();
            error_vis_failure <- source::<ViewNodeId>();

            view.set_visualization_data <+ update;
            view.set_error_visualization_data <+ error_update;
            view.disable_visualization <+ visualization_failure;
        }

        Self { model, network }
            .spawn_visualization_handler(notifications, manager, update, visualization_failure)
            .spawn_visualization_handler(
                error_notifications,
                error_manager,
                error_update,
                error_vis_failure,
            )
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
            let logger = &model.logger;
            info!(logger, "Received update for visualization: {notification:?}");
            match notification {
                manager::Notification::ValueUpdate { target, data, .. } => {
                    model.handle_value_update(&update_endpoint, target, data);
                }
                manager::Notification::FailedToAttach { visualization, error } => {
                    error!(logger, "Visualization {visualization.id} failed to attach: {error}.");
                    model.handle_controller_failure(&failure_endpoint, visualization.expression_id);
                }
                manager::Notification::FailedToDetach { visualization, error } => {
                    error!(logger, "Visualization {visualization.id} failed to detach: {error}.");
                    // Here we cannot really do much. Failing to detach might mean that
                    // visualization was already detached, that we detached it
                    // but failed to observe this (e.g. due to a connectivity
                    // issue) or that we did something really wrong. For now, we
                    // will just forget about this visualization. Better to unlikely "leak"
                    // it rather than likely break visualizations on the node altogether.
                    let forgotten = manager.forget_visualization(visualization.expression_id);
                    if let Some(forgotten) = forgotten {
                        error!(logger, "The visualization will be forgotten: {forgotten:?}")
                    }
                }
                manager::Notification::FailedToModify { desired, error } => {
                    error!(
                        logger,
                        "Visualization {desired.id} failed to be modified: {error}. Will hide it in GUI."
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
}
