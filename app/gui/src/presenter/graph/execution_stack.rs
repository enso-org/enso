use crate::prelude::*;

use crate::executor::global::spawn_stream_handler;
use crate::model::execution_context::LocalCall;
use crate::presenter::graph::state::State;
use crate::presenter::graph::ViewNodeId;
use enso_frp as frp;
use ide_view as view;

#[derive(Debug)]
struct Model {
    logger:     Logger,
    controller: controller::ExecutedGraph,
    view:       view::graph_editor::GraphEditor,
    state:      Rc<State>,
}

impl Model {
    fn new(
        parent: impl AnyLogger,
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
        state: Rc<State>,
    ) -> Self {
        let logger = parent.sub("presenter::graph::ExecutionStack");
        Self { logger, controller, view, state }
    }

    fn expression_entered(&self, local_call: &view::graph_editor::LocalCall) {
        let local_call = LocalCall {
            definition: (**local_call.definition).clone(),
            call:       local_call.call,
        };
        self.enter_expression(local_call);
    }

    fn node_entered(&self, node_id: ViewNodeId) {
        debug!(self.logger, "Requesting entering the node {node_id}.");
        if let Some(call) = self.state.ast_node_id_of_view(node_id) {
            match self.controller.node_method_pointer(call) {
                Ok(method_pointer) => {
                    let definition = (*method_pointer).clone();
                    let local_call = LocalCall { call, definition };
                    self.enter_expression(local_call);
                }
                Err(_) =>
                    info!(self.logger, "Ignoring request to enter non-enterable node {call}."),
            }
        } else {
            error!(self.logger, "Cannot enter {node_id:?}: no AST node bound to the view.");
        }
    }

    fn node_exited(&self) {
        debug!(self.logger, "Requesting exiting the current node.");
        let controller = self.controller.clone_ref();
        let logger = self.logger.clone_ref();
        executor::global::spawn(async move {
            info!(logger, "Exiting node.");
            if let Err(e) = controller.exit_node().await {
                debug!(logger, "Exiting node failed: {e}.");
            }
        });
    }

    fn enter_expression(&self, local_call: LocalCall) {
        let controller = self.controller.clone_ref();
        let logger = self.logger.clone_ref();
        executor::global::spawn(async move {
            info!(logger, "Entering expression {local_call:?}.");
            if let Err(e) = controller.enter_method_pointer(&local_call).await {
                error!(logger, "Entering node failed: {e}.");
            }
        });
    }
}

#[derive(Debug)]
pub struct ExecutionStack {
    network: frp::Network,
    model:   Rc<Model>,
}

impl ExecutionStack {
    pub fn new(
        parent: impl AnyLogger,
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
        state: Rc<State>,
    ) -> Self {
        let network = frp::Network::new("presenter::graph::ExecutionStack");
        let model = Rc::new(Model::new(parent, controller, view, state));
        let view = &model.view;
        let breadcrumbs = &view.model.breadcrumbs;

        frp::extend! { network
            eval view.node_entered ((node) model.node_entered(*node));
            eval_ view.node_exited (model.node_exited());

            eval_ breadcrumbs.output.breadcrumb_pop(model.node_exited());
            eval breadcrumbs.output.breadcrumb_push ([model](opt_local_call) {
                if let Some(local_call) = opt_local_call {
                    model.expression_entered(local_call);
                }
            });
        }

        Self { network, model }.setup_controller_notification_handlers()
    }

    fn setup_controller_notification_handlers(self) -> Self {
        use crate::controller::graph::executed::Notification;
        let graph_notifications = self.model.controller.subscribe();
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, graph_notifications, move |notification, model| {
            info!(model.logger, "Received controller notification {notification:?}");
            match notification {
                Notification::EnteredNode(frame) => {
                    let definition = frame.definition.clone().into();
                    let call = frame.call;
                    let local_call = view::graph_editor::LocalCall { call, definition };
                    model.view.model.breadcrumbs.push_breadcrumb(local_call);
                }
                Notification::SteppedOutOfNode(_) => model.view.model.breadcrumbs.pop_breadcrumb(),
                _ => {}
            }
            std::future::ready(())
        });
        self
    }
}
