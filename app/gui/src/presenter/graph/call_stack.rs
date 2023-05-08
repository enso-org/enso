//! The module with [`CallStack`] presenter. See [`crate::presenter`] documentation to know
//! more about presenters in general.

use crate::prelude::*;

use crate::executor::global::spawn_stream_handler;
use crate::model::execution_context::LocalCall;
use crate::presenter::graph::state::State;
use crate::presenter::graph::ViewNodeId;

use enso_frp as frp;
use ide_view as view;



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
struct Model {
    controller: controller::ExecutedGraph,
    view:       view::graph_editor::GraphEditor,
    state:      Rc<State>,
}

impl Model {
    fn new(
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
        state: Rc<State>,
    ) -> Self {
        Self { controller, view, state }
    }

    fn push_stack(&self, stack: Vec<view::graph_editor::LocalCall>) {
        let self_ = self.clone_ref();
        executor::global::spawn(async move {
            for local_call in stack {
                let local_call = LocalCall {
                    definition: (**local_call.definition).clone(),
                    call:       local_call.call,
                };
                self_.enter_expression(local_call).await;
            }
        });
    }

    fn pop_stack(&self, count: usize) {
        for _ in 0..count {
            self.node_exited();
        }
    }

    fn node_entered(&self, node_id: ViewNodeId) {
        debug!("Requesting entering the node {node_id}.");
        analytics::remote_log_event("integration::node_entered");
        if let Some(call) = self.state.ast_node_id_of_view(node_id) {
            if let Ok(computed_value) = self.controller.node_computed_value(call) {
                if let Some(method_pointer) = computed_value.method_call.as_ref() {
                    let self_ = self.clone_ref();
                    let local_call = LocalCall { call, definition: method_pointer.clone() };
                    executor::global::spawn(async move {
                        self_.enter_expression(local_call).await;
                    });
                } else {
                    info!("Ignoring request to enter non-enterable node {call}.")
                }
            } else {
                info!("Ignoring request to enter not computed node {call}.")
            }
        } else {
            error!("Cannot enter {node_id:?}: no AST node bound to the view.")
        }
    }

    fn node_exited(&self) {
        debug!("Requesting exiting the current node.");
        analytics::remote_log_event("integration::node_exited");
        let controller = self.controller.clone_ref();
        let store_stack = self.store_updated_stack_task();
        executor::global::spawn(async move {
            info!("Exiting node.");
            match controller.exit_node().await {
                Ok(()) =>
                    if let Err(err) = store_stack() {
                        // We cannot really do anything when updating metadata fails.
                        // Can happen in improbable case of serialization failure.
                        error!("Failed to store an updated call stack: {err}");
                    },
                Err(err) => {
                    error!("Exiting node failed: {err}");

                    let event = "integration::exiting_node_failed";
                    let field = "error";
                    let data = analytics::AnonymousData(|| err.to_string());
                    analytics::remote_log_value(event, field, data)
                }
            }
        });
    }

    async fn enter_expression(&self, local_call: LocalCall) {
        let store_stack = self.store_updated_stack_task();
        info!("Entering expression {local_call:?}.");
        match self.controller.enter_method_pointer(&local_call).await {
            Ok(()) =>
                if let Err(err) = store_stack() {
                    // We cannot really do anything when updating metadata fails.
                    // Can happen in improbable case of serialization failure.
                    error!("Failed to store an updated call stack: {err}");
                },
            Err(err) => {
                error!("Entering node failed: {err}.");
                let event = "integration::entering_node_failed";
                let field = "error";
                let data = analytics::AnonymousData(|| err.to_string());
                analytics::remote_log_value(event, field, data)
            }
        };
    }

    fn store_updated_stack_task(&self) -> impl FnOnce() -> FallibleResult + 'static {
        let main_module = self.controller.graph().module.clone_ref();
        let controller = self.controller.clone_ref();
        move || {
            let new_call_stack = controller.call_stack();
            main_module.update_project_metadata(|metadata| {
                metadata.call_stack = new_call_stack;
            })
        }
    }

    fn add_breadcrumb_in_view(&self, frame: LocalCall) {
        let definition = frame.definition.clone().into();
        let call = frame.call;
        let local_call = view::graph_editor::LocalCall { call, definition };
        self.view.model.breadcrumbs.push_breadcrumb(local_call);
    }
}



// ======================
// === CallStack ===
// ======================

/// Call Stack presenter, synchronizing the call stack of currently displayed graph with
/// the breadcrumbs displayed above. It also handles the entering/exiting nodes requests.
#[derive(Debug)]
pub struct CallStack {
    _network: frp::Network,
    model:    Rc<Model>,
}

impl CallStack {
    /// Constructor. The returned presenter works right away.
    pub fn new(
        controller: controller::ExecutedGraph,
        view: view::graph_editor::GraphEditor,
        state: Rc<State>,
    ) -> Self {
        let network = frp::Network::new("presenter::graph::CallStack");
        let model = Rc::new(Model::new(controller, view, state));
        let view = &model.view;
        let breadcrumbs = &view.model.breadcrumbs;

        frp::extend! { network
            eval view.node_entered ((node) model.node_entered(*node));
            eval_ view.node_exited (model.node_exited());

            eval breadcrumbs.output.breadcrumb_push ((stack) model.push_stack(stack.clone()));
            eval breadcrumbs.output.breadcrumb_pop ((count) model.pop_stack(*count));
        }

        Self { _network: network, model }
            .initialize_breadcrumbs()
            .setup_controller_notification_handlers()
    }

    fn setup_controller_notification_handlers(self) -> Self {
        use crate::controller::graph::executed::Notification;
        let graph_notifications = self.model.controller.subscribe();
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, graph_notifications, move |notification, model| {
            info!("Received controller notification {notification:?}");
            match notification {
                Notification::EnteredNode(frame) => model.add_breadcrumb_in_view(frame),
                Notification::SteppedOutOfNode(_) => model.view.model.breadcrumbs.pop_breadcrumb(),
                _ => {}
            }
            std::future::ready(())
        });
        self
    }

    fn initialize_breadcrumbs(self) -> Self {
        for frame in self.model.controller.call_stack() {
            self.model.add_breadcrumb_in_view(frame)
        }
        self
    }
}
