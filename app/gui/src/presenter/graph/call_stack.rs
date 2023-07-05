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

#[derive(Debug)]
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
        let store_stack = self.store_updated_stack_task();
        let controller = self.controller.clone_ref();
        executor::global::spawn(async move {
            let stack = stack
                .into_iter()
                .map(|local_call| LocalCall {
                    definition: (**local_call.definition).clone(),
                    call:       local_call.call,
                })
                .collect();
            info!("Entering call stack {stack:?}.");
            match controller.enter_stack(stack).await {
                Ok(()) => store_stack(),
                Err(error) => {
                    error!("Entering stack failed: {error}.");
                    let event = "integration::entering_node_failed";
                    let field = "error";
                    let data = analytics::AnonymousData(|| error.to_string());
                    analytics::remote_log_value(event, field, data);
                }
            }
        });
    }

    fn pop_stack(&self, frame_count: usize) {
        debug!("Requesting exiting a part of the call stack.");
        analytics::remote_log_event("integration::node_exited");
        let controller = self.controller.clone_ref();
        let store_stack = self.store_updated_stack_task();
        executor::global::spawn(async move {
            info!("Exiting stack.");
            match controller.exit_stack(frame_count).await {
                Ok(()) => store_stack(),
                Err(error) => {
                    error!("Exiting stack failed: {error}");
                    let event = "integration::exiting_node_failed";
                    let field = "error";
                    let data = analytics::AnonymousData(|| error.to_string());
                    analytics::remote_log_value(event, field, data)
                }
            }
        });
    }

    fn node_entered(&self, node_id: ViewNodeId) {
        debug!("Requesting entering the node {node_id}.");
        analytics::remote_log_event("integration::node_entered");
        if let Some(call) = self.state.ast_node_id_of_view(node_id) {
            if let Ok(computed_value) = self.controller.node_computed_value(call) {
                if let Some(method_pointer) = computed_value.method_call.as_ref() {
                    let controller = self.controller.clone_ref();
                    let local_call = LocalCall { call, definition: method_pointer.clone() };
                    let store_stack = self.store_updated_stack_task();
                    executor::global::spawn(async move {
                        info!("Entering expression {local_call:?}.");
                        match controller.enter_stack(vec![local_call]).await {
                            Ok(()) => store_stack(),
                            Err(error) => {
                                error!("Entering node failed: {error}.");
                                let event = "integration::entering_node_failed";
                                let field = "error";
                                let data = analytics::AnonymousData(|| error.to_string());
                                analytics::remote_log_value(event, field, data);
                            }
                        };
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
        self.pop_stack(1)
    }

    fn store_updated_stack_task(&self) -> impl FnOnce() {
        let main_module = self.controller.graph().module.clone_ref();
        let controller = self.controller.clone_ref();
        move || {
            let _transaction = main_module
                .undo_redo_repository()
                .open_ignored_transaction_or_ignore_current("Updating call stack metadata");
            let new_call_stack = controller.call_stack();
            let result = main_module.update_project_metadata(|metadata| {
                metadata.call_stack = new_call_stack;
            });
            if let Err(error) = result {
                // We cannot really do anything when updating metadata fails.
                // Can happen in improbable case of serialization failure.
                error!("Failed to store an updated call stack: {error}");
            }
        }
    }

    fn add_breadcrumbs_in_view(&self, stack: Vec<LocalCall>) {
        let view_stack = stack
            .into_iter()
            .map(|frame| view::graph_editor::LocalCall {
                call:       frame.call,
                definition: frame.definition.into(),
            })
            .collect::<Vec<_>>();
        self.view.model.breadcrumbs.push_breadcrumbs(view_stack);
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
                Notification::EnteredStack(stack) => model.add_breadcrumbs_in_view(stack),
                Notification::ExitedStack(count) =>
                    model.view.model.breadcrumbs.pop_breadcrumbs(count),
                _ => {}
            }
            std::future::ready(())
        });
        self
    }

    fn initialize_breadcrumbs(self) -> Self {
        let stack = self.model.controller.call_stack();
        self.model.add_breadcrumbs_in_view(stack);
        self
    }
}
