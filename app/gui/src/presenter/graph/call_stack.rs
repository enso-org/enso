//! The module with [`CallStack`] presenter. See [`crate::presenter`] documentation to know
//! more about presenters in general.

use crate::prelude::*;

use crate::model::execution_context::LocalCall;
use crate::presenter::graph::state::State;
use crate::presenter::graph::ViewNodeId;

use enso_frp as frp;
use ensogl_breadcrumbs::Breadcrumb;
use ide_view as view;



// =============
// === Model ===
// =============

#[derive(Debug)]
struct Model {
    controller:    controller::ExecutedGraph,
    view:          view::project::View,
    state:         Rc<State>,
    // Full stack displayed in the breadcrumbs. Includes the deeper levels, that might not be
    // active due to the current selection.
    stack_history: Rc<RefCell<Vec<view::project_view_top_bar::LocalCall>>>,
}

impl Model {
    /// Default constructor. Creates a new model with empty stack.
    fn new(
        controller: controller::ExecutedGraph,
        view: view::project::View,
        state: Rc<State>,
    ) -> Self {
        Self { controller, view, state, stack_history: default() }
    }

    /// Initialize the breadcrumbs view. Initially there is only the main module.
    fn init_breadcrumb_view(&self) {
        let breadcrumbs = &self.view.top_bar().breadcrumbs;
        breadcrumbs.clear();
        breadcrumbs.push(Breadcrumb::new_without_icon("main"));
    }

    /// Returns a closure that stores the call stack in the project metadata. This will store the
    /// callstack at the time of calling the closure.
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


    // === UI Control API ===
    // Methods that modify the UI state of the breadcrumbs. Note that they will always propagate
    // the changes to the controller. These should be exclusivity used to update the state of the
    // call stack. They will result in the breadcrumbs view being updated, which in turn will
    // result in the methods from the `UI Driven API` being called to update the controller.
    // They are also update the internal stack history, which should always be in sync with the
    // visible breadcrumbs.

    /// Add a part of the call stack. Note that this will check if the stack is already in the
    /// breadcrumbs and if so, it will only update the selection.
    fn add_stack_levels(
        &self,
        stack: Vec<view::project_view_top_bar::LocalCall>,
        stack_pointer: usize,
    ) {
        if stack.is_empty() {
            return;
        }

        self.stack_history.borrow_mut().truncate(stack_pointer);
        self.stack_history.borrow_mut().extend(stack.clone());

        let stack = stack.into_iter().map(|call| call.into()).collect_vec();
        let breadcrumb_index = stack_pointer + 1;
        self.view.top_bar().breadcrumbs.set_entries_from((stack, breadcrumb_index));
    }

    /// Move the selection in the breadcrumbs to the left. This will result in the controller
    /// exiting the stack by the given number of levels.
    fn shift_breadcrumb_selection_left(&self, count: usize) {
        let breadcrumbs = &self.view.top_bar().breadcrumbs;
        for _ in 0..count {
            breadcrumbs.move_up();
        }
    }


    // === UI Driven API ===
    // These methods are used by the UI (Graph editor and Breadcrumbs) to update the call stack.
    // Note that the state of the breadcrumbs is the main source of modifications to the call
    // stack. Any modification to the breadcrumbs will be propagated to the controller, thus care
    // needs to be taken to avoid infinite loops and any updates to the call stack should go
    // through the breadcrumbs.

    /// Method to call when exiting a node in the graph editor. This will move the selection in
    /// the breadcrumbs (and the call stack itself) one level up.
    fn node_exited(&self) {
        self.shift_breadcrumb_selection_left(1)
    }

    /// Method to call when entering a node in the graph editor. This will add a new level to the
    /// breadcrumbs. It will update the breadcrumbs in the following way: if the node was already
    /// in the breadcrumbs, it will move the selection to that node, otherwise it will clear all
    /// the deeper levels and add the new node at the end.
    fn node_entered(&self, node_id: ViewNodeId) {
        analytics::remote_log_event("integration::node_entered");
        if let Some(call) = self.state.ast_node_id_of_view(node_id) {
            if let Ok(computed_value) = self.controller.node_computed_value(call) {
                if let Some(method_pointer) = computed_value.method_call.as_ref() {
                    let local_call = LocalCall { call, definition: method_pointer.clone() };
                    let stack_pointer = self.controller.call_stack().len();
                    self.add_stack_levels(
                        vec![view::project_view_top_bar::LocalCall {
                            call:       local_call.call,
                            definition: local_call.definition.into(),
                        }],
                        stack_pointer,
                    );
                } else {
                    debug!("Ignoring request to enter non-enterable node {call}.")
                }
            } else {
                debug!("Ignoring request to enter not computed node {call}.")
            }
        } else {
            error!("Cannot enter {node_id:?}: no AST node bound to the view.")
        }
    }

    /// Method to call when a breadcrumb is selected. This will update the call stack to match the
    /// selection.
    fn breadcrumb_selected(&self, index: usize) {
        let current_stack = self.controller.call_stack();
        if current_stack.len() >= index {
            self.pop_stack(current_stack.len() - index);
        } else {
            let to_push = self.stack_history.borrow().iter().skip(index - 1).cloned().collect_vec();
            if to_push.is_empty() {
                warn!("Cannot select breadcrumb {index}. No such item in stack history {stack_history:?}.", stack_history = self.stack_history);
                return;
            }
            self.push_stack(to_push);
        }
    }

    fn visible_breadcrumbs(&self, entries: &[Breadcrumb]) {
        debug!("Visible Breadcrumbs changed to {entries:?}");
        debug_assert_eq!(
            entries.len(),
            self.stack_history.borrow().len() + 1,
            "Visible breadcrumbs should be equal to stack history, but are {entries:#?}, \
            while stack history is {stack_history:#?}.",
            stack_history = self.stack_history
        );
    }


    // === Controller API ===
    // These methods are used to update the state of the call stack in the controller. They are
    // called by the UI driven logic and propagate the changes to the controller.

    /// Push a new call stack to the current stack. This will notify the controller to enter the
    /// stack. If the controller fails to enter the stack, the last item from breadcrumbs will be
    /// popped again.
    pub fn push_stack(&self, stack: Vec<view::project_view_top_bar::LocalCall>) {
        let store_stack = self.store_updated_stack_task();
        let controller = self.controller.clone_ref();
        let breadcrumbs = self.view.top_bar().breadcrumbs.clone_ref();
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
                    // Revert the breadcrumbs
                    breadcrumbs.pop();
                    // Log the error.
                    debug!("Entering stack failed: {error}.");
                    let event = "integration::entering_node_failed";
                    let field = "error";
                    let data = analytics::AnonymousData(|| error.to_string());
                    analytics::remote_log_value(event, field, data);
                }
            }
        });
    }

    /// Pop a part of the call stack. This will notify the controller to exit the stack.
    pub fn pop_stack(&self, frame_count: usize) {
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
}



// ======================
// === CallStack ===
// ======================

fn to_local_call_stack(stack: Vec<LocalCall>) -> Vec<view::project_view_top_bar::LocalCall> {
    stack
        .into_iter()
        .map(|frame| view::project_view_top_bar::LocalCall {
            call:       frame.call,
            definition: frame.definition.into(),
        })
        .collect()
}

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
        graph_editor_view: view::graph_editor::GraphEditor,
        project_view: view::project::View,
        state: Rc<State>,
    ) -> Self {
        let network = frp::Network::new("presenter::graph::CallStack");
        let model = Rc::new(Model::new(controller, project_view, state));
        let view = &model.view;
        let breadcrumbs = &view.top_bar().breadcrumbs;

        frp::extend! { network
            eval graph_editor_view.node_entered ((node) model.node_entered(*node));
            eval_ graph_editor_view.node_exited (model.node_exited());

            selected_update <- breadcrumbs.selected.on_change();
            eval selected_update ((index) model.breadcrumb_selected(*index));
            entried_update <- breadcrumbs.entries.on_change();
            eval entried_update ((entries) model.visible_breadcrumbs(entries));
        }

        Self { _network: network, model }.initialize_breadcrumbs()
    }

    /// Initialize the breadcrumbs view. Initially there is only the main module.
    fn initialize_breadcrumbs(self) -> Self {
        self.model.init_breadcrumb_view();
        let stack = self.model.controller.call_stack();
        let call_stack = to_local_call_stack(stack);
        self.model.add_stack_levels(call_stack, 1); // 1 because of the main module
        self
    }
}
