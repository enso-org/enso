//! A module with Executed Graph Controller.
//!
//! This controller provides operations on a specific graph with some execution context - these
//! operations usually involves retrieving values on nodes: that's are i.e. operations on
//! visualisations, retrieving types on ports, etc.
use crate::prelude::*;

use crate::model::execution_context::ComputedValueInfoRegistry;
use crate::model::execution_context::Visualization;
use crate::model::execution_context::VisualizationId;
use crate::model::execution_context::VisualizationUpdateData;
use crate::model::synchronized::ExecutionContext;



// ====================
// === Notification ===
// ====================

/// Notification about change in the executed graph.
///
/// It may pertain either the state of the graph itself or the notifications from the execution.
#[derive(Clone,Debug,PartialEq)]
pub enum Notification {
    /// The notification passed from the graph controller.
    Graph(crate::controller::graph::Notification),
    /// The notification from the execution context about the computed value information
    /// being updated.
    ComputedValueInfo(crate::model::execution_context::ComputedValueExpressions),
}



// ==============
// === Handle ===
// ==============
/// Handle providing executed graph controller interface.
#[derive(Clone,CloneRef,Debug)]
pub struct Handle {
    /// A handle to basic graph operations.
    pub graph:controller::Graph,
    /// Execution Context handle, its call stack top contains `graph`'s definition.
    execution_ctx:Rc<ExecutionContext>,
}

impl Handle {
    /// Create handle for given graph and execution context.
    ///
    /// This takes a (shared) ownership of execution context which will be shared between all copies
    /// of this handle. It is held through `Rc` because the registry in the project controller needs
    /// to store a weak handle to the execution context as well (to be able to properly route some
    /// notifications, like visualization updates).
    ///
    /// However, in a typical setup, this controller handle (and its copies) shall be the only
    /// strong references to the execution context and it is expected that it will be dropped after
    /// the last copy of this controller is dropped.
    /// Then the context when being dropped shall remove itself from the Language Server.
    pub fn new(graph:controller::Graph, execution_ctx:Rc<ExecutionContext>) -> Self {
        Handle{graph,execution_ctx}
    }

    /// See `attach_visualization` in `ExecutionContext`.
    pub async fn attach_visualization
    (&self, visualization:Visualization)
    -> FallibleResult<impl Stream<Item=VisualizationUpdateData>> {
        self.execution_ctx.attach_visualization(visualization).await
    }

    /// See `detach_visualization` in `ExecutionContext`.
    pub async fn detach_visualization(&self, id:&VisualizationId) -> FallibleResult<Visualization> {
        self.execution_ctx.detach_visualization(id).await
    }

    /// See `expression_info_registry` in `ExecutionContext`.
    pub fn computed_value_info_registry(&self) -> &ComputedValueInfoRegistry {
        self.execution_ctx.computed_value_info_registry()
    }

    /// Subscribe to updates about changes in this executed graph.
    ///
    /// The stream of notification contains both notifications from the graph and from the execution
    /// context.
    pub fn subscribe(&self) -> impl Stream<Item=Notification> {
        let registry     = self.execution_ctx.computed_value_info_registry();
        let value_stream = registry.subscribe().map(Notification::ComputedValueInfo);
        let graph_stream = self.graph.subscribe().map(Notification::Graph);
        futures::stream::select(value_stream,graph_stream)
    }
}

impl Deref for Handle {
    type Target = controller::Graph;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use enso_protocol::language_server;
    use utils::test::traits::*;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    /// Test that checks that value computed notification is properly relayed by the executed graph.
    #[wasm_bindgen_test]
    fn dispatching_value_computed_notification() {
        // Setup the controller.
        let mut fixture    = TestWithLocalPoolExecutor::set_up();
        let mut ls         = language_server::MockClient::default();
        let execution_data = model::synchronized::execution_context::tests::MockData::new();
        let execution      = execution_data.context_provider(&mut ls);
        let graph_data     = controller::graph::tests::MockData::new_inline("1 + 2");
        let connection     = language_server::Connection::new_mock_rc(ls);
        let (_,graph)      = graph_data.create_controllers_with_ls(connection.clone_ref());
        let execution      = Rc::new(execution(connection.clone_ref()));
        let executed_graph = Handle::new(graph,execution.clone_ref());

        // Generate notification.
        let notification = execution_data.mock_values_computed_update();
        let update       = &notification.updates[0];

        // Notification not yet send.
        let registry          = executed_graph.computed_value_info_registry();
        let mut notifications = executed_graph.subscribe().boxed_local();
        notifications.expect_pending();
        assert!(registry.get(&update.id).is_none());

        // Sending notification.
        execution.handle_expression_values_computed(notification.clone()).unwrap();
        fixture.run_until_stalled();

        // Observing that notification was relayed.
        let observed_notification = notifications.expect_next();
        let typename_in_registry  = registry.get(&update.id).unwrap().typename.clone();
        let expected_typename     = update.typename.clone().map(ImString::new);
        assert_eq!(observed_notification,Notification::ComputedValueInfo(vec![update.id]));
        assert_eq!(typename_in_registry,expected_typename);
        notifications.expect_pending();
    }
}
