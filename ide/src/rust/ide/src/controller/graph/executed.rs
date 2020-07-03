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

use enso_protocol::language_server::MethodPointer;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[fail(display = "The node {} has not been evaluated yet.", _0)]
#[derive(Debug,Fail,Clone,Copy)]
pub struct NotEvaluatedYet(double_representation::node::Id);

#[allow(missing_docs)]
#[fail(display = "The node {} does not resolve to a method call.", _0)]
#[derive(Debug,Fail,Clone,Copy)]
pub struct NoResolvedMethod(double_representation::node::Id);



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
    /// Notification emitted when the node has been entered.
    EnteredNode(double_representation::node::Id),
    /// Notification emitted when the node was step out.
    SteppedOutOfNode(double_representation::node::Id),
}



// ==============
// === Handle ===
// ==============
/// Handle providing executed graph controller interface.
#[derive(Clone,CloneRef,Debug)]
pub struct Handle {
    #[allow(missing_docs)]
    pub logger:Logger,
    /// A handle to basic graph operations.
    graph:Rc<RefCell<controller::Graph>>,
    /// Execution Context handle, its call stack top contains `graph`'s definition.
    execution_ctx:Rc<ExecutionContext>,
    /// The handle to project controller is necessary, as entering nodes might need to switch
    /// modules, and only the project can provide their controllers.
    project:controller::Project,
    /// The publisher allowing sending notification to subscribed entities. Note that its outputs is
    /// merged with publishers from the stored graph and execution controllers.
    notifier:crate::notification::Publisher<Notification>,
}

impl Handle {
    /// Create handle for the executed graph that will be running the given method.
    pub async fn new
    ( project:&controller::Project
    , method:MethodPointer
    ) -> FallibleResult<Self> {
        let graph     = controller::Graph::new_method(&project, &method).await?;
        let execution = project.create_execution_context(method.clone()).await?;
        Ok(Self::new_internal(graph,&project,execution))
    }

    /// Create handle for given graph and execution context.
    ///
    /// The given graph and execution context must be for the same method. Prefer using `new`,
    /// unless writing test code.
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
    pub fn new_internal
    ( graph:controller::Graph
    , project:&controller::Project
    , execution_ctx:Rc<ExecutionContext>
    ) -> Self {
        let logger   = Logger::sub(&graph.logger,"Executed");
        let graph    = Rc::new(RefCell::new(graph));
        let project  = project.clone_ref();
        let notifier = default();
        Handle {logger,graph,execution_ctx,project,notifier}
    }

    /// See `attach_visualization` in `ExecutionContext`.
    pub async fn attach_visualization
    (&self, visualization:Visualization)
    -> FallibleResult<impl Stream<Item=VisualizationUpdateData>> {
        self.execution_ctx.attach_visualization(visualization).await
    }

    /// See `detach_visualization` in `ExecutionContext`.
    pub async fn detach_visualization(&self, id:VisualizationId) -> FallibleResult<Visualization> {
        self.execution_ctx.detach_visualization(id).await
    }

    /// See `detach_all_visualizations` in `ExecutionContext`.
    pub async fn detach_all_visualizations(&self) -> Vec<FallibleResult<Visualization>> {
        self.execution_ctx.detach_all_visualizations().await
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
        let value_stream = registry.subscribe().map(Notification::ComputedValueInfo).boxed_local();
        let graph_stream = self.graph().subscribe().map(Notification::Graph).boxed_local();
        let self_stream  = self.notifier.subscribe().boxed_local();
        futures::stream::select_all(vec![value_stream,graph_stream,self_stream])
    }

    /// Enter node by given ID.
    ///
    /// This will cause pushing a new stack frame to the execution context and changing the graph
    /// controller to point to a new definition.
    ///
    /// Fails if there's no information about target method pointer (e.g. because node value hasn't
    /// been yet computed by the engine) or if method graph cannot be created (see
    /// `graph_for_method` documentation).
    pub async fn enter_node(&self, node:double_representation::node::Id) -> FallibleResult<()> {
        debug!(self.logger, "Entering node {node}.");
        let registry   = self.execution_ctx.computed_value_info_registry();
        let node_info  = registry.get(&node).ok_or_else(|| NotEvaluatedYet(node))?;
        let method_ptr = node_info.method_call.as_ref().ok_or_else(|| NoResolvedMethod(node))?;
        let graph      = controller::Graph::new_method(&self.project,&method_ptr).await?;
        let call       = model::execution_context::LocalCall {
            call       : node,
            definition : method_ptr.clone()
        };
        self.execution_ctx.push(call).await?;

        debug!(self.logger,"Replacing graph with {graph:?}.");
        self.graph.replace(graph);
        debug!(self.logger,"Sending graph invalidation signal.");
        self.notifier.publish(Notification::EnteredNode(node)).await;

        Ok(())
    }

    /// Leave the current node. Reverse of `enter_node`.
    ///
    /// Fails if this execution context is already at the stack's root or if the parent graph
    /// cannot be retrieved.
    pub async fn exit_node(&self) -> FallibleResult<()> {
        let frame  = self.execution_ctx.pop().await?;
        let method = self.execution_ctx.current_method();
        let graph  = controller::Graph::new_method(&self.project,&method).await?;
        self.graph.replace(graph);
        self.notifier.publish(Notification::SteppedOutOfNode(frame.call)).await;
        Ok(())
    }

    /// Get the controller for the currently active graph.
    ///
    /// Note that the controller returned by this method may change as the nodes are stepped into.
    pub fn graph(&self) -> controller::Graph {
        self.graph.borrow().clone_ref()
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
        let project        = controller::project::test::setup_mock_project(|_| {}, |_| {});
        let executed_graph = Handle::new_internal(graph,&project,execution.clone_ref());

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
