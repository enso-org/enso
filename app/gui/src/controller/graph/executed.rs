//! A module with Executed Graph Controller.
//!
//! This controller provides operations on a specific graph with some execution context - these
//! operations usually involves retrieving values on nodes: that's are i.e. operations on
//! visualisations, retrieving types on ports, etc.

use crate::prelude::*;

use crate::model::execution_context::ComponentGroup;
use crate::model::execution_context::ComputedValueInfo;
use crate::model::execution_context::ComputedValueInfoRegistry;
use crate::model::execution_context::LocalCall;
use crate::model::execution_context::QualifiedMethodPointer;
use crate::model::execution_context::Visualization;
use crate::model::execution_context::VisualizationId;
use crate::model::execution_context::VisualizationUpdateData;
use crate::retry::retry_operation_errors_cap;

use double_representation::name::QualifiedName;
use engine_protocol::language_server::ExecutionEnvironment;
use engine_protocol::language_server::MethodPointer;
use futures::stream;
use futures::TryStreamExt;
use span_tree::generate::context::CalledMethodInfo;
use span_tree::generate::context::Context;
use std::assert_matches::debug_assert_matches;
use std::time::Duration;


// ==============
// === Export ===
// ==============

pub use crate::controller::graph::Connection;
pub use crate::controller::graph::Connections;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "The node {} has not been evaluated yet.", _0)]
pub struct NotEvaluatedYet(double_representation::node::Id);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "The node {} does not resolve to a method call.", _0)]
pub struct NoResolvedMethod(double_representation::node::Id);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "Operation is not permitted in read only mode.")]
pub struct ReadOnly;

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "Previous operation modifying call stack is still in progress.")]
pub struct SyncingStack;


// ====================
// === Notification ===
// ====================

/// Notification about change in the executed graph.
///
/// It may pertain either the state of the graph itself or the notifications from the execution.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Notification {
    /// The notification passed from the graph controller.
    Graph(controller::graph::Notification),
    /// The notification from the execution context about the computed value information
    /// being updated.
    ComputedValueInfo(model::execution_context::ComputedValueExpressions),
    /// Notification emitted when a call stack has been entered.
    EnteredStack(Vec<LocalCall>),
    /// Notification emitted when a number of frames of the call stack have been exited.
    ExitedStack(usize),
}



// ==============
// === Handle ===
// ==============

/// Handle providing executed graph controller interface.
#[derive(Clone, CloneRef, Debug)]
pub struct Handle {
    /// A handle to basic graph operations.
    graph:         Rc<RefCell<controller::Graph>>,
    /// Execution Context handle, its call stack top contains `graph`'s definition.
    execution_ctx: model::ExecutionContext,
    /// The handle to project controller is necessary, as entering nodes might need to switch
    /// modules, and only the project can provide their controllers.
    project:       model::Project,
    /// The publisher allowing sending notification to subscribed entities. Note that its outputs
    /// is merged with publishers from the stored graph and execution controllers.
    notifier:      notification::Publisher<Notification>,
    /// A mutex guarding a process syncing Execution Context stack with the current graph. As
    /// the syncing requires multiple async calls to the engine, and the stack updates depend on
    /// each other, we should not mix various operations (e.g. entering node while still in
    /// process of entering another node).
    syncing_stack: Rc<sync::SingleThreadMutex<()>>,
}

impl Handle {
    /// Create handle for the executed graph that will be running the given method.
    #[profile(Task)]
    pub async fn new(project: model::Project, method: MethodPointer) -> FallibleResult<Self> {
        let graph = controller::Graph::new_method(&project, &method).await?;
        let context_id = Uuid::new_v4();
        let execution = project.create_execution_context(method.clone(), context_id).await?;
        Ok(Self::new_internal(graph, project, execution))
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
    pub fn new_internal(
        graph: controller::Graph,
        project: model::Project,
        execution_ctx: model::ExecutionContext,
    ) -> Self {
        Handle {
            graph: Rc::new(RefCell::new(graph)),
            execution_ctx,
            project,
            notifier: default(),
            syncing_stack: default(),
        }
    }

    /// See [`model::ExecutionContext::when_ready`].
    pub fn when_ready(&self) -> StaticBoxFuture<Option<()>> {
        self.execution_ctx.when_ready()
    }

    /// See [`model::ExecutionContext::attach_visualization`].
    pub async fn attach_visualization(
        &self,
        visualization: Visualization,
    ) -> FallibleResult<impl Stream<Item = VisualizationUpdateData>> {
        self.execution_ctx.attach_visualization(visualization).await
    }

    /// See [`model::ExecutionContext::get_ai_completion`].
    pub async fn get_ai_completion(&self, code: &str, stop: &str) -> FallibleResult<String> {
        self.execution_ctx.get_ai_completion(code, stop).await
    }

    /// See [`model::ExecutionContext::modify_visualization`].
    pub fn modify_visualization(
        &self,
        id: VisualizationId,
        method_pointer: Option<QualifiedMethodPointer>,
        arguments: Option<Vec<String>>,
    ) -> BoxFuture<FallibleResult> {
        self.execution_ctx.modify_visualization(id, method_pointer, arguments)
    }

    /// See [`model::ExecutionContext::detach_visualization`].
    #[profile(Detail)]
    pub async fn detach_visualization(&self, id: VisualizationId) -> FallibleResult<Visualization> {
        self.execution_ctx.detach_visualization(id).await
    }

    /// See [`model::ExecutionContext::detach_all_visualizations`].
    #[profile(Detail)]
    pub async fn detach_all_visualizations(&self) -> Vec<FallibleResult<Visualization>> {
        self.execution_ctx.detach_all_visualizations().await
    }

    /// See [`model::ExecutionContext::expression_info_registry`].
    pub fn computed_value_info_registry(&self) -> &ComputedValueInfoRegistry {
        self.execution_ctx.computed_value_info_registry()
    }

    /// See [`model::ExecutionContext::component_groups`].
    pub fn component_groups(&self) -> Rc<Vec<ComponentGroup>> {
        self.execution_ctx.component_groups()
    }

    /// Subscribe to updates about changes in this executed graph.
    ///
    /// The stream of notification contains both notifications from the graph and from the execution
    /// context.
    pub fn subscribe(&self) -> impl Stream<Item = Notification> {
        let registry = self.execution_ctx.computed_value_info_registry();
        let value_stream = registry.subscribe().map(Notification::ComputedValueInfo).boxed_local();
        let graph_stream = self.graph().subscribe().map(Notification::Graph).boxed_local();
        let self_stream = self.notifier.subscribe().boxed_local();

        // Note: [Argument Names-related invalidations]
        let db_stream = self
            .project
            .suggestion_db()
            .subscribe()
            .map(|notification| match notification {
                model::suggestion_database::Notification::Updated =>
                    Notification::Graph(controller::graph::Notification::PortsUpdate),
            })
            .boxed_local();
        let update_stream = registry
            .subscribe()
            .map(|_| Notification::Graph(controller::graph::Notification::PortsUpdate))
            .boxed_local();

        let streams = vec![value_stream, graph_stream, self_stream, db_stream, update_stream];
        stream::select_all(streams)
    }

    // Note [Argument Names-related invalidations]
    // ===========================================
    // Currently the shape of span tree depends on how method calls are recognized and resolved.
    // This involves lookups in the metadata, computed values registry and suggestion database.
    // As such, any of these will lead to emission of invalidation signal, as span tree shape
    // affects the connection identifiers.
    //
    // This is inefficient and should be addressed in the future.
    // See: https://github.com/enso-org/ide/issues/787


    /// Get a type of the given expression as soon as it is available.
    pub fn expression_type(&self, id: ast::Id) -> StaticBoxFuture<Option<ImString>> {
        let registry = self.execution_ctx.computed_value_info_registry();
        registry.clone_ref().get_type(id)
    }

    /// Enter the given stack of nodes by node ID and method pointer.
    ///
    /// This will push new stack frames to the execution context and change the graph controller to
    /// point to a new definition.
    ///
    /// ### Errors
    /// - Fails if method graph cannot be created (see `graph_for_method` documentation).
    /// - Fails if the project is in read-only mode.
    pub async fn enter_stack(&self, stack: Vec<LocalCall>) -> FallibleResult {
        if self.project.read_only() {
            Err(ReadOnly.into())
        } else if let Some(last_call) = stack.last() {
            let _syncing = self.syncing_stack.try_lock().map_err(|_| SyncingStack)?;
            // Before adding new items to stack, first make sure we're actually able to construct
            // the graph controller.
            let graph = controller::Graph::new_method(&self.project, &last_call.definition).await?;
            let mut successful_calls = 0;
            let result = stream::iter(stack.iter())
                .then(|local_call| async {
                    info!("Entering node {}.", local_call.call);
                    self.execution_ctx.push(local_call.clone()).await?;
                    Ok(())
                })
                .map_ok(|()| successful_calls += 1)
                .try_collect::<()>()
                .await;
            match &result {
                Ok(()) => {
                    info!("Replacing graph with {graph:?}.");
                    self.graph.replace(graph);
                    info!("Sending graph invalidation signal.");
                    self.notifier.publish(Notification::EnteredStack(stack)).await;
                }
                Err(_) => {
                    let successful_calls_to_revert = iter::repeat(()).take(successful_calls);
                    for () in successful_calls_to_revert {
                        let error_msg = "Error while restoring execution context stack after \
                                    unsuccessful entering node";
                        let retry_result = retry_operation_errors_cap(
                            || self.execution_ctx.pop(),
                            self.retry_times_for_restoring_stack_operations(),
                            error_msg,
                            0,
                        )
                        .await;
                        debug_assert_matches!(FallibleResult::from(retry_result), Ok(_));
                    }
                }
            }

            result
        } else {
            // The stack passed as argument is empty; nothing to do.
            Ok(())
        }
    }

    /// Attempts to get the computed value of the specified node.
    ///
    /// Fails if there's no information e.g. because node value hasn't been yet computed by the
    /// engine.
    pub fn node_computed_value(
        &self,
        node: double_representation::node::Id,
    ) -> FallibleResult<Rc<ComputedValueInfo>> {
        let registry = self.execution_ctx.computed_value_info_registry();
        let node_info = registry.get(&node).ok_or(NotEvaluatedYet(node))?;
        Ok(node_info)
    }

    /// Leave the given number of stack frames. Reverse of `enter_stack`.
    ///
    /// ### Errors
    /// - Fails if this execution context is already at the stack's root or if the parent graph
    /// cannot be retrieved.
    /// - Fails if the project is in read-only mode.
    pub async fn exit_stack(&self, frame_count: usize) -> FallibleResult {
        if self.project.read_only() {
            Err(ReadOnly.into())
        } else {
            let method = self.execution_ctx.method_at_frame_back(frame_count)?;
            let _syncing = self.syncing_stack.try_lock().map_err(|_| SyncingStack)?;
            let graph = controller::Graph::new_method(&self.project, &method).await?;

            let mut successful_pops = Vec::new();
            let result = stream::iter(iter::repeat(()).take(frame_count))
                .then(|()| self.execution_ctx.pop())
                .map_ok(|local_call| successful_pops.push(local_call))
                .try_collect::<()>()
                .await;
            match &result {
                Ok(()) => {
                    self.graph.replace(graph);
                    self.notifier.publish(Notification::ExitedStack(frame_count)).await;
                }
                Err(_) =>
                    for frame in successful_pops.into_iter().rev() {
                        let error_msg = "Error while restoring execution context stack after \
                                    unsuccessful leaving node";
                        let retry_result = retry_operation_errors_cap(
                            || self.execution_ctx.push(frame.clone()),
                            self.retry_times_for_restoring_stack_operations(),
                            error_msg,
                            0,
                        )
                        .await;
                        debug_assert_matches!(FallibleResult::from(retry_result), Ok(_));
                    },
            }
            result
        }
    }

    fn retry_times_for_restoring_stack_operations(&self) -> impl Iterator<Item = Duration> {
        iter::repeat(()).scan(Duration::from_secs(1), |delay, ()| {
            *delay = min(*delay * 2, Duration::from_secs(30));
            Some(*delay)
        })
    }

    /// Interrupt the program execution.
    pub async fn interrupt(&self) -> FallibleResult {
        self.execution_ctx.interrupt().await?;
        Ok(())
    }

    /// Restart the program execution.
    ///
    /// ### Errors
    /// - Fails if the project is in read-only mode.
    pub async fn restart(&self) -> FallibleResult {
        if self.project.read_only() {
            Err(ReadOnly.into())
        } else {
            self.execution_ctx.restart().await?;
            Ok(())
        }
    }

    /// Get the current call stack frames.
    pub fn call_stack(&self) -> Vec<LocalCall> {
        self.execution_ctx.stack_items().collect()
    }

    /// Get the controller for the currently active graph.
    ///
    /// Note that the controller returned by this method may change as the nodes are stepped into.
    pub fn graph(&self) -> controller::Graph {
        self.graph.borrow().clone_ref()
    }

    /// Get suggestion database from currently active graph.
    pub fn suggestion_db(&self) -> Rc<model::SuggestionDatabase> {
        self.graph.borrow().suggestion_db.clone()
    }

    /// Get parser from currently active graph.
    pub fn parser(&self) -> parser::Parser {
        self.graph.borrow().parser.clone()
    }


    /// Get a full qualified name of the module in the [`graph`]. The name is obtained from the
    /// module's path and the `project` name.
    pub fn module_qualified_name(&self, project: &dyn model::project::API) -> QualifiedName {
        self.graph().module.path().qualified_module_name(project.qualified_name())
    }

    /// Returns information about all the connections between graph's nodes.
    ///
    /// In contrast with the `controller::Graph::connections` this uses information received from
    /// the LS to enrich the generated span trees with function signatures (arity and argument
    /// names).
    pub fn connections(&self) -> FallibleResult<Connections> {
        self.graph.borrow().connections(self)
    }

    /// Create connection in graph.
    ///
    /// ### Errors
    /// - Fails if the project is in read-only mode.
    pub fn connect(&self, connection: &Connection) -> FallibleResult {
        if self.project.read_only() {
            Err(ReadOnly.into())
        } else {
            self.graph.borrow().connect(connection, self)
        }
    }

    /// Remove the connections from the graph.
    ///
    /// ### Errors
    /// - Fails if the project is in read-only mode.
    pub fn disconnect(&self, connection: &Connection) -> FallibleResult {
        if self.project.read_only() {
            Err(ReadOnly.into())
        } else {
            self.graph.borrow().disconnect(connection, self)
        }
    }

    /// Set the execution environment.
    pub async fn set_execution_environment(
        &self,
        execution_environment: ExecutionEnvironment,
    ) -> FallibleResult {
        self.execution_ctx.set_execution_environment(execution_environment).await?;
        Ok(())
    }

    /// Get the current execution environment.
    pub fn execution_environment(&self) -> ExecutionEnvironment {
        self.execution_ctx.execution_environment()
    }

    /// Trigger a clean execution of the current graph with the "live" execution environment. That
    /// means old computations and caches will be discarded.
    pub async fn trigger_clean_live_execution(&self) -> FallibleResult {
        self.execution_ctx.trigger_clean_live_execution().await?;
        Ok(())
    }
}


// === Span Tree Context ===

/// Span Tree generation context for a graph that does not know about execution.
/// Provides information based on computed value registry, using metadata as a fallback.
impl Context for Handle {
    fn call_info(&self, id: ast::Id) -> Option<CalledMethodInfo> {
        let info = self.computed_value_info_registry().get(&id)?;
        let method_call = info.method_call.as_ref()?;
        let suggestion_db = self.project.suggestion_db();
        let maybe_entry = suggestion_db.lookup_by_method_pointer(method_call).map(|(id, entry)| {
            let invocation_info = entry.invocation_info(&suggestion_db, &self.parser());
            invocation_info.with_suggestion_id(id).with_called_on_type(false)
        });

        // When the entry was not resolved but the `defined_on_type` has a `.type` suffix,
        // try resolving it again with the suffix stripped. This indicates that a method was
        // called on type, either because it is a static method, or because it uses qualified
        // method syntax.
        maybe_entry.or_else(|| {
            let defined_on_type = method_call.defined_on_type.strip_suffix(".type")?.to_owned();
            let method_call = MethodPointer { defined_on_type, ..method_call.clone() };
            let (id, entry) = suggestion_db.lookup_by_method_pointer(&method_call)?;
            let invocation_info = entry.invocation_info(&suggestion_db, &self.parser());
            Some(invocation_info.with_suggestion_id(id).with_called_on_type(true))
        })
    }
}

impl model::undo_redo::Aware for Handle {
    fn undo_redo_repository(&self) -> Rc<model::undo_redo::Repository> {
        self.graph.borrow().undo_redo_repository()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
pub mod tests {
    use super::*;

    use crate::model::execution_context::ExpressionId;
    use crate::test;

    use crate::test::mock::Fixture;
    use ast::crumbs::InfixCrumb;
    use controller::graph::Endpoint;
    use engine_protocol::language_server::types::test::value_update_with_type;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);



    #[derive(Debug, Default)]
    pub struct MockData {
        pub graph:  controller::graph::tests::MockData,
        pub module: model::module::test::MockData,
        pub ctx:    model::execution_context::plain::test::MockData,
    }

    impl MockData {
        pub fn controller(&self) -> Handle {
            let parser = parser::Parser::new();
            let repository = Rc::new(model::undo_redo::Repository::new());
            let module = self.module.plain(&parser, repository);
            let method = self.graph.method();
            let mut project = model::project::MockAPI::new();
            let ctx = Rc::new(self.ctx.create());
            let proj_name = test::mock::data::project_qualified_name();
            model::project::test::expect_name(&mut project, test::mock::data::PROJECT_NAME);
            model::project::test::expect_qualified_name(&mut project, &proj_name);
            model::project::test::expect_parser(&mut project, &parser);
            model::project::test::expect_module(&mut project, module);
            model::project::test::expect_execution_ctx(&mut project, ctx);
            // Root ID is needed to generate module path used to get the module.
            model::project::test::expect_root_id(&mut project, crate::test::mock::data::ROOT_ID);
            // Both graph controllers need suggestion DB to provide context to their span trees.
            let suggestion_db = self.graph.suggestion_db();
            model::project::test::expect_suggestion_db(&mut project, suggestion_db);
            let project = Rc::new(project);
            Handle::new(project.clone_ref(), method).boxed_local().expect_ok()
        }
    }

    // Test that checks that value computed notification is properly relayed by the executed graph.
    #[test]
    fn dispatching_value_computed_notification() {
        use crate::test::mock::Fixture;
        // Setup the controller.
        let mut fixture = crate::test::mock::Unified::new().fixture();
        let Fixture { executed_graph, execution, executor, .. } = &mut fixture;

        // Generate notification.
        let updated_id = ExpressionId::new_v4();
        let typename = crate::test::mock::data::TYPE_NAME;
        let update = value_update_with_type(updated_id, typename);

        // Notification not yet send.
        let registry = executed_graph.computed_value_info_registry();
        let mut notifications = executed_graph.subscribe().boxed_local();
        notifications.expect_pending();
        assert!(registry.get(&updated_id).is_none());

        // Sending notification.
        execution.computed_value_info_registry().apply_updates(vec![update]);
        executor.run_until_stalled();

        // Observing that notification was relayed.
        // Both computed values update and graph invalidation are expected, in any order.
        notifications.expect_both(
            |notification| match notification {
                Notification::ComputedValueInfo(updated_ids) => {
                    assert_eq!(updated_ids, &vec![updated_id]);
                    let typename_in_registry = registry.get(&updated_id).unwrap().typename.clone();
                    let expected_typename = Some(ImString::new(typename));
                    assert_eq!(typename_in_registry, expected_typename);
                    true
                }
                _ => false,
            },
            |notification| match notification {
                Notification::Graph(graph_notification) => {
                    assert_eq!(graph_notification, &controller::graph::Notification::PortsUpdate);
                    true
                }
                _ => false,
            },
        );

        notifications.expect_pending();
    }


    // Test that moving nodes is possible in read-only mode.
    #[test]
    fn read_only_mode_does_not_restrict_moving_nodes() {
        use model::module::Position;

        let fixture = crate::test::mock::Unified::new().fixture();
        let Fixture { executed_graph, graph, .. } = fixture;

        let nodes = executed_graph.graph().nodes().unwrap();
        let node = &nodes[0];

        let pos1 = Position::new(500.0, 250.0);
        let pos2 = Position::new(300.0, 150.0);

        graph.set_node_position(node.id(), pos1).unwrap();
        assert_eq!(graph.node(node.id()).unwrap().position(), Some(pos1));
        graph.set_node_position(node.id(), pos2).unwrap();
        assert_eq!(graph.node(node.id()).unwrap().position(), Some(pos2));
    }

    // Test that certain actions are forbidden in read-only mode.
    #[test]
    fn read_only_mode() {
        fn run(code: &str, f: impl FnOnce(&Handle)) {
            let mut data = crate::test::mock::Unified::new();
            data.set_code(code);
            let fixture = data.fixture();
            fixture.read_only.set(true);
            let Fixture { executed_graph, .. } = fixture;
            f(&executed_graph);
        }


        // === Editing the node. ===

        let default_code = r#"
main =
    foo = 2 * 2
"#;
        run(default_code, |executed| {
            let nodes = executed.graph().nodes().unwrap();
            let node = &nodes[0];
            assert!(executed.graph().set_expression(node.info.id(), "5 * 20").is_err());
        });


        // === Collapsing nodes. ===

        let code = r#"
main =
    foo = 2
    bar = foo + 6
    baz = 2 + foo + bar
    caz = baz / 2 * baz
"#;
        run(code, |executed| {
            let nodes = executed.graph().nodes().unwrap();
            // Collapse two middle nodes.
            let nodes_range = vec![nodes[1].id(), nodes[2].id()];
            assert!(executed.graph().collapse(nodes_range, "extracted").is_err());
        });


        // === Connecting nodes. ===

        let code = r#"
main =
    2 + 2
    5 * 5
"#;
        run(code, |executed| {
            let nodes = executed.graph().nodes().unwrap();
            let sum_node = &nodes[0];
            let product_node = &nodes[1];

            assert_eq!(sum_node.expression().to_string(), "2 + 2");
            assert_eq!(product_node.expression().to_string(), "5 * 5");

            let connection = Connection {
                source: Endpoint::root(product_node.id()),
                target: Endpoint::target_at(sum_node, [InfixCrumb::LeftOperand]).unwrap(),
            };
            assert!(executed.connect(&connection).is_err());
        });
    }
}
