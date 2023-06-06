//! A ExecutionContext model which is synchronized with LanguageServer state.

use crate::prelude::*;

use crate::model::execution_context::ComponentGroup;
use crate::model::execution_context::ComputedValueInfoRegistry;
use crate::model::execution_context::LocalCall;
use crate::model::execution_context::QualifiedMethodPointer;
use crate::model::execution_context::Visualization;
use crate::model::execution_context::VisualizationId;
use crate::model::execution_context::VisualizationUpdateData;

use engine_protocol::language_server;
use engine_protocol::language_server::ExecutionEnvironment;



// ====================
// === Notification ===
// ====================

/// Notification received by the synchronized execution context.
///
/// They are based on the relevant language server notifications.
#[derive(Clone, Debug)]
pub enum Notification {
    /// Evaluation of this execution context has completed successfully.
    ///
    /// It does not mean that there are no errors or panics, "successful" refers to the interpreter
    /// run itself. This notification is expected basically on each computation that does not crash
    /// the compiler.
    Completed,
    /// Visualization update data.
    ///
    /// Execution context is responsible for routing them into the computed value registry.
    ExpressionUpdates(Vec<language_server::ExpressionUpdate>),
}



// ==========================
// === Synchronized Model ===
// ==========================

/// An ExecutionContext model synchronized with LanguageServer. It will be automatically removed
/// from LS once dropped.
#[derive(Debug)]
pub struct ExecutionContext {
    id:              model::execution_context::Id,
    model:           model::execution_context::Plain,
    language_server: Rc<language_server::Connection>,
}

impl ExecutionContext {
    /// The unique identifier of this execution context.
    pub fn id(&self) -> model::execution_context::Id {
        self.id
    }

    /// Create new ExecutionContext. It will be created in LanguageServer and the ExplicitCall
    /// stack frame will be pushed.
    ///
    /// NOTE: By itself this execution context will not be able to receive any updates from the
    /// language server.
    #[profile(Debug)]
    pub fn create(
        language_server: Rc<language_server::Connection>,
        root_definition: language_server::MethodPointer,
        id: model::execution_context::Id,
    ) -> impl Future<Output = FallibleResult<Self>> {
        async move {
            info!("Creating.");
            let id = language_server.client.create_execution_context(&id).await?.context_id;
            let model = model::execution_context::Plain::new(root_definition);
            info!("Created. Id: {id}.");
            let this = Self { id, model, language_server };
            this.push_root_frame().await?;
            info!("Pushed root frame.");
            Ok(this)
        }
    }

    fn push_root_frame(&self) -> impl Future<Output = FallibleResult> {
        let method_pointer = self.model.entry_point.borrow().clone();
        let this_argument_expression = default();
        let positional_arguments_expressions = default();

        let call = language_server::ExplicitCall {
            method_pointer,
            this_argument_expression,
            positional_arguments_expressions,
        };
        let frame = language_server::StackItem::ExplicitCall(call);
        let result = self.language_server.push_to_execution_context(&self.id, &frame);
        result.map(|res| res.map_err(|err| err.into()))
    }

    /// Load the component groups defined in libraries imported into the execution context.
    async fn load_component_groups(&self) {
        let log_group_parsing_error = |err: &failure::Error| {
            let msg = format!(
                "Failed to parse a component group returned by the Engine. The group will not \
                appear in the Favorites section of the Component Browser. Error: {err}"
            );
            error!("{msg}");
        };
        match self.language_server.get_component_groups(&self.id).await {
            Ok(ls_response) => {
                let ls_groups = ls_response.component_groups;
                let groups = ls_groups
                    .into_iter()
                    .filter_map(|group| group.try_into().inspect_err(log_group_parsing_error).ok())
                    .collect();
                *self.model.component_groups.borrow_mut() = Rc::new(groups);
                info!("Loaded component groups.");
            }
            Err(err) => {
                let msg = format!(
                    "Failed to load component groups. No groups will appear in the Favorites \
                    section of the Component Browser. Error: {err}"
                );
                error!("{msg}");
            }
        }
    }

    /// Detach visualization from current execution context.
    ///
    /// Necessary because the Language Server requires passing both visualization ID and expression
    /// ID for the visualization attach point, and `Visualization` structure contains both.
    async fn detach_visualization_inner(
        &self,
        vis: Visualization,
    ) -> FallibleResult<Visualization> {
        let vis_id = vis.id;
        let exe_id = self.id;
        let ast_id = vis.expression_id;
        let ls = self.language_server.clone_ref();
        info!("About to detach visualization by id: {vis_id}.");
        ls.detach_visualisation(&exe_id, &vis_id, &ast_id).await?;
        if let Err(err) = self.model.detach_visualization(vis_id) {
            warn!("Failed to update model after detaching visualization: {err:?}.")
        }
        Ok(vis)
    }

    /// Handles the update about expressions being computed.
    pub fn handle_notification(self: &Rc<Self>, notification: Notification) -> FallibleResult {
        match notification {
            Notification::Completed =>
                if !self.model.is_ready.replace(true) {
                    info!("Context {} Became ready", self.id);
                    let this = self.clone();
                    executor::global::spawn(async move {
                        this.load_component_groups().await;
                    });
                },
            Notification::ExpressionUpdates(updates) => {
                self.model.computed_value_info_registry.apply_updates(updates);
            }
        }
        Ok(())
    }
}

impl model::execution_context::API for ExecutionContext {
    fn when_ready(&self) -> StaticBoxFuture<Option<()>> {
        self.model.when_ready()
    }

    fn current_method(&self) -> language_server::MethodPointer {
        self.model.current_method()
    }

    fn method_at_frame_back(&self, count: usize) -> FallibleResult<language_server::MethodPointer> {
        self.model.method_at_frame_back(count)
    }

    fn visualization_info(&self, id: VisualizationId) -> FallibleResult<Visualization> {
        self.model.visualization_info(id)
    }

    fn all_visualizations_info(&self) -> Vec<Visualization> {
        self.model.all_visualizations_info()
    }

    fn active_visualizations(&self) -> Vec<VisualizationId> {
        self.model.active_visualizations()
    }

    fn component_groups(&self) -> Rc<Vec<ComponentGroup>> {
        self.model.component_groups()
    }

    /// Access the registry of computed values information, like types or called method pointers.
    fn computed_value_info_registry(&self) -> &Rc<ComputedValueInfoRegistry> {
        self.model.computed_value_info_registry()
    }

    fn stack_items<'a>(&'a self) -> Box<dyn Iterator<Item = LocalCall> + 'a> {
        self.model.stack_items()
    }

    fn push(&self, stack_item: LocalCall) -> BoxFuture<FallibleResult> {
        async move {
            let expression_id = stack_item.call;
            let call = language_server::LocalCall { expression_id };
            let frame = language_server::StackItem::LocalCall(call);
            self.language_server.push_to_execution_context(&self.id, &frame).await?;
            self.model.push(stack_item);
            Ok(())
        }
        .boxed_local()
    }

    fn pop(&self) -> BoxFuture<FallibleResult<LocalCall>> {
        async move {
            // We do pop first, because we want to call any ls method if the operation is impossible
            // in the plain model.
            let frame = self.model.pop()?;
            let result = self.language_server.pop_from_execution_context(&self.id).await;
            if let Err(err) = result {
                self.model.push(frame);
                Err(err.into())
            } else {
                Ok(frame)
            }
        }
        .boxed_local()
    }

    fn attach_visualization(
        &self,
        vis: Visualization,
    ) -> BoxFuture<FallibleResult<futures::channel::mpsc::UnboundedReceiver<VisualizationUpdateData>>>
    {
        // Note: [mwu]
        //  We must register our visualization in the model first, because Language server can send
        //  us visualization updates through the binary socket before confirming that visualization
        //  has been successfully attached.
        let config = vis.config(self.id);
        let stream = self.model.attach_visualization(vis.clone());

        async move {
            let result = self
                .language_server
                .attach_visualisation(&vis.id, &vis.expression_id, &config)
                .await;
            if let Err(e) = result {
                self.model.detach_visualization(vis.id)?;
                Err(e.into())
            } else {
                Ok(stream)
            }
        }
        .boxed_local()
    }

    fn detach_visualization(
        &self,
        vis_id: VisualizationId,
    ) -> BoxFuture<FallibleResult<Visualization>> {
        async move {
            let vis = self.model.visualization_info(vis_id)?;
            self.detach_visualization_inner(vis).await
        }
        .boxed_local()
    }

    fn modify_visualization(
        &self,
        id: VisualizationId,
        method_pointer: Option<QualifiedMethodPointer>,
        arguments: Option<Vec<String>>,
    ) -> BoxFuture<FallibleResult> {
        let result = self.model.modify_visualization(id, method_pointer, arguments);
        let new_config = self.model.visualization_config(id, self.id);
        async move {
            result?;
            self.language_server.modify_visualisation(&id, &new_config?).await?;
            Ok(())
        }
        .boxed_local()
    }

    fn dispatch_visualization_update(
        &self,
        visualization_id: VisualizationId,
        data: VisualizationUpdateData,
    ) -> FallibleResult {
        debug!("Dispatching visualization update through the context {}", self.id());
        self.model.dispatch_visualization_update(visualization_id, data)
    }

    fn interrupt(&self) -> BoxFuture<FallibleResult> {
        async move {
            self.language_server.client.interrupt(&self.id).await?;
            Ok(())
        }
        .boxed_local()
    }

    fn restart(&self) -> BoxFuture<FallibleResult> {
        async move {
            self.language_server
                .client
                .recompute(
                    &self.id,
                    &language_server::InvalidatedExpressions::All,
                    &Some(self.model.execution_environment.get()),
                )
                .await?;
            Ok(())
        }
        .boxed_local()
    }

    fn rename_method_pointers(&self, old_project_name: String, new_project_name: String) {
        self.model.rename_method_pointers(old_project_name, new_project_name);
    }

    fn set_execution_environment(
        &self,
        execution_environment: ExecutionEnvironment,
    ) -> BoxFuture<FallibleResult> {
        self.model.execution_environment.set(execution_environment);
        async move {
            info!("Setting execution environment to {execution_environment:?}.");
            self.language_server
                .client
                .set_execution_environment(&self.id, &execution_environment)
                .await?;
            Ok(())
        }
        .boxed_local()
    }

    fn execution_environment(&self) -> ExecutionEnvironment {
        self.model.execution_environment.get()
    }

    fn trigger_clean_live_execution(&self) -> BoxFuture<FallibleResult> {
        async move {
            self.language_server
                .client
                .recompute(
                    &self.id,
                    &language_server::InvalidatedExpressions::All,
                    &Some(ExecutionEnvironment::Live),
                )
                .await?;
            Ok(())
        }
        .boxed_local()
    }
}

impl Drop for ExecutionContext {
    fn drop(&mut self) {
        let id = self.id;
        let ls = self.language_server.clone_ref();
        executor::global::spawn(async move {
            let result = ls.client.destroy_execution_context(&id).await;
            if result.is_err() {
                error!("Error when destroying Execution Context: {result:?}.");
            }
        });
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod test {
    use super::*;
    use double_representation::identifier::Identifier;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::model::execution_context::plain::test::MockData;
    use crate::model::execution_context::ComponentGroup;
    use crate::model::traits::*;

    use double_representation::name::project;
    use double_representation::name::QualifiedName;
    use engine_protocol::language_server::response;
    use engine_protocol::language_server::CapabilityRegistration;
    use engine_protocol::language_server::ExpressionUpdates;
    use json_rpc::expect_call;

    #[derive(Debug)]
    pub struct Fixture {
        context: ExecutionContext,
        data:    MockData,
        test:    TestWithLocalPoolExecutor,
    }

    impl Fixture {
        fn new() -> Fixture {
            Self::new_customized(|_, _| {})
        }

        fn new_customized(
            ls_setup: impl FnOnce(&mut language_server::MockClient, &MockData),
        ) -> Fixture {
            Self::new_customized_with_data(MockData::new(), ls_setup)
        }

        fn new_customized_with_data(
            data: MockData,
            ls_setup: impl FnOnce(&mut language_server::MockClient, &MockData),
        ) -> Fixture {
            let mut ls_client = language_server::MockClient::default();
            Self::mock_default_calls(&data, &mut ls_client);
            ls_setup(&mut ls_client, &data);
            ls_client.require_all_calls();
            let connection = language_server::Connection::new_mock_rc(ls_client);
            let mut test = TestWithLocalPoolExecutor::set_up();
            let method = data.main_method_pointer();
            let context = ExecutionContext::create(connection, method, data.context_id);
            let context = test.expect_completion(context).unwrap();
            Fixture { data, context, test }
        }

        /// What is expected server's response to a successful creation of this context.
        fn expected_creation_response(data: &MockData) -> response::CreateExecutionContext {
            let context_id = data.context_id;
            let can_modify =
                CapabilityRegistration::create_can_modify_execution_context(context_id);
            let receives_updates =
                CapabilityRegistration::create_receives_execution_context_updates(context_id);
            response::CreateExecutionContext { context_id, can_modify, receives_updates }
        }

        /// Sets up mock client expectations for context creation and destruction.
        fn mock_create_destroy_calls(data: &MockData, ls: &mut language_server::MockClient) {
            let id = data.context_id;
            let result = Self::expected_creation_response(data);
            expect_call!(ls.create_execution_context(id)  => Ok(result));
            expect_call!(ls.destroy_execution_context(id) => Ok(()));
        }

        /// Sets up mock client expectations for all the calls issued by default by the
        /// [`ExecutionContext::create`] and [`ExecutionContext::drop`] methods.
        pub fn mock_default_calls(data: &MockData, ls: &mut language_server::MockClient) {
            Self::mock_create_destroy_calls(data, ls);
            let id = data.context_id;
            let root_frame = language_server::ExplicitCall {
                method_pointer:                   data.main_method_pointer(),
                this_argument_expression:         None,
                positional_arguments_expressions: vec![],
            };
            let stack_item = language_server::StackItem::ExplicitCall(root_frame);
            expect_call!(ls.push_to_execution_context(id,stack_item) => Ok(()));
        }

        /// Generates a mock update for a random expression ID.
        ///
        /// It will set the typename of the expression to mock typename.
        pub fn mock_expression_update() -> language_server::ExpressionUpdate {
            use engine_protocol::language_server::types::test::value_update_with_type;
            let expression_id = model::execution_context::ExpressionId::new_v4();
            value_update_with_type(expression_id, crate::test::mock::data::TYPE_NAME)
        }

        /// Generates a mock update for a single expression.
        ///
        /// The updated expression ID will be random. The typename will be mock typename.
        pub fn mock_expression_updates(data: &MockData) -> ExpressionUpdates {
            ExpressionUpdates {
                context_id: data.context_id,
                updates:    vec![Self::mock_expression_update()],
            }
        }
    }

    #[test]
    fn creating_context() {
        let f = Fixture::new();
        assert_eq!(f.data.context_id, f.context.id);
        let name_in_data = f.data.module_qualified_name();
        let name_in_ctx_model =
            QualifiedName::try_from(&f.context.model.entry_point.borrow().module);
        assert_eq!(name_in_data, name_in_ctx_model.unwrap());
        assert_eq!(Vec::<LocalCall>::new(), f.context.model.stack_items().collect_vec());
    }

    #[test]
    fn pushing_and_popping_stack_item() {
        let expression_id = model::execution_context::ExpressionId::new_v4();
        let Fixture { data, mut test, context } = Fixture::new_customized(|ls, data| {
            let id = data.context_id;
            let expected_call_frame = language_server::LocalCall { expression_id };
            let expected_stack_item = language_server::StackItem::LocalCall(expected_call_frame);
            expect_call!(ls.push_to_execution_context(id,expected_stack_item) => Ok(()));
            expect_call!(ls.pop_from_execution_context(id) => Ok(()));
        });
        test.run_task(async move {
            assert!(context.pop().await.is_err());
            let item =
                LocalCall { call: expression_id, definition: data.main_method_pointer() };
            context.push(item.clone()).await.unwrap();
            assert_eq!((item,), context.model.stack_items().expect_tuple());
            context.pop().await.unwrap();
            assert_eq!(Vec::<LocalCall>::new(), context.model.stack_items().collect_vec());
            assert!(context.pop().await.is_err());
        });
    }

    #[test]
    fn attaching_visualizations_and_notifying() {
        let method_pointer = QualifiedMethodPointer::module_method(
            MockData::new().module_qualified_name(),
            Identifier::from_text("foo").unwrap(),
        );
        let arguments = vec![];
        let vis = Visualization {
            id: model::execution_context::VisualizationId::new_v4(),
            expression_id: model::execution_context::ExpressionId::new_v4(),
            method_pointer,
            arguments,
        };
        let Fixture { mut test, context, .. } = Fixture::new_customized(|ls, data| {
            let exe_id = data.context_id;
            let vis_id = vis.id;
            let ast_id = vis.expression_id;
            let config = vis.config(exe_id);

            expect_call!(ls.attach_visualisation(vis_id,ast_id,config) => Ok(()));
            expect_call!(ls.detach_visualisation(exe_id,vis_id,ast_id) => Ok(()));
        });

        test.run_task(async move {
            let wrong_id = model::execution_context::VisualizationId::new_v4();
            let events = context.attach_visualization(vis.clone()).await.unwrap();
            let mut events = events.boxed_local();
            events.expect_pending();

            let update = VisualizationUpdateData::new(vec![1, 2, 3]);
            context.dispatch_visualization_update(vis.id, update.clone()).unwrap();
            assert_eq!(events.expect_next(), update);

            events.expect_pending();
            let other_vis_id = VisualizationId::new_v4();
            context.dispatch_visualization_update(other_vis_id, update.clone()).unwrap_err();
            events.expect_pending();
            assert!(context.detach_visualization(wrong_id).await.is_err());
            events.expect_pending();
            assert!(context.detach_visualization(vis.id).await.is_ok());
            events.expect_terminated();
            assert!(context.detach_visualization(vis.id).await.is_err());
            context.dispatch_visualization_update(vis.id, update.clone()).unwrap_err();
        });
    }

    // TODO [mwu]
    //   The test below has been disabled as shaky, see https://github.com/enso-org/ide/issues/637
    #[ignore]
    #[test]
    fn detaching_all_visualizations() {
        let method_pointer = QualifiedMethodPointer::module_method(
            MockData::new().module_qualified_name(),
            Identifier::from_text("foo").unwrap(),
        );
        let arguments = vec!["foo".to_owned()];
        let vis = Visualization {
            id: model::execution_context::VisualizationId::new_v4(),
            expression_id: model::execution_context::ExpressionId::new_v4(),
            method_pointer,
            arguments,
        };
        let vis2 = Visualization { id: VisualizationId::new_v4(), ..vis.clone() };

        let Fixture { mut test, context, .. } = Fixture::new_customized(|ls, data| {
            let exe_id = data.context_id;
            let vis_id = vis.id;
            let vis2_id = vis2.id;
            let ast_id = vis.expression_id;
            let config = vis.config(exe_id);
            let config2 = vis2.config(exe_id);

            expect_call!(ls.attach_visualisation(vis_id,ast_id,config)   => Ok(()));
            expect_call!(ls.attach_visualisation(vis2_id,ast_id,config2) => Ok(()));
            expect_call!(ls.detach_visualisation(exe_id,vis_id,ast_id)   => Ok(()));
            expect_call!(ls.detach_visualisation(exe_id,vis2_id,ast_id)  => Ok(()));
        });
        test.run_task(async move {
            // We discard visualization update streams -- they are covered by a separate test.
            let _ = context.attach_visualization(vis.clone()).await.unwrap();
            let _ = context.attach_visualization(vis2.clone()).await.unwrap();

            context.detach_all_visualizations().await;
        });
    }

    #[test]
    fn modifying_visualizations() {
        let method_pointer = QualifiedMethodPointer::module_method(
            MockData::new().module_qualified_name(),
            Identifier::from_text("foo").unwrap(),
        );
        let arguments = vec!["bar".to_owned()];
        let vis = Visualization {
            id: model::execution_context::VisualizationId::new_v4(),
            expression_id: model::execution_context::ExpressionId::new_v4(),
            method_pointer,
            arguments: arguments.clone(),
        };
        let vis_id = vis.id;
        let new_expression = QualifiedMethodPointer::module_method(
            MockData::new().module_qualified_name(),
            Identifier::from_text("quux").unwrap(),
        );
        let Fixture { mut test, context, .. } = Fixture::new_customized(|ls, data| {
            let exe_id = data.context_id;
            let ast_id = vis.expression_id;
            let config = vis.config(exe_id);

            let expected_config = language_server::types::VisualisationConfiguration {
                execution_context_id: data.context_id,
                expression: new_expression.clone().into(),
                positional_arguments_expressions: arguments.clone(),
            };

            expect_call!(ls.attach_visualisation(vis_id,ast_id,config) => Ok(()));
            expect_call!(ls.modify_visualisation(vis_id,expected_config) => Ok(()));
        });

        test.run_task(async move {
            context.attach_visualization(vis.clone()).await.unwrap();
            let method_pointer = Some(new_expression);
            context.modify_visualization(vis_id, method_pointer, Some(arguments)).await.unwrap();
        });
    }

    /// Check that the [`ExecutionContext::load_component_groups`] method correctly parses
    /// a mocked Language Server response and loads the result into a field of the
    /// [`ExecutionContext`].
    #[test]
    fn loading_component_groups() {
        // Prepare sample component groups to be returned by a mock Language Server client.
        fn library_component(name: &str) -> language_server::LibraryComponent {
            language_server::LibraryComponent { name: name.to_string(), shortcut: None }
        }
        let sample_ls_component_groups = vec![
            // A sample component group in local namespace, with non-empty color, and with exports
            // from the local namespace as well as from the standard library.
            language_server::LibraryComponentGroup {
                library: "local.Unnamed_10".to_string(),
                name:    "Test Group 1".to_string(),
                color:   Some("#C047AB".to_string()),
                icon:    None,
                exports: vec![
                    library_component("Standard.Base.System.File.new"),
                    library_component("local.Unnamed_10.Main.main"),
                ],
            },
            // A sample component group from the standard library, without a predefined color.
            language_server::LibraryComponentGroup {
                library: "Standard.Base".to_string(),
                name:    "Input".to_string(),
                color:   None,
                icon:    None,
                exports: vec![library_component("Standard.Base.System.File.new")],
            },
        ];

        // Create a test fixture based on the sample data.
        let fixture = Fixture::new_customized(move |client, data| {
            let component_groups = language_server::response::GetComponentGroups {
                component_groups: sample_ls_component_groups,
            };
            let id = data.context_id;
            expect_call!(client.get_component_groups(id) => Ok(component_groups));
        });
        let Fixture { mut test, context, .. } = fixture;

        // Run a test and verify that the sample component groups were parsed correctly and have
        // expected contents.
        test.run_task(async move {
            context.load_component_groups().await;
            let groups = context.model.component_groups.borrow();
            assert_eq!(groups.len(), 2);

            // Verify that the first component group was parsed and has expected contents.
            let first_group = &groups[0];
            assert_eq!(first_group.name, "Test Group 1".to_string());
            let color = first_group.color.unwrap();
            assert_eq!((color.red * 255.0) as u8, 0xC0);
            assert_eq!((color.green * 255.0) as u8, 0x47);
            assert_eq!((color.blue * 255.0) as u8, 0xAB);
            let expected_components: Vec<QualifiedName> = vec![
                "Standard.Base.System.File.new".try_into().unwrap(),
                "local.Unnamed_10.Main.main".try_into().unwrap(),
            ];
            assert_eq!(first_group.components, expected_components);

            // Verify that the second component group was parsed and has expected contents.
            assert_eq!(groups[1], ComponentGroup {
                project:    project::QualifiedName::standard_base_library(),
                name:       "Input".into(),
                color:      None,
                components: vec!["Standard.Base.System.File.new".try_into().unwrap(),],
            });
        });
    }
}
