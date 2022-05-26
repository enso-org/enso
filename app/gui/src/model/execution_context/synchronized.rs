//! A ExecutionContext model which is synchronized with LanguageServer state.

use crate::prelude::*;

use crate::model::execution_context::ComputedValueInfoRegistry;
use crate::model::execution_context::LocalCall;
use crate::model::execution_context::VirtualComponentGroup;
use crate::model::execution_context::Visualization;
use crate::model::execution_context::VisualizationId;
use crate::model::execution_context::VisualizationUpdateData;
use crate::model::module;

use engine_protocol::language_server;
use ensogl::data::color;



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
    logger:          Logger,
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
    pub fn create(
        parent: impl AnyLogger,
        language_server: Rc<language_server::Connection>,
        root_definition: language_server::MethodPointer,
    ) -> impl Future<Output = FallibleResult<Self>> {
        let logger = Logger::new_sub(&parent, "ExecutionContext");
        async move {
            info!(logger, "Creating.");
            let id = language_server.client.create_execution_context().await?.context_id;
            let logger = Logger::new_sub(&parent, iformat! {"ExecutionContext {id}"});
            let model = model::execution_context::Plain::new(&logger, root_definition);
            info!(logger, "Created. Id: {id}.");
            let this = Self { id, model, language_server, logger };
            this.push_root_frame().await?;
            info!(this.logger, "Pushed root frame.");
            Ok(this)
        }
    }

    fn push_root_frame(&self) -> impl Future<Output = FallibleResult> {
        let method_pointer = self.model.entry_point.clone();
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
        let logger = self.logger.clone_ref();
        info!(logger, "About to detach visualization by id: {vis_id}.");
        ls.detach_visualisation(&exe_id, &vis_id, &ast_id).await?;
        if let Err(err) = self.model.detach_visualization(vis_id) {
            warning!(logger, "Failed to update model after detaching visualization: {err:?}.")
        }
        Ok(vis)
    }

    /// Handles the update about expressions being computed.
    pub fn handle_notification(&self, notification: Notification) -> FallibleResult {
        match notification {
            Notification::Completed =>
                if !self.model.is_ready.replace(true) {
                    info!(self.logger, "Context {self.id} Became ready");
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

    fn visualization_info(&self, id: VisualizationId) -> FallibleResult<Visualization> {
        self.model.visualization_info(id)
    }

    fn all_visualizations_info(&self) -> Vec<Visualization> {
        self.model.all_visualizations_info()
    }

    fn active_visualizations(&self) -> Vec<VisualizationId> {
        self.model.active_visualizations()
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
        expression: Option<String>,
        module: Option<module::QualifiedName>,
    ) -> BoxFuture<FallibleResult> {
        let result = self.model.modify_visualization(id, expression, module);
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
        debug!(self.logger, "Dispatching visualization update through the context {self.id()}");
        self.model.dispatch_visualization_update(visualization_id, data)
    }

    fn load_component_groups(&self) -> BoxFuture<FallibleResult> {
        async move {
            let response = self.language_server.get_component_groups(&self.id).await?;
            let virtual_component_groups = response
                .component_groups
                .into_iter()
                .map(|group| VirtualComponentGroup {
                    name:    group.group.into(),
                    color:   group.color.as_ref().and_then(|c| from_hex(c)),
                    exports: group.exports.into_iter().map(|e| e.name.into()).collect(),
                })
                .collect();
            *self.model.virtual_component_groups.borrow_mut() = virtual_component_groups;
            Ok(())
        }
        .boxed_local()
    }
}

impl Drop for ExecutionContext {
    fn drop(&mut self) {
        let id = self.id;
        let ls = self.language_server.clone_ref();
        let logger = self.logger.clone_ref();
        executor::global::spawn(async move {
            let result = ls.client.destroy_execution_context(&id).await;
            if result.is_err() {
                error!(logger, "Error when destroying Execution Context: {result:?}.");
            }
        });
    }
}

// /// ```
// /// # use assert_approx_eq::assert_approx_eq;
// /// let color = from_hex("#C047AB".to_string());
// /// assert!(color.is_some());
// /// const PRECISION: f32 = 0.001;
// /// assert_approx_eq!(color.unwrap().red, 0.753, PRECISION);
// /// assert_approx_eq!(color.unwrap().green, 0.278, PRECISION);
// /// assert_approx_eq!(color.unwrap().blue, 0.671, PRECISION);
// /// ```
// FIXME: move to lib/rust/ensogl/core/src/data/color/space/def.rs
fn from_hex(color_string: &str) -> Option<color::Rgb> {
    // see: https://developer.mozilla.org/en-US/docs/Web/CSS/hex-color
    // TODO: regexp?
    if !color_string.starts_with('#') {
        return None;
    }
    let component = |range: Range<usize>, repeat| -> Option<u8> {
        let hex = color_string[range].repeat(repeat);
        let parsed = hex::decode(hex).ok()?;
        Some(parsed[0])
    };
    let (red, green, blue) = match color_string.len() {
        4 => (component(1..2, 2)?, component(2..3, 2)?, component(3..4, 2)?),
        7 => (component(1..3, 1)?, component(3..5, 1)?, component(5..7, 1)?),
        _ => return None,
    };
    Some(color::Rgb::from_base_255(red, green, blue))
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::model::execution_context::plain::test::MockData;
    use crate::model::module::QualifiedName;
    use crate::model::traits::*;

    use assert_approx_eq::assert_approx_eq;
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
            let data = MockData::new();
            let mut ls_client = language_server::MockClient::default();
            Self::mock_create_push_destroy_calls(&data, &mut ls_client);
            ls_setup(&mut ls_client, &data);
            ls_client.require_all_calls();
            let connection = language_server::Connection::new_mock_rc(ls_client);
            let mut test = TestWithLocalPoolExecutor::set_up();
            let logger = Logger::new("Fixture");
            let method = data.main_method_pointer();
            let context = ExecutionContext::create(logger, connection, method);
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
            expect_call!(ls.create_execution_context()    => Ok(result));
            expect_call!(ls.destroy_execution_context(id) => Ok(()));
        }

        /// Sets up mock client expectations for context creation, initial frame push
        /// and destruction.
        pub fn mock_create_push_destroy_calls(
            data: &MockData,
            ls: &mut language_server::MockClient,
        ) {
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

        /// Generates a mock update for a random expression id.
        ///
        /// It will set the typename of the expression to mock typename.
        pub fn mock_expression_update() -> language_server::ExpressionUpdate {
            use engine_protocol::language_server::types::test::value_update_with_type;
            let expression_id = model::execution_context::ExpressionId::new_v4();
            value_update_with_type(expression_id, crate::test::mock::data::TYPE_NAME)
        }

        /// Generates a mock update for a single expression.
        ///
        /// The updated expression id will be random. The typename will be mock typename.
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
        let name_in_ctx_model = QualifiedName::try_from(&f.context.model.entry_point);
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
        let vis = Visualization {
            id:                model::execution_context::VisualizationId::new_v4(),
            expression_id:     model::execution_context::ExpressionId::new_v4(),
            preprocessor_code: "".to_string(),
            context_module:    MockData::new().module_qualified_name(),
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
        let vis = Visualization {
            id:                model::execution_context::VisualizationId::new_v4(),
            expression_id:     model::execution_context::ExpressionId::new_v4(),
            preprocessor_code: "".to_string(),
            context_module:    MockData::new().module_qualified_name(),
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
        let vis = Visualization {
            id:                model::execution_context::VisualizationId::new_v4(),
            expression_id:     model::execution_context::ExpressionId::new_v4(),
            preprocessor_code: "x -> x.to_json.to_string".to_string(),
            context_module:    MockData::new().module_qualified_name(),
        };
        let vis_id = vis.id;
        let new_expression = "x -> x";
        let new_module = "Test.Test_Module";
        let Fixture { mut test, context, .. } = Fixture::new_customized(|ls, data| {
            let exe_id = data.context_id;
            let ast_id = vis.expression_id;
            let config = vis.config(exe_id);

            let expected_config = language_server::types::VisualisationConfiguration {
                execution_context_id: data.context_id,
                visualisation_module: new_module.to_owned(),
                expression:           new_expression.to_owned(),
            };

            expect_call!(ls.attach_visualisation(vis_id,ast_id,config) => Ok(()));
            expect_call!(ls.modify_visualisation(vis_id,expected_config) => Ok(()));
        });

        test.run_task(async move {
            context.attach_visualization(vis.clone()).await.unwrap();
            let expression = Some(new_expression.to_owned());
            let module = Some(QualifiedName::from_text(new_module).unwrap());
            context.modify_visualization(vis_id, expression, module).await.unwrap();
        });
    }

    #[test]
    fn loading_virtual_component_groups() {
        fn library_component(name: &str) -> language_server::LibraryComponent {
            language_server::LibraryComponent { name: name.to_string(), shortcut: None }
        }
        let sample_component_groups = response::GetComponentGroups {
            component_groups: vec![
                language_server::LibraryComponentGroup {
                    library: "local.Unnamed_10".to_string(),
                    group:   "Test Group 1".to_string(),
                    color:   Some("#C047AB".to_string()),
                    icon:    None,
                    exports: vec![
                        library_component("Standard.Base.System.File.new"),
                        library_component("local.Unnamed_10.Main.main"),
                    ],
                },
                language_server::LibraryComponentGroup {
                    library: "Standard.Base".to_string(),
                    group:   "Input".to_string(),
                    color:   None,
                    icon:    None,
                    exports: vec![
                        library_component("Standard.Base.System.File.new"),
                        library_component("Standard.Base.System.File.read_text"),
                    ],
                },
            ],
        };
        let Fixture { data, mut test, context } = Fixture::new_customized(|ls, data| {
            let id = data.context_id;
            expect_call!(ls.get_component_groups(id) => Ok(sample_component_groups));
        });
        test.run_task(async move {
            context.load_component_groups().await.unwrap();
            let groups = context.model.virtual_component_groups.borrow();
            assert_eq!(groups.len(), 2);

            // Check the contents of the first, local group.
            let local_group = &groups[0];
            assert_eq!(local_group.name, "Test Group 1".to_string());
            assert!(local_group.color.is_some());
            const PRECISION: f32 = 0.001;
            assert_approx_eq!(local_group.color.unwrap().red, 0.753, PRECISION);
            assert_approx_eq!(local_group.color.unwrap().green, 0.278, PRECISION);
            assert_approx_eq!(local_group.color.unwrap().blue, 0.671, PRECISION);
            let expected_exports =
                vec!["Standard.Base.System.File.new".into(), "local.Unnamed_10.Main.main".into()];
            assert_eq!(local_group.exports, expected_exports);

            // Check the contents of the second group.
            assert_eq!(groups[1], VirtualComponentGroup {
                name:    "Input".into(),
                color:   None,
                exports: vec![
                    "Standard.Base.System.File.new".into(),
                    "Standard.Base.System.File.read_text".into(),
                ],
            });
        });
    }
}
