//! Project controller.
//!
//! Responsible for owning any remote connection clients, and providing controllers for specific
//! files and modules. Expected to live as long as the project remains open in the IDE.

use crate::prelude::*;

use crate::controller::FilePath;
use crate::controller::Visualization;
use crate::model::execution_context::VisualizationId;
use crate::model::execution_context::VisualizationUpdateData;
use crate::model::module::QualifiedName as ModuleQualifiedName;
use crate::model::module::Path          as ModulePath;
use crate::model::synchronized::ExecutionContext;

use enso_protocol::binary;
use enso_protocol::binary::message::VisualisationContext;
use enso_protocol::language_server;
use parser::Parser;
use uuid::Uuid;



// ===============
// === Aliases ===
// ===============

type ExecutionContextId = model::execution_context::Id;



// =================================
// === ExecutionContextsRegistry ===
// =================================

// === Errors ===

#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Fail)]
#[fail(display="No visualization with id {} was found in the registry.", _0)]
pub struct NoSuchVisualization(VisualizationId);


// === Aliases ===

type ExecutionContextWeakMap = WeakValueHashMap<ExecutionContextId,Weak<ExecutionContext>>;


// === Definition ===

/// Stores the weak handles to the synchronized execution context models.
/// Implements dispatching the visualization updates.
#[derive(Clone,Debug,Default)]
pub struct ExecutionContextsRegistry(RefCell<ExecutionContextWeakMap>);

impl ExecutionContextsRegistry {
    /// Routes the visualization update into the appropriate execution context.
    pub fn dispatch_visualization_update
    (&self
    , context : VisualisationContext
    , data    : VisualizationUpdateData
    ) -> FallibleResult<()> {
        let context_id       = context.context_id;
        let visualization_id = context.visualization_id;
        let ctx = self.0.borrow_mut().get(&context_id);
        let ctx = ctx.ok_or_else(|| NoSuchVisualization(context_id))?;
        ctx.dispatch_visualization_update(visualization_id,data)
    }

    /// Registers a new ExecutionContext. It will be eligible for receiving future updates routed
    /// through `dispatch_visualization_update`.
    pub fn insert(&self, context:Rc<ExecutionContext>) {
        self.0.borrow_mut().insert(context.id(),context);
    }
}



// ==========================
// === Project Controller ===
// ==========================

/// Project controller's state.
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug)]
pub struct Handle {
    pub project_name        : Rc<String>,
    pub language_server_rpc : Rc<language_server::Connection>,
    pub visualization       : Visualization,
    pub language_server_bin : Rc<binary::Connection>,
    pub module_registry     : Rc<model::registry::Registry<ModulePath,model::synchronized::Module>>,
    pub execution_contexts  : Rc<ExecutionContextsRegistry>,
    pub parser              : Parser,
    pub logger              : Logger,
}

impl Handle {
    /// Create a new project controller.
    pub fn new
    ( parent                     : impl AnyLogger
    , language_server_client     : language_server::Connection
    , mut language_server_binary : binary::Connection
    , project_name               : impl Str
    ) -> Self {
        let logger = Logger::sub(parent,"Project Controller");
        info!(logger,"Creating a project controller for project {project_name.as_ref()}");
        let binary_protocol_events  = language_server_binary.event_stream();
        let embedded_visualizations = default();
        let language_server_rpc     = Rc::new(language_server_client);
        let language_server_bin     = Rc::new(language_server_binary);
        let language_server         = language_server_rpc.clone();
        let visualization           = Visualization::new(language_server,embedded_visualizations);
        let project_name            = Rc::new(project_name.into());
        let module_registry         = default();
        let execution_contexts      = default();
        let parser                  = Parser::new_or_panic();

        let ret = Handle {project_name,module_registry,execution_contexts,parser,
            language_server_rpc,language_server_bin,logger,visualization};

        let binary_handler = ret.binary_event_handler();
        crate::executor::global::spawn(binary_protocol_events.for_each(binary_handler));
        ret
    }

    /// Returns the primary content root id for this project.
    pub fn content_root_id(&self) -> Uuid {
        self.language_server_rpc.content_root()
    }

    /// Returns a handling function capable of processing updates from the binary protocol.
    /// Such function will be then typically used to process events stream from the binary
    /// connection handler.
    pub fn binary_event_handler
        (&self) -> impl Fn(enso_protocol::binary::Event) -> futures::future::Ready<()> {
        let logger                  = self.logger.clone_ref();
        let weak_execution_contexts = Rc::downgrade(&self.execution_contexts);
        move |event| {
            debug!(logger, "Received an event from the binary protocol: {event:?}");
            use enso_protocol::binary::client::Event;
            use enso_protocol::binary::Notification;
            match event {
                Event::Notification(Notification::VisualizationUpdate {context,data}) => {
                    let data = VisualizationUpdateData::new(data);
                    if let Some(execution_contexts) = weak_execution_contexts.upgrade() {
                        let result = execution_contexts.dispatch_visualization_update(context,data);
                        if let Err(error) = result {
                            error!(logger,"Failed to handle the visualization update: {error}.");
                        }
                    } else {
                        error!(logger,"Received a visualization update despite project being \
                        already dropped.");
                    }
                }
                Event::Closed => {
                    error!(logger,"Lost binary data connection!");
                    // TODO [wmu]
                    //  The problem should be reported to the user and the connection should be
                    //  reestablished, see https://github.com/luna/ide/issues/145
                }
                Event::Error(error) => {
                    error!(logger,"Error emitted by the binary data connection: {error}.");
                }
            }
            futures::future::ready(())
        }
    }

    /// Returns a text controller for a given file path.
    ///
    /// It supports both modules and plain text files.
    pub async fn text_controller(&self, path:FilePath) -> FallibleResult<controller::Text> {
        if let Ok(path) = model::module::Path::from_file_path(path.clone()) {
            info!(self.logger,"Obtaining controller for module {path}");
            let module = self.module_controller(path).await?;
            Ok(controller::Text::new_for_module(module))
        } else {
            let ls = self.language_server_rpc.clone_ref();
            info!(self.logger,"Obtaining controller for plain text {path}");
            Ok(controller::Text::new_for_plain_text(path,ls))
        }
    }

    /// Returns a module controller which have module opened from file.
    pub async fn module_controller
    (&self, path:ModulePath) -> FallibleResult<controller::Module> {
        info!(self.logger,"Obtaining module controller for {path}");
        let model_loader = self.load_module(path.clone());
        let model        = self.module_registry.get_or_load(path.clone(),model_loader).await?;
        Ok(self.module_controller_with_model(path,model))
    }

    /// Creates a path describing a module in this project.
    ///
    /// The segments should not include the leading "src/" directory, as this function adds.
    pub fn module_path_from_qualified_name
    (&self, name_segments:impl IntoIterator<Item:AsRef<str>>) -> FallibleResult<ModulePath> {
        model::module::Path::from_name_segments(self.content_root_id(), name_segments)
    }

    /// Generates full module's qualified name that includes the leading project name segment.
    pub fn qualified_module_name(&self, path:&model::module::Path) -> ModuleQualifiedName {
        ModuleQualifiedName::from_path(path,self.project_name.deref())
    }

    fn module_controller_with_model
    (&self, path:ModulePath, model:Rc<model::synchronized::Module>)
    -> controller::Module {
        let ls     = self.language_server_rpc.clone_ref();
        let parser = self.parser.clone_ref();
        controller::Module::new(&self.logger,path,model,ls,parser)
    }

    fn load_module(&self, path:ModulePath)
    -> impl Future<Output=FallibleResult<Rc<model::synchronized::Module>>> {
        let language_server = self.language_server_rpc.clone_ref();
        let parser          = self.parser.clone_ref();
        model::synchronized::Module::open(path,language_server,parser)
    }

    /// Creates a new execution context with given definition as a root; and registers the context
    /// for receiving update.
    pub async fn create_execution_context
    (&self
    , module_path:Rc<model::module::Path>
    , root_definition:double_representation::definition::DefinitionName
    ) -> FallibleResult<Rc<ExecutionContext>> {
        let ls_rpc  = self.language_server_rpc.clone_ref();
        let context = ExecutionContext::create(&self.logger,ls_rpc,module_path,root_definition);
        let context = context.await?;
        let context = Rc::new(context);
        self.register_execution_context(&context);
        Ok(context)
    }

    /// Registers for receiving updated an execution context. Don't call this manually, if using
    /// `create_execution_context` method -- it is automatically done.
    pub fn register_execution_context(&self, execution_context:&Rc<ExecutionContext>) {
        self.execution_contexts.insert(execution_context.clone_ref());
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use crate::DEFAULT_PROJECT_NAME;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use json_rpc::expect_call;
    use language_server::response;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;
    use enso_protocol::language_server::CapabilityRegistration;
    use enso_protocol::types::Sha3_224;


    wasm_bindgen_test_configure!(run_in_browser);

    /// Sets up project controller using mock Language Server clients.
    /// Passed functions should be used to setup expectations upon the mock clients.
    /// Additionally, an `event_stream` expectation will be setup for a binary protocol, as
    /// project controller always calls it.
    fn setup_mock_project
    ( setup_mock_json   : impl FnOnce(&mut language_server::MockClient)
    , setup_mock_binary : impl FnOnce(&mut enso_protocol::binary::MockClient)
    ) -> controller::Project {
        let mut json_client   = language_server::MockClient::default();
        let mut binary_client = enso_protocol::binary::MockClient::default();
        binary_client.expect_event_stream().return_once(|| {
            futures::stream::empty().boxed_local()
        });

        setup_mock_json(&mut json_client);
        setup_mock_binary(&mut binary_client);
        let json_connection   = language_server::Connection::new_mock(json_client);
        let binary_connection = binary::Connection::new_mock(binary_client);
        let logger            = Logger::default();
        controller::Project::new(logger,json_connection,binary_connection,DEFAULT_PROJECT_NAME)
    }

    #[wasm_bindgen_test]
    fn obtain_module_controller() {
        let mut test  = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let path         = ModulePath::from_mock_module_name("TestModule");
            let another_path = ModulePath::from_mock_module_name("TestModule2");

            let project = setup_mock_project(|ls_json| {
                mock_calls_for_opening_text_file(ls_json,path.file_path().clone(),"2+2");
                mock_calls_for_opening_text_file(ls_json,another_path.file_path().clone(),"22+2");
            }, |_| {});
            let module            = project.module_controller(path.clone()).await.unwrap();
            let same_module       = project.module_controller(path.clone()).await.unwrap();
            let another_module    = project.module_controller(another_path.clone()).await.unwrap();

            assert_eq!(path,         *module.path);
            assert_eq!(another_path, *another_module.path);
            assert!(Rc::ptr_eq(&module.model, &same_module.model));
        });
    }

    #[wasm_bindgen_test]
    fn obtain_plain_text_controller() {
        TestWithLocalPoolExecutor::set_up().run_task(async move {
            let project_ctrl = setup_mock_project(|_|{}, |_|{});
            let root_id      = default();
            let path         = FilePath::new(root_id,&["TestPath"]);
            let another_path = FilePath::new(root_id,&["TestPath2"]);

            let text_ctrl       = project_ctrl.text_controller(path.clone()).await.unwrap();
            let another_ctrl    = project_ctrl.text_controller(another_path.clone()).await.unwrap();
            let language_server = project_ctrl.language_server_rpc;

            assert!(Rc::ptr_eq(&language_server,&text_ctrl.language_server()));
            assert!(Rc::ptr_eq(&language_server,&another_ctrl.language_server()));
            assert_eq!(path        , *text_ctrl   .file_path().deref()  );
            assert_eq!(another_path, *another_ctrl.file_path().deref()  );
        });
    }

    #[wasm_bindgen_test]
    fn obtain_text_controller_for_module() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let module_path  = ModulePath::from_mock_module_name("Test");
            let file_path    = module_path.file_path();
            let project_ctrl = setup_mock_project(|mock_json_client| {
                mock_calls_for_opening_text_file(mock_json_client,file_path.clone(),"2 + 2");
            }, |_| {});
            let text_ctrl = project_ctrl.text_controller(file_path.clone()).await.unwrap();
            let content   = text_ctrl.read_content().await.unwrap();
            assert_eq!("2 + 2", content.as_str());
        });
    }

    fn mock_calls_for_opening_text_file
    (client:&language_server::MockClient, path:language_server::Path, content:&str) {
        let content          = content.to_string();
        let current_version  = Sha3_224::new(content.as_bytes());
        let write_capability = CapabilityRegistration::create_can_edit_text_file(path.clone());
        let write_capability = Some(write_capability);
        let open_response    = response::OpenTextFile {content,current_version,write_capability};
        expect_call!(client.open_text_file(path=path.clone()) => Ok(open_response));
        client.expect.apply_text_file_edit(|_| Ok(()));
        expect_call!(client.close_text_file(path) => Ok(()));
    }
}
