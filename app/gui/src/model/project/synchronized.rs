//! A Project Model that synchronizes all its operations with the Language Server.

use crate::model::traits::*;
use crate::prelude::*;

use crate::model::execution_context;
use crate::model::execution_context::synchronized::Notification as ExecutionUpdate;
use crate::model::execution_context::VisualizationUpdateData;
use crate::model::module;
use crate::model::SuggestionDatabase;
use crate::notification;
use crate::transport::web::WebSocket;

use double_representation::identifier::ReferentName;
use double_representation::project::QualifiedName;
use engine_protocol::binary;
use engine_protocol::binary::message::VisualisationContext;
use engine_protocol::language_server;
use engine_protocol::language_server::CapabilityRegistration;
use engine_protocol::language_server::ContentRoot;
use engine_protocol::language_server::ExpressionUpdates;
use engine_protocol::language_server::MethodPointer;
use engine_protocol::project_manager;
use engine_protocol::project_manager::MissingComponentAction;
use engine_protocol::project_manager::ProjectName;
use flo_stream::Subscriber;
use parser::Parser;



// =================
// === Profiling ===
// =================

thread_local! {
    /// A common preamble used to start every shader program.
    static RPC_EVENT_LOGGER: enso_profiler::MetadataLogger<& 'static str> = enso_profiler::MetadataLogger::new("RpcEvent");
}

/// Log an RPC Event to the profiling framework.
pub fn log_rpc_event(event_name: &'static str) {
    RPC_EVENT_LOGGER.with(|logger| logger.log(event_name));
}



// =================================
// === ExecutionContextsRegistry ===
// =================================

// === Errors ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "No execution context with id {} was found in the registry.", _0)]
pub struct NoSuchExecutionContext(execution_context::Id);


// === Aliases ===

type ExecutionContextWeakMap =
    WeakValueHashMap<execution_context::Id, Weak<execution_context::Synchronized>>;


// === Definition ===

/// Stores the weak handles to the synchronized execution context models.
/// Implements dispatching the visualization updates.
#[derive(Clone, Debug, Default)]
pub struct ExecutionContextsRegistry(RefCell<ExecutionContextWeakMap>);

impl ExecutionContextsRegistry {
    /// Retrieve the execution context with given Id and calls the given function with it.
    ///
    /// Handles the error of context not being present in the registry.
    pub fn with_context<R>(
        &self,
        id: execution_context::Id,
        f: impl FnOnce(Rc<execution_context::Synchronized>) -> FallibleResult<R>,
    ) -> FallibleResult<R> {
        let ctx = self.0.borrow_mut().get(&id);
        let ctx = ctx.ok_or(NoSuchExecutionContext(id))?;
        f(ctx)
    }

    /// Route the visualization update into the appropriate execution context.
    pub fn dispatch_visualization_update(
        &self,
        context: VisualisationContext,
        data: VisualizationUpdateData,
    ) -> FallibleResult {
        self.with_context(context.context_id, |ctx| {
            ctx.dispatch_visualization_update(context.visualization_id, data)
        })
    }

    /// Handles the update about expressions being computed.
    pub fn handle_update(
        &self,
        id: execution_context::Id,
        update: ExecutionUpdate,
    ) -> FallibleResult {
        self.with_context(id, |ctx| ctx.handle_notification(update))
    }

    /// Registers a new ExecutionContext. It will be eligible for receiving future updates routed
    /// through `dispatch_visualization_update`.
    pub fn insert(&self, context: Rc<execution_context::Synchronized>) {
        self.0.borrow_mut().insert(context.id(), context);
    }
}



// ====================
// === ContentRoots ===
// ====================

#[derive(Clone, Debug, Fail)]
#[fail(display = "Content root {} does not exist.", id)]
struct MissingContentRoot {
    id: Uuid,
}

/// A repository of content roots attached to a specific project.
#[derive(Clone, Debug)]
pub struct ContentRoots {
    logger: Logger,
    roots:  RefCell<HashMap<Uuid, Rc<ContentRoot>>>,
}

impl ContentRoots {
    /// Create ContentRoots, initializing with the roots retrieved during connection initialization.
    pub fn new_from_connection(
        parent: impl AnyLogger,
        connection: &language_server::Connection,
    ) -> Self {
        let logger = Logger::new_sub(parent, "ContentRoots");
        let roots_vec = connection.content_roots().map(|r| (r.id(), Rc::new(r.clone()))).collect();
        let roots = RefCell::new(roots_vec);
        Self { logger, roots }
    }

    /// Return all content roots.
    pub fn all(&self) -> Vec<Rc<ContentRoot>> {
        self.roots.borrow().values().cloned().collect()
    }

    /// Add a new content root.
    ///
    /// If there is already a root with given id, it will be replaced and warning will be printed.
    pub fn add(&self, content_root: ContentRoot) {
        let content_root = Rc::new(content_root);
        if let Some(existing) = self.roots.borrow_mut().insert(content_root.id(), content_root) {
            warning!(
                self.logger,
                "Adding content root: there is already content root with given \
                id: {existing:?}"
            );
        }
    }

    /// Get content root by id.
    pub fn get(&self, id: Uuid) -> FallibleResult<Rc<ContentRoot>> {
        self.roots.borrow().get(&id).cloned().ok_or_else(|| MissingContentRoot { id }.into())
    }

    /// Remove the content root with given id.
    ///
    /// If there is no content root with such id, a warning will be printed.
    pub fn remove(&self, id: Uuid) {
        if self.roots.borrow_mut().remove(&id).is_none() {
            warning!(self.logger, "Removing content root: no content root with given id: {id}");
        }
    }
}


// =============
// === Model ===
// =============

// === Errors ===

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Fail)]
#[fail(display = "Project Manager is unavailable.")]
pub struct ProjectManagerUnavailable;

/// A wrapper for an error with information that user tried to open project with unsupported
/// engine's version (which is likely the cause of the problems).
#[derive(Debug, Fail)]
pub struct UnsupportedEngineVersion {
    project_name: String,
    root_cause:   failure::Error,
}

impl UnsupportedEngineVersion {
    fn error_wrapper(properties: &Properties) -> impl Fn(failure::Error) -> failure::Error {
        let engine_version = properties.engine_version.clone();
        let project_name = properties.name.project.as_str().to_owned();
        move |root_cause| {
            let requirement = enso_config::engine_version_requirement();
            if !requirement.matches(&engine_version) {
                let project_name = project_name.clone();
                UnsupportedEngineVersion { project_name, root_cause }.into()
            } else {
                root_cause
            }
        }
    }
}

impl Display for UnsupportedEngineVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let package_yaml_path = controller::project::package_yaml_path(&self.project_name);
        let version_supported = enso_config::engine_version_supported;
        write!(
            f,
            "Failed to open project: unsupported engine version. Please update \
            engine_version in {} to {}.",
            package_yaml_path, version_supported
        )
    }
}



// === Data ===

/// A structure containing the project's properties.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct Properties {
    /// ID of the project, as used by the Project Manager service.
    pub id:             Uuid,
    pub name:           QualifiedName,
    pub engine_version: semver::Version,
}


// == Model ==

/// Project Model.
#[allow(missing_docs)]
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Project {
    pub properties:          Rc<RefCell<Properties>>,
    #[derivative(Debug = "ignore")]
    pub project_manager:     Option<Rc<dyn project_manager::API>>,
    pub language_server_rpc: Rc<language_server::Connection>,
    pub language_server_bin: Rc<binary::Connection>,
    pub module_registry:     Rc<model::registry::Registry<module::Path, module::Synchronized>>,
    pub execution_contexts:  Rc<ExecutionContextsRegistry>,
    pub visualization:       controller::Visualization,
    pub suggestion_db:       Rc<SuggestionDatabase>,
    pub content_roots:       Rc<ContentRoots>,
    pub parser:              Parser,
    pub logger:              Logger,
    pub notifications:       notification::Publisher<model::project::Notification>,
    pub urm:                 Rc<model::undo_redo::Manager>,
}

impl Project {
    /// Create a new project model.
    #[profile(Detail)]
    pub async fn new(
        parent: impl AnyLogger,
        project_manager: Option<Rc<dyn project_manager::API>>,
        language_server_rpc: Rc<language_server::Connection>,
        language_server_bin: Rc<binary::Connection>,
        properties: Properties,
    ) -> FallibleResult<Self> {
        let wrap = UnsupportedEngineVersion::error_wrapper(&properties);
        let logger = Logger::new_sub(parent, "Project Controller");
        info!(logger, "Creating a model of project {properties.name}");
        let binary_protocol_events = language_server_bin.event_stream();
        let json_rpc_events = language_server_rpc.events();
        let embedded_visualizations = default();
        let language_server = language_server_rpc.clone();
        let module_registry = default();
        let execution_contexts = default();
        let visualization =
            controller::Visualization::new(language_server, embedded_visualizations, &logger);
        let parser = Parser::new_or_panic();
        let language_server = &*language_server_rpc;
        let suggestion_db = SuggestionDatabase::create_synchronized(language_server);
        let suggestion_db = Rc::new(suggestion_db.await.map_err(&wrap)?);
        let content_roots = ContentRoots::new_from_connection(&logger, &*language_server);
        let content_roots = Rc::new(content_roots);
        let notifications = notification::Publisher::default();
        let urm = Rc::new(model::undo_redo::Manager::new(&logger));
        let properties = Rc::new(RefCell::new(properties));

        let ret = Project {
            properties,
            project_manager,
            language_server_rpc,
            language_server_bin,
            module_registry,
            execution_contexts,
            visualization,
            suggestion_db,
            content_roots,
            parser,
            logger,
            notifications,
            urm,
        };

        let binary_handler = ret.binary_event_handler();
        crate::executor::global::spawn(binary_protocol_events.for_each(binary_handler));

        let json_rpc_handler = ret.json_event_handler();
        crate::executor::global::spawn(json_rpc_events.for_each(json_rpc_handler));

        ret.acquire_suggestion_db_updates_capability().await.map_err(|err| wrap(err.into()))?;
        Ok(ret)
    }

    /// Initializes the json and binary connection to Language Server, and creates a Project Model
    #[profile(Detail)]
    pub async fn new_connected(
        parent: impl AnyLogger,
        project_manager: Option<Rc<dyn project_manager::API>>,
        language_server_rpc: String,
        language_server_bin: String,
        properties: Properties,
    ) -> FallibleResult<model::Project> {
        let wrap = UnsupportedEngineVersion::error_wrapper(&properties);
        let client_id = Uuid::new_v4();
        let json_ws = WebSocket::new_opened(&parent, &language_server_rpc).await?;
        let binary_ws = WebSocket::new_opened(&parent, &language_server_bin).await?;
        let client_json = language_server::Client::new(json_ws);
        let client_binary = binary::Client::new(&parent, binary_ws);
        crate::executor::global::spawn(client_json.runner());
        crate::executor::global::spawn(client_binary.runner());
        let connection_json =
            language_server::Connection::new(client_json, client_id).await.map_err(&wrap)?;
        let connection_binary =
            binary::Connection::new(client_binary, client_id).await.map_err(&wrap)?;
        let language_server_rpc = Rc::new(connection_json);
        let language_server_bin = Rc::new(connection_binary);
        let model = Self::new(
            parent,
            project_manager,
            language_server_rpc,
            language_server_bin,
            properties,
        )
        .await?;
        Ok(Rc::new(model))
    }

    /// Creates a project model by opening a given project in project_manager, and initializing
    /// the received json and binary connections.
    #[profile(Detail)]
    pub async fn new_opened(
        parent: &Logger,
        project_manager: Rc<dyn project_manager::API>,
        id: Uuid,
    ) -> FallibleResult<model::Project> {
        let action = MissingComponentAction::Install;
        let opened = project_manager.open_project(&id, &action).await?;
        let namespace = opened.project_namespace;
        let name = opened.project_name;
        let project_manager = Some(project_manager);
        let json_endpoint = opened.language_server_json_address.to_string();
        let binary_endpoint = opened.language_server_binary_address.to_string();
        let properties = Properties {
            id,
            name: QualifiedName::from_segments(namespace, name)?,
            engine_version: semver::Version::parse(&opened.engine_version)?,
        };
        Self::new_connected(parent, project_manager, json_endpoint, binary_endpoint, properties)
            .await
    }

    /// Returns a handling function capable of processing updates from the binary protocol.
    /// Such function will be then typically used to process events stream from the binary
    /// connection handler.
    pub fn binary_event_handler(
        &self,
    ) -> impl Fn(engine_protocol::binary::Event) -> futures::future::Ready<()> {
        let logger = self.logger.clone_ref();
        let publisher = self.notifications.clone_ref();
        let weak_execution_contexts = Rc::downgrade(&self.execution_contexts);
        move |event| {
            debug!(logger, "Received an event from the binary protocol: {event:?}");
            use engine_protocol::binary::client::Event;
            use engine_protocol::binary::Notification;
            match event {
                Event::Notification(Notification::VisualizationUpdate { context, data }) => {
                    debug!(
                        logger,
                        "Visualization binary data: {String::from_utf8_lossy(data.as_ref())}"
                    );
                    let data = VisualizationUpdateData::new(data);
                    if let Some(execution_contexts) = weak_execution_contexts.upgrade() {
                        let result =
                            execution_contexts.dispatch_visualization_update(context, data);
                        if let Err(error) = result {
                            error!(logger, "Failed to handle the visualization update: {error}.");
                        }
                    } else {
                        error!(
                            logger,
                            "Received a visualization update despite project being \
                        already dropped."
                        );
                    }
                }
                Event::Closed => {
                    error!(logger, "Lost binary connection with the Language Server!");
                    let which = model::project::BackendConnection::LanguageServerBinary;
                    let notification = model::project::Notification::ConnectionLost(which);
                    publisher.notify(notification);
                    // TODO [wmu]
                    //   The connection should be reestablished, see
                    //   https://github.com/enso-org/ide/issues/145
                }
                Event::Error(error) => {
                    error!(logger, "Error emitted by the binary data connection: {error}.");
                }
            }
            futures::future::ready(())
        }
    }

    /// Handler that routes execution updates to their respective contexts.
    ///
    /// The function has a weak handle to the execution context registry, will stop working once
    /// the registry is dropped.
    pub fn execution_update_handler(
        &self,
    ) -> impl Fn(execution_context::Id, ExecutionUpdate) + Clone {
        let logger = self.logger.clone_ref();
        let registry = Rc::downgrade(&self.execution_contexts);
        move |id, update| {
            if let Some(registry) = registry.upgrade() {
                if let Err(error) = registry.handle_update(id, update) {
                    error!(logger, "Failed to handle the execution context update: {error}");
                }
            } else {
                warning!(
                    logger,
                    "Received an execution context notification despite execution \
                            context being already dropped."
                );
            }
        }
    }

    /// Returns a handling function capable of processing updates from the json-rpc protocol.
    /// Such function will be then typically used to process events stream from the json-rpc
    /// connection handler.
    pub fn json_event_handler(
        &self,
    ) -> impl Fn(engine_protocol::language_server::Event) -> futures::future::Ready<()> {
        // TODO [mwu]
        //  This handler for JSON-RPC notifications is very similar to the function above that
        // handles  binary protocol notifications. However, it is not practical to
        // generalize them, as the  underlying RPC handlers and their types are separate.
        //  This generalization should be reconsidered once the old JSON-RPC handler is phased out.
        //  See: https://github.com/enso-org/ide/issues/587
        let logger = self.logger.clone_ref();
        let publisher = self.notifications.clone_ref();
        let weak_suggestion_db = Rc::downgrade(&self.suggestion_db);
        let weak_content_roots = Rc::downgrade(&self.content_roots);
        let execution_update_handler = self.execution_update_handler();
        move |event| {
            debug!(logger, "Received an event from the json-rpc protocol: {event:?}");
            use engine_protocol::language_server::Event;
            use engine_protocol::language_server::Notification;

            // Profiler logging
            if let Event::Notification(notification) = &event {
                let name: &'static str = notification.into();
                log_rpc_event(name);
            }

            // Event Handling
            match event {
                Event::Notification(Notification::FileEvent(_)) => {}
                Event::Notification(Notification::ExpressionUpdates(updates)) => {
                    let ExpressionUpdates { context_id, updates } = updates;
                    let execution_update = ExecutionUpdate::ExpressionUpdates(updates);
                    execution_update_handler(context_id, execution_update);
                }
                Event::Notification(Notification::ExecutionStatus(_)) => {}
                Event::Notification(Notification::ExecutionComplete { context_id }) => {
                    execution_update_handler(context_id, ExecutionUpdate::Completed);
                }
                Event::Notification(Notification::ExpressionValuesComputed(_)) => {
                    // the notification is superseded by `ExpressionUpdates`.
                }
                Event::Notification(Notification::ExecutionFailed(update)) => {
                    error!(
                        logger,
                        "Execution failed in context {update.context_id}. Error: \
                        {update.message}."
                    );
                }
                Event::Notification(Notification::SuggestionDatabaseUpdates(update)) =>
                    if let Some(suggestion_db) = weak_suggestion_db.upgrade() {
                        suggestion_db.apply_update_event(update);
                    },
                Event::Notification(Notification::ContentRootAdded { root }) => {
                    if let Some(content_roots) = weak_content_roots.upgrade() {
                        content_roots.add(root);
                    }
                }
                Event::Notification(Notification::ContentRootRemoved { id }) => {
                    if let Some(content_roots) = weak_content_roots.upgrade() {
                        content_roots.remove(id);
                    }
                }
                Event::Notification(Notification::VisualisationEvaluationFailed(update)) => {
                    error!(
                        logger,
                        "Visualisation evaluation failed in context {update.context_id} \
                        for visualisation {update.visualisation_id} of expression \
                        {update.expression_id}. Error: {update.message}"
                    );
                }
                Event::Closed => {
                    error!(logger, "Lost JSON-RPC connection with the Language Server!");
                    let which = model::project::BackendConnection::LanguageServerJson;
                    let notification = model::project::Notification::ConnectionLost(which);
                    publisher.notify(notification);
                    // TODO [wmu]
                    //  The connection should be reestablished,
                    //  see https://github.com/enso-org/ide/issues/145
                }
                Event::Error(error) => {
                    error!(logger, "Error emitted by the JSON-RPC data connection: {error}.");
                }
            }
            futures::future::ready(())
        }
    }

    fn acquire_suggestion_db_updates_capability(
        &self,
    ) -> impl Future<Output = json_rpc::Result<()>> {
        let capability = CapabilityRegistration::create_receives_suggestions_database_updates();
        self.language_server_rpc
            .acquire_capability(&capability.method, &capability.register_options)
    }

    #[profile(Task)]
    fn load_module(
        &self,
        path: module::Path,
    ) -> impl Future<Output = FallibleResult<Rc<module::Synchronized>>> {
        let language_server = self.language_server_rpc.clone_ref();
        let parser = self.parser.clone_ref();
        let urm = self.urm();
        let repository = urm.repository.clone_ref();
        async move {
            let module =
                module::Synchronized::open(path, language_server, parser, repository).await?;
            urm.module_opened(module.clone());
            Ok(module)
        }
    }
}

impl model::project::API for Project {
    fn name(&self) -> ReferentName {
        self.properties.borrow().name.project.clone()
    }

    fn qualified_name(&self) -> QualifiedName {
        self.properties.borrow().name.clone()
    }

    fn json_rpc(&self) -> Rc<language_server::Connection> {
        self.language_server_rpc.clone_ref()
    }

    fn binary_rpc(&self) -> Rc<binary::Connection> {
        self.language_server_bin.clone_ref()
    }

    fn engine_version(&self) -> semver::Version {
        self.properties.borrow().engine_version.clone()
    }

    fn parser(&self) -> Parser {
        self.parser.clone_ref()
    }

    fn visualization(&self) -> &controller::Visualization {
        &self.visualization
    }

    fn suggestion_db(&self) -> Rc<SuggestionDatabase> {
        self.suggestion_db.clone_ref()
    }

    fn content_roots(&self) -> Vec<Rc<ContentRoot>> {
        self.content_roots.all()
    }

    fn content_root_by_id(&self, id: Uuid) -> FallibleResult<Rc<ContentRoot>> {
        self.content_roots.get(id)
    }

    #[profile(Detail)]
    fn module(&self, path: module::Path) -> BoxFuture<FallibleResult<model::Module>> {
        async move {
            info!(self.logger, "Obtaining module for {path}");
            let model_loader = self.load_module(path.clone());
            let model: model::Module = self.module_registry.get_or_load(path, model_loader).await?;
            Ok(model)
        }
        .boxed_local()
    }

    #[profile(Detail)]
    fn create_execution_context(
        &self,
        root_definition: MethodPointer,
    ) -> BoxFuture<FallibleResult<model::ExecutionContext>> {
        async move {
            let logger = &self.logger;
            let ls_rpc = self.language_server_rpc.clone_ref();
            let context = execution_context::Synchronized::create(&logger, ls_rpc, root_definition);
            let context = Rc::new(context.await?);
            self.execution_contexts.insert(context.clone_ref());
            let context: model::ExecutionContext = context;
            Ok(context)
        }
        .boxed_local()
    }

    fn rename_project(&self, name: String) -> BoxFuture<FallibleResult> {
        async move {
            let referent_name = name.as_str().try_into()?;
            let project_manager = self.project_manager.as_ref().ok_or(ProjectManagerUnavailable)?;
            let project_id = self.properties.borrow().id;
            let project_name = ProjectName::new_unchecked(name);
            project_manager.rename_project(&project_id, &project_name).await?;
            self.properties.borrow_mut().name.project = referent_name;
            Ok(())
        }
        .boxed_local()
    }

    fn project_content_root_id(&self) -> Uuid {
        self.language_server_rpc.project_root().id()
    }

    fn subscribe(&self) -> Subscriber<model::project::Notification> {
        self.notifications.subscribe()
    }

    fn urm(&self) -> Rc<model::undo_redo::Manager> {
        self.urm.clone_ref()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use engine_protocol::language_server::response;
    use engine_protocol::language_server::Notification::ExpressionUpdates;
    use engine_protocol::types::Sha3_224;
    use futures::SinkExt;
    use json_rpc::expect_call;


    #[allow(unused)]
    struct Fixture {
        test:                 TestWithLocalPoolExecutor,
        project:              Project,
        binary_events_sender: futures::channel::mpsc::UnboundedSender<binary::client::Event>,
        json_events_sender:   futures::channel::mpsc::UnboundedSender<language_server::Event>,
    }

    impl Fixture {
        fn new(
            setup_mock_json: impl FnOnce(&mut language_server::MockClient),
            setup_mock_binary: impl FnOnce(&mut engine_protocol::binary::MockClient),
        ) -> Self {
            let mut test = TestWithLocalPoolExecutor::set_up();
            let project_manager = project_manager::MockClient::default();
            let mut json_client = language_server::MockClient::default();
            let mut binary_client = engine_protocol::binary::MockClient::default();

            let (binary_events_sender, binary_events) = futures::channel::mpsc::unbounded();
            binary_client.expect_event_stream().return_once(|| binary_events.boxed_local());
            let json_events_sender = json_client.setup_events();

            let initial_suggestions_db = language_server::response::GetSuggestionDatabase {
                entries:         vec![],
                current_version: 0,
            };
            expect_call!(json_client.get_suggestions_database() => Ok(initial_suggestions_db));
            let capability_reg =
                CapabilityRegistration::create_receives_suggestions_database_updates();
            let method = capability_reg.method;
            let options = capability_reg.register_options;
            expect_call!(json_client.acquire_capability(method,options) => Ok(()));

            setup_mock_json(&mut json_client);
            setup_mock_binary(&mut binary_client);
            let json_connection = Rc::new(language_server::Connection::new_mock(json_client));
            let binary_connection = Rc::new(binary::Connection::new_mock(binary_client));
            let project_manager = Rc::new(project_manager);
            let logger = Logger::new("Fixture");
            let properties = Properties {
                id:             Uuid::new_v4(),
                name:           crate::test::mock::data::project_qualified_name(),
                engine_version: semver::Version::new(0, 2, 1),
            };
            let project_fut = Project::new(
                logger,
                Some(project_manager),
                json_connection,
                binary_connection,
                properties,
            )
            .boxed_local();
            let project = test.expect_completion(project_fut).unwrap();
            Fixture { test, project, binary_events_sender, json_events_sender }
        }
    }

    #[wasm_bindgen_test]
    fn notify_backend_connection_lost() {
        use crate::model::project::BackendConnection::*;
        use crate::model::project::Notification;

        fn run(expected_event: Notification, close_socket: impl FnOnce(&mut Fixture)) {
            let mut f = Fixture::new(|_| {}, |_| {});
            let mut events = f.project.subscribe().boxed_local();
            events.expect_pending();
            close_socket(&mut f);
            events.expect_pending();
            f.test.run_until_stalled();
            let event = events.expect_next();
            assert_eq!(event, expected_event);
        }

        run(Notification::ConnectionLost(LanguageServerBinary), |f| {
            f.binary_events_sender.send(binary::Event::Closed).boxed_local().expect_ok();
        });

        run(Notification::ConnectionLost(LanguageServerJson), |f| {
            f.json_events_sender.send(json_rpc::Event::Closed).boxed_local().expect_ok();
        });
    }

    #[wasm_bindgen_test]
    fn obtain_module_controller() {
        let path = module::Path::from_mock_module_name("TestModule");
        let another_path = module::Path::from_mock_module_name("TestModule2");
        let Fixture { mut test, project, .. } = Fixture::new(
            |ls_json| {
                mock_calls_for_opening_text_file(ls_json, path.file_path().clone(), "2+2");
                mock_calls_for_opening_text_file(ls_json, another_path.file_path().clone(), "22+2");
            },
            |_| {},
        );

        test.run_task(async move {
            let module = project.module(path.clone_ref()).await.unwrap();
            let same_module = project.module(path.clone_ref()).await.unwrap();
            let another_module = project.module(another_path.clone_ref()).await.unwrap();

            assert_eq!(path, *module.path());
            assert_eq!(another_path, *another_module.path());
            // We have to downcast module, otherwise we would compare vtable pointers. See
            // https://rust-lang.github.io/rust-clippy/master/index.html#vtable_address_comparisons
            let module = module.as_any().downcast_ref::<module::Synchronized>().unwrap();
            let same_module = same_module.as_any().downcast_ref::<module::Synchronized>().unwrap();
            assert!(std::ptr::eq(module, same_module));
        });
    }

    fn mock_calls_for_opening_text_file(
        client: &language_server::MockClient,
        path: language_server::Path,
        content: &str,
    ) {
        let content = content.to_string();
        let current_version = Sha3_224::new(content.as_bytes());
        let write_capability = CapabilityRegistration::create_can_edit_text_file(path.clone());
        let write_capability = Some(write_capability);
        let open_response = response::OpenTextFile { content, current_version, write_capability };
        expect_call!(client.open_text_file(path=path.clone()) => Ok(open_response));
        client.expect.apply_text_file_edit(|_| Ok(()));
        expect_call!(client.close_text_file(path) => Ok(()));
    }

    /// This tests checks mainly if:
    /// * project controller correctly creates execution context
    /// * created execution context appears in the registry
    /// * project controller correctly dispatches the LS notification with type information
    /// * the type information is correctly recorded and available in the execution context
    #[wasm_bindgen_test]
    fn execution_context_management() {
        use execution_context::synchronized::test::Fixture as ExecutionFixture;
        use language_server::Event;

        let context_data = execution_context::plain::test::MockData::new();
        let Fixture { mut test, project, json_events_sender, .. } = Fixture::new(
            |mock_json_client| {
                ExecutionFixture::mock_create_push_destroy_calls(&context_data, mock_json_client);
                mock_json_client.require_all_calls();
            },
            |_| {},
        );

        // No context present yet.
        let no_op = |_| Ok(());
        let result1 = project.execution_contexts.with_context(context_data.context_id, no_op);
        assert!(result1.is_err());

        // Create execution context.
        let execution = project.create_execution_context(context_data.main_method_pointer());
        let execution = test.expect_completion(execution).unwrap();

        // Now context is in registry.
        let result1 = project.execution_contexts.with_context(context_data.context_id, no_op);
        assert!(result1.is_ok());

        // Context has no information about type.
        let notification = ExecutionFixture::mock_expression_updates(&context_data);
        let value_update = &notification.updates[0];
        let expression_id = value_update.expression_id;
        let value_registry = execution.computed_value_info_registry();
        assert!(value_registry.get(&expression_id).is_none());

        // Send notification with type information.
        let event = Event::Notification(ExpressionUpdates(notification.clone()));
        json_events_sender.unbounded_send(event).unwrap();
        test.run_until_stalled();

        // Context now has the information about type.
        let value_info = value_registry.get(&expression_id).unwrap();
        assert_eq!(value_info.typename, value_update.typename.clone().map(ImString::new));
        assert_eq!(value_info.method_call, value_update.method_pointer);
    }
}
