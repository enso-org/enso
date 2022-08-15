//! A module containing the whole IDE initialization.

use crate::prelude::*;

use crate::config;
use crate::ide::Ide;
use crate::transport::web::WebSocket;
use crate::FailedIde;

use engine_protocol::project_manager;
use engine_protocol::project_manager::ProjectName;
use ensogl::application::Application;
use uuid::Uuid;



// =================
// === Constants ===
// =================

// TODO[ao] We need to set a big timeout on Project Manager to make sure it will have time to
//     download required version of Engine. This should be handled properly when implementing
//     https://github.com/enso-org/ide/issues/1034
const PROJECT_MANAGER_TIMEOUT_SEC: u64 = 2 * 60 * 60;



// ==============
// === Errors ===
// ==============

/// Error raised when project with given name was not found.
#[derive(Clone, Debug, Fail)]
#[fail(display = "Project with the name {} was not found.", name)]
pub struct ProjectNotFound {
    name: ProjectName,
}



// ===================
// === Initializer ===
// ===================

/// The IDE initializer.
#[derive(Clone, Debug)]
pub struct Initializer {
    config: config::Startup,
    logger: Logger,
}

impl Initializer {
    /// Create [`Initializer`] with given configuration.
    pub fn new(config: config::Startup) -> Self {
        let logger = Logger::new("ide::Initializer");
        Self { config, logger }
    }

    /// Initialize all Ide objects and structures (executor, views, controllers, integration etc.)
    #[profile(Task)]
    pub async fn start(self) -> Result<Ide, FailedIde> {
        info!(self.logger, "Starting IDE with the following config: {self.config:?}");

        ensogl_text_msdf_sys::initialized().await;
        let ensogl_app = ensogl::application::Application::new(self.config.dom_parent_id());
        register_views(&ensogl_app);
        let view = ensogl_app.new_view::<ide_view::root::View>();

        // IDE was opened with `project` argument, we should skip the Welcome Screen.
        // We are doing it early, because Controllers initialization
        // takes some time and Welcome Screen might be visible for a brief moment while
        // controllers are not ready.
        match self.config.initial_view {
            config::InitialView::WelcomeScreen => (),
            config::InitialView::Project => view.switch_view_to_project(),
        }

        let status_bar = view.status_bar().clone_ref();
        ensogl_app.display.add_child(&view);
        // TODO [mwu] Once IDE gets some well-defined mechanism of reporting
        //      issues to user, such information should be properly passed
        //      in case of setup failure.
        match self.initialize_ide_controller().await {
            Ok(controller) => {
                let ide = Ide::new(ensogl_app, view.clone_ref(), controller);
                info!(self.logger, "Setup done.");
                Ok(ide)
            }
            Err(error) => {
                let message = format!("Failed to initialize application: {error}");
                error!(self.logger, "{message}");
                status_bar.add_event(ide_view::status_bar::event::Label::new(message));
                Err(FailedIde { view })
            }
        }
    }

    /// Initialize and return a new Ide Controller.
    ///
    /// This will setup all required connections to backend properly, according to the
    /// configuration.
    #[profile(Task)]
    pub async fn initialize_ide_controller(&self) -> FallibleResult<controller::Ide> {
        use crate::config::BackendService::*;
        match &self.config.backend {
            ProjectManager { endpoint } => {
                let project_manager = self.setup_project_manager(endpoint).await?;
                let project_name = self.config.project_name.clone();
                let controller = controller::ide::Desktop::new(project_manager, project_name);
                Ok(Rc::new(controller.await?))
            }
            LanguageServer { json_endpoint, binary_endpoint, namespace, project_name } => {
                let json_endpoint = json_endpoint.clone();
                let binary_endpoint = binary_endpoint.clone();
                let namespace = namespace.clone();
                let project_name = project_name.clone().into();
                // TODO[ao]: we should think how to handle engine's versions in cloud.
                //     https://github.com/enso-org/ide/issues/1195
                let version = semver::Version::parse(enso_config::engine_version_supported)?;
                let controller = controller::ide::Plain::from_ls_endpoints(
                    namespace,
                    project_name,
                    version,
                    json_endpoint,
                    binary_endpoint,
                )
                .await?;
                Ok(Rc::new(controller))
            }
        }
    }

    /// Create and configure a new project manager client and register it within the global
    /// executor.
    #[profile(Task)]
    pub async fn setup_project_manager(
        &self,
        endpoint: &str,
    ) -> FallibleResult<Rc<dyn project_manager::API>> {
        let transport = WebSocket::new_opened(self.logger.clone_ref(), endpoint).await?;
        let mut project_manager = project_manager::Client::new(transport);
        project_manager.set_timeout(std::time::Duration::from_secs(PROJECT_MANAGER_TIMEOUT_SEC));
        executor::global::spawn(project_manager.runner());
        Ok(Rc::new(project_manager))
    }
}



// ==========================
// === WithProjectManager ===
// ==========================

/// Ide Initializer with project manager.
///
/// This structure do the specific initialization part when we are connected to Project Manager,
/// like list projects, find the one we want to open, open it, or create new one if it does not
/// exist.
#[allow(missing_docs)]
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct WithProjectManager {
    pub logger:          Logger,
    #[derivative(Debug = "ignore")]
    pub project_manager: Rc<dyn project_manager::API>,
    pub project_name:    ProjectName,
}

impl WithProjectManager {
    /// Constructor.
    pub fn new(project_manager: Rc<dyn project_manager::API>, project_name: ProjectName) -> Self {
        let logger = Logger::new("initializer::WithProjectManager");
        Self { logger, project_manager, project_name }
    }

    /// Create and initialize a new Project Model, for a project with name passed in constructor.
    ///
    /// If the project with given name does not exist yet, it will be created.
    pub async fn initialize_project_model(self) -> FallibleResult<model::Project> {
        let project_id = self.get_project_or_create_new().await?;
        let logger = &self.logger;
        let project_manager = self.project_manager;
        model::project::Synchronized::new_opened(logger, project_manager, project_id).await
    }

    /// Creates a new project and returns its id, so the newly connected project can be opened.
    pub async fn create_project(&self) -> FallibleResult<Uuid> {
        use project_manager::MissingComponentAction::Install;
        info!(self.logger, "Creating a new project named '{self.project_name}'.");
        let version = enso_config::ARGS.preferred_engine_version.as_ref().map(ToString::to_string);
        let name = &self.project_name;
        let response = self.project_manager.create_project(name, &None, &version, &Install);
        Ok(response.await?.project_id)
    }

    async fn lookup_project(&self) -> FallibleResult<Uuid> {
        let response = self.project_manager.list_projects(&None).await?;
        let mut projects = response.projects.iter();
        projects
            .find(|project_metadata| project_metadata.name == self.project_name)
            .map(|md| md.id)
            .ok_or_else(|| ProjectNotFound { name: self.project_name.clone() }.into())
    }

    /// Look for the project with the name specified when constructing this initializer,
    /// or, if it does not exist, create it. The id of found/created project is returned.
    pub async fn get_project_or_create_new(&self) -> FallibleResult<Uuid> {
        let project = self.lookup_project().await;
        if let Ok(project_id) = project {
            Ok(project_id)
        } else {
            info!(self.logger, "Attempting to create {self.project_name}");
            self.create_project().await
        }
    }
}



// =============
// === Utils ===
// =============

/// Creates a new running executor with its own event loop. Registers them as a global executor.
pub fn setup_global_executor() -> executor::web::EventLoopExecutor {
    let executor = executor::web::EventLoopExecutor::new_running();
    executor::global::set_spawner(executor.spawner.clone());
    executor
}

/// Register all the standard views for the IDE.
pub fn register_views(app: &Application) {
    app.views.register::<ide_view::root::View>();
    app.views.register::<ide_view::graph_editor::GraphEditor>();
    app.views.register::<ide_view::graph_editor::component::breadcrumbs::ProjectName>();
    app.views.register::<ide_view::code_editor::View>();
    app.views.register::<ide_view::project::View>();
    app.views.register::<ide_view::searcher::View>();
    app.views.register::<ide_view::welcome_screen::View>();
    app.views.register::<ide_view::component_browser::View>();
    app.views.register::<ide_view::component_browser::list_panel::ComponentBrowserPanel>();
    app.views.register::<ide_view::component_browser::list_panel::column_grid::ColumnGrid>();
    app.views.register::<ide_view::component_browser::component_group::View>();
    app.views.register::<ide_view::component_browser::component_group::wide::View>();
    app.views.register::<ensogl_component::text::Area>();
    app.views.register::<ensogl_component::selector::NumberPicker>();
    app.views.register::<ensogl_component::selector::NumberRangePicker>();

    // As long as .label() of a View is the same, shortcuts and commands are currently also
    // expected to be the same, so it should not be important which concrete type parameter of
    // ListView we use below.
    type PlaceholderEntryType = ensogl_component::list_view::entry::Label;
    app.views.register::<ensogl_component::list_view::ListView<PlaceholderEntryType>>();

    if enso_config::ARGS.is_in_cloud.unwrap_or(false) {
        app.views.register::<ide_view::window_control_buttons::View>();
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use json_rpc::expect_call;
    use wasm_bindgen_test::wasm_bindgen_test;



    #[wasm_bindgen_test(async)]
    async fn get_project_or_create_new() {
        let logger = Logger::new("test");
        let mock_client = project_manager::MockClient::default();
        let project_name = ProjectName::new_unchecked("TestProject");
        let project = project_manager::ProjectMetadata {
            name:           project_name.clone(),
            id:             uuid::Uuid::new_v4(),
            last_opened:    default(),
            engine_version: Some("127.0.01".to_owned()),
            namespace:      "local".to_owned(),
        };
        let expected_id = project.id;
        let projects = vec![project];
        let project_lists = project_manager::response::ProjectList { projects };
        let count = None;
        expect_call!(mock_client.list_projects(count) => Ok(project_lists));

        let project_manager = Rc::new(mock_client);
        let initializer = WithProjectManager { logger, project_manager, project_name };
        let project = initializer.get_project_or_create_new().await;
        assert_eq!(expected_id, project.expect("Couldn't get project."))
    }
}
