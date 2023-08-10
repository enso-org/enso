//! A module containing the whole IDE initialization.

use crate::prelude::*;

use crate::config;
use crate::config::ProjectToOpen;
use crate::ide::Ide;
use crate::retry::retry_operation;
use crate::transport::web::WebSocket;
use crate::FailedIde;

use engine_protocol::project_manager;
use engine_protocol::project_manager::ProjectName;
use ensogl::application::Application;
use std::time::Duration;
use uuid::Uuid;



// =================
// === Constants ===
// =================

// TODO[ao] We need to set a big timeout on Project Manager to make sure it will have time to
//     download required version of Engine. This should be handled properly when implementing
//     https://github.com/enso-org/ide/issues/1034
const PROJECT_MANAGER_TIMEOUT_SEC: u64 = 2 * 60 * 60;

/// Times to wait for the subsequent IDE initialization retry.
///
/// The IDE initialization is prone to connectivity problems, therefore we retry it several times.
/// The number of retries is equal to this slice length.
const INITIALIZATION_RETRY_TIMES: &[Duration] =
    &[Duration::from_secs(1), Duration::from_secs(2), Duration::from_secs(4)];



// ==============
// === Errors ===
// ==============

/// Error raised when project with given name was not found.
#[derive(Clone, Debug, Fail)]
#[fail(display = "Project '{}' was not found.", name)]
pub struct ProjectNotFound {
    name: ProjectToOpen,
}



// ===================
// === Initializer ===
// ===================

/// The IDE initializer.
#[derive(Clone, Debug)]
pub struct Initializer {
    config: config::Startup,
}

impl Initializer {
    /// Create [`Initializer`] with given configuration.
    pub fn new(config: config::Startup) -> Self {
        Self { config }
    }

    /// Initialize all Ide objects and structures (executor, views, controllers, integration etc.)
    #[profile(Task)]
    pub async fn start(self) -> Result<Ide, FailedIde> {
        info!("Starting IDE with the following config: {:?}", self.config);

        ensogl_text_msdf::initialized().await;
        let ensogl_app = ensogl::application::Application::new(self.config.dom_parent_id());
        let pixel_read_period = enso_config::ARGS.groups.debug.options.pixel_read_period.value;
        ensogl_app.display.set_pixel_read_period(pixel_read_period as usize);
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

        if enso_config::ARGS.groups.profile.options.emit_user_timing_measurements.value {
            ensogl_app.display.connect_profiler_to_user_timing();
        }
        ensogl_app.display.add_child(&view);
        // TODO [mwu] Once IDE gets some well-defined mechanism of reporting
        //      issues to user, such information should be properly passed
        //      in case of setup failure.

        match self.initialize_ide_controller_with_retries().await {
            Ok(controller) => {
                let ide = Ide::new(ensogl_app, view, controller);
                info!("IDE was successfully initialized.");
                Ok(ide)
            }
            Err(error) => {
                let message = format!("Failed to initialize application: {error}");
                ide_view::notification::logged::error(message, &None);
                Err(FailedIde { view })
            }
        }
    }

    async fn initialize_ide_controller_with_retries(&self) -> FallibleResult<controller::Ide> {
        let retry_times = INITIALIZATION_RETRY_TIMES.iter().copied();
        let error_message = "Failed to initialize controller.";
        retry_operation(|| self.initialize_ide_controller(), retry_times, error_message)
            .await
            .into()
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
                let project_to_open = self.config.project_to_open.clone();
                let controller = controller::ide::Desktop::new(project_manager, project_to_open);
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
        let transport = WebSocket::new_opened(endpoint).await?;
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
    #[derivative(Debug = "ignore")]
    pub project_manager: Rc<dyn project_manager::API>,
    pub project_to_open: ProjectToOpen,
}

impl WithProjectManager {
    /// Constructor.
    pub fn new(
        project_manager: Rc<dyn project_manager::API>,
        project_to_open: ProjectToOpen,
    ) -> Self {
        Self { project_manager, project_to_open }
    }

    /// Create and initialize a new Project Model, for a project with name passed in constructor.
    ///
    /// If the project with given name does not exist yet, it will be created.
    pub async fn initialize_project_model(self) -> FallibleResult<model::Project> {
        let project_id = self.get_project_or_create_new().await?;
        let project_manager = self.project_manager;
        model::project::Synchronized::new_opened(project_manager, project_id).await
    }

    /// Creates a new project and returns its id, so the newly connected project can be opened.
    pub async fn create_project(&self, project_name: &ProjectName) -> FallibleResult<Uuid> {
        use project_manager::MissingComponentAction::Install;
        info!("Creating a new project named '{}'.", project_name);
        let version = &enso_config::ARGS.groups.engine.options.preferred_version.value;
        let version = (!version.is_empty()).as_some_from(|| version.clone());
        let response = self.project_manager.create_project(project_name, &None, &version, &Install);
        Ok(response.await?.project_id)
    }

    async fn lookup_project(&self) -> FallibleResult<Uuid> {
        let response = self.project_manager.list_projects(&None).await?;
        let mut projects = response.projects.iter();
        projects
            .find(|project_metadata| self.project_to_open.matches(project_metadata))
            .map(|md| md.id)
            .ok_or_else(|| ProjectNotFound { name: self.project_to_open.clone() }.into())
    }

    /// Look for the project with the name specified when constructing this initializer,
    /// or, if it does not exist, create it. The id of found/created project is returned.
    pub async fn get_project_or_create_new(&self) -> FallibleResult<Uuid> {
        let project = self.lookup_project().await;
        if let Ok(project_id) = project {
            Ok(project_id)
        } else if let ProjectToOpen::Name(name) = &self.project_to_open {
            info!("Attempting to create {}", name);
            self.create_project(name).await
        } else {
            // This can happen only if we are told to open project by id but it cannot be found.
            // We cannot fallback to creating a new project in this case, as we cannot create a
            // project with a given id. Thus, we simply propagate the lookup result.
            project
        }
    }
}



// =============
// === Utils ===
// =============

/// Register all the standard views for the IDE.
pub fn register_views(app: &Application) {
    app.views.register::<ide_view::root::View>();
    app.views.register::<ide_view::graph_editor::GraphEditor>();
    app.views.register::<ide_view::project_view_top_bar::project_name::ProjectName>();
    app.views.register::<ide_view::code_editor::View>();
    app.views.register::<ide_view::project::View>();
    app.views.register::<ide_view::searcher::View>();
    app.views.register::<ide_view::welcome_screen::View>();
    app.views.register::<ide_view::component_browser::View>();
    app.views.register::<ide_view::component_browser::component_list_panel::View>();
    app.views.register::<ide_view::component_browser::component_list_panel::grid::View>();
    app.views.register::<ide_view::documentation::breadcrumbs::Breadcrumbs>();
    app.views.register::<ensogl_component::text::Text>();
    app.views.register::<ensogl_component::selector::NumberPicker>();
    app.views.register::<ensogl_component::selector::NumberRangePicker>();

    // As long as .label() of a View is the same, shortcuts and commands are currently also
    // expected to be the same, so it should not be important which concrete type parameter of
    // ListView we use below.
    type PlaceholderEntryType = ensogl_component::list_view::entry::Label;
    app.views.register::<ensogl_component::list_view::ListView<PlaceholderEntryType>>();
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
        let project_to_open = ProjectToOpen::Name(project_name);
        let initializer = WithProjectManager { project_manager, project_to_open };
        let project = initializer.get_project_or_create_new().await;
        assert_eq!(expected_id, project.expect("Couldn't get project."))
    }
}
