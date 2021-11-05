//! A module containing the whole IDE initialization.

use crate::prelude::*;

use crate::config;
use crate::controller::project::CONFIG;
use crate::ide::Ide;
use crate::transport::web::WebSocket;

use engine_protocol::project_manager;
use engine_protocol::project_manager::ProjectName;
use ensogl::application::Application;
use ensogl::system::web;
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
    /// and forget them to keep them alive.
    pub fn start_and_forget(self) {
        let executor = setup_global_executor();
        executor::global::spawn(async move {
            info!(self.logger, "Starting IDE with the following config: {self.config:?}");

            let application = Application::new(&web::get_html_element_by_id("root").unwrap());
            let view = application.new_view::<ide_view::project::View>();
            let status_bar = view.status_bar().clone_ref();
            // We know the name of new project before it loads. We set it right now to avoid
            // displaying placeholder on the scene during loading.
            view.graph().model.breadcrumbs.project_name(self.config.project_name.to_string());
            application.display.add_child(&view);
            // TODO [mwu] Once IDE gets some well-defined mechanism of reporting
            //      issues to user, such information should be properly passed
            //      in case of setup failure.
            let result: FallibleResult<Ide> = (async {
                let controller = self.initialize_ide_controller().await?;
                Ok(Ide::new(application, view.clone_ref(), controller).await)
            })
            .await;

            match result {
                Ok(ide) => {
                    info!(self.logger, "Setup done.");
                    std::mem::forget(ide);
                }
                Err(err) => {
                    let message = iformat!("Failed to initialize application: {err}");
                    error!(self.logger, "{message}");
                    status_bar.add_event(ide_view::status_bar::event::Label::new(message));
                    std::mem::forget(view);
                }
            }

            web::get_element_by_id("loader")
                .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()))
                .ok();
        });
        std::mem::forget(executor);
    }

    /// Initialize and return a new Ide Controller.
    ///
    /// This will setup all required connections to backend properly, according to the
    /// configuration.
    pub async fn initialize_ide_controller(&self) -> FallibleResult<controller::Ide> {
        use crate::config::BackendService::*;
        match &self.config.backend {
            ProjectManager { endpoint } => {
                let project_manager = self.setup_project_manager(endpoint).await?;
                let project_name = self.config.project_name.clone();
                let controller = controller::ide::Desktop::new_with_opened_project(
                    project_manager,
                    project_name,
                )
                .await?;
                Ok(Rc::new(controller))
            }
            LanguageServer { json_endpoint, binary_endpoint, namespace } => {
                let json_endpoint = json_endpoint.clone();
                let binary_endpoint = binary_endpoint.clone();
                let namespace = namespace.clone();
                let project_name = self.config.project_name.clone();
                // TODO[ao]: we should think how to handle engine's versions in cloud.
                //     https://github.com/enso-org/ide/issues/1195
                let version = CONFIG.engine_version.clone();
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
        let version = Some(CONFIG.engine_version.to_string());
        let ProjectName(name) = &self.project_name;
        let response = self.project_manager.create_project(name, &version, &Install);
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
        let project_name = ProjectName::new("TestProject");
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
