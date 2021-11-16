//! The Desktop IDE Controller
//!
//! See [`crate::controller::ide`] for more detailed description of IDE Controller API.

use crate::prelude::*;

use crate::controller::ide::ManagingProjectAPI;
use crate::controller::ide::Notification;
use crate::controller::ide::StatusNotificationPublisher;
use crate::controller::ide::API;
use crate::ide::initializer;
use crate::notification;

use engine_protocol::project_manager;
use engine_protocol::project_manager::MissingComponentAction;
use engine_protocol::project_manager::ProjectMetadata;
use engine_protocol::project_manager::ProjectName;
use parser::Parser;



// =================
// === Constants ===
// =================

const UNNAMED_PROJECT_NAME: &str = "Unnamed";



// =============================
// === The Controller Handle ===
// =============================

/// The Desktop IDE Controller handle.
///
/// The desktop controller has access to the Project Manager, and thus is able to perform all
/// project management operations.
#[derive(Clone, CloneRef, Derivative)]
#[derivative(Debug)]
pub struct Handle {
    logger:               Logger,
    current_project:      Rc<CloneRefCell<model::Project>>,
    #[derivative(Debug = "ignore")]
    project_manager:      Rc<dyn project_manager::API>,
    status_notifications: StatusNotificationPublisher,
    parser:               Parser,
    notifications:        notification::Publisher<Notification>,
}

impl Handle {
    /// Create a project controller handle with already loaded project model.
    pub fn new_with_project(
        project_manager: Rc<dyn project_manager::API>,
        initial_project: model::Project,
    ) -> Self {
        let logger = Logger::new("controller::ide::Desktop");
        let current_project = Rc::new(CloneRefCell::new(initial_project));
        let status_notifications = default();
        let parser = Parser::new_or_panic();
        let notifications = default();
        Self {
            logger,
            current_project,
            project_manager,
            status_notifications,
            parser,
            notifications,
        }
    }

    /// Create a project controller handle which opens the project with the given name, or creates
    /// it if it does not exist.
    pub async fn new_with_opened_project(
        project_manager: Rc<dyn project_manager::API>,
        name: ProjectName,
    ) -> FallibleResult<Self> {
        // TODO[ao]: Reuse of initializer used in previous code design. It should be soon replaced
        //      anyway, because we will soon resign from the "open or create" approach when opening
        //      IDE. See https://github.com/enso-org/ide/issues/1492 for details.
        let initializer = initializer::WithProjectManager::new(project_manager.clone_ref(), name);
        let model = initializer.initialize_project_model().await?;
        Ok(Self::new_with_project(project_manager, model))
    }
}

impl API for Handle {
    fn current_project(&self) -> model::Project {
        self.current_project.get()
    }
    fn status_notifications(&self) -> &StatusNotificationPublisher {
        &self.status_notifications
    }
    fn parser(&self) -> &Parser {
        &self.parser
    }

    fn subscribe(&self) -> StaticBoxStream<Notification> {
        self.notifications.subscribe().boxed_local()
    }

    fn manage_projects(&self) -> FallibleResult<&dyn ManagingProjectAPI> {
        Ok(self)
    }
}

impl ManagingProjectAPI for Handle {
    fn create_new_project(&self) -> BoxFuture<FallibleResult> {
        async move {
            use model::project::Synchronized as Project;

            let list = self.project_manager.list_projects(&None).await?;
            let names: HashSet<String> = list.projects.into_iter().map(|p| p.name.into()).collect();
            let without_suffix = UNNAMED_PROJECT_NAME.to_owned();
            let with_suffix = (1..).map(|i| format!("{}_{}", UNNAMED_PROJECT_NAME, i));
            let mut candidates = std::iter::once(without_suffix).chain(with_suffix);
            // The iterator have no end, so we can safely unwrap.
            let name = candidates.find(|c| !names.contains(c)).unwrap();
            let version = Some(enso_config::engine_version_supported.to_string());
            let action = MissingComponentAction::Install;

            let create_result =
                self.project_manager.create_project(&name, &version, &action).await?;
            let new_project_id = create_result.project_id;
            let project_mgr = self.project_manager.clone_ref();
            let new_project = Project::new_opened(&self.logger, project_mgr, new_project_id);
            self.current_project.set(new_project.await?);
            executor::global::spawn(self.notifications.publish(Notification::NewProjectCreated));
            Ok(())
        }
        .boxed_local()
    }

    fn list_projects(&self) -> BoxFuture<FallibleResult<Vec<ProjectMetadata>>> {
        async move {
            let pm_response = self.project_manager.list_projects(&None).await?;
            Ok(pm_response.projects)
        }
        .boxed_local()
    }

    fn open_project(&self, id: Uuid) -> BoxFuture<FallibleResult> {
        async move {
            let logger = &self.logger;
            let project_mgr = self.project_manager.clone_ref();
            let new_project = model::project::Synchronized::new_opened(logger, project_mgr, id);
            self.current_project.set(new_project.await?);
            executor::global::spawn(self.notifications.publish(Notification::ProjectOpened));
            Ok(())
        }
        .boxed_local()
    }
}
