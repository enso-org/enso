//! The Desktop IDE Controller
//!
//! See [`crate::controller::ide`] for more detailed description of IDE Controller API.

use crate::prelude::*;

use crate::controller::ide::ManagingProjectAPI;
use crate::controller::ide::Notification;
use crate::controller::ide::StatusNotificationPublisher;
use crate::controller::ide::API;

use double_representation::name::project;
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
    current_project: Rc<CloneCell<Option<model::Project>>>,
    #[derivative(Debug = "ignore")]
    project_manager: Rc<dyn project_manager::API>,
    status_notifications: StatusNotificationPublisher,
    parser: Parser,
    notifications: notification::Publisher<Notification>,
    component_browser_private_entries_visibility_flag: Rc<Cell<bool>>,
}

impl Handle {
    /// Create IDE controller.
    pub fn new(project_manager: Rc<dyn project_manager::API>) -> FallibleResult<Self> {
        Ok(Self {
            current_project: default(),
            project_manager,
            status_notifications: default(),
            parser: default(),
            notifications: default(),
            component_browser_private_entries_visibility_flag: default(),
        })
    }
}

impl API for Handle {
    fn current_project(&self) -> Option<model::Project> {
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

    fn are_component_browser_private_entries_visible(&self) -> bool {
        self.component_browser_private_entries_visibility_flag.get()
    }

    fn set_component_browser_private_entries_visibility(&self, visibility: bool) {
        debug!(
            "Setting the visibility of private entries in the component browser to {visibility}."
        );
        self.component_browser_private_entries_visibility_flag.set(visibility);
    }
}

impl ManagingProjectAPI for Handle {
    #[profile(Objective)]
    fn create_new_project(
        &self,
        name: Option<String>,
        template: Option<project::Template>,
    ) -> BoxFuture<FallibleResult> {
        async move {
            let list = self.project_manager.list_projects(&None).await?;
            let existing_names: HashSet<_> =
                list.projects.into_iter().map(|p| p.name.into()).collect();
            let name = name.unwrap_or_else(|| make_project_name(&template));
            let name = choose_unique_project_name(&existing_names, &name);
            let name = ProjectName::new_unchecked(name);
            let version = &enso_config::ARGS.groups.engine.options.preferred_version.value;
            let version = (!version.is_empty()).as_some_from(|| version.clone());
            let action = MissingComponentAction::Install;

            let create_result = self
                .project_manager
                .create_project(&name, &template.map(|t| t.into()), &version, &action)
                .await?;
            self.open_project(create_result.project_id).await
        }
        .boxed_local()
    }

    fn close_project(&self) {
        self.current_project.set(None);
        self.notifications.notify(Notification::ProjectClosed);
    }

    #[profile(Objective)]
    fn list_projects(&self) -> BoxFuture<FallibleResult<Vec<ProjectMetadata>>> {
        async move { Ok(self.project_manager.list_projects(&None).await?.projects) }.boxed_local()
    }

    #[profile(Objective)]
    fn open_project(&self, id: Uuid) -> BoxFuture<FallibleResult> {
        async move {
            let project_mgr = self.project_manager.clone_ref();
            let new_project = model::project::Synchronized::new_opened(project_mgr, id);
            self.current_project.set(Some(new_project.await?));
            self.notifications.notify(Notification::ProjectOpened);
            Ok(())
        }
        .boxed_local()
    }
}

/// Select a new name for the project in a form of <suggested_name>_N, where N is a unique sequence
/// number.
fn choose_unique_project_name(existing_names: &HashSet<String>, suggested_name: &str) -> String {
    let first_candidate = suggested_name.to_owned();
    let nth_project_name = |i| format!("{suggested_name}_{i}");
    let candidates = (1..).map(nth_project_name);
    let mut candidates = iter::once(first_candidate).chain(candidates);
    // The iterator have no end, so we can safely unwrap.
    candidates.find(|c| !existing_names.contains(c)).unwrap()
}

/// Come up with a project name.
fn make_project_name(template: &Option<project::Template>) -> String {
    template
        .as_ref()
        .map(|t| t.to_project_name())
        .unwrap_or_else(|| UNNAMED_PROJECT_NAME.to_owned())
}
