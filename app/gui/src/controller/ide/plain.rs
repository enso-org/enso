//! The Plain IDE Controller.
//!
//! See [`crate::controller::ide`] for more detailed description of IDE Controller API.

use crate::prelude::*;

use crate::controller::ide::ManagingProjectAPI;
use crate::controller::ide::Notification;
use crate::controller::ide::StatusNotificationPublisher;
use crate::model::project::synchronized::Properties;

use double_representation::name::project;
use engine_protocol::project_manager::ProjectName;
use engine_protocol::project_manager::ProjectNormalizedName;
use parser::Parser;



// =============
// === Error ===
// =============

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Project operations are not supported.")]
pub struct ProjectOperationsNotSupported;



// ===============================
// === Plain Controller Handle ===
// ===============================

/// Plain IDE Controller Handle.
///
/// The Plain Controller does not allow for managing projects: it has the single project model
/// as a project opened in IDE which does not change (it is set up during construction).
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Handle {
    pub status_notifications: StatusNotificationPublisher,
    pub parser: Parser,
    pub project: model::Project,
    component_browser_private_entries_visibility_flag: Rc<Cell<bool>>,
}

impl Handle {
    /// Create IDE Controller for a given opened project.
    pub fn new(project: model::Project) -> Self {
        let status_notifications = default();
        let parser = Parser::new();
        let component_browser_private_entries_visibility_flag = default();
        Self {
            status_notifications,
            parser,
            project,
            component_browser_private_entries_visibility_flag,
        }
    }

    /// Create IDE Controller from Language Server endpoints, describing the opened project.
    pub async fn from_ls_endpoints(
        namespace: String,
        normalized_name: ProjectNormalizedName,
        displayed_name: ProjectName,
        version: semver::Version,
        json_endpoint: String,
        binary_endpoint: String,
    ) -> FallibleResult<Self> {
        let properties = Properties {
            //TODO [ao]: this should be not the default; instead project model should not need the
            // id.    See https://github.com/enso-org/ide/issues/1572
            id: default(),
            project_name: project::QualifiedName::new(namespace, normalized_name),
            displayed_name,
            engine_version: version,
        };
        let project = model::project::Synchronized::new_connected(
            None,
            json_endpoint,
            binary_endpoint,
            properties,
        )
        .await?;
        let status_notifications = default();
        let parser = Parser::new();
        let component_browser_private_entries_visibility_flag = default();
        Ok(Self {
            status_notifications,
            parser,
            project,
            component_browser_private_entries_visibility_flag,
        })
    }
}

impl controller::ide::API for Handle {
    fn current_project(&self) -> Option<model::Project> {
        Some(self.project.clone_ref())
    }
    fn status_notifications(&self) -> &StatusNotificationPublisher {
        &self.status_notifications
    }
    fn parser(&self) -> &Parser {
        &self.parser
    }

    fn subscribe(&self) -> StaticBoxStream<Notification> {
        futures::stream::empty().boxed_local()
    }

    fn manage_projects(&self) -> FallibleResult<&dyn ManagingProjectAPI> {
        Err(ProjectOperationsNotSupported.into())
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
