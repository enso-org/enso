//! The Plain IDE Controller.
//!
//! See [`crate::controller::ide`] for more detailed description of IDE Controller API.

use crate::prelude::*;

use crate::controller::ide::ManagingProjectAPI;
use crate::controller::ide::Notification;
use crate::controller::ide::StatusNotificationPublisher;
use crate::model::project::synchronized::Properties;

use double_representation::project;
use engine_protocol::project_manager::ProjectName;
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
    pub logger:               Logger,
    pub status_notifications: StatusNotificationPublisher,
    pub parser:               Parser,
    pub project:              model::Project,
}

impl Handle {
    /// Create IDE Controller for a given opened project.
    pub fn new(project: model::Project) -> Self {
        let logger = Logger::new("controller::ide::Plain");
        let status_notifications = default();
        let parser = Parser::new_or_panic();
        Self { logger, status_notifications, parser, project }
    }

    /// Create IDE Controller from Language Server endpoints, describing the opened project.
    pub async fn from_ls_endpoints(
        namespace: String,
        project_name: ProjectName,
        version: semver::Version,
        json_endpoint: String,
        binary_endpoint: String,
    ) -> FallibleResult<Self> {
        let logger = Logger::new("controller::ide::Plain");
        let properties = Properties {
            //TODO [ao]: this should be not the default; instead project model should not need the
            // id.    See https://github.com/enso-org/ide/issues/1572
            id:             default(),
            name:           project::QualifiedName::from_segments(namespace, project_name)?,
            engine_version: version,
        };
        let project = model::project::Synchronized::new_connected(
            &logger,
            None,
            json_endpoint,
            binary_endpoint,
            properties,
        )
        .await?;
        let status_notifications = default();
        let parser = Parser::new_or_panic();
        Ok(Self { logger, status_notifications, parser, project })
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
}
