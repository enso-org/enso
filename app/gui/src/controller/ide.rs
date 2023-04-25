//! IDE controller
//!
//! The IDE controller expose functionality bound to the application as a whole, not to specific
//! component or opened project.

use crate::prelude::*;

use crate::config::ProjectToOpen;

use double_representation::name::project;
use mockall::automock;
use parser::Parser;


// ==============
// === Export ===
// ==============

pub mod desktop;
pub mod plain;

pub use engine_protocol::project_manager::ProjectMetadata;
pub use engine_protocol::project_manager::ProjectName;



// ============================
// === Status Notifications ===
// ============================

/// The handle used to pair the ProcessStarted and ProcessFinished notifications.
pub type BackgroundTaskHandle = usize;

/// A notification which should be displayed to the User on the status bar.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum StatusNotification {
    /// Notification about single event, should be logged in an event log window.
    Event { label: String },
    /// Notification about new background task done in IDE (like compiling library).
    BackgroundTaskStarted { label: String, handle: BackgroundTaskHandle },
    /// Notification that some task notified in [`BackgroundTaskStarted`] has been finished.
    BackgroundTaskFinished { handle: BackgroundTaskHandle },
}

/// A publisher for status notification events.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct StatusNotificationPublisher {
    publisher:           notification::Publisher<StatusNotification>,
    next_process_handle: Rc<Cell<usize>>,
}

impl StatusNotificationPublisher {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Publish a new status event (see [`StatusNotification::Event`])
    #[profile(Debug)]
    pub fn publish_event(&self, label: impl Into<String>) {
        let label = label.into();
        let notification = StatusNotification::Event { label };
        executor::global::spawn(self.publisher.publish(notification));
    }

    /// Publish a notification about new process (see [`StatusNotification::ProcessStarted`]).
    ///
    /// Returns the handle to be used when notifying about process finishing.
    #[profile(Debug)]
    pub fn publish_background_task(&self, label: impl Into<String>) -> BackgroundTaskHandle {
        let label = label.into();
        let handle = self.next_process_handle.get();
        self.next_process_handle.set(handle + 1);
        let notification = StatusNotification::BackgroundTaskStarted { label, handle };
        executor::global::spawn(self.publisher.publish(notification));
        handle
    }

    /// Publish a notfication that process has finished (see
    /// [`StatusNotification::ProcessFinished`])
    #[profile(Debug)]
    pub fn published_background_task_finished(&self, handle: BackgroundTaskHandle) {
        let notification = StatusNotification::BackgroundTaskFinished { handle };
        executor::global::spawn(self.publisher.publish(notification));
    }

    /// The asynchronous stream of published notifications.
    pub fn subscribe(&self) -> impl Stream<Item = StatusNotification> {
        self.publisher.subscribe()
    }
}



// ====================
// === Notification ===
// ====================

/// Notification of IDE Controller.
///
/// In contrast to [`StatusNotification`], which is a notification from any application part to
/// be delivered to User (displayed on some event log or status bar), this is a notification to be
/// used internally in code.
#[derive(Copy, Clone, Debug)]
pub enum Notification {
    /// User opened a new or existing project.
    ProjectOpened,
    /// User closed the project.
    ProjectClosed,
}



// ===========
// === API ===
// ===========

// === Errors ===

/// Error raised when a project with given name or ID was not found.
#[derive(Clone, Debug, Fail)]
#[fail(display = "Project '{}' was not found.", project)]
pub struct ProjectNotFound {
    project: ProjectToOpen,
}


// === Managing API ===

/// The API of all project management operations.
///
/// It is a separate trait, because those methods  are not supported in some environments (see also
/// [`API::manage_projects`]).
pub trait ManagingProjectAPI {
    /// Create a new project and open it in the IDE.
    ///
    /// `name` is an optional project name. It overrides the name of the template if given.
    /// `template` is an optional project template name. Available template names are defined in
    /// `lib/scala/pkg/src/main/scala/org/enso/pkg/Template.scala`.
    fn create_new_project(
        &self,
        name: Option<String>,
        template: Option<project::Template>,
    ) -> BoxFuture<FallibleResult>;

    /// Return a list of existing projects.
    fn list_projects(&self) -> BoxFuture<FallibleResult<Vec<ProjectMetadata>>>;

    /// Open the project with given UUID.
    fn open_project(&self, id: Uuid) -> BoxFuture<FallibleResult>;

    /// Close the currently opened project. Does nothing if no project is open.
    fn close_project(&self);

    /// Open project by name. It makes two calls to the Project Manager: one for listing projects
    /// and then for the project opening.
    fn open_project_by_name(&self, name: String) -> BoxFuture<FallibleResult> {
        async move {
            let project_id = self.find_project(&ProjectToOpen::Name(name.into())).await?;
            self.open_project(project_id).await
        }
        .boxed_local()
    }

    /// Open a project by name or ID. If no project with the given name exists, it will be created.
    fn open_or_create_project(&self, project_to_open: ProjectToOpen) -> BoxFuture<FallibleResult> {
        async move {
            match self.find_project(&project_to_open).await {
                Ok(project_id) => self.open_project(project_id).await,
                Err(error) =>
                    if let ProjectToOpen::Name(name) = project_to_open {
                        info!("Attempting to create project with name '{name}'.");
                        self.create_new_project(Some(name.to_string()), None).await
                    } else {
                        Err(error)
                    },
            }
        }
        .boxed_local()
    }

    /// Find a project by name or ID.
    fn find_project<'a: 'c, 'b: 'c, 'c>(
        &'a self,
        project_to_open: &'b ProjectToOpen,
    ) -> BoxFuture<'c, FallibleResult<Uuid>> {
        async move {
            self.list_projects()
                .await?
                .into_iter()
                .find(|project_metadata| project_to_open.matches(project_metadata))
                .map(|metadata| metadata.id)
                .ok_or_else(|| ProjectNotFound { project: project_to_open.clone() }.into())
        }
        .boxed_local()
    }
}


// === Main API ===

/// The API of IDE Controller.
#[automock]
pub trait API: Debug {
    /// The model of currently opened project.
    ///
    /// IDE can have only one project opened at a time.
    ///
    /// Returns `None` if no project is opened at the moment.
    fn current_project(&self) -> Option<model::Project>;

    /// Getter of Status Notification Publisher.
    fn status_notifications(&self) -> &StatusNotificationPublisher;

    /// The Parser Handle.
    fn parser(&self) -> &Parser;

    /// Subscribe the controller notifications.
    fn subscribe(&self) -> StaticBoxStream<Notification>;

    /// Return the Managing Project API.
    ///
    /// It may be some delegated object or just the reference to self.
    // Automock macro does not work without explicit lifetimes here.
    #[allow(clippy::needless_lifetimes)]
    fn manage_projects<'a>(&'a self) -> FallibleResult<&'a dyn ManagingProjectAPI>;

    /// Return whether private entries should be visible in the component browser.
    fn are_component_browser_private_entries_visible(&self) -> bool;

    /// Sets whether private entries should be visible in the component browser.
    fn set_component_browser_private_entries_visibility(&self, visibility: bool);
}

/// A polymorphic handle of IDE controller.
pub type Ide = Rc<dyn API>;

/// The IDE Controller for desktop environments.
pub type Desktop = desktop::Handle;

/// The Plain IDE controller with a single project and no possibility to change it.
pub type Plain = plain::Handle;

impl Debug for MockAPI {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Mocked Ide Controller")
    }
}
