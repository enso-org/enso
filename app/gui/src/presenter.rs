//! The Presenter is a layer between logical part of the IDE (controllers, engine models) and the
//! views (the P letter in MVP pattern). The presenter reacts to changes in the controllers, and
//! actively updates the view. It also passes all user interactions from view to controllers.
pub mod code;
pub mod graph;
pub mod project;
pub mod searcher;

pub use code::Code;
pub use graph::Graph;
pub use project::Project;
pub use searcher::Searcher;

use crate::prelude::*;

use crate::controller::ide::StatusNotification;
use crate::executor::global::spawn_stream_handler;
use crate::presenter;

use enso_frp as frp;
use ide_view as view;
use ide_view::graph_editor::SharedHashMap;



// =============
// === Model ===
// =============

#[derive(Debug)]
struct Model {
    logger:          Logger,
    current_project: RefCell<Option<Project>>,
    controller:      controller::Ide,
    view:            view::root::View,
}

impl Model {
    /// Instantiate a new project presenter, which will display current project in the view.
    fn setup_and_display_new_project(self: Rc<Self>) {
        // Remove the old integration first. We want to be sure the old and new integrations will
        // not race for the view.
        *self.current_project.borrow_mut() = None;

        if let Some(project_model) = self.controller.current_project() {
            self.view.switch_view_to_project();
            // We know the name of new project before it loads. We set it right now to avoid
            // displaying placeholder on the scene during loading.
            let project_view = self.view.project();
            let status_bar = self.view.status_bar().clone_ref();
            let breadcrumbs = &project_view.graph().model.breadcrumbs;
            breadcrumbs.project_name(project_model.name().to_string());

            let status_notifications = self.controller.status_notifications().clone_ref();
            let ide_controller = self.controller.clone_ref();
            let project_controller =
                controller::Project::new(project_model, status_notifications.clone_ref());

            executor::global::spawn(async move {
                match presenter::Project::initialize(
                    ide_controller,
                    project_controller,
                    project_view,
                    status_bar,
                )
                .await
                {
                    Ok(project) => {
                        *self.current_project.borrow_mut() = Some(project);
                    }
                    Err(err) => {
                        let err_msg = format!("Failed to initialize project: {}", err);
                        error!(self.logger, "{err_msg}");
                        status_notifications.publish_event(err_msg)
                    }
                }
            });
        }
    }

    /// Open a project by name. It makes two calls to Project Manager: one for listing projects and
    /// a second one for opening the project.
    pub fn open_project(&self, project_name: String) {
        let logger = self.logger.clone_ref();
        let controller = self.controller.clone_ref();
        crate::executor::global::spawn(async move {
            if let Ok(managing_api) = controller.manage_projects() {
                if let Err(err) = managing_api.open_project_by_name(project_name).await {
                    error!(logger, "Cannot open project by name: {err}.");
                }
            } else {
                warning!(logger, "Project opening failed: no ProjectManagingAPI available.");
            }
        });
    }

    /// Create a new project. `template` is an optional name of the project template passed to the
    /// Engine. It makes a call to Project Manager.
    fn create_project(&self, template: Option<&str>) {
        let logger = self.logger.clone_ref();
        let controller = self.controller.clone_ref();
        let template = template.map(ToOwned::to_owned);
        crate::executor::global::spawn(async move {
            if let Ok(managing_api) = controller.manage_projects() {
                if let Err(err) = managing_api.create_new_project(template.clone()).await {
                    if let Some(template) = template {
                        error!(
                            logger,
                            "Could not create new project from template {template}: {err}."
                        );
                    } else {
                        error!(logger, "Could not create new project: {err}.");
                    }
                }
            } else {
                warning!(logger, "Project creation failed: no ProjectManagingAPI available.");
            }
        });
    }
}



// =================
// === Presenter ===
// =================

/// The root presenter, handling the synchronization between IDE controller and root view.
///
/// See [`crate::presenter`] docs for information about presenters in general.
#[derive(Clone, CloneRef, Debug)]
pub struct Presenter {
    network: frp::Network,
    model:   Rc<Model>,
}

impl Presenter {
    /// Create new root presenter.
    ///
    /// The returned presenter is working and does not require any initialization. The current
    /// project will be displayed (if any).
    pub fn new(controller: controller::Ide, view: ide_view::root::View) -> Self {
        let logger = Logger::new("Presenter");
        let current_project = default();
        let model = Rc::new(Model { logger, controller, view, current_project });

        frp::new_network! { network
            let welcome_view_frp = &model.view.welcome_screen().frp;
            eval welcome_view_frp.open_project((name) model.open_project(name.to_owned()));
            eval welcome_view_frp.create_project((templ) model.create_project(templ.as_deref()));

            let root_frp = &model.view.frp;
            root_frp.switch_view_to_project <+ welcome_view_frp.create_project.constant(());
            root_frp.switch_view_to_project <+ welcome_view_frp.open_project.constant(());
        }


        Self { model, network }.init()
    }

    fn init(self) -> Self {
        self.setup_status_bar_notification_handler();
        self.setup_controller_notification_handler();
        self.set_projects_list_on_welcome_screen();
        self.model.clone_ref().setup_and_display_new_project();
        self
    }

    fn setup_status_bar_notification_handler(&self) {
        use controller::ide::BackgroundTaskHandle as ControllerHandle;
        use ide_view::status_bar::process::Id as ViewHandle;

        let logger = self.model.logger.clone_ref();
        let process_map = SharedHashMap::<ControllerHandle, ViewHandle>::new();
        let status_bar = self.model.view.status_bar().clone_ref();
        let status_notifications = self.model.controller.status_notifications().subscribe();
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, status_notifications, move |notification, _| {
            match notification {
                StatusNotification::Event { label } => {
                    status_bar.add_event(ide_view::status_bar::event::Label::new(label));
                }
                StatusNotification::BackgroundTaskStarted { label, handle } => {
                    status_bar.add_process(ide_view::status_bar::process::Label::new(label));
                    let view_handle = status_bar.last_process.value();
                    process_map.insert(handle, view_handle);
                }
                StatusNotification::BackgroundTaskFinished { handle } => {
                    if let Some(view_handle) = process_map.remove(&handle) {
                        status_bar.finish_process(view_handle);
                    } else {
                        warning!(logger, "Controllers finished process not displayed in view");
                    }
                }
            }
            futures::future::ready(())
        });
    }

    fn setup_controller_notification_handler(&self) {
        let stream = self.model.controller.subscribe();
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, stream, move |notification, model| {
            match notification {
                controller::ide::Notification::NewProjectCreated
                | controller::ide::Notification::ProjectOpened =>
                    model.setup_and_display_new_project(),
            }
            futures::future::ready(())
        });
    }

    fn set_projects_list_on_welcome_screen(&self) {
        let controller = self.model.controller.clone_ref();
        let welcome_view_frp = self.model.view.welcome_screen().frp.clone_ref();
        let logger = self.model.logger.clone_ref();
        crate::executor::global::spawn(async move {
            if let Ok(project_manager) = controller.manage_projects() {
                match project_manager.list_projects().await {
                    Ok(projects) => {
                        let names = projects.into_iter().map(|p| p.name.into()).collect::<Vec<_>>();
                        welcome_view_frp.set_projects_list(names);
                    }
                    Err(err) => {
                        error!(logger, "Unable to get list of projects: {err}.");
                    }
                }
            }
        });
    }
}


// === Getters ===

#[allow(missing_docs)]
impl Presenter {
    pub fn view(&self) -> &view::root::View {
        &self.model.view
    }

    pub fn controller(&self) -> &controller::Ide {
        &self.model.controller
    }
}
