//! The integration layer between IDE controllers and the view.

pub mod file_system;
pub mod project;
pub mod visualization;

use crate::prelude::*;

use crate::controller::ide::StatusNotification;
use crate::model::undo_redo::Aware;

use ide_view::graph_editor::SharedHashMap;


// =======================
// === IDE Integration ===
// =======================

// === Model ===

/// The model of integration object. It is extracted and kept in Rc, so it can be referred to from
/// various FRP endpoints or executor tasks.
#[derive(Debug)]
struct Model {
    logger:              Logger,
    controller:          controller::Ide,
    view:                ide_view::root::View,
    project_integration: RefCell<Option<project::Integration>>,
}

impl Model {
    /// Create a new project integration
    fn setup_and_display_new_project(self: Rc<Self>) {
        // Remove the old integration first. We want to be sure the old and new integrations will
        // not race for the view.
        *self.project_integration.borrow_mut() = None;

        if let Some(project_model) = self.controller.current_project() {
            // We know the name of new project before it loads. We set it right now to avoid
            // displaying placeholder on the scene during loading.
            let project_view = self.view.project();
            let breadcrumbs = &project_view.graph().model.breadcrumbs;
            breadcrumbs.project_name(project_model.name().to_string());

            let status_notifications = self.controller.status_notifications().clone_ref();
            let project = controller::Project::new(project_model, status_notifications.clone_ref());

            executor::global::spawn(async move {
                match project.initialize().await {
                    Ok(result) => {
                        let view = project_view;
                        let status_bar = self.view.status_bar().clone_ref();
                        let text = result.main_module_text;
                        let graph = result.main_graph;
                        let ide = self.controller.clone_ref();
                        let project = project.model;
                        let main = result.main_module_model;
                        let integration = project::Integration::new(
                            view, status_bar, graph, text, ide, project, main,
                        );
                        // We don't want any initialization-related changes to appear on undo stack.
                        integration.graph_controller().undo_redo_repository().clear_all();
                        *self.project_integration.borrow_mut() = Some(integration);
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
}


// === Integration ===

/// The Integration Object
///
/// It is responsible for integrating IDE controllers and views, so user actions will work, and
/// notifications from controllers will update the view.
#[derive(Clone, CloneRef, Debug)]
pub struct Integration {
    model: Rc<Model>,
}

impl Integration {
    /// Create the integration of given controller and view.
    pub fn new(controller: controller::Ide, view: ide_view::root::View) -> Self {
        let logger = Logger::new("ide::Integration");
        let project_integration = default();
        let model = Rc::new(Model { logger, controller, view, project_integration });
        Self { model }.init()
    }

    /// Initialize integration, so FRP outputs of the view will call the proper controller methods,
    /// and controller notifications will be delivered to the view accordingly.
    pub fn init(self) -> Self {
        self.initialize_status_bar_integration();
        self.initialize_controller_integration();
        self.model.clone_ref().setup_and_display_new_project();
        self
    }

    fn initialize_status_bar_integration(&self) {
        use controller::ide::BackgroundTaskHandle as ControllerHandle;
        use ide_view::status_bar::process::Id as ViewHandle;

        let logger = self.model.logger.clone_ref();
        let process_map = SharedHashMap::<ControllerHandle, ViewHandle>::new();
        let status_bar = self.model.view.status_bar().clone_ref();
        let status_notif_sub = self.model.controller.status_notifications().subscribe();
        let status_notif_updates = status_notif_sub.for_each(move |notification| {
            info!(logger, "Received notification {notification:?}");
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

        executor::global::spawn(status_notif_updates)
    }

    fn initialize_controller_integration(&self) {
        let stream = self.model.controller.subscribe();
        let weak = Rc::downgrade(&self.model);
        executor::global::spawn(stream.for_each(move |notification| {
            if let Some(model) = weak.upgrade() {
                match notification {
                    controller::ide::Notification::NewProjectCreated
                    | controller::ide::Notification::ProjectOpened =>
                        model.setup_and_display_new_project(),
                }
            }
            futures::future::ready(())
        }));
    }
}
