pub mod graph;
pub mod project;

pub use graph::Graph;
pub use project::Project;

use crate::prelude::*;

use crate::controller::ide::StatusNotification;
use crate::executor::global::spawn_stream_handler;
use crate::presenter;

use ide_view as view;
use ide_view::graph_editor::SharedHashMap;

#[derive(Debug)]
struct Model {
    logger:          Logger,
    current_project: RefCell<Option<presenter::Project>>,
    controller:      controller::Ide,
    view:            view::root::View,
}

impl Model {
    /// Create a new project integration
    fn setup_and_display_new_project(self: Rc<Self>) {
        // Remove the old integration first. We want to be sure the old and new integrations will
        // not race for the view.
        *self.current_project.borrow_mut() = None;

        if let Some(project_model) = self.controller.current_project() {
            // We know the name of new project before it loads. We set it right now to avoid
            // displaying placeholder on the scene during loading.
            let project_view = self.view.project();
            let breadcrumbs = &project_view.graph().model.breadcrumbs;
            breadcrumbs.project_name(project_model.name().to_string());

            let status_notifications = self.controller.status_notifications().clone_ref();
            let project_controller =
                controller::Project::new(project_model, status_notifications.clone_ref());

            executor::global::spawn(async move {
                match presenter::Project::initialize(project_controller, project_view).await {
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
}

#[derive(Clone, CloneRef, Debug)]
pub struct Presenter {
    model: Rc<Model>,
}

impl Presenter {
    pub fn new(controller: controller::Ide, view: ide_view::root::View) -> Self {
        let logger = Logger::new("Presenter");
        let current_project = default();
        let model = Rc::new(Model { logger, controller, view, current_project });
        Self { model }.init()
    }

    fn init(self) -> Self {
        self.setup_status_bar_notification_handler();
        self.setup_controller_notification_handler();
        self.model.clone_ref().setup_and_display_new_project();
        self
    }

    fn setup_status_bar_notification_handler(&self) {
        use controller::ide::BackgroundTaskHandle as ControllerHandle;
        use ide_view::status_bar::process::Id as ViewHandle;

        let logger = self.model.logger.clone_ref();
        let process_map = SharedHashMap::<ControllerHandle, ViewHandle>::new();
        let status_bar = self.model.view.status_bar().clone_ref();
        let status_notif_sub = self.model.controller.status_notifications().subscribe();
        let weak = Rc::downgrade(&self.model);
        spawn_stream_handler(weak, status_notif_sub, move |notification, _| {
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
}
