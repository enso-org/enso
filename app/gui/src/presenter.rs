//! The Presenter is a layer between logical part of the IDE (controllers, engine models) and the
//! views (the P letter in MVP pattern). The presenter reacts to changes in the controllers, and
//! actively updates the view. It also passes all user interactions from view to controllers.

use crate::prelude::*;

use crate::config::ProjectToOpen;
use crate::controller::ide::StatusNotification;
use crate::executor::global::spawn_stream_handler;
use crate::presenter;

use enso_frp as frp;
use ensogl::system::js;
use ide_view as view;
use ide_view::graph_editor::SharedHashMap;
use std::time::Duration;


// ==============
// === Export ===
// ==============

pub mod code;
pub mod graph;
pub mod project;
pub mod searcher;

pub use code::Code;
pub use graph::Graph;
pub use project::Project;
pub use searcher::Searcher;



// =================
// === Constants ===
// =================

/// We don't know how long opening the project will take, but we still want to show a fake
/// progress indicator for the user. This constant represents how long the spinner will run for in
/// milliseconds.
const OPEN_PROJECT_SPINNER_TIME_MS: u64 = 5_000;
/// The interval in milliseconds at which we should increase the spinner
const OPEN_PROJECT_SPINNER_UPDATE_PERIOD_MS: u64 = 10;



// =============
// === Model ===
// =============

#[derive(Debug)]
struct Model {
    current_project: RefCell<Option<Project>>,
    controller:      controller::Ide,
    view:            view::root::View,
}

impl Model {
    /// Instantiate a new project presenter, which will display current project in the view.
    #[profile(Task)]
    fn setup_and_display_new_project(self: Rc<Self>) {
        // Remove the old integration first. We want to be sure the old and new integrations will
        // not race for the view.
        *self.current_project.borrow_mut() = None;
        let project_model = match self.controller.current_project() {
            Some(model) => model,
            None => return,
        };
        self.view.switch_view_to_project();
        // We know the name of new project before it loads. We set it right now to avoid
        // displaying a placeholder on the scene during loading.
        let project_view = self.view.project();
        let status_bar = self.view.status_bar().clone_ref();
        let breadcrumbs = &project_view.graph().model.breadcrumbs;
        breadcrumbs.project_name(project_model.name().to_string());

        let status_notifications = self.controller.status_notifications().clone_ref();
        let ide_controller = self.controller.clone_ref();
        let project_controller = controller::Project::new(project_model, status_notifications);
        let project_presenter = presenter::Project::initialize(
            ide_controller,
            project_controller,
            project_view,
            status_bar,
        );
        crate::executor::global::spawn(async move {
            match project_presenter.await {
                Ok(project) => {
                    *self.current_project.borrow_mut() = Some(project);
                }
                Err(err) => {
                    let err_msg = format!("Failed to initialize project: {err}");
                    error!("{err_msg}");
                    self.controller.status_notifications().publish_event(err_msg);
                }
            }
        });
    }

    fn close_project(&self) {
        *self.current_project.borrow_mut() = None;
        // Clear the graph editor so that it will not display any nodes from the previous
        // project when the new project is loaded.
        self.view.project().graph().remove_all_nodes();
    }

    /// Open a project by name. It makes two calls to Project Manager: one for listing projects and
    /// a second one for opening the project.
    #[profile(Task)]
    pub fn open_project(&self, project_name: String) {
        let controller = self.controller.clone_ref();
        crate::executor::global::spawn(with_progress_indicator(|| async move {
            if let Ok(managing_api) = controller.manage_projects() {
                if let Err(err) = managing_api.open_project_by_name(project_name).await {
                    error!("Cannot open project by name: {err}.");
                }
            } else {
                warn!("Project Manager API not available, cannot open project.");
            }
        }));
    }

    /// Create a new project. `template` is an optional name of the project template passed to the
    /// Engine. It makes a call to Project Manager.
    #[profile(Task)]
    fn create_project(&self, template: Option<&str>) {
        let controller = self.controller.clone_ref();
        if let Ok(template) =
            template.map(double_representation::name::project::Template::from_text).transpose()
        {
            crate::executor::global::spawn(with_progress_indicator(|| async move {
                if let Ok(managing_api) = controller.manage_projects() {
                    if let Err(err) = managing_api.create_new_project(None, template.clone()).await
                    {
                        if let Some(template) = template {
                            error!("Could not create new project from template {template}: {err}.");
                        } else {
                            error!("Could not create new project: {err}.");
                        }
                    }
                } else {
                    warn!("Project Manager API not available, cannot create project.");
                }
            }))
        } else if let Some(template) = template {
            error!("Invalid project template name: {template}");
        };
    }

    /// Open a project by name or ID. If no project with the given name exists, it will be created.
    #[profile(Task)]
    fn open_or_create_project(&self, project: ProjectToOpen) {
        let controller = self.controller.clone_ref();
        crate::executor::global::spawn(with_progress_indicator(|| async move {
            if let Ok(managing_api) = controller.manage_projects() {
                if let Err(error) = managing_api.open_or_create_project(project).await {
                    error!("Cannot open or create project. {error}");
                }
            } else {
                warn!("Project Manager API not available, cannot open or create project.");
            }
        }));
    }
}

/// Show a full-screen spinner for the exact duration of the specified function.
async fn with_progress_indicator<F, T>(f: F)
where
    F: FnOnce() -> T,
    T: Future<Output = ()>, {
    // TODO[ss]: Use a safer variant of getting the JS app. This one gets a variable from JS, casts
    // it to a type, etc. Somewhere in EnsoGL we might already have some logic for getting the JS
    // app and throwing an error if it's not defined.
    let Ok(app) = js::app() else { return error!("Failed to get JavaScript EnsoGL app.") };
    app.show_progress_indicator(0.0);

    let (finished_tx, finished_rx) = futures::channel::oneshot::channel();
    let spinner_progress = futures::stream::unfold(0, |time| async move {
        enso_web::sleep(Duration::from_millis(OPEN_PROJECT_SPINNER_UPDATE_PERIOD_MS)).await;
        let new_time = time + OPEN_PROJECT_SPINNER_UPDATE_PERIOD_MS;
        if new_time < OPEN_PROJECT_SPINNER_TIME_MS {
            let progress = new_time as f32 / OPEN_PROJECT_SPINNER_TIME_MS as f32;
            Some((progress, new_time))
        } else {
            None
        }
    })
    .take_until(finished_rx);
    executor::global::spawn(spinner_progress.for_each(|progress| async move {
        let Ok(app) = js::app() else { return error!("Failed to get JavaScript EnsoGL app.") };
        app.show_progress_indicator(progress);
    }));

    f().await;

    // This fails when the spinner progressed until the end before the function got completed
    // and therefore the receiver got dropped, so we'll ignore the result.
    let _ = finished_tx.send(());

    let Ok(app) = js::app() else { return error!("Failed to get JavaScript EnsoGL app.") };
    app.hide_progress_indicator();
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
    #[profile(Task)]
    pub fn new(controller: controller::Ide, view: ide_view::root::View) -> Self {
        let current_project = default();
        let model = Rc::new(Model { controller, view, current_project });

        frp::new_network! { network
            let welcome_view_frp = &model.view.welcome_screen().frp;
            eval welcome_view_frp.open_project((name) model.open_project(name.to_owned()));
            eval welcome_view_frp.create_project((templ) model.create_project(templ.as_deref()));

            let root_frp = &model.view.frp;
            root_frp.switch_view_to_project <+ welcome_view_frp.create_project.constant(());
            root_frp.switch_view_to_project <+ welcome_view_frp.open_project.constant(());

            eval root_frp.selected_project ([model] (project) {
                if let Some(project) = project {
                    model.close_project();
                    model.open_project(project.name.to_string());
                }
            });
        }

        Self { model, network }.init()
    }

    #[profile(Detail)]
    fn init(self) -> Self {
        self.setup_status_bar_notification_handler();
        self.setup_controller_notification_handler();
        executor::global::spawn(self.clone_ref().set_projects_list_on_welcome_screen());
        self.model.clone_ref().setup_and_display_new_project();
        self
    }

    fn setup_status_bar_notification_handler(&self) {
        use controller::ide::BackgroundTaskHandle as ControllerHandle;
        use ide_view::status_bar::process::Id as ViewHandle;

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
                        warn!("Controllers finished process not displayed in view");
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
                controller::ide::Notification::ProjectOpened =>
                    model.setup_and_display_new_project(),
                controller::ide::Notification::ProjectClosed => {
                    model.close_project();
                }
            }
            futures::future::ready(())
        });
    }

    #[profile(Detail)]
    async fn set_projects_list_on_welcome_screen(self) {
        if let Ok(project_manager) = self.model.controller.manage_projects() {
            match project_manager.list_projects().await {
                Ok(projects) => {
                    let names = projects.into_iter().map(|p| p.name.into()).collect::<Vec<_>>();
                    self.model.view.welcome_screen().frp.set_projects_list(names);
                }
                Err(err) => {
                    error!("Unable to get list of projects: {err}.");
                }
            }
        }
    }

    /// Open a project by name or ID. If no project with the given name exists, it will be created.
    pub fn open_or_create_project(&self, project: ProjectToOpen) {
        self.model.open_or_create_project(project)
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
