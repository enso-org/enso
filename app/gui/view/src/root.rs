//! Root View of the IDE.
//!
//! The main entry point to the IDE which can display either Welcome Screen or Project View.
//! Initially displays Welcome Screen. Lazily initializes Project View on `switch_view_to_project`
//! call.

use ensogl::prelude::*;

use engine_protocol::project_manager::ProjectMetadata;
use enso_frp as frp;
use ensogl::application;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::StyleWatchFrp;
use std::rc::Rc;



// =============
// === Model ===
// =============

/// Two possible states of Root View.
#[derive(Clone, Debug, PartialEq)]
enum State {
    /// Displaying Welcome Screen.
    WelcomeScreen,
    /// Displaying Project View with some opened project.
    OpenedProject,
}

/// Root View model. Stores both Welcome Screen and Project views and handles their
/// visibility.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    app:            Application,
    display_object: display::object::Instance,
    state:          Rc<CloneCell<State>>,
    status_bar:     crate::status_bar::View,
    welcome_view:   crate::welcome_screen::View,
    project_view:   Rc<CloneCell<Option<crate::project::View>>>,
    frp:            Frp,
}

impl Model {
    /// Constuctor.
    pub fn new(app: &Application, frp: &Frp) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        let state = Rc::new(CloneCell::new(State::WelcomeScreen));
        let status_bar = crate::status_bar::View::new(&app);
        display_object.add_child(&status_bar);
        let welcome_view = app.new_view::<crate::welcome_screen::View>();
        let project_view = Rc::new(CloneCell::new(None));
        display_object.add_child(&welcome_view);
        let frp = frp.clone_ref();

        Self { app, display_object, state, status_bar, welcome_view, project_view, frp }
    }

    /// Switch displayed view from Project View to Welcome Screen. Project View will not be
    /// deallocated.
    pub fn switch_view_to_welcome_screen(&self) {
        self.state.set(State::WelcomeScreen);
        if let Some(project_view) = self.project_view.get() {
            self.display_object.remove_child(&project_view);
        }
        self.display_object.add_child(&self.welcome_view);
    }

    /// Switch displayed view from Welcome Screen to Project View. Will initialize Project View if
    /// it wasn't initialized before.
    pub fn switch_view_to_project(&self) {
        self.state.set(State::OpenedProject);
        self.display_object.remove_child(&self.welcome_view);
        self.display_object.add_child(&self.get_or_init_project_view());
    }

    /// Perform lazy initialization of the underlaying Project View.
    pub fn get_or_init_project_view(&self) -> crate::project::View {
        self.init_project_view();
        self.project_view.get().expect("Project view initialization failed.")
    }

    fn init_project_view(&self) {
        if self.project_view.get().is_none() {
            let network = &self.frp.network;
            let view = self.app.new_view::<crate::project::View>();
            let project_list_frp = &view.project_list().frp;
            let status_bar = &self.status_bar;
            let display_object = &self.display_object;
            frp::new_bridge_network! { [network, view.network] project_bridge
                fs_vis_shown <- view.fullscreen_visualization_shown.on_true();
                fs_vis_hidden <- view.fullscreen_visualization_shown.on_false();
                eval fs_vis_shown ((_) status_bar.unset_parent());
                eval fs_vis_hidden ([display_object, status_bar](_) display_object.add_child(&status_bar));

                self.frp.source.selected_project <+ project_list_frp.selected_project;
            }
            self.project_view.set(Some(view));
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Switch displayed view to Project View. Lazily intializes Project View.
        switch_view_to_project(),
        /// Switch displayed view to Welcome Screen.
        switch_view_to_welcome_screen(),
    }
    Output {
        /// The selected project in the project list
        selected_project (Option<ProjectMetadata>),
    }
}



// ============
// === View ===
// ============

/// Root View of the IDE. Displays either Welcome Screen or Project View.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct View {
    model:   Model,
    pub frp: Frp,
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl View {
    /// Constuctor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app, &frp);
        let network = &frp.network;
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let offset_y = style.get_number(ensogl_hardcoded_theme::application::status_bar::offset_y);

        frp::extend! { network
            init <- source::<()>();

            eval_ frp.switch_view_to_project(model.switch_view_to_project());
            eval_ frp.switch_view_to_welcome_screen(model.switch_view_to_welcome_screen());
            offset_y <- all(&init,&offset_y)._1();
            eval offset_y ((offset_y) model.status_bar.set_y(*offset_y));

            model.status_bar.add_event <+ app.frp.show_notification.map(|message| {
                message.into()
            });
        }
        init.emit(());
        Self { model, frp }
    }

    /// Status Bar view from Project View.
    pub fn status_bar(&self) -> &crate::status_bar::View {
        &self.model.status_bar
    }

    /// Lazily initializes Project View.
    pub fn project(&self) -> crate::project::View {
        self.model.get_or_init_project_view()
    }

    /// Welcome View.
    pub fn welcome_screen(&self) -> &crate::welcome_screen::View {
        &self.model.welcome_view
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for View {
    fn label() -> &'static str {
        "RootView"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }

    fn app(&self) -> &Application {
        &self.model.app
    }
}
