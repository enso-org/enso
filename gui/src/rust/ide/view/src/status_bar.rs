//! A module containing IDE status bar component definitions (frp, model, view, etc.)
//!
//! The component is currently rather a stub: it has endpoints for setting many events and
//! processes and keep them in a list, but it shows only a label of the last event/process
//! added.
//TODO[ao] Implement the status bar according to https://github.com/enso-org/ide/issues/1193
//    description
use crate::prelude::*;

use crate::graph_editor::component::node::input::area::TEXT_SIZE;

use ensogl::application::Application;
use ensogl::display;
use ensogl::display::camera::Camera2d;
use ensogl::display::shape::*;
use ensogl::display::style;
use ensogl::display::Scene;
use ensogl_gui_components::shadow;
use ensogl_text as text;
use ensogl_theme as theme;
use std::future::Future;



// =================
// === Constants ===
// =================

/// The height of the status bar.
const HEIGHT: f32 = 28.0;
/// Padding inside the status bar.
pub const PADDING: f32 = 12.0;
/// Margin between status bar and edge of the screen
const MARGIN: f32 = 12.0;
/// This should be as large as the shadow around the background.
const MAGIC_SHADOW_MARGIN: f32 = 40.0;



// =============
// === Event ===
// =============

/// Structures related to events in a status bar.
pub mod event {
    use crate::prelude::*;

    /// An id of some event displayed in a status bar.
    #[derive(Clone, CloneRef, Copy, Debug, Default, Eq, From, Hash, Into, PartialEq)]
    pub struct Id(pub usize);

    im_string_newtype! {
        /// A label assigned to some event displayed in a status bar.
        Label
    }
}



// ===============
// === Process ===
// ===============

/// Structures related to processes in a status bar.
pub mod process {
    use crate::prelude::*;

    /// An id of some process displayed in a status bar.
    #[derive(Clone, CloneRef, Copy, Debug, Default, Eq, From, Hash, Into, PartialEq)]
    pub struct Id(pub u64);

    impl Id {
        /// Return the next id.
        pub fn next(self) -> Id {
            Id(self.0 + 1)
        }
    }

    im_string_newtype! {
        /// A label assigned to some process displayed in a status bar.
        Label
    }
}



// ==================
// === Background ===
// ==================

mod background {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let theme             = ensogl_theme::application::status_bar::background;
            let theme             = style::Path::from(theme);
            let width             = Var::<Pixels>::from("input_size.x");
            let height            = Var::<Pixels>::from("input_size.y");

            let corner_radius     = style.get_number(theme.sub("corner_radius"));
            let shape_width       = width  - MAGIC_SHADOW_MARGIN.px() * 2.0;
            let shape_height      = height - MAGIC_SHADOW_MARGIN.px() * 2.0;
            let shape             = Rect((&shape_width,&shape_height));
            let shape             = shape.corners_radius(corner_radius.px());

            let bg_color          = style.get_color(&theme);
            let bg                = shape.fill(bg_color);
            let shadow_parameters = shadow::parameters_from_style_path(style,theme.sub("shadow"));
            let shadow            = shadow::from_shape_with_parameters
                (shape.into(),shadow_parameters);

            (shadow + bg).into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        add_event      (event::Label),
        add_process    (process::Label),
        finish_process (process::Id),
        clear_all      (),
    }
    Output {
        last_event        (event::Id),
        last_process      (process::Id),
        displayed_event   (Option<event::Id>),
        displayed_process (Option<process::Id>),
    }
}



// =============
// === Model ===
// =============

/// An internal model of Status Bar component
#[derive(Clone, CloneRef, Debug)]
struct Model {
    logger:          Logger,
    display_object:  display::object::Instance,
    root:            display::object::Instance,
    background:      background::View,
    label:           text::Area,
    events:          Rc<RefCell<Vec<event::Label>>>,
    processes:       Rc<RefCell<HashMap<process::Id, process::Label>>>,
    next_process_id: Rc<RefCell<process::Id>>,
    camera:          Camera2d,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = app.display.scene();
        let logger = Logger::new("StatusBar");
        let display_object = display::object::Instance::new(&logger);
        let root = display::object::Instance::new(&logger);
        let background = background::View::new(&logger);
        let label = text::Area::new(app);
        let events = default();
        let processes = default();
        let next_process_id = Rc::new(RefCell::new(process::Id(1)));
        let camera = scene.camera();

        scene.layers.panel.add_exclusive(&background);
        label.remove_from_scene_layer(&scene.layers.main);
        label.add_to_scene_layer(&scene.layers.panel_text);

        let text_color_path = theme::application::status_bar::text;
        let style = StyleWatch::new(&app.display.scene().style_sheet);
        let text_color = style.get_color(text_color_path);
        label.frp.set_color_all.emit(text_color);
        label.frp.set_default_color.emit(text_color);

        Self {
            logger,
            display_object,
            root,
            background,
            label,
            events,
            processes,
            next_process_id,
            camera,
        }
        .init()
    }

    fn init(self) -> Self {
        self.display_object.add_child(&self.root);
        self.root.add_child(&self.background);
        self.root.add_child(&self.label);

        self.update_layout();
        self.camera_changed();

        self
    }

    fn camera_changed(&self) {
        let screen = self.camera.screen();
        let x = -screen.width / 2.0 + MARGIN;
        let y = -screen.height / 2.0 + MARGIN;
        self.root.set_position_x(x.round());
        self.root.set_position_y(y.round());
    }

    fn update_layout(&self) {
        self.label.set_position_x(PADDING);
        self.label.set_position_y(HEIGHT / 2.0 + TEXT_SIZE / 2.0);

        let bg_width = if self.label.width.value() > 0.0 {
            PADDING + self.label.width.value() + PADDING
        } else {
            0.0
        };
        let bg_height = HEIGHT;
        self.background.size.set(Vector2(
            bg_width + 2.0 * MAGIC_SHADOW_MARGIN,
            bg_height + 2.0 * MAGIC_SHADOW_MARGIN,
        ));
        self.background.set_position(Vector3(bg_width / 2.0, bg_height / 2.0, 0.0));
    }

    fn add_event(&self, label: &event::Label) -> event::Id {
        let mut events = self.events.borrow_mut();
        let new_id = event::Id(events.len());
        events.push(label.clone_ref());
        new_id
    }

    fn add_process(&self, label: &process::Label) -> process::Id {
        let mut processes = self.processes.borrow_mut();
        let mut next_process_id = self.next_process_id.borrow_mut();
        let new_id = *next_process_id;
        *next_process_id = next_process_id.next();
        processes.insert(new_id, label.clone_ref());
        new_id
    }

    /// Returns true if there was process with given id.
    fn finish_process(&self, id: process::Id) -> bool {
        self.processes.borrow_mut().remove(&id).is_some()
    }

    /// Returns empty string if no event received so far.
    fn last_event_message(&self) -> event::Label {
        self.events.borrow().last().cloned().unwrap_or_default()
    }

    fn clear_all(&self) {
        self.events.borrow_mut().clear();
        self.processes.borrow_mut().clear();
    }
}



// ============
// === View ===
// ============

/// The StatusBar component view.
///
/// The status bar gathers information about events and processes occurring in the Application.
// TODO: This is a stub. Extend it when doing https://github.com/enso-org/ide/issues/1193
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    frp:   Frp,
    model: Model,
}

impl View {
    /// Create new StatusBar view.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        let network = &frp.network;
        let scene = app.display.scene();

        enso_frp::extend! { network
            event_added       <- frp.add_event.map(f!((label) model.add_event(label)));
            process_added     <- frp.add_process.map(f!((label) model.add_process(label)));
            _process_finished <- frp.finish_process.filter_map(f!((id)
                model.finish_process(*id).as_some(*id)
            ));
            displayed_process_finished <- frp.finish_process
                .map2(&frp.output.displayed_process, |fin,dis|(*fin,*dis))
                .filter(|(fin,dis)| dis.contains(fin));

            label_after_adding_event <- frp.add_event.map(
                |label| AsRef::<ImString>::as_ref(label).clone_ref()
            );
            label_after_adding_process <- frp.add_process.map(
                |label| AsRef::<ImString>::as_ref(label).clone_ref()
            );
            label_after_finishing_process <- displayed_process_finished.map(
                f_!([model] AsRef::<ImString>::as_ref(&model.last_event_message()).clone_ref())
            );

            label <- any(label_after_adding_event,label_after_adding_process,label_after_finishing_process);
            eval label ((label) model.label.set_content(label.to_string()));

            eval_ frp.clear_all (model.clear_all());

            frp.source.last_event   <+ event_added;
            frp.source.last_process <+ process_added;

            frp.source.displayed_event <+ event_added.map(|id| Some(*id));
            frp.source.displayed_event <+ process_added.constant(None);
            frp.source.displayed_event <+ frp.clear_all.constant(None);
            frp.source.displayed_process <+ process_added.map(|id| Some(*id));
            frp.source.displayed_process <+ event_added.constant(None);
            frp.source.displayed_process <+ displayed_process_finished.constant(None);
            frp.source.displayed_process <+ frp.clear_all.constant(None);

            eval_ model.label.output.width (model.update_layout());
            eval_ scene.frp.camera_changed (model.camera_changed());
        }

        Self { frp, model }
    }

    /// Returns a future will add a new process to the status bar, evaluate `f` and then mark the
    /// process as finished.
    pub fn process<'f, F>(
        &self,
        label: process::Label,
        f: F,
    ) -> impl Future<Output = F::Output> + 'f
    where
        F: Future + 'f,
    {
        let add_process = self.frp.add_process.clone_ref();
        let last_process = self.frp.last_process.clone_ref();
        let finish_process = self.frp.finish_process.clone_ref();
        async move {
            add_process.emit(label);
            let id = last_process.value();
            let result = f.await;
            finish_process.emit(id);
            result
        }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.model.display_object
    }
}

impl Deref for View {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}
