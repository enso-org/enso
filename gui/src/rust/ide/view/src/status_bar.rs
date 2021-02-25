//! A module containing IDE status bar component definitions (frp, model, view, etc.)
//!
//! The component is currently rather a stub: it has endpoints for setting many events and
//! processes and keep them in a list, but it shows only a label of the last event/process
//! added.
//TODO[ao] Implement the status bar according to https://github.com/enso-org/ide/issues/1193
//    description
use crate::prelude::*;

use ensogl::application::Application;
use ensogl::display;
use ensogl::display::Scene;
use ensogl_text as text;
use std::future::Future;


// =================
// === Constants ===
// =================

/// The height of the status bar.
pub const HEIGHT:f32 = 40.0;
/// Padding inside the status bar.
pub const PADDING:f32 = 12.0;



// =============
// === Event ===
// =============

/// Structures related to events in a status bar.
pub mod event {
    use crate::prelude::*;

    /// An id of some event displayed in a status bar.
    #[derive(Clone,CloneRef,Copy,Debug,Default,Eq,From,Hash,Into,PartialEq)]
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
    #[derive(Clone,CloneRef,Copy,Debug,Default,Eq,From,Hash,Into,PartialEq)]
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



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        add_event      (event::Label),
        add_process    (process::Label),
        finish_process (process::Id),
    }
    Output {
        width             (f32),
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
#[derive(Clone,CloneRef,Debug)]
struct Model {
    logger          : Logger,
    display_object  : display::object::Instance,
    label           : text::Area,
    events          : Rc<RefCell<Vec<event::Label>>>,
    processes       : Rc<RefCell<HashMap<process::Id,process::Label>>>,
    next_process_id : Rc<RefCell<process::Id>>,
}

impl Model {
    fn new(app:&Application) -> Self {
        let logger          = Logger::new("StatusBar");
        let display_object  = display::object::Instance::new(&logger);
        let label           = text::Area::new(app);
        let events          = default();
        let processes       = default();
        let next_process_id = default();
        display_object.add_child(&label);
        Self {logger,display_object,label,events,processes,next_process_id}
    }

    fn set_width(&self, width:f32) {
        self.label.set_position_x(-width/2.0 + PADDING);
    }

    fn add_event(&self, label:&event::Label) -> event::Id {
        let mut events = self.events.borrow_mut();
        let new_id     = event::Id(events.len());
        events.push(label.clone_ref());
        new_id
    }

    fn add_process(&self, label:&process::Label) -> process::Id {
        let mut processes       = self.processes.borrow_mut();
        let mut next_process_id = self.next_process_id.borrow_mut();
        let new_id              = *next_process_id;
        *next_process_id        = next_process_id.next();
        processes.insert(new_id,label.clone_ref());
        new_id
    }

    /// Returns true if there was process with given id.
    fn finish_process(&self, id:process::Id) -> bool {
        self.processes.borrow_mut().remove(&id).is_some()
    }

    /// Returns empty string if no event received so far.
    fn last_event_message(&self) -> event::Label {
        self.events.borrow().last().cloned().unwrap_or_default()
    }
}



// ============
// === View ===
// ============

/// The StatusBar component view.
///
/// The status bar gathers information about events and processes occurring in the Application.
// TODO: This is a stub. Extend it when doing https://github.com/enso-org/ide/issues/1193
#[derive(Clone,CloneRef,Debug)]
pub struct View {
    frp   : Frp,
    model : Model,
}

impl View {
    /// Create new StatusBar view.
    pub fn new(app:&Application) -> Self {
        let frp         = Frp::new();
        let model       = Model::new(&app);
        let network     = &frp.network;
        let scene       = app.display.scene();
        let scene_shape = scene.shape().clone_ref();

        model.label.remove_from_scene_layer_DEPRECATED(&scene.layers.main);
        model.label.add_to_scene_layer_DEPRECATED(&scene.layers.breadcrumbs);

        enso_frp::extend! { network
            event_added       <- frp.add_event.map(f!((label) model.add_event(label)));
            process_added     <- frp.add_process.map(f!((label) model.add_process(label)));
            _process_finished <- frp.finish_process.filter_map(f!((id)
                model.finish_process(*id).as_some(*id)
            ));
            displayed_process_finished <- frp.finish_process.all(&frp.output.displayed_process).filter(|(fin,dis)| dis.contains(fin));

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

            frp.source.last_event   <+ event_added;
            frp.source.last_process <+ process_added;

            frp.source.displayed_event <+ event_added.map(|id| Some(*id));
            frp.source.displayed_event <+ process_added.constant(None);
            frp.source.displayed_process <+ process_added.map(|id| Some(*id));
            frp.source.displayed_process <+ event_added.constant(None);
            frp.source.displayed_process <+ displayed_process_finished.constant(None);

            width <- scene_shape.map(|scene_shape| scene_shape.width);
            eval width ((width) model.set_width(*width));
            eval scene_shape ((scene_shape)
                model.display_object.set_position_y(-scene_shape.height / 2.0 + HEIGHT / 2.0);
            );
            frp.source.width <+ width;
        }

        model.set_width(scene_shape.value().width);

        Self {frp,model}
    }

    /// Returns a future will add a new process to the status bar, evaluate `f` and then mark the
    /// process as finished.
    pub fn process<'f, F>(&self, label:process::Label, f:F) -> impl Future<Output=F::Output> + 'f
    where F : Future + 'f {
        let add_process    = self.frp.add_process.clone_ref();
        let last_process   = self.frp.last_process.clone_ref();
        let finish_process = self.frp.finish_process.clone_ref();
        async move {
            add_process.emit(label);
            let id     = last_process.value();
            let result = f.await;
            finish_process.emit(id);
            result
        }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance<Scene> { &self.model.display_object }
}

impl Deref for View {
    type Target = Frp;

    fn deref(&self) -> &Self::Target { &self.frp }
}
