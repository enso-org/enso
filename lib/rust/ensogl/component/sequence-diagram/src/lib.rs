//! An EnsoGL implementation of a basic sequence diagram.

#![recursion_limit = "256"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;

use crate::labeled_line::Cap;
use crate::labeled_line::LabeledLine;

use enso_profiler_data::Profile;
use enso_profiler_enso_data::Metadata;
use ensogl::frp;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;


// ==============
// === Export ===
// ==============

pub mod labeled_line;
pub mod shape;



// =================
// === Constants ===
// =================

const ROW_HEIGHT: f32 = 50.0;
const LINE_WIDTH: f32 = 5.0;
const INFINITE: f32 = 99999.0;



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_profile(Vec<Profile<Metadata>>),
        set_origin(f32)
    }
    Output {
        height(f32)
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &frp::Network,
        api: &Self::Private,
        _app: &Application,
        model: &Model,
        _style: &StyleWatchFrp,
    ) {
        frp::extend! { network
            eval api.input.set_profile ((profile) model.set_profile(profile));
            let model = model.clone_ref();
            api.output.height <+ api.input.set_profile.map(move |_| model.height());


        }
    }
}



// =============
// === Model ===
// =============

/// Internal model of the SequenceDiagram.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    // Required for dynamically creating new lines.
    app:            Application,
    display_object: display::object::Instance,
    actor_lines:    Rc<RefCell<Vec<LabeledLine>>>,
    message_lines:  Rc<RefCell<Vec<LabeledLine>>>,
    origin_x:       Rc<Cell<f32>>,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "SequenceDiagram"
    }

    fn new(app: &Application) -> Self {
        let app = app.clone();
        let display_object = display::object::Instance::new();
        let actor_lines = default();
        let message_lines = default();
        let origin_x = default();
        Self { app, display_object, actor_lines, message_lines, origin_x }
    }
}

impl Model {
    fn set_profile(&self, profiles: &[Profile<Metadata>]) {
        if let [profile_a, profile_b] = profiles {
            let diagram =
                enso_profiler_enso_data::beanpole::Diagram::from_profiles(&[profile_a, profile_b]);

            let actor_lines = diagram
                .processes()
                .iter()
                .enumerate()
                .map(|(ix, process)| {
                    let line = LabeledLine::new(&self.app);
                    self.display_object.add_child(&line);
                    line.set_content(process.to_string());
                    line.frp().set_size(Vector2::new(LINE_WIDTH, INFINITE));
                    line.set_y(ROW_HEIGHT * ix as f32);
                    line.set_rotation_z(90.0_f32.to_radians());
                    line
                })
                .collect();

            let message_lines = diagram
                .messages()
                .iter()
                .map(|message| {
                    let line = LabeledLine::new(&self.app);
                    self.display_object.add_child(&line);
                    let label = format!("{} ({:.2})", &message.label, message.time);
                    line.set_content(label);

                    let cap = {
                        if message.recipient.id as i32 - message.sender.id as i32 > 0 {
                            Cap::Start
                        } else {
                            Cap::End
                        }
                    };
                    line.set_cap(cap);

                    let height_rows =
                        (message.recipient.id as i32 - message.sender.id as i32).unsigned_abs();
                    let height_px = ROW_HEIGHT * height_rows as f32;
                    let start = message.recipient.id.min(message.sender.id) as u32;
                    line.frp().set_size(Vector2::new(LINE_WIDTH, height_px));

                    line.set_x(message.time as f32);
                    line.set_y(ROW_HEIGHT * start as f32 + height_px / 2.0);
                    line
                })
                .collect();

            self.actor_lines.replace(actor_lines).into_iter().for_each(|item| item.unset_parent());
            self.message_lines
                .replace(message_lines)
                .into_iter()
                .for_each(|item| item.unset_parent());
        } else {
            warn!("Invalid profile data received");
            self.reset()
        }
    }

    fn reset(&self) {
        self.actor_lines.take();
        self.message_lines.take();
        self.origin_x.take();
    }

    fn height(&self) -> f32 {
        let lines = self.actor_lines.deref().borrow().deref().len();
        lines as f32 * ROW_HEIGHT
    }

    /// Set the timestamp placed at the x-coordinate origin. For example, a value of 10.0, will mean
    /// that a message that happens at time 10ms, will be placed at the zero x-coordinate (the same
    /// position as the root display object).
    pub fn set_origin_x(&self, origin_x: f32) {
        let old_origin = self.origin_x.replace(origin_x);
        let delta = old_origin - origin_x;
        self.message_lines
            .deref()
            .borrow()
            .deref()
            .iter()
            .for_each(|line| line.update_x(|x| x + delta))
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

/// A visualisation of messages passed between different actors.
///
/// This visualisation renders a horizontal line for each actor and shows an arrow for each message.
/// The arrow will point from the sender of the message to the receive of the message. Hovering
/// over a actor line will show the name of the actor as a tooltip, hovering over a message will
/// show the name of the message as a tooltip.
pub type SequenceDiagram = component::ComponentView<Model, Frp>;
