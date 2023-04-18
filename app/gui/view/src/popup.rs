//! A temporary text message on top of the screen.

use crate::prelude::*;

use ensogl::animation::delayed::DelayedAnimation;
use ensogl::application::Application;
use ensogl::display;
use ensogl::Animation;
use ensogl_component::label::Label;



// =================
// === Constants ===
// =================

const PADDING_TOP: f32 = 50.0;
const DEFAULT_DELAY_MS: f32 = 5_000.0;



// =============
// === Model ===
// =============

/// Text label that disappears after a predefined delay.
#[derive(Debug, Clone, CloneRef)]
struct Model {
    label:           Label,
    network:         frp::Network,
    delay_animation: DelayedAnimation,
    /// Show the pop-up with the given message.
    pub show:        frp::Source<String>,
}

impl Model {
    /// Constructor.
    fn new(app: &Application) -> Self {
        let network = frp::Network::new("Popup");
        let label = Label::new(app);
        label.set_opacity(0.0);

        let opacity_animation = Animation::new(&network);
        network.store(&opacity_animation);
        let delay_animation = DelayedAnimation::new(&network);
        delay_animation.set_delay(DEFAULT_DELAY_MS);
        delay_animation.set_duration(0.0);
        network.store(&delay_animation);

        frp::extend! { network
            show <- source::<String>();

            eval show ([label, delay_animation](text) {
                label.set_content(text);
                delay_animation.reset();
                delay_animation.start();
            });

            opacity_animation.target <+ show.constant(1.0);
            opacity_animation.target <+ delay_animation.on_end.constant(0.0);
            label.set_opacity <+ opacity_animation.value;
        }

        Self { label, network, show, delay_animation }
    }

    /// Set the message.
    fn set_label(&self, content: String) {
        self.show.emit(content);
    }

    /// Set a delay in milliseconds after which the label will disappear.
    fn set_delay(&self, delay: f32) {
        self.delay_animation.set_delay(delay);
    }

    /// Return the height of the label.
    fn label_height(&self) -> f32 {
        self.label.size.value().y
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        set_label (String),
        set_delay (f32),
    }
    Output {}
}



// ============
// === View ===
// ============

/// A temporary text message on top of the screen.
#[derive(Debug, Clone, CloneRef)]
pub struct View {
    frp:   Frp,
    model: Model,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        let network = &frp.network;

        frp::extend! { network
            init <- source_();
            let shape  = app.display.default_scene.shape();
            _eval <- all_with(shape, &init, f!([model](scene_size, _init) {
                let half_height = scene_size.height / 2.0;
                let label_height = model.label_height();
                let pos_y = half_height - PADDING_TOP - label_height / 2.0;
                model.label.display_object().set_y(pos_y);
            }));

            eval frp.set_label((content) model.set_label(content.clone()));
            eval frp.set_delay((delay) model.set_delay(*delay));
        }
        init.emit(());

        Self { frp, model }
    }

    /// Get the FRP node for the content of the pop-up, for testing purposes.
    pub fn content_frp_node(&self) -> &frp::Source<String> {
        &self.model.show
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.model.label.display_object()
    }
}

impl Deref for View {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}
