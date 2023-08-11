//! A temporary text message on top of the screen.

use crate::prelude::*;

use ensogl::animation::delayed::DelayedAnimation;
use ensogl::application::Application;
use ensogl::display;
use ensogl::Animation;
use ensogl_component::label::Label;
use frp::stream::EventOutput;
use frp::HasLabel;



// =================
// === Constants ===
// =================

const PADDING_TOP: f32 = 50.0;
const DEFAULT_DELAY_MS: f32 = 5_000.0;



// =============
// === Model ===
// =============

/// Text label that disappears after a predefined delay.
#[derive(Debug, Clone, CloneRef, display::Object)]
struct Model {
    #[display_object]
    label:             Label,
    opacity_animation: Animation<f32>,
    delay_animation:   DelayedAnimation,
}

impl Model {
    /// Constructor.
    fn new(app: &Application, network: &frp::Network) -> Self {
        let label = Label::new(app);
        label.set_opacity(0.0);
        // Add the pop-up to the panel layer so its position is fixed. The default for Label is the
        // tooltip layer, which moves when panning.
        let scene = &app.display.default_scene;
        let background_layer = &scene.layers.panel;
        let text_layer = &scene.layers.panel_text;
        label.set_layers(background_layer, text_layer);

        let opacity_animation = Animation::new(network);
        network.store(&opacity_animation);
        let delay_animation = DelayedAnimation::new(network);
        delay_animation.set_delay(DEFAULT_DELAY_MS);
        delay_animation.set_duration(0.0);
        network.store(&delay_animation);

        Self { label, opacity_animation, delay_animation }
    }

    /// Set the message.
    fn set_label(&self, content: String) {
        self.label.set_content(content);
        self.delay_animation.reset();
        self.delay_animation.start();
    }

    /// Set the position of the label based on the height of the scene.
    fn set_label_position(&self, scene_height: f32) {
        let half_height = scene_height / 2.0;
        let label_height = self.label.size.value().y;
        let pos_y = half_height - PADDING_TOP - label_height / 2.0;
        self.label.display_object().set_y(pos_y);
    }

    /// Set a delay in milliseconds after which the label will disappear.
    fn set_delay(&self, delay: f32) {
        self.delay_animation.set_delay(delay);
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
#[derive(Debug, Clone, CloneRef, Deref, display::Object)]
pub struct View {
    #[deref]
    frp:   Frp,
    #[display_object]
    model: Model,
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let model = Model::new(app, network);

        frp::extend! { network
            init <- source_();
            let scene_shape = app.display.default_scene.shape();
            _eval <- all_with(scene_shape, &init, f!((scene_shape, _init)
                model.set_label_position(scene_shape.height);
            ));

            model.opacity_animation.target <+ frp.set_label.constant(1.0);
            model.opacity_animation.target <+ model.delay_animation.on_end.constant(0.0);
            model.label.set_opacity <+ model.opacity_animation.value;

            eval frp.set_label ((content) model.set_label(content.clone()));
            eval frp.set_delay ((delay) model.set_delay(*delay));
        }
        init.emit(());

        Self { frp, model }
    }

    /// Get the FRP node for the content of the pop-up, for testing purposes.
    pub fn content_frp_node(&self) -> impl EventOutput<Output = String> + HasLabel {
        self.frp.set_label.clone_ref()
    }
}
