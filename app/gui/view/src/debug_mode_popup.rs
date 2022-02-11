//! Text message on top of the screen that signals about enabling/disabling Debug Mode of Graph
//! Editor.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::animation::delayed::DelayedAnimation;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::Animation;
use ensogl_component::label::Label;



// =================
// === Constants ===
// =================

/// Mitigate limitations of constant strings concatenation.
macro_rules! define_debug_mode_shortcut {
    ($shortcut:literal) => {
        /// A keyboard shortcut used to enable/disable Debug Mode.
        pub const DEBUG_MODE_SHORTCUT: &str = $shortcut;
        const DEBUG_MODE_ENABLED: &str =
            concat!("Debug Mode enabled. To disable, press `", $shortcut, "`.");
    };
}
define_debug_mode_shortcut!("ctrl shift d");
const DEBUG_MODE_DISABLED: &str = "Debug Mode disabled.";

const LABEL_VISIBILITY_DELAY_MS: f32 = 3_000.0;
const LABEL_PADDING_TOP: f32 = 50.0;



// ==================
// === PopupLabel ===
// ==================

/// Text label that disappears after a predefined delay.
#[derive(Debug, Clone, CloneRef)]
struct PopupLabel {
    label:   Label,
    network: frp::Network,
    show:    frp::Source<String>,
}

impl display::Object for PopupLabel {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        self.label.display_object()
    }
}

impl PopupLabel {
    /// Constructor. Label will have `text` content and will disappear after `delay` milliseconds.
    pub fn new(app: &Application, text: &str, delay: f32) -> Self {
        let network = frp::Network::new("PopupLabel");
        let label = Label::new(app);
        label.set_content(text);
        label.set_opacity(0.0);
        let background_layer = &app.display.scene().layers.panel;
        let text_layer = &app.display.scene().layers.panel_text;
        label.set_layers(background_layer, text_layer);

        let opacity_animation = Animation::new(&network);
        network.store(&opacity_animation);
        let delay_animation = DelayedAnimation::new(&network);
        delay_animation.set_delay(delay);
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

        Self { label, network, show, }
    }
}



// =============
// === Model ===
// =============

#[derive(Debug, Clone, CloneRef)]
struct Model {
    display_object: display::object::Instance,
    label: PopupLabel,
    logger:         Logger,
}

impl Model {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let logger = Logger::new("DebugModePopup");
        let display_object = display::object::Instance::new(&logger);
        let label = PopupLabel::new(app, DEBUG_MODE_ENABLED, LABEL_VISIBILITY_DELAY_MS);
        display_object.add_child(&label);

        Self { display_object, label, logger }
    }

    /// Show "Debug Mode enabled" label. Hides another one.
    pub fn show_enabled_label(&self) {
        self.label.show.emit(String::from(DEBUG_MODE_ENABLED));
    }

    /// Show "Debug Mode disabled" label. Hides another one.
    pub fn show_disabled_label(&self) {
        self.label.show.emit(String::from(DEBUG_MODE_DISABLED));
    }

    /// Return the height of the label.
    pub fn label_height(&self) -> f32 {
        // Both labels are identical, so we can ask either one.
        self.label.label.size.value().y
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        // Debug Mode was enabled.
        enabled(),
        // Debug Mode was disabled.
        disabled(),
    }
    Output {}
}



// ============
// === View ===
// ============

/// Text message on top of the screen that signals about enabling/disabling Debug Mode of Graph
/// Editor.
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
            let shape  = app.display.scene().shape();
            _eval <- all_with(shape, &init, f!([model](scene_size, _init) {
                let half_height = scene_size.height / 2.0;
                let label_height = model.label_height();
                let pos_y = half_height - LABEL_PADDING_TOP - label_height / 2.0;
                model.display_object.set_position_y(pos_y);
            }));

            eval_ frp.enabled(model.show_enabled_label());
            eval_ frp.disabled(model.show_disabled_label());
        }
        init.emit(());

        Self { frp, model }
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
