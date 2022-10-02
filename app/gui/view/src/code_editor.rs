//! A Code Editor component.

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application;
use ensogl::application::shortcut;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::StyleWatchFrp;
use ensogl::DEPRECATED_Animation;
use ensogl_text as text;



// =================
// === Constants ===
// =================

/// The height of code editor in project view.
pub const HEIGHT_FRACTION: f32 = 0.3;
/// The padding between text area and scene left boundary.
pub const PADDING_LEFT: f32 = 7.0;



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Show the Code Editor.
        show(),
        /// Hide the Code Editor.
        hide(),
        /// Toggle Code Editor visibility.
        toggle(),
    }

    Output {
        is_visible (bool),
    }
}



// ============
// === View ===
// ============

/// The View of IDE Code Editor.
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    model:  text::Text,
    styles: StyleWatchFrp,
    frp:    Frp,
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl View {
    /// Create Code Editor component.
    pub fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let styles = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let frp = Frp::new();
        let network = &frp.network;
        let model = app.new_view::<text::Text>();
        let height_fraction = DEPRECATED_Animation::<f32>::new(network);

        model.set_position_x(PADDING_LEFT);
        model.remove_from_scene_layer(&scene.layers.main);
        model.add_to_scene_layer(&scene.layers.panel_text);
        // TODO[ao]: To have code editor usable we treat it as constantly mouse-hovered, but this
        //  should be changed in the second part of focus management
        //  (https://github.com/enso-org/ide/issues/823)
        model.hover();

        frp::extend! { network
            let is_visible     =  frp.output.is_visible.clone_ref();
            show_after_toggle <- frp.toggle.gate_not(&is_visible);
            hide_after_toggle <- frp.toggle.gate    (&is_visible);
            show              <- any(frp.input.show,show_after_toggle);
            hide              <- any(frp.input.hide,hide_after_toggle);

            eval_ show (height_fraction.set_target_value(HEIGHT_FRACTION));
            eval_ show (model.focus());
            eval_ hide (height_fraction.set_target_value(0.0));
            eval_ hide ([model] {
                model.remove_all_cursors();
                model.defocus();
            });

            frp.source.is_visible <+ bool(&frp.input.hide,&frp.input.show);
            frp.source.is_visible <+ frp.toggle.map2(&is_visible, |(),b| !b);

            def init = source_();
            let shape  = app.display.default_scene.shape();
            position <- all_with3(&height_fraction.value,shape,&init, |height_f,scene_size,_init| {
                let height = height_f * scene_size.height;
                let x      = -scene_size.width  / 2.0 + PADDING_LEFT;
                let y      = -scene_size.height / 2.0 + height;
                Vector2(x,y)
            });
            eval position ((pos) model.set_position_xy(*pos));

            let color = styles.get_color(ensogl_hardcoded_theme::code::syntax::base);
            eval color ((color) model.set_property_default(color));
        }
        init.emit(());
        model.set_property_default(color.value());

        Self { model, styles, frp }
    }

    /// Return the Text Area component inside this editor.
    pub fn text_area(&self) -> &text::Text {
        &self.model
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}

impl FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for View {
    fn label() -> &'static str {
        "CodeEditor"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }

    fn app(&self) -> &Application {
        self.model.app()
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        (&[(Press, "ctrl `", "toggle"), (Press, "escape", "hide")])
            .iter()
            .map(|(a, b, c)| Self::self_shortcut(*a, *b, *c))
            .collect()
    }
}
