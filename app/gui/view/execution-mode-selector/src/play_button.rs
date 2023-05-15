use enso_prelude::*;
use ensogl::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::display;
use ensogl::display::shape::StyleWatchFrp;
use ensogl_derive_theme::FromTheme;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::graph_editor::execution_mode_selector::play_button as theme;



// =============
// === Style ===
// ==============

#[derive(Debug, Clone, Copy, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    triangle_size: f32,
    offset:        f32,
    padding_x:     f32,
    padding_y:     f32,
}



// ==============
// === Shapes ===
// ==============

mod play_icon {
    use super::*;

    use std::f32::consts::PI;

    ensogl::shape! {
        above = [display::shape::compound::rectangle::shape];
        (style: Style) {
            let triangle_size = style.get_number(theme::triangle_size);
            let color = style.get_color(theme::color);
            let triangle = Triangle(triangle_size, triangle_size).rotate((PI / 2.0).radians());
            let triangle = triangle.fill(color);
            let bg_size = Var::canvas_size();
            let bg = Rect(bg_size).fill(INVISIBLE_HOVER_COLOR);
            (bg + triangle).into()
        }
    }
}

mod spinner_icon {
    use super::*;

    use std::f32::consts::FRAC_PI_3;

    ensogl::shape! {
        above = [display::shape::compound::rectangle::shape];
        (style: Style) {
            let color = style.get_color(theme::spinner::color);
            let speed = style.get_number(theme::spinner::speed);
            let width = Var::<Pixels>::from("input_size.x");
            let time = Var::<f32>::from("input_time");
            let unit = &width / 16.0;
            let arc = RoundedArc(&unit * 5.0, (4.0 * FRAC_PI_3).radians(), &unit * 2.0);
            let rotated_arc = arc.rotate(time * speed);
            rotated_arc.fill(color).into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        reset   (),
    }
    Output {
        pressed (),
    }
}



// =============
// === Model ===
// =============

#[derive(Debug, Clone, CloneRef)]
pub struct Model {
    display_object: display::object::Instance,
    play_icon:      play_icon::View,
    spinner_icon:   spinner_icon::View,
}

impl Model {
    fn update_style(&self, style: &Style) {
        let triangle_size = Vector2::new(style.triangle_size, style.triangle_size);
        let padding = Vector2::new(style.padding_x, style.padding_y);
        let size = triangle_size + 2.0 * padding;
        self.play_icon.set_size(size);
        self.spinner_icon.set_size(size);
        self.play_icon.set_x(-size.x / 2.0 - style.offset);
        self.spinner_icon.set_x(-size.x / 2.0 - style.offset);
    }

    fn set_playing(&self, playing: bool) {
        if playing {
            self.display_object.remove_child(&self.play_icon);
            self.display_object.add_child(&self.spinner_icon);
        } else {
            self.display_object.remove_child(&self.spinner_icon);
            self.display_object.add_child(&self.play_icon);
        }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ===================
// === Play Button ===
// ===================

impl component::Model for Model {
    fn label() -> &'static str {
        "PlayButton"
    }

    fn new(_app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let play_icon = play_icon::View::new();
        let spinner_icon = spinner_icon::View::new();

        display_object.add_child(&play_icon);

        Self { display_object, play_icon, spinner_icon }
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &enso_frp::Network,
        frp: &<Self as ensogl::application::frp::API>::Private,
        _app: &Application,
        model: &Model,
        style_watch: &StyleWatchFrp,
    ) {
        let play_icon = &model.play_icon;
        let input = &frp.input;
        let output = &frp.output;

        let style = Style::from_theme(network, style_watch);

        frp::extend! { network
            eval style.update ((style) model.update_style(style));

            eval_ input.reset (model.set_playing(false));

            let play_icon_mouse_down = play_icon.on_event::<mouse::Down>();
            output.pressed <+ play_icon_mouse_down.constant(());

            eval_ output.pressed (model.set_playing(true));
        }
        style.init.emit(());
    }
}

/// A button to execute the workflow in a fully enabled way within the current execution
/// environment. The button should be visible in any execution environment where one or more
/// contexts are disabled.
pub type PlayButton = component::ComponentView<Model, Frp>;
