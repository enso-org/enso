//! Model for the slider component

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_text as text;



// ========================
// === Model definition ===
// ========================

/// Slider model structure
pub struct Model {
    /// Background element
    pub background:  background::View,
    /// Slider track element that moves dependent on the value of the slider
    pub track:       track::View,
    /// Slider label
    pub label:       text::Text,
    /// Slider value text left of the decimal point
    pub value_left:  text::Text,
    /// Slider value text decimal point
    pub value_dot:   text::Text,
    /// Slider value text right of the decimal point
    pub value_right: text::Text,

    pub root: display::object::Instance,
    pub app:  Application,
}

impl Model {
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let value_left = app.new_view::<text::Text>();
        let value_dot = app.new_view::<text::Text>();
        let value_right = app.new_view::<text::Text>();
        let background = background::View::new();
        let track = track::View::new();

        let app = app.clone_ref();
        let scene = &app.display.default_scene;

        root.add_child(&background);
        root.add_child(&track);
        root.add_child(&label);
        root.add_child(&value_left);
        root.add_child(&value_dot);
        root.add_child(&value_right);

        value_left.add_to_scene_layer(&scene.layers.label);
        value_dot.add_to_scene_layer(&scene.layers.label);
        value_right.add_to_scene_layer(&scene.layers.label);
        label.add_to_scene_layer(&scene.layers.label);

        let model =
            Self { background, track, label, value_left, value_dot, value_right, root, app };

        model.set_size(Vector2::new(200.0, 50.0));
        model.value_dot.set_content(".");

        model
    }

    /// Set component size
    pub fn set_size(&self, size: Vector2<f32>) {
        self.background.size.set(size);
        self.track.size.set(size);
    }

    /// Set slider track color
    pub fn set_track_color(&self, color: color::Lcha) {
        self.track.color.set(color::Rgba::from(color).into());
    }
    /// Set slider background color
    pub fn set_background_color(&self, color: color::Lcha) {
        self.background.color.set(color::Rgba::from(color).into());
    }

    /// Set slider label visibility
    pub fn set_label_visible(&self, enabled: bool) {
        if enabled {
            self.root.add_child(&self.label);
        } else {
            self.root.remove_child(&self.label);
        }
    }

    /// Set visibility of value text right of the decimal point
    pub fn set_value_decimal_visible(&self, enabled: bool) {
        if enabled {
            self.root.add_child(&self.value_dot);
            self.root.add_child(&self.value_right);
        } else {
            self.root.remove_child(&self.value_dot);
            self.root.remove_child(&self.value_right);
        }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}



// ========================
// === Shape definition ===
// ========================

struct Background {
    pub width:  Var<Pixels>,
    pub height: Var<Pixels>,
    pub shape:  AnyShape,
}

impl Background {
    /// Create new rounded background shape
    fn new() -> Self {
        let width: Var<Pixels> = "input_size.x".into();
        let height: Var<Pixels> = "input_size.y".into();

        let shape = Rect((&width, &height)).corners_radius(&height / 2.0).into();

        Background { width, height, shape }
    }
}

mod background {
    use super::*;

    ensogl_core::shape! {
        (style:Style, color:Vector4) {
            Background::new()
                .shape
                .fill(color)
                .into()
        }
    }
}

mod track {
    use super::*;

    ensogl_core::shape! {
        above = [background];
        (style:Style, value:f32, color:Vector4) {
            let Background{
                width,
                height,
                shape: background
            } = Background::new();

            Rect( (&width*&value, &height) )
                .translate_x(&width*(&value-1.0)*0.5)
                .intersection(background)
                .fill(color)
                .into()
        }
    }
}
