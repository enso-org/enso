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

pub struct Model {
    pub background: background::View,
    pub track:      track::View,
    pub label:      text::Text,
    pub value:      text::Text,
    pub root:       display::object::Instance,

    width:  Cell<f32>,
    height: Cell<f32>,

    pub app: Application,
}

impl Model {
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let value = app.new_view::<text::Text>();
        let background = background::View::new();
        let track = track::View::new();

        let app = app.clone_ref();
        let scene = &app.display.default_scene;

        root.add_child(&background);
        root.add_child(&track);
        root.add_child(&label);
        root.add_child(&value);

        value.remove_from_scene_layer(&scene.layers.main);
        value.add_to_scene_layer(&scene.layers.label);
        label.remove_from_scene_layer(&scene.layers.main);
        label.add_to_scene_layer(&scene.layers.label);

        let model = Self {
            background,
            track,
            label,
            value,
            root,
            app,
            height: Cell::new(200.0),
            width: Cell::new(200.0),
        };

        model.set_size(Vector2::new(200.0, 50.0));
        model.set_background_color(color::Rgba(0.8, 0.8, 0.8, 1.0));
        model.set_inactive();

        model
    }

    pub fn set_size(&self, size: Vector2) {
        self.width.set(size.x);
        self.height.set(size.y);
        self.background.size.set(size);
        self.track.size.set(size);
    }

    pub fn set_width(&self, width: f32) {
        self.width.set(width);

        let size = Vector2::new(width, self.height.get());

        self.background.size.set(size);
        self.track.size.set(size);
    }

    pub fn set_height(&self, height: f32) {
        self.height.set(height);

        let size = Vector2::new(self.width.get(), height);

        self.background.size.set(size);
        self.track.size.set(size);
    }

    pub fn set_track_color(&self, color: color::Rgba) {
        self.track.color.set(color.into());
    }
    pub fn set_background_color(&self, color: color::Rgba) {
        self.background.color.set(color.into());
    }

    pub fn text_align_width(&self, width: f32) {
        self.value.set_position_x(-width / 2.0);
    }
    pub fn text_align_height(&self, height: f32) {
        self.value.set_position_y(height / 2.0);
    }

    pub fn update_track(&self, value: f32) {
        self.track.value.set(value);
    }

    pub fn set_active(&self) {
        self.set_track_color(color::Rgba(0.4, 0.4, 0.8, 1.0));
    }

    pub fn set_inactive(&self) {
        self.set_track_color(color::Rgba(0.4, 0.4, 0.6, 1.0));
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
    fn new() -> Self {
        let width: Var<Pixels> = "input_size.x".into();
        let height: Var<Pixels> = "input_size.y".into();

        let shape = Rect((&width, &height)).corners_radius(&height / 2.0).into();

        Background { width, height, shape }
    }
}

mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        (color:Vector4) {
            Background::new()
                .shape
                .fill(color)
                .into()
        }
    }
}

mod track {
    use super::*;

    ensogl_core::define_shape_system! {
        (value:f32, color:Vector4) {
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
