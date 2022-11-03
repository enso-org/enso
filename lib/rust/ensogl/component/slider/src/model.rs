//! Model for the slider component

use ensogl_core::prelude::*;
use ensogl_core::display::shape::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_text as text;
use ensogl_shadow as shadow;


pub struct Model {
    pub background:         background::View,
    pub track:              track::View,
    pub label:              text::Text,
    pub root:               display::object::Instance,
    
    value:                  Cell<f32>,

    pub app: Application,
}

impl Model {
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let background = background::View::new();
        let track = track::View::new();

        let value = Cell::new(0.5);

        background.size.set(Vector2::new(200.0, 200.0));
        background.color.set(color::Rgba(0.8,0.8,0.8, 1.0).into());

        track.size.set(Vector2::new(200.0, 200.0));
        track.color.set(color::Rgba(0.4, 0.4, 0.6, 1.0).into());
        track.value.set(value.get().into());

        let app = app.clone_ref();
        let scene = &app.display.default_scene;

        root.add_child(&label);
        root.add_child(&background);
        root.add_child(&track);

        Self {
            background,
            track,
            label,
            root,
            value,
            app,
        }
    }

    pub fn set_size(&self, size: Vector2) {
        self.background.size.set(size);
        self.track.size.set(size);
    }

    pub fn set_color(&self, color: color::Rgba) {
        self.track.color.set(color.into());
    }

    pub fn drag(&self, dist: Vector2) {
        // FIXME: keep value in FRP and not in component model!
        let value = self.value.get() + dist.x * 0.001;
        let value = value.min(1.0).max(0.0);
        self.value.set(value);
        self.track.value.set(value);
    }

    pub fn set_active(&self) {
        self.set_color(color::Rgba(0.4, 0.4, 0.8, 1.0));
    }

    pub fn set_inactive(&self) {
        self.set_color(color::Rgba(0.4, 0.4, 0.6, 1.0));
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}


struct Background {
    pub width:          Var<Pixels>,
    pub height:         Var<Pixels>,
    pub shape:          AnyShape,
}

impl Background {
    fn new(style: &StyleWatch) -> Self {
        let width: Var<Pixels> = "input_size.x".into();
        let height: Var<Pixels> = "input_size.y".into();

        let shape = Rect((&width, &height))
            .corners_radius(&height/2.0)
            .into();

        Background { width, height, shape }
    }
}

mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style, color:Vector4) {
            Background::new(style)
                .shape
                .fill(color)
                .into()
        }
    }
}

mod track {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style, value:f32, color:Vector4) {
            let Background{
                width,
                height,
                shape: background
            } = Background::new(style);
            
            Rect( (&width*&value, &height) )
                .translate_x(&width*(&value-1.0)*0.5)
                .intersection(background)
                .fill(color)
                .into()
        }
    }
}
