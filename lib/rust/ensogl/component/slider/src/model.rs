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
    pub track:              background::View,   //track::View,
    pub label:              text::Text,
    pub root:               display::object::Instance,

    background_color:       Rc<RefCell<color::Rgba>>,
    track_color:            Rc<RefCell<color::Rgba>>,

    pub app: Application,
}

impl Model {
    pub fn new(app: &Application) -> Self {
        let root = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        let background = background::View::new();
        let background_color = Rc::new(RefCell::new(color::Rgba(1.0,0.0,0.0, 1.0)));
        let track = background::View::new();
        let track_color = default();

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
            background_color,
            track_color,
            app,
        }
    }

    pub fn set_size(&self, size: Vector2) {
        self.background.size.set(size);
    }

    pub fn set_color(&self, color: color::Rgba) {
        self.background_color.as_ref().replace(color);
        self.background.color.set(color.into());
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}

mod background {
    use super::*;

    struct Background {
        pub width:          Var<Pixels>,
        pub height:         Var<Pixels>,
        pub shape:          AnyShape,
    }

    impl Background {
        fn new(style: &StyleWatch) -> Self {
            let sprite_width: Var<Pixels> = "input_size.x".into();
            let sprite_height: Var<Pixels> = "input_size.y".into();
    
            let width = &sprite_width - shadow::size(style).px();
            let height = &sprite_height - shadow::size(style).px();
    
            let shape = Rect((&width, &height))
                .into();
    
            Background { width, height, shape }
        }
    }

    ensogl_core::define_shape_system! {
        (style:Style, corner_left:f32, corner_right:f32, color:Vector4, show_shadow:f32) {
            let background = Background::new(style);
            let shadow     = shadow::from_shape_with_alpha(
                background.shape.clone(),
                &show_shadow,
                style
            );
            let background = background.shape.fill(color);
            
            (shadow + background).into()
        }
    }
}
