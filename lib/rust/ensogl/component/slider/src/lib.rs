//! UI components that allows picking a number or range through mouse interaction.

#![recursion_limit = "1024"]

pub mod model;

use ensogl_core::prelude::*;
use crate::model::*;

use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;

use ensogl_core::data::color;

pub struct Slider {
    pub frp: Rc<Frp>,
    model: Rc<Model>,
    pub app: Application,
}

impl Slider {
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(&app));
        let app = app.clone_ref();    
        let frp = Rc::new(Frp::new());

        Self { frp, model, app }.init()
    }
}

impl display::Object for Slider {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}

impl Deref for Slider {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl FrpNetworkProvider for Slider {
    fn network(&self) -> &enso_frp::Network {
        self.frp.network()
    }
}

impl application::View for Slider {
    fn label() -> &'static str {
        "Slider"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }

    fn app(&self) -> &Application {
        &self.app
    }
}


ensogl_core::define_endpoints_2! {
    Input {
        set_width(f32),
        set_height(f32),
        set_color(color::Rgba),

        set_default(f32),
        set_value(f32),
        set_min(f32),
        set_max(f32),
        set_precision(f32),

        set_tooltip(Option<String>),
        set_label(Option<String>),
        
        resize(Vector2),
    }
    Output {
        value(f32),
    }
}

impl Slider {
    fn init(self) -> Self {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;

        let scene = &self.app.display.default_scene;
        let mouse = &scene.mouse.frp;

        //mouse.

        frp::extend! { network

            eval input.resize (
                (v) {
                    model.set_size(*v);
                }
            );

            
            // User input

            background_click        <- model.background.events.mouse_down.constant(());
            background_release      <- model.background.events.mouse_release.constant(());
            background_drag         <- bool(&background_release, &background_click);

            track_click             <- model.track.events.mouse_down.constant(());
            track_release           <- model.track.events.mouse_release.constant(());
            track_drag              <- bool(&track_release, &track_click);

            component_click         <- any2(&background_click, &track_click);
            component_drag          <- any2(&background_drag, &track_drag);
            component_release       <- any2(&background_release, &track_release);

            drag_pos_start          <- mouse.position.sample(&component_click);
            drag_pos_end            <- mouse.position.gate(&component_drag);
            drag_pos_end            <- any2(&drag_pos_end, &drag_pos_start); // FIXME: Does not set to start position on click!
            
            //drag_delta              <- all2(&drag_pos_end, &drag_pos_start).map(|(end, start)| end - start);
            drag_pos_prev           <- drag_pos_end.previous();
            drag_delta              <- all2(&drag_pos_end, &drag_pos_prev).map(|(end, start)| end - start);

            eval drag_delta (
                (v) {
                    model.drag(*v);
                }
            );

            eval_ component_click (
                model.set_active();
            );

            eval_ component_release (
                model.set_inactive();
            );

        }

        self
    }
}



