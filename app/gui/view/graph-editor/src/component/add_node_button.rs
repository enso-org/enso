use ensogl_component::button::prelude::*;

use crate::display::camera::Camera2d;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::Scene;
use ensogl_hardcoded_theme::graph_editor::add_node_button as theme;


// =============
// === Shape ===
// =============

mod shape {
    use super::*;

    ensogl::define_shape_system! {
        (background_color:Vector4<f32>, icon_color:Vector4<f32>) {
            let size       = Var::canvas_size();
            let radius     = Min::min(size.x(),size.y()) / 2.0;

            let angle      = Radians::from(90.0.degrees());
            let bar_length = &radius * 4.0 / 3.0;
            let bar_width  = &bar_length / 6.5;
            #[allow(clippy::blacklisted_name)] // The `bar` name here is totally legit.
            let bar        = Rect((bar_length, &bar_width));
            let plus       = (bar.rotate(angle) + bar).into();
            shape(background_color, icon_color, plus, radius)
        }
    }
}

impl ButtonShape for shape::DynamicShape {
    fn debug_name() -> &'static str {
        "AddNodeButton"
    }

    fn size_path(state: State) -> StaticPath {
        theme::size
    }

    fn background_color_path(state: State) -> StaticPath {
        match state {
            State::Unconcerned => theme::background,
            State::Hovered => theme::hover::background,
            State::Pressed => theme::click::background,
        }
    }

    fn icon_color_path(state: State) -> StaticPath {
        match state {
            State::Unconcerned => theme::color,
            State::Hovered => theme::hover::color,
            State::Pressed => theme::click::color,
        }
    }

    fn background_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>> {
        &self.background_color
    }

    fn icon_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>> {
        &self.icon_color
    }
}

type View = ensogl_component::button::View<shape::DynamicShape>;

#[derive(Clone, CloneRef, Debug)]
pub struct AddNodeButton {
    network:     frp::Network,
    view:        View,
    style_watch: StyleWatchFrp,
}

impl AddNodeButton {
    pub fn new(app: &Application) -> Self {
        let view = ensogl_component::button::View::new(app);
        let network = frp::Network::new("AddNodeButton");
        let scene = app.display.scene();
        let camera = scene.camera();
        let style_watch = StyleWatchFrp::new(&scene.style_sheet);

        let size = style_watch.get_number(theme::size);
        let margin = style_watch.get_number(theme::margin);
        frp::extend! { network
            init <- source();
            let camera_changed = scene.frp.camera_changed.clone_ref();
            update_position <- all(init, camera_changed, size, margin);
            eval update_position ([view, camera] (&((), (), size, margin)) {
                Self::update_position(&view, &camera, size, margin);
            });
        }

        scene.layers.panel.add_exclusive(&view);
        init.emit(());

        Self { network, view, style_watch }
    }

    fn update_position(view: &View, camera: &Camera2d, size: f32, margin: f32) {
        let screen = camera.screen();
        let x = -screen.width / 2.0 + margin;
        let y = -screen.height / 2.0 + margin + size * 2.0 + 30.0;
        DEBUG!("Setting position {x},{y}");
        view.set_position_x(x.round());
        view.set_position_y(y.round());
    }
}

impl display::Object for AddNodeButton {
    fn display_object(&self) -> &display::object::Instance {
        self.view.display_object()
    }
}
