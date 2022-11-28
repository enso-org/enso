//! A module containing definition of (+) button for adding nodes.

use ensogl_component::button::prelude::*;

use crate::display::camera::Camera2d;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl_hardcoded_theme::graph_editor::add_node_button as theme;



// =============
// === Shape ===
// =============

mod shape {
    use super::*;

    ensogl::shape! {
        (style: Style, background_color:Vector4<f32>, icon_color:Vector4<f32>) {
            let size = Var::canvas_size();
            let shadow_size = style.get_number(ensogl_hardcoded_theme::shadow::size);
            let radius = Min::min(size.x(),size.y()) / 2.0 - shadow_size.px();

            let angle      = Radians::from(90.0.degrees());
            let bar_length = &radius * 4.0 / 3.0;
            let bar_width  = &bar_length / 10.0;
            #[allow(clippy::disallowed_names)] // The `bar` name here is totally legit.
            let bar        = Rect((bar_length, &bar_width));
            let plus       = (bar.rotate(angle) + bar).into();
            let shape = shape(background_color, icon_color, plus, radius);
            let shadow = ensogl_component::shadow::from_shape(shape.clone(), style);
            (shadow + shape).into()
        }
    }
}

impl ButtonShape for shape::Shape {
    fn debug_name() -> &'static str {
        "AddNodeButton"
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

    fn background_color(&self) -> &ProxyParam<Attribute<Vector4<f32>>> {
        &self.background_color
    }

    fn icon_color(&self) -> &ProxyParam<Attribute<Vector4<f32>>> {
        &self.icon_color
    }
}



// =====================
// === AddNodeButton ===
// =====================

type View = ensogl_component::button::View<shape::Shape>;

/// Add Node Button Component.
///
/// This is a button with + icon, which sticks to the left-bottom corner of the scene. It exposes
/// the FRP of EnsoGL Button Component, including the main "click" event.
#[derive(Clone, CloneRef, Debug)]
pub struct AddNodeButton {
    network:     frp::Network,
    view:        View,
    style_watch: StyleWatchFrp,
}

impl Deref for AddNodeButton {
    type Target = ensogl_component::button::Frp;
    fn deref(&self) -> &Self::Target {
        self.view.deref()
    }
}

impl AddNodeButton {
    /// Create new component.
    pub fn new(app: &Application) -> Self {
        let view = ensogl_component::button::View::new(app);
        let network = frp::Network::new("AddNodeButton");
        let scene = &app.display.default_scene;
        let camera = scene.camera();
        let style_watch = StyleWatchFrp::new(&scene.style_sheet);

        let size = style_watch.get_number(theme::size);
        let shadow = style_watch.get_number(ensogl_hardcoded_theme::shadow::size);
        let margin = style_watch.get_number(theme::margin);
        frp::extend! { network
            init <- source();
            let camera_changed = scene.frp.camera_changed.clone_ref();
            update_position <- all(init, camera_changed, size, margin);
            eval update_position ([view, camera] (&((), (), size, margin)) {
                Self::update_position(&view, &camera, size, margin);
            });
            update_size <- all(init, size, shadow);
            view.set_size <+ update_size.map(|&((), size, shadow)| {
                let view_size_1d = size + shadow * 2.0;
                Vector2(view_size_1d, view_size_1d)
            });
        }

        scene.layers.panel.add(&view);
        init.emit(());

        Self { network, view, style_watch }
    }

    fn update_position(view: &View, camera: &Camera2d, size: f32, margin: f32) {
        let screen = camera.screen();
        let x = -screen.width / 2.0 + margin + size / 2.0;
        let y = -screen.height / 2.0 + margin + size / 2.0;
        view.set_x(x.round());
        view.set_y(y.round());
    }
}

impl display::Object for AddNodeButton {
    fn display_object(&self) -> &display::object::Instance {
        self.view.display_object()
    }
}
