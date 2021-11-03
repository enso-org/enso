//! The fullscreen button in the Top Button panel.

use crate::window_control_buttons::common::prelude::*;

pub use ensogl_theme::application::window_control_buttons::fullscreen as theme;

/// The view component with the fullscreen button.
pub type View = common::View<shape::DynamicShape>;

/// The shape for "fullscreen" button. The icon consists if two triangles ◤◢ centered around single
/// point.
pub mod shape {
    use super::*;
    ensogl::define_shape_system! {
        (background_color:Vector4<f32>, icon_color:Vector4<f32>) {
            let size        = Var::canvas_size();
            let radius      = Min::min(size.x(),size.y()) / 2.0;
            let round       = &radius / 6.0;
            let rect        = Rect((&radius,&radius)).corners_radius(&round);
            let strip_sizes = (&radius * 2.0 / 9.0, &radius*2.0);
            let strip       = Rect(strip_sizes).rotate(Radians::from(45.0.degrees()));
            let icon        = rect - strip;
            shape(background_color, icon_color, icon.into(), radius)
        }
    }
}

impl ButtonShape for shape::DynamicShape {
    fn debug_name() -> &'static str {
        "FullscreenButton"
    }

    fn background_color_path(state: State) -> StaticPath {
        match state {
            State::Unconcerned => theme::normal::background_color,
            State::Hovered => theme::hovered::background_color,
            State::Pressed => theme::pressed::background_color,
        }
    }

    fn icon_color_path(state: State) -> StaticPath {
        match state {
            State::Unconcerned => theme::normal::icon_color,
            State::Hovered => theme::hovered::icon_color,
            State::Pressed => theme::pressed::icon_color,
        }
    }

    fn background_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>> {
        &self.background_color
    }

    fn icon_color(&self) -> &DynamicParam<Attribute<Vector4<f32>>> {
        &self.icon_color
    }
}
