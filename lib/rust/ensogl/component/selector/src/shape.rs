//! This module contains the shapes and shape related functionality required.
use crate::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::shape::*;
use ensogl_hardcoded_theme as theme;
use ensogl_shadow as shadow;



// ==================
// === Background ===
// ==================

/// Utility struct that contains the background shape for the selector components, as well as some
/// meta information about it. This information can be used to align other shapes with the
/// background.
struct Background {
    pub width:         Var<Pixels>,
    pub height:        Var<Pixels>,
    #[allow(dead_code)]
    // This field is not used but should stay as part of the API for future use.
    pub corner_radius: Var<Pixels>,
    pub shape:         AnyShape,
}

impl Background {
    fn new(corner_left: &Var<f32>, corner_right: &Var<f32>, style: &StyleWatch) -> Background {
        let sprite_width: Var<Pixels> = "input_size.x".into();
        let sprite_height: Var<Pixels> = "input_size.y".into();

        let width = &sprite_width - shadow::size(style).px();
        let height = &sprite_height - shadow::size(style).px();
        let corner_radius = &height / 2.0;
        let rect_left = Rect((&width / 2.0, &height)).corners_radius(&corner_radius * corner_left);
        let rect_left = rect_left.translate_x(-&width / 4.0);
        let rect_right =
            Rect((&width / 2.0, &height)).corners_radius(&corner_radius * corner_right);
        let rect_right = rect_right.translate_x(&width / 4.0);
        let rect_center = Rect((&corner_radius * 2.0, &height));

        let shape = (rect_left + rect_right + rect_center).into();

        Background { width, height, corner_radius, shape }
    }
}

/// Background shape. Appears as a rect with rounded corners. The roundness of each corner can be
/// toggled by passing `0.0` or `1.0` to either `corner_left` and `corner_right` where `0.0` means
/// "not rounded" and `1.0` means "rounded".
pub mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,corner_left:f32,corner_right:f32,color:Vector4,show_shadow:f32) {
            let background = Background::new(&corner_left,&corner_right,style);
            let shadow     = shadow::from_shape_with_alpha(background.shape.clone(),
                &show_shadow,style);
            let background = background.shape.fill(color);
            (shadow + background).into()
        }
    }
}



// ===============
// === IO Rect ===
// ===============

/// Utility shape that is invisible but provides mouse input. Fills the whole sprite.
pub mod io_rect {
    use super::*;

    ensogl_core::define_shape_system! {
        () {
            let sprite_width  : Var<Pixels> = "input_size.x".into();
            let sprite_height : Var<Pixels> = "input_size.y".into();

            let rect  = Rect((&sprite_width,&sprite_height));
            let shape = rect.fill(HOVER_COLOR);

            shape.into()
        }
    }
}



// =============
// === Track ===
// =============

/// Track of the selector. Appears as filled area of the background. Has a definable start and
/// end-point (`left`, `right`) which are passed as normalised values relative to the maximum
/// width.  For consistency with the background shape, also has the property to round either side
/// of the track, when required to fit the background shape. (See `Background` above.).
pub mod track {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,left:f32,right:f32,corner_left:f32,corner_right:f32,corner_inner:f32,
         track_color:Vector4) {
            let background    = Background::new(&corner_left,&corner_right,style);
            let width         = background.width;
            let height        = background.height;
            let corner_radius = corner_inner * &height/2.0;


            let track_width = (&right - &left) * &width;
            let track_start = left * &width;
            let track       = Rect((&track_width,&height)).corners_radius(corner_radius);
            let track       = track.translate_x(&track_start + (track_width / 2.0) );
            let track       = track.translate_x(-width/2.0);
            let track       = track.intersection(&background.shape);

            let track_color = Var::<color::Rgba>::from(track_color);
            let track       = track.fill(track_color);
            track.into()
          }
    }
}



// ================
// === Overflow ===
// ================

/// Struct that contains the shape used to indicate an overflow (a triangle), and some metadata
/// that can be used to place and align it.
struct OverflowShape {
    #[allow(dead_code)]
    // This field is not used but should stay as part of the API for future use.
    pub width:  Var<Pixels>,
    #[allow(dead_code)]
    // This field is not used but should stay as part of the API for future use.
    pub height: Var<Pixels>,
    pub shape:  AnyShape,
}

impl OverflowShape {
    fn new(style: &StyleWatch) -> Self {
        let sprite_width: Var<Pixels> = "input_size.x".into();
        let sprite_height: Var<Pixels> = "input_size.y".into();

        let width = &sprite_width - shadow::size(style).px();
        let height = &sprite_height - shadow::size(style).px();
        let overflow_color = style.get_color(theme::component::slider::overflow::color);
        let shape = Triangle(&sprite_height / 6.0, &sprite_height / 6.0);
        let shape = shape.fill(&overflow_color);

        let hover_area = Circle(&height);
        let hover_area = hover_area.fill(HOVER_COLOR);

        let shape = (shape + hover_area).into();
        OverflowShape { width, height, shape }
    }
}

/// Overflow shape that indicates a value can not be shown. Appears as a triangle/arrow pointing
/// left.
pub mod left_overflow {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let overflow_shape = OverflowShape::new(style);
            let shape = overflow_shape.shape.rotate((-90.0_f32).to_radians().radians());
            shape.into()
          }
    }
}

/// Overflow shape that indicates a value can not be shown. Appears as a triangle/arrow pointing
/// right.
pub mod right_overflow {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let overflow_shape = OverflowShape::new(style);
            let shape = overflow_shape.shape.rotate(90.0_f32.to_radians().radians());
            shape.into()
          }
    }
}



// =======================
// === Shape Utilities ===
// =======================

use enso_frp::Network;
use ensogl_core::frp::io::Mouse;
use ensogl_core::gui::component::ShapeView;
use ensogl_core::gui::component::ShapeViewEvents;

pub use super::frp::*;
pub use super::model::*;
use ensogl_core::display;
use ensogl_core::display::Scene;

/// Return whether a dragging action has been started from the shape passed to this function. A
/// dragging action is started by a mouse down on the shape, followed by a movement of the mouse.
/// Dragging is ended by a mouse up.
pub fn shape_is_dragged(
    network: &Network,
    shape: &ShapeViewEvents,
    mouse: &Mouse,
) -> enso_frp::Stream<bool> {
    enso_frp::extend! { network
        mouse_up              <- mouse.up.constant(());
        mouse_down            <- mouse.down.constant(());
        over_shape            <- bool(&shape.mouse_out,&shape.mouse_over);
        mouse_down_over_shape <- mouse_down.gate(&over_shape);
        is_dragging_shape     <- bool(&mouse_up,&mouse_down_over_shape);
    }
    is_dragging_shape
}

/// Returns the position of a mouse down on a shape. The position is given in the shape's local
/// coordinate system
pub fn relative_shape_down_position<T: 'static + display::Object + CloneRef>(
    network: &Network,
    scene: &Scene,
    shape: &ShapeView<T>,
) -> enso_frp::Stream<Vector2> {
    let mouse = &scene.mouse.frp;
    enso_frp::extend! { network
        mouse_down            <- mouse.down.constant(());
        over_shape            <- bool(&shape.events.mouse_out,&shape.events.mouse_over);
        mouse_down_over_shape <- mouse_down.gate(&over_shape);
        click_positon         <- mouse.position.sample(&mouse_down_over_shape);
        click_positon         <- click_positon.map(f!([scene,shape](pos)
            scene.screen_to_object_space(&shape,*pos)
        ));
    }
    click_positon
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use enso_frp::io::mouse::Button;
    use enso_frp::stream::EventEmitter;
    use enso_frp::stream::ValueProvider;

    #[test]
    fn test_shape_is_dragged() {
        let network = enso_frp::Network::new("TestNetwork");
        let mouse = enso_frp::io::Mouse::default();
        let shape = ShapeViewEvents::default();

        let is_dragged = shape_is_dragged(&network, &shape, &mouse);
        let _watch = is_dragged.register_watch();


        // Default is false.
        assert_eq!(is_dragged.value(), false);

        // Mouse down over shape activates dragging.
        shape.mouse_over.emit(());
        mouse.down.emit(Button::from_code(0));
        assert_eq!(is_dragged.value(), true);

        // Release mouse stops dragging.
        mouse.up.emit(Button::from_code(0));
        assert_eq!(is_dragged.value(), false);

        // Mouse down while not over shape  does not activate dragging.
        shape.mouse_out.emit(());
        mouse.down.emit(Button::from_code(0));
        assert_eq!(is_dragged.value(), false);
    }
}
