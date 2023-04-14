//! Icons to be used of the action bar of a node.

use crate::prelude::*;
use ensogl::display::shape::*;

use ensogl::data::color;
use ensogl_component::toggle_button::ColorableShape;
use std::f32::consts::FRAC_PI_2;
use std::f32::consts::FRAC_PI_6;



/// Icon for the visibility button. Looks like an open eye.
pub mod visibility {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color  = Var::<color::Rgba>::from(color_rgba);

            let width        = Var::<Pixels>::from("input_size.x");
            let height       = Var::<Pixels>::from("input_size.y");
            let right_angle  = 90.0_f32.to_radians().radians();
            let unit         = &width/16.0;
            let outer_radius = &unit*5.0;
            let pupil        = Circle(&unit * 1.0);
            let inner_circle = Circle(&unit * 3.0);
            let outer_circle = Circle(outer_radius);
            let right_edge   = Triangle(&unit * 7.9, &unit * 4.6);
            let right_edge   = right_edge.rotate(right_angle);
            let right_edge   = right_edge.translate_x(&unit * 5.3);
            let left_edge    = right_edge.rotate(2.0 * right_angle);
            let eye_outer    = outer_circle + right_edge + left_edge;
            let eye          = (eye_outer - inner_circle) + pupil;
            let eye_colored  = eye.fill(fill_color);
            let hover_area   = Rect((width,height)).fill(INVISIBLE_HOVER_COLOR);

            (eye_colored+hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

/// Alternative version of the icon for the visibility button. Looks like a ring with three gaps.
/// Not currently used.
pub mod visibility2 {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color   = Var::<color::Rgba>::from(color_rgba);
            let width        = Var::<Pixels>::from("input_size.x");
            let height       = Var::<Pixels>::from("input_size.y");
            let right_angle  = 90.0_f32.to_radians().radians();
            let unit         = &width/16.0;
            let outer_circle = Circle(&unit * 7.0);
            let inner_circle = Circle(&unit * 4.0);
            let ring         = outer_circle - inner_circle;
            let gap          = Rect((&unit * 3.0, &unit * 16.0));
            let gap          = gap.translate_y(-&width/2.0);
            let gap2         = &gap.rotate(right_angle);
            let gap3         = &gap.rotate(right_angle * 2.5);
            let icon         = ring - &gap -gap2 - gap3;
            let hover_area   = Rect((width,height)).fill(INVISIBLE_HOVER_COLOR);
            let icon         = icon.fill(fill_color);

            (icon+hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

fn make_ring<T: Into<Var<Pixels>>, U: Into<Var<Pixels>>>(
    outer_radius: T,
    inner_radius: U,
) -> AnyShape {
    let outer_circle = Circle(outer_radius.into());
    let inner_circle = Circle(inner_radius.into());
    let ring = outer_circle - inner_circle;
    ring.into()
}

/// Icon for the freeze / lock button. Looks like a padlock.
pub mod freeze {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color       = Var::<color::Rgba>::from(color_rgba);
            let width            = Var::<Pixels>::from("input_size.x");
            let height           = Var::<Pixels>::from("input_size.y");
            let unit             = &width/16.0;
            let right_angle      = 90.0_f32.to_radians().radians();
            let lock_body_radius = &unit * 5.0;
            let lock_body        = make_ring(&lock_body_radius,&unit*2.0);
            let lock_body        = lock_body.translate_y(-&unit * 3.0);
            let lock_top_radius  = &unit * 4.0;
            let lock_top_width   = &unit * 2.0;
            let lock_top         = make_ring(&lock_top_radius,&lock_top_radius - &lock_top_width);
            let lock_top         = lock_top.intersection(HalfPlane().rotate(right_angle*2.0));
            let lock_top         = lock_top.translate_y(lock_body_radius - &unit * 3.0);
            let vertical_bar     = Rect((&lock_top_width,&unit * 4.0));
            let left_bar         = vertical_bar.translate_x(&lock_top_radius-&lock_top_width/2.0);
            let right_bar        = vertical_bar.translate_x(-&lock_top_radius+&lock_top_width/2.0);
            let icon             = lock_body + lock_top + left_bar + right_bar;
            let icon             = icon.translate_y(unit);
            let hover_area       = Rect((width,height)).fill(INVISIBLE_HOVER_COLOR);
            let icon             = icon.fill(fill_color);
            (icon + hover_area).pixel_snap().into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

/// Icon for the skip button. Looks like a circle with a right facing arrow.
pub mod skip {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color   = Var::<color::Rgba>::from(color_rgba);
            let width        = Var::<Pixels>::from("input_size.x");
            let height       = Var::<Pixels>::from("input_size.y");
            let right_angle  = 90.0_f32.to_radians().radians();
            let unit         = &width/16.0;
            let circle       = Circle(&unit*7.0);
            let line_width   = &unit * 3.0;
            let line_height  = &unit * 8.0;
            let offset       = &unit * -0.22;
            let line_rounded = Rect((&line_width,&line_height)).corners_radius(&line_width);
            let line_top     = &line_rounded.rotate(right_angle / 2.0);
            let line_top     = line_top.translate_y(-&line_height / 4.0 - &offset);
            let line_bottom  = &line_rounded.rotate(-right_angle / 2.0);
            let line_bottom  = &line_bottom.translate_y(&line_height / 4.0 + &offset);
            let skip         = line_top + line_bottom;
            let skip         = skip.translate_x(&unit * 0.5);
            let icon         = circle - skip;
            let hover_area   = Rect((width,height)).fill(INVISIBLE_HOVER_COLOR);
            let icon         = icon.fill(fill_color);
            (icon + hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

/// Icon for the button to disable the output context. Looks like a crossed-out arrow loop.
pub mod disable_output_context {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color = Var::<color::Rgba>::from(color_rgba);
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / 16.0;
            let arrow_loop = arrow_loop(&unit);
            let stripe = Rect((&unit * 17.33, &unit * 2.0))
                .rotate((-FRAC_PI_6).radians())
                .translate_y(&unit * -2.33);
            let stripe_clip = stripe
                .translate_x(-&unit)
                .translate_y(&unit * 3.0.sqrt());
            let icon = AnyShape::from(arrow_loop + stripe - stripe_clip).fill(fill_color);
            let hover_area = Rect((width,height)).fill(INVISIBLE_HOVER_COLOR);
            (icon + hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

/// Icon for the button to enable the output context. Looks like an arrow loop.
pub mod enable_output_context {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style, color_rgba: Vector4<f32>) {
            let fill_color = Var::<color::Rgba>::from(color_rgba);
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / 16.0;
            let arrow_loop = arrow_loop(&unit).fill(fill_color);
            let hover_area = Rect((width,height)).fill(INVISIBLE_HOVER_COLOR);
            (arrow_loop + hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.color_rgba.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

/// Draw a right-turning arrow loop with the arrow at the top.
fn arrow_loop(unit: &Var<Pixels>) -> AnyShape {
    let outer_rect = Rect((unit * 14.0, unit * 12.0)).corners_radius(unit * 6.0);
    let loop_ = &outer_rect - outer_rect.shrink(unit * 2.0);
    let arrow_head = Triangle(unit * 8.0, unit * 7.0)
        .rotate(FRAC_PI_2.radians())
        .translate_x(unit * 2.5)
        .translate_y(unit * 5.0);
    let cut_out = Rect((unit * 4.0, unit * 2.0))
        .rotate((-FRAC_PI_6).radians())
        .translate_x(unit * (5.5 * f32::cos(FRAC_PI_6)))
        .translate_y(unit * (-4.0 / 3.0 + 3.0.sqrt() + 5.5 * f32::sin(FRAC_PI_6)));
    let arrow_loop = (loop_ + arrow_head - cut_out).translate_y(-unit).translate_x(-unit);
    arrow_loop.into()
}
