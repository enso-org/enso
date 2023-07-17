use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid::entry::icon::dull_color_alpha;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid::entry::special_icons;
use ide_view_component_list_panel_icons::common_part::*;
use std::f32::consts::PI;
use ensogl_toggle_button::ColorableShape;

// ==================
// === Constannts ===
// ==================

const SIZE: f32 = 16.0;

// =============
// === Icons ===
// =============

/// Local scope section button. A dot inside a circle.
pub mod local_scope {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
            let dull_color = vivid_color.clone().multiply_alpha(&dull_alpha);
            let dot = Circle(&unit * 3.0);
            let dot = dot.fill(&vivid_color);
            let outer = Circle(&unit * 8.0) - Circle(&unit * 5.0);
            let outer = outer.fill(dull_color);
            let shape = outer + dot;
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

pub mod command_key {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let center = Rect((6.0.px(), 6.0.px()));
            let center = center - Rect((2.0.px(), 2.0.px()));
            let circle_tl = Circle(3.0.px());
            let hole = Circle(1.0.px()) + Rect((1.0.px(), 1.0.px())).translate((0.5.px(), (-0.5).px()));
            let circle_tl = circle_tl - hole;
            let circle_tr = circle_tl.rotate((PI/2.0).radians());
            let circle_bl = circle_tl.flip_y();
            let circle_br = circle_tr.flip_y();
            let circle_tl = circle_tl.translate(((-4.0).px(), 4.0.px()));
            let circle_tr = circle_tr.translate((4.0.px(), 4.0.px()));
            let circle_bl = circle_bl.translate(((-4.0).px(), (-4.0).px()));
            let circle_br = circle_br.translate((4.0.px(), (-4.0).px()));

            let shape = center + circle_tl + circle_tr + circle_bl + circle_br;
            let shape = shape.fill(vivid_color);
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}
pub mod unstable {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let canvas_width = Var::<Pixels>::from("input_size.x");
            let canvas_height = Var::<Pixels>::from("input_size.y");
            let unit = &canvas_width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let x_radius = 8.0;
            let y_radius = x_radius * 0.465;
            let bottom = Ellipse(x_radius.px(), y_radius.px());
            let bottom_pos_y = -SIZE / 2.0 + y_radius;
            let bottom = bottom.translate_y(bottom_pos_y.px());
            let bottom_center = Ellipse(5.0.px(), 2.0.px()).translate_y((-3.0).px());
            let height = 12.5;
            let y_offset = height / 2.0 - 3.0;
            let bottom_triangle = Triangle(10.0.px(), height.px()).translate_y(y_offset.px());
            let bottom_mask = bottom_center + bottom_triangle;
            let bottom = bottom - bottom_mask;

            let x_radius = 4.0;
            let y_radius = 1.5;
            let middle = Ellipse(x_radius.px(), y_radius.px());
            let middle_pos_y = -0.5;
            let middle = middle.translate_y(middle_pos_y.px());
            let height = 10.0;
            let y_offset = middle_pos_y + height / 2.0;
            let middle_triangle = Triangle(8.0.px(), height.px()).translate_y(y_offset.px());
            let middle = middle + middle_triangle;
            let mask_y = middle_pos_y + 2.5;
            let mask = HalfPlane().translate_y(mask_y.px());
            let mask_ellipse = Ellipse(3.0.px(), 1.0.px()).translate_y(mask_y.px());
            let mask = mask + mask_ellipse;
            let middle = middle - mask;

            let height = 5.5;
            let top_pos_y = middle_pos_y + 4.5;
            let y_offset = top_pos_y + height / 2.0;
            let top_triangle = Triangle(4.4.px(), height.px()).translate_y(y_offset.px());
            let triangle_mask = HalfPlane().translate_y((SIZE / 2.0 - 1.0).px());
            let top_triangle = top_triangle - triangle_mask;
            let top_ellipse = Ellipse(2.2.px(), 0.4.px()).translate_y(top_pos_y.px());
            let top = top_triangle + top_ellipse;

            let shape = bottom + middle + top;
            let shape = shape.fill(vivid_color);
            let hover_area = Rect((&canvas_width, &canvas_height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}
pub mod marketplace {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            use special_icons::marketplace as theme;
            let dull_alpha: Var<f32> = style.get_number(theme::dull_alpha).into();
            let secondary_alpha: Var<f32> = style.get_number(theme::secondary_alpha).into();
            let tertiary_alpha: Var<f32> = style.get_number(theme::tertiary_alpha).into();
            let dull_color = vivid_color.clone().multiply_alpha(&dull_alpha);
            let secondary_color = vivid_color.clone().multiply_alpha(&secondary_alpha);
            let tertiary_color = vivid_color.clone().multiply_alpha(&tertiary_alpha);
            let size = 7.0;
            let half = size / 2.0;
            let corners_radius = 1.0;
            let plus = plus(size,1.5);
            let plus = plus.fill(vivid_color);
            let plus = plus.translate(((-half - 0.5).px(),(half + 0.5).px()));

            let rect1 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
            let rect1 = rect1.fill(secondary_color);
            let rect1 = rect1.translate(((-half - 0.5).px(),(-half - 0.5).px()));

            let rect2 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
            let rect2 = rect2.fill(tertiary_color);
            let rect2 = rect2.translate(((half + 0.5).px(),(-half - 0.5).px()));

            let rect3 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
            let rect3 = rect3.fill(dull_color);
            let rect3 = rect3.translate(((half + 0.5).px(),(half + 0.5).px()));

            let shape = plus + rect1 + rect2 + rect3;
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}
pub mod right_side_panel {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
            let dull_color = vivid_color.clone().multiply_alpha(&dull_alpha);
            let left = Rect((8.0.px(), 16.0.px())).corners_radiuses(3.0.px(), 0.0.px(), 3.0.px(), 0.0.px());
            let left = left.translate_x((-4.0).px());
            let left = left.fill(dull_color);

            let right = Rect((7.0.px(), 16.0.px())).corners_radiuses(0.0.px(), 3.0.px(), 0.0.px(), 3.0.px());
            let right = right.translate_x(4.5.px());
            let button = Rect((3.0.px(), 1.0.px())).translate((4.5.px(), (-0.5).px()));
            let button2 = button.translate_y(2.0.px());
            let button3 = button2.translate_y(2.0.px());
            let right = right - button - button2 - button3;
            let right = right.fill(vivid_color);

            let shape = left + right;
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}
