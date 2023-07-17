//! All icons that are used in the Component Browser.

use crate::prelude::*;
use ide_view_component_list_panel_icons::common_part::*;

use ensogl_core::data::color;
use ensogl_core::display::shape::compound::path::path;
use ensogl_core::display::IntoGlsl;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as theme;
use ide_view_component_list_panel_icons::define_icons;
use ide_view_component_list_panel_icons::SHRINK_AMOUNT;
use std::f32::consts::PI;
use theme::grid::entry::icon::dull_color_alpha;
use theme::grid::entry::special_icons;


// ==============
// === Export ===
// ==============

pub use ide_view_component_list_panel_icons::any;
pub use ide_view_component_list_panel_icons::SIZE;



const VIVID_COLOR: Var<color::LinearRgba> = Var::Static(color::LinearRgba::new(1.0, 0.0, 0.0, 1.0));


// =============
// === Icons ===
// =============

define_icons! {
    /// A five-pointed star.
    pub mod star(Star) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let shape = FiveStar(8.0.px(),0.447);
                let shape = shape.fill(VIVID_COLOR.glsl());
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A compass icon. Eight-pointed star.
    /// NOTE: This icon is not used in the Component Browser, but rather in the visualization
    /// container. It is placed here temporarily, to demonstrate it in the icons demo scene easily.
    /// It will be moved when refactoring the visualization container.
    /// See https://github.com/enso-org/enso/issues/7315.
    pub mod compass(Compass) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let foreground = Rect((4.0.px(), 4.0.px()));
                let triangle = Triangle(4.6.px(), 6.0.px());
                let up = triangle.translate_y(5.0.px());
                let right = up.rotate((PI / 2.0).radians());
                let down = right.rotate((PI / 2.0).radians());
                let left = down.rotate((PI / 2.0).radians());
                let foreground = foreground + up + right + down + left;
                let foreground = foreground.fill(VIVID_COLOR.glsl());
                let background = foreground.rotate((PI / 4.0).radians()).scale(0.875);
                let background = background.fill(dull_color.glsl());
                let center = Circle(2.0.px());
                let shape = background + foreground - center;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Local scope section button. A dot inside a circle.
    pub mod local_scope(LocalScope) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let dot = Circle(3.0.px());
                let dot = dot.fill(VIVID_COLOR.glsl());
                let outer = Circle(8.0.px()) - Circle(5.0.px());
                let outer = outer.fill(dull_color.glsl());
                let shape = outer + dot;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Sub-modules section button. Three rectangles placed behind each other with perspective.
    pub mod sub_modules(SubModules) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let corners_radius = 1.5;
                let top = Rect((8.0.px(), 1.5.px()));
                let top = top.corners_radius(corners_radius.px()).translate_y(4.75.px());
                let middle = Rect((12.0.px(), 1.5.px()));
                let middle = middle.corners_radius(corners_radius.px()).translate_y(2.25.px());
                let bottom = Rect((16.0.px(), 6.0.px()));
                let bottom = bottom.corners_radius(corners_radius.px()).translate_y((-2.5).px());
                let shape = top + middle + bottom;
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A road cone.
    pub mod unstable(Unsable) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
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
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Command key icon from Mac's keyboard.
    pub mod command_key(CommandKey) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
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
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two rectangles with a gap between them, the right one is vivid and represents a side panel
    /// with buttons.
    pub mod right_side_panel(RightSidePanel) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let left = Rect((8.0.px(), 16.0.px())).corners_radiuses(3.0.px(), 0.0.px(), 3.0.px(), 0.0.px());
                let left = left.translate_x((-4.0).px());
                let left = left.fill(dull_color.glsl());

                let right = Rect((7.0.px(), 16.0.px())).corners_radiuses(0.0.px(), 3.0.px(), 0.0.px(), 3.0.px());
                let right = right.translate_x(4.5.px());
                let button = Rect((3.0.px(), 1.0.px())).translate((4.5.px(), (-0.5).px()));
                let button2 = button.translate_y(2.0.px());
                let button3 = button2.translate_y(2.0.px());
                let right = right - button - button2 - button3;
                let right = right.fill(VIVID_COLOR.glsl());

                let shape = left + right;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Four rounded rectangles in different colors arranged in a grid.
    pub mod libraries(Libraries) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                use special_icons::libraries as theme;
                let dull_alpha: Var<f32> = style.get_number(theme::dull_alpha).into();
                let secondary_alpha: Var<f32> = style.get_number(theme::secondary_alpha).into();
                let tertiary_alpha: Var<f32> = style.get_number(theme::tertiary_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let secondary_color = &VIVID_COLOR * &secondary_alpha;
                let tertiary_color = &VIVID_COLOR * &tertiary_alpha;
                let size = 7.0;
                let half = size / 2.0;
                let corners_radius = 1.0;
                let rect0 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect0 = rect0.fill(VIVID_COLOR.glsl());
                let rect0 = rect0.translate(((-half - 0.5).px(),(half + 0.5).px()));

                let rect1 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect1 = rect1.fill(secondary_color.glsl());
                let rect1 = rect1.translate(((-half - 0.5).px(),(-half - 0.5).px()));

                let rect2 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect2 = rect2.fill(tertiary_color.glsl());
                let rect2 = rect2.translate(((half + 0.5).px(),(-half - 0.5).px()));

                let rect3 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect3 = rect3.fill(dull_color.glsl());
                let rect3 = rect3.translate(((half + 0.5).px(),(half + 0.5).px()));

                let shape = rect0 + rect1 + rect2 + rect3;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A plus and three rounded rectangles in different colors aranged in a grid.
    pub mod marketplace(Marketplace) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                use special_icons::marketplace as theme;
                let dull_alpha: Var<f32> = style.get_number(theme::dull_alpha).into();
                let secondary_alpha: Var<f32> = style.get_number(theme::secondary_alpha).into();
                let tertiary_alpha: Var<f32> = style.get_number(theme::tertiary_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let secondary_color = &VIVID_COLOR * &secondary_alpha;
                let tertiary_color = &VIVID_COLOR * &tertiary_alpha;
                let size = 7.0;
                let half = size / 2.0;
                let corners_radius = 1.0;
                let plus = plus(size,1.5);
                let plus = plus.fill(VIVID_COLOR.glsl());
                let plus = plus.translate(((-half - 0.5).px(),(half + 0.5).px()));

                let rect1 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect1 = rect1.fill(secondary_color.glsl());
                let rect1 = rect1.translate(((-half - 0.5).px(),(-half - 0.5).px()));

                let rect2 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect2 = rect2.fill(tertiary_color.glsl());
                let rect2 = rect2.translate(((half + 0.5).px(),(-half - 0.5).px()));

                let rect3 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect3 = rect3.fill(dull_color.glsl());
                let rect3 = rect3.translate(((half + 0.5).px(),(half + 0.5).px()));

                let shape = plus + rect1 + rect2 + rect3;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two dots inside square brackets.
    pub mod array_new(ArrayNew) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                let brackets = Rect((SIZE.px(), 11.0.px()));
                let mask = Rect((8.0.px(), 11.0.px())) + Rect((12.0.px(), 7.0.px()));
                let brackets = brackets - mask;
                let brackets = brackets.translate_y((-0.5).px());
                let brackets = brackets.fill(VIVID_COLOR.glsl());

                let dot = Rect((3.0.px(), 3.0.px()));
                let left = dot.translate(((-2.5).px(), (-0.5).px()));
                let right = dot.translate((2.5.px(), (-0.5).px()));
                let dots = left + right;
                let dots = dots.fill(dull_color.glsl());

                let shape = dots + brackets;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A rounded rectangle with an arrow pointing in from the left.
    pub mod data_input(DataInput) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                // === Rectangle ===

                let rect = Rect((11.0.px(),12.0.px())).corners_radius(2.0.px());
                let rect = rect.translate_x(2.5.px());
                let rect = rect.fill(dull_color.glsl());


                // === Arrow ===

                let arrow = arrow(11.0,2.0,4.0,6.0).rotate((PI/2.0).radians());
                let arrow = arrow.translate_x(4.0.px());
                let arrow = arrow.fill(VIVID_COLOR.glsl());


                // === Shape ===

                let shape = rect + arrow;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A rounded rectangle with an arrow pointing out to the right.
    pub mod data_output(DataOutput) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                // === Rect ===

                let rect = Rect((10.0.px(),12.0.px())).corners_radius(2.0.px());
                let rect = rect.translate_x((-3.0).px());
                let rect = rect.fill(dull_color.glsl());


                // === Arrow ===

                let arrow =
                    arrow(11.0,2.0,4.0,6.0).rotate((PI/2.0).radians()).translate_x(8.0.px());
                let arrow = arrow.fill(VIVID_COLOR.glsl());


                // === Shape ===

                let shape = rect + arrow;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A rounded rectangle with the letter "A" and a text cursor.
    pub mod text_input(TextInput) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                // === Rect ===

                let rect = Rect((16.0.px(),10.0.px())).corners_radius(2.0.px());
                let rect = rect.fill(dull_color.glsl());


                // === Cursor ===

                let cursor = cursor().translate_x(3.0.px()).fill(VIVID_COLOR.glsl());


                // === Letter ===

                // We construct the letter "A", consisting of a diagonal stroke on the left,
                // a diagonal stroke on the right and a horizontal bar in the middle.
                let left_stroke = Segment((0.0.px(),2.5.px()),((-2.5).px(),(-2.5).px()),1.2.px());
                let right_stroke = Segment((0.0.px(),2.5.px()),(2.5.px(),(-2.5).px()),1.2.px());
                let bar = Rect((4.0.px(),1.2.px())).translate_y((-1.0).px());
                let letter = left_stroke + right_stroke + bar;
                let letter = letter.translate_x((-3.0).px());
                let letter = letter.fill(VIVID_COLOR.glsl());


                // === Shape ===

                let shape = rect + cursor + letter;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A rounded rectangle with the number "5" and a text cursor.
    pub mod number_input(NumberInput) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                // === Rect ===

                let rect = Rect((16.0.px(),10.0.px())).corners_radius(2.0.px());
                let rect = rect.fill(dull_color.glsl());


                // === Cursor ===

                let cursor = cursor().translate_x(3.0.px()).fill(VIVID_COLOR.glsl());


                // === Number 5 ===

                // The number "5" consists of a short horizontal bar at the top, a vertical bar
                // connected to it on the left and a big arc below, connected to the vertical bar.
                let top = Rect((3.0.px(),1.0.px()));
                let left = Rect((1.0.px(),3.0.px()));
                let left = left.translate_x((-1.0).px()).translate_y((-1.0).px());


                // == Number 5 Arc ==

                let arc_center = Vector2(-0.25_f32,-3.5_f32);
                // The point where the inner side of the arc connects with the vertical bar.
                let arc_connection = Vector2(-0.5_f32,-2.5_f32);
                // Offset from the arc center to the connection.
                let connection_offset = arc_connection - arc_center;
                let stroke_width = 1.0;
                // The outer radius of the arc.
                let radius: f32 = connection_offset.norm() + stroke_width;
                let connection_direction = connection_offset.x.atan2(connection_offset.y);

                let arc = arc(radius,stroke_width,connection_direction,228_f32.to_radians());
                let arc = arc.translate((arc_center.x.px(),arc_center.y.px()));

                let number = (top + left + arc).translate_x((-3.0).px()).translate_y(2.5.px());
                let number = number.fill(VIVID_COLOR.glsl());


                // === Shape ===

                let shape = rect + cursor + number;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A table with 4x2 cells and a cursor shape in front of it.
    pub mod table_edit(TableEdit) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                // We need to create the table in two parts, left and right of the cursor to achieve
                // the right cell arangement.
                let left_table = table(2,2, 4.0).translate(((-8.0).px(), (-4.0).px()));
                let right_table = table(2,2, 4.0).translate(((-1.0).px(),(-4.0).px()));
                let gap = Rect((4.0.px(),16.0.px()));
                let table = left_table + right_table - gap;
                let table = table.fill(dull_color.glsl());
                let cursor = cursor().fill(VIVID_COLOR.glsl());

                let shape = table + cursor;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// An arrow to the left on top and an arrow to the right below.
    pub mod convert(Convert) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let upper_arrow = arrow(11.0,2.0,4.0,6.0).rotate((-PI/2.0).radians());
                let upper_arrow = upper_arrow.translate(((-8.0).px(),2.0.px()));
                let lower_arrow = arrow(11.0,2.0,4.0,6.0).rotate((PI/2.0).radians());
                let lower_arrow = lower_arrow.translate((8.0.px(),(-1.0).px()));

                let shape = upper_arrow + lower_arrow;
                let shape = shape.fill(VIVID_COLOR.glsl());
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A table with an eraser in front.
    pub mod dataframe_clean(DataframeClean) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let table_color = dull_color;
                let table = table(2,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let table = table.fill(table_color.glsl());

                let eraser_x = 3.5;
                let eraser_y = -2.0;
                let eraser_rotation = -0.25 * std::f32::consts::PI;

                let eraser = Rect((9.0.px(),5.0.px()));
                let eraser = eraser.corners_radius(1.0.px());
                let eraser_bar = Rect((1.0.px(),6.0.px())).translate_x((-2.0).px());
                let eraser = eraser - eraser_bar;
                let eraser = eraser.rotate(eraser_rotation.radians());
                let eraser = eraser.translate((eraser_x.px(), eraser_y.px()));
                let eraser = eraser.fill(VIVID_COLOR.glsl());
                let eraser_bg = Rect((13.0.px(), 9.0.px()));
                let eraser_bg = eraser_bg.rotate(eraser_rotation.radians());
                let eraser_bg = eraser_bg.translate((eraser_x.px(), eraser_y.px()));

                let shape = table - eraser_bg + eraser;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A light column on the left, a dark column in the middle and a plus on the right.
    pub mod add_column(AddColumn) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let old_color = dull_color;
                let new_color = VIVID_COLOR;

                let old_column = table(1,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let old_column = old_column.fill(old_color.glsl());
                let new_column = table(1,3, 4.0).translate(((-3.0).px(),(-7.0).px()));
                let new_column = new_column.fill(new_color.glsl());
                let plus = plus(6.0,2.0).fill(new_color.glsl()).translate_x(5.0.px());

                let shape = old_column + new_column + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A light row at the top, a dark row in the middle and a plus at the bottom.
    pub mod add_row(AddRow) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let old_color = dull_color;
                let new_color = VIVID_COLOR;

                let old_row = table(3,1, 4.0).translate(((-7.0).px(),4.0.px())).fill(old_color.glsl());
                let new_row =
                    table(3,1, 4.0).translate(((-7.0).px(),(-1.0).px())).fill(new_color.glsl());
                let plus = plus(6.0,2.0).fill(new_color.glsl()).translate_y((-5.0).px());

                let shape = old_row + new_row + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two light columns on the left and one dark column detached on the right.
    pub mod select_column(SelectColumn) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let unselected = table(2,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let unselected = unselected.fill(dull_color.glsl());
                let selected = table(1,3, 4.0).translate((4.0.px(),(-7.0).px()));
                let selected = selected.fill(VIVID_COLOR.glsl());

                let shape = unselected + selected;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two light rows at the top and one dark row detached at the bottom.
    pub mod select_row(SelectRow) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let unselected = table(3,2, 4.0).translate(((-7.0).px(),(-1.0).px()));
                let unselected = unselected.fill(dull_color.glsl());
                let selected = table(3,1, 4.0).translate(((-7.0).px(),(-8.0).px()));
                let selected = selected.fill(VIVID_COLOR.glsl());

                let shape = unselected + selected;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A light column, a dark column and a lightning bolt on the right.
    pub mod dataframe_map_column(DataframeMapColumn) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let dull_color = dull_color;

                let weak_column = table(1,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let weak_column = weak_column.fill(dull_color.glsl());
                let strong_column = table(1,3, 4.0).translate(((-3.0).px(),(-7.0).px()));
                let strong_column = strong_column.fill(VIVID_COLOR.glsl());
                let lightning = lightning_bolt().translate_x(5.25.px()).fill(VIVID_COLOR.glsl());

                let shape = weak_column + strong_column + lightning;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A light row, a dark row and a lightning bolt below.
    pub mod dataframe_map_row(DataframeMapRow) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let dull_color = dull_color;

                let weak_row = table(3,1, 4.0).translate(((-7.0).px(),4.0.px())).fill(dull_color.glsl());
                let strong_row =
                    table(3,1, 4.0).translate(((-7.0).px(),(-1.0).px())).fill(VIVID_COLOR.glsl());
                let lightning = lightning_bolt().rotate((PI/2.0).radians());
                let lightning = lightning.translate_y((-5.25).px()).fill(VIVID_COLOR.glsl());

                let shape = weak_row + strong_row + lightning;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two columns with a plus in-between.
    pub mod dataframes_join(DataframesJoin) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let column_color = dull_color.clone();
                let plus_color = dull_color;

                let left_column = table(1,3, 3.0).translate(((-8.0).px(),(-5.5).px()));
                let left_column = left_column.fill(column_color.glsl());
                let right_column = table(1,3, 3.0).translate((5.0.px(),(-5.5).px()));
                let right_column = right_column.fill(column_color.glsl());
                let plus = plus(6.0,2.0).fill(plus_color.glsl());

                let shape = left_column + right_column + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two rows with a plus in-between.
    pub mod dataframes_union(DataframesUnion) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let row_color = dull_color.clone();
                let plus_color = dull_color;

                let top_row = table(3,1, 3.0).translate(((-5.5).px(),5.0.px()));
                let top_row = top_row.fill(row_color.glsl());
                let bottom_row = table(3,1, 3.0).translate(((-5.5).px(),(-8.0).px()));
                let bottom_row = bottom_row.fill(row_color.glsl());
                let plus = plus(6.0,1.0).fill(plus_color.glsl());

                let shape = top_row + bottom_row + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A capital "Σ".
    pub mod sigma(Sigma) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let shape = path(2.0,&[
                    ( 4.0 ,  4.0),
                    ( 4.0 ,  5.5),
                    (-5.0 ,  5.5),
                    ( 0.5 ,  0.0),
                    (-5.0 , -5.5),
                    ( 4.0 , -5.5),
                    ( 5.0 , -3.5),
                ]);
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape of a sheet of paper that has been ripped apart with a vertical crack through the
    /// middle. Both pieces contain two thin rectangles as a simple representation of lines of text.
    pub mod split_text(SplitText) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                // === Page border ===

                let page = Rect((16.0.px(),14.0.px())).corners_radius(2.0.px());
                let page = &page - page.shrink(1.0.px());
                let gap = Rect((3.0.px(),15.0.px())).translate_x(0.5.px());
                let page = page - gap;


                // === Lines ===

                let line1 = Rect((3.0.px(),1.0.px())).translate_x((-4.5).px());
                let line2 = Rect((2.0.px(),1.0.px())).translate(((-5.0).px(),(-3.0).px()));
                let line3 = Rect((2.0.px(),1.0.px())).translate_x(5.0.px());
                let line4 = Rect((3.0.px(),1.0.px())).translate((4.5.px(),(-3.0).px()));
                let page = page + line1 + line2 + line3 + line4;
                let page = page.fill(dull_color.glsl());


                // === Crack ===

                let crack = path(1.0,&[
                    ( 0.0  ,  6.5),
                    (-1.25 ,  3.25),
                    ( 0.0  ,  0.0),
                    (-1.25 , -3.25),
                    ( 0.0  , -6.5),
                ]);
                let crack = crack.fill(VIVID_COLOR.glsl());

                let crack_left  = crack.translate_x((-1.0).px());
                let crack_right = crack.translate_x(2.0.px());


                // === Shape ===

                let shape = page + crack_left + crack_right;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Some rectangles and circles in different colors.
    pub mod data_science(DataScience) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let circle = Circle(2.0.px());
                let circle1 = circle.translate_y(5.5.px()).fill(dull_color.glsl());
                let circle2 = circle.translate(((-5.5).px(),(-3.0).px())).fill(dull_color.glsl());
                let circle3 = circle.translate((5.5.px(),(-3.0).px())).fill(dull_color.glsl());

                let circle4 = circle.fill(VIVID_COLOR.glsl());
                let rect = Rect((4.0.px(),4.0.px()));
                let rect1 = rect.translate(((-5.5).px(),3.0.px())).fill(VIVID_COLOR.glsl());
                let rect2 = rect.translate_y((-5.5).px()).fill(VIVID_COLOR.glsl());

                let shape = rect1 + rect2 + circle1 + circle2 + circle3 + circle4;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A WiFi symbol, consisting of a small circle and three arcs of increasing size above it.
    pub mod network(Network) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let circle = Circle(1.0.px()).fill(VIVID_COLOR.glsl());
                let arc1 = RoundedArc((10.5/3.0*1.0).px(),(PI/2.0).radians(),1.5.px());
                let arc1= arc1.fill(VIVID_COLOR.glsl());
                let arc2 = RoundedArc((10.5/3.0*2.0).px(),(PI/2.0).radians(),1.5.px());
                let arc2 = arc2.fill(VIVID_COLOR.glsl());
                let arc3 = RoundedArc((10.5/3.0*3.0).px(),(PI/2.0).radians(),1.5.px());
                let arc3 = arc3.fill(dull_color.glsl());

                let shape = circle + arc1 + arc2 + arc3;
                let shape = shape.translate_y((-5.5).px());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A dark rectangle containing the simple terminal prompt ">_".
    pub mod system(System) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                use special_icons::system as theme;
                let background = Rect((14.0.px(),14.0.px())).corners_radius(2.0.px());
                let background = background.translate_y((-0.5).px());
                let background = background.fill(style.get_color(theme::background));
                let greater = path(1.5,&[
                    (-3.75 ,  2.25),
                    (-1.25 , -0.25),
                    (-3.75 , -2.25),
                ]);
                let bar = Rect((4.0.px(),1.5.px())).translate((2.5.px(),(-2.75).px()));
                let content = greater + bar;
                let content = content.fill(style.get_color(theme::content));

                let shape = background + content;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two half arrow, one on top and pointing to the right, one at the bottom and pointing to the
    /// left. The shape has an outline in a darker color.
    pub mod io(IO) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let half_arrow = arrow(14.0,5.0,7.0,11.0).rotate((PI/2.0).radians()) - BottomHalfPlane();
                let upper = half_arrow.translate((7.0.px(),0.5.px()));
                let lower = half_arrow.rotate(PI.radians()).translate(((-7.0).px(),(-1.0).px()));

                let base  = upper + lower;
                let outer = base.fill(VIVID_COLOR.glsl());
                let inner = base.shrink(0.5.px()).fill(dull_color.glsl());

                let shape = outer + inner;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape of a funnel, consisting of a big upside-down triangle at the top connected with
    /// a thin rectangular tube shape below with a triangular end piece. The whole shape has an
    /// outline.
    pub mod preparation(Preparation) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                // === Outline ===

                let outline = path(1.0,&[
                    (-6.5 ,  6.0),
                    ( 6.0 ,  6.0),
                    ( 6.0 ,  5.5),
                    ( 1.0 ,  0.5),
                    ( 1.0 , -7.0),
                    (-1.5 , -4.5),
                    (-1.5 ,  0.5),
                    (-6.5 ,  5.5),
                    (-6.5 ,  6.0),
                ]);
                let outline = outline.fill(VIVID_COLOR.glsl());


                // === Fill ===

                let big_triangle = Triangle(13.5.px(),6.75.px()).rotate(PI.radians());
                let big_triangle = big_triangle.translate(((-0.25).px(),2.625.px()));
                let pipe = Rect((2.5.px(),6.0.px())).translate(((-0.25).px(),(-1.5).px()));
                let small_triangle = Triangle(5.0.px(),2.5.px()).rotate((-PI/2.0).radians());
                let small_triangle = small_triangle.translate(((-0.25).px(),(-4.5).px()));
                let fill = big_triangle + pipe + small_triangle;
                let fill = fill.fill(dull_color.glsl());


                // === Shape ===

                let shape = fill.shrink(SHRINK_AMOUNT.px()) + outline.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two intersecting circles. The circles, their outlines and the intersection are displayed in
    /// different colors.
    pub mod join(Join) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                use special_icons::join as theme;
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let intersection_alpha: Var<f32> = style.get_number(theme::intersection_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let intersection_color = &VIVID_COLOR * &intersection_alpha;
                let left_circle = Circle(4.5.px()).translate_x((-2.5).px());
                let right_circle = Circle(4.5.px()).translate_x(2.5.px());
                let intersection = &left_circle * &right_circle;
                let left_outline = left_circle.grow(1.0.px()) - &left_circle;
                let left_outline = left_outline.fill(VIVID_COLOR.glsl());
                let right_outline = right_circle.grow(1.0.px()) - &right_circle;
                let right_outline = right_outline.fill(VIVID_COLOR.glsl());

                let left_circle = left_circle.fill(dull_color.glsl());
                let right_circle = right_circle.fill(dull_color.glsl());
                let intersection = intersection.fill(intersection_color.glsl());

                let shape =
                    left_circle + right_circle + intersection + left_outline + right_outline;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A page with three lines representing text. The upper line is part of an arrow pointing out
    /// to the right.
    pub mod text(Text) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let page = Rect((10.0.px(),14.0.px())).corners_radius(2.0.px());
                let page = page.translate_x((-2.0).px());
                let page = &page - page.shrink(1.0.px());

                let arrow = arrow(13.0,1.0,3.0,6.0)
                    .rotate((PI/2.0).radians())
                    .translate((8.0.px(),3.0.px()));

                let line1 = Rect((6.0.px(),1.0.px())).translate_x((-2.0).px());
                let line2 = line1.translate_y((-3.0).px());

                let shape = page + arrow + line1 + line2;
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A clock shape.
    pub mod date_and_time(DateAndTime) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let circle = Circle(8.0.px());
                let circle = circle.fill(dull_color.glsl());

                let big_hand = Rect((2.0.px(), 7.0.px())).corners_radius(1.0.px());
                let big_hand = big_hand.translate_y(2.5.px()).pixel_snap();
                let small_hand = Rect((2.0.px(), 5.5.px())).corners_radius(1.0.px());
                let small_hand = small_hand.translate_y((-1.75).px());
                let small_hand = small_hand.rotate((-PI / 3.0).radians());
                let hands = big_hand + small_hand;
                let hands = hands.fill(VIVID_COLOR.glsl());

                let shape = circle + hands;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A calendar.
    pub mod calendar(Calendar) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;

                let bg = Rect((15.0.px(), 12.0.px()));
                let bg = bg.corners_radiuses(0.0.px(), 0.0.px(), 2.0.px(), 2.0.px());
                let bg = bg.translate(((-0.5).px(), (-2.0).px()));
                let bg = bg.fill(dull_color.glsl());

                let top = Rect((15.0.px(), 2.0.px()));
                let top = top.corners_radiuses(2.0.px(), 2.0.px(), 0.0.px(), 0.0.px());
                let top = top.translate(((-0.5).px(), 5.0.px()));
                let handle = Rect((2.0.px(), 4.0.px())).corners_radius(1.0.px());
                let handle = handle.translate_y(6.0.px());
                let left_handle = handle.translate_x((-4.5).px());
                let right_handle = handle.translate_x(3.5.px());
                let top = top + left_handle + right_handle;
                let top = top.fill(VIVID_COLOR.glsl());

                let dot = Circle(1.0.px());
                let dots = dot.repeat((3.0.px(), 3.0.px())).fill(VIVID_COLOR.glsl());
                let dots = dots.translate(((-5.0).px(), 1.0.px()));
                let mask = Rect((11.0.px(), 8.0.px()));
                let mask = mask.translate(((-0.5).px(), (-2.0).px()));
                let bottom_left_corner = Rect((2.0.px(), 2.0.px()));
                let bottom_left_corner = bottom_left_corner.translate((4.0.px(), (-5.0).px()));
                let mask = mask - bottom_left_corner;
                let dots = dots.intersection(&mask);

                let shape = bg + top + dots;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape location marker. A thick circle outline going over into a triangle that poins
    /// down. Around the tip there is an ellipse outline.
    pub mod spatial(Spatial) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let circle = Circle(4.5.px()).translate_y(3.5.px());
                let circle = &circle - circle.shrink(2.0.px());
                let triangle = Triangle(7.0,5.75).rotate(PI.radians()).translate_y((-2.125).px());
                let marker = circle + &triangle;

                let ellipse = Ellipse(6.5.px(),2.5.px()).translate_y((-5.0).px());
                let ellipse = &ellipse - ellipse.shrink(1.0.px());
                // If we used just the triangle for the gap then it would also cut into the lower
                // part of the ellipse.
                let ellipse_gap = triangle.grow(1.5.px()) - BottomHalfPlane().translate_y((-5.0).px());
                let ellipse = ellipse - ellipse_gap;

                let shape = marker + ellipse;
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape of a christal ball with a bas below.
    pub mod predictive(Predictive) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let circle = Circle(5.5.px());
                let sphere = &circle - circle.shrink(1.0.px());

                let reflection1 = arc(3.5,1.0,-114.0_f32.to_radians(),-95.0_f32.to_radians());
                let reflection2 = arc(3.5,1.0,276.0_f32.to_radians(),13.0_f32.to_radians());
                let sphere = sphere + reflection1 + reflection2;
                let sphere = sphere.translate_y(1.5.px());

                let base = Triangle(21.0,8.0).translate_y((-4.0).px());
                let base = base * Rect((13.0.px(),5.0.px())).translate_y((-5.0).px());
                let base = base - circle.translate_y(1.5.px()).grow(2.0.px());

                let shape = sphere + base;
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape of an android.
    pub mod machine_learning(MachineLearning) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let body = Rect((10.0.px(),15.0.px()))
                    .corners_radiuses(5.0.px(),5.0.px(),2.0.px(),2.0.px())
                    .translate_y((-0.5).px());
                let body = &body - body.shrink(1.0.px());

                let collar = Rect((9.0.px(),1.0.px()));

                let left_eye = Rect((1.5.px(),1.5.px())).translate(((-1.75).px(),2.75.px()));
                let right_eye = Rect((1.5.px(),1.5.px())).translate((1.75.px(),2.75.px()));
                let antenna = Rect((1.0.px(),1.5.px())).translate_y(7.25.px());
                let left_arm = Rect((1.0.px(),4.5.px())).translate(((-6.5).px(),(-2.75).px()));
                let right_arm = Rect((1.0.px(),4.5.px())).translate((6.5.px(),(-2.75).px()));

                let shape = body + collar + left_eye + right_eye + antenna + left_arm + right_arm;
                let shape = shape.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The simplified shape of a camera. It consists of a small red circle in a bigger circle
    /// outline, representing the lens and a base above that the camera is mounted on.
    pub mod computer_vision(ComputerVision) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                use special_icons::computer_vision as theme;
                let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
                let dull_color = &VIVID_COLOR * &dull_alpha;
                let lens = Circle(2.0.px()).fill(style.get_color(theme::highlight));
                let outline = Circle(4.5.px()) - Circle(3.5.px());
                let outline = outline.fill(VIVID_COLOR.glsl());

                let base = Circle(7.0.px());
                let base = base.translate_y(6.0.px()) * BottomHalfPlane();
                let base = base.translate_y(7.0.px());
                let base = base + Rect((14.0.px(),2.0.px())).translate_y(7.0.px());
                let base = base - Circle(5.5.px());
                let base = base.fill(dull_color.glsl());

                let shape = lens + outline + base;
                let shape = shape.translate_y((-2.0).px());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Outline of a circle. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Type`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod r#type(Type) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let circle = Circle(5.5.px()) - Circle(4.0.px());
                let shape = circle.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Outline of a circle. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Constructor`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod constructor(Constructor) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let circle = Circle(5.5.px()) - Circle(4.0.px());
                let shape = circle.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A filled triangle pointing to the right. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Function`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod function(Function) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let triangle = Triangle(12.0, 12.0).rotate((PI/2.0).radians());
                let shape = triangle.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A small filled circle. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Local`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod local(Local) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let dot = Circle(4.0.px());
                let shape = dot.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A rectangle rotated by 45 degrees. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Method`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod method(Method) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let rhomb = path(1.5, &[
                    (6.0, 0.0),
                    (0.0, -6.0),
                    (-6.0, 0.0),
                    (0.0, 6.0),
                    (6.0, 0.0),
                ]);
                let shape = rhomb.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// An outline of a square with rounded corners. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Module`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod module(Module) {
        ensogl_core::cached_shape! {
            size = (SIZE, SIZE);
            alignment = center;
            (style: Style) {
                let rect = Rect((14.0.px(), 14.0.px())).corners_radius(3.0.px());
                let rect = &rect - rect.shrink(1.5.px());
                let shape = rect.fill(VIVID_COLOR.glsl());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }
}

impl Default for Id {
    fn default() -> Self {
        Self::DataScience
    }
}
