//! All icons that are used in the searcher.

use ensogl::prelude::*;

use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::display::object::ObjectOps;
use ensogl::display::shape::compound::path::path;
use ensogl::display::shape::*;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::system::web::StyleSetter;
use ensogl_theme::application::searcher::icons as theme;
use std::f32::consts::PI;
use wasm_bindgen::prelude::*;

/// The width and height of all icons.
const ICON_SIZE: f32 = 16.0;

// The following constant exists for development purposes only.
// Due to a rendering error, shapes appear too big when the camera is zoomed in very closely.
// (Documented here: https://github.com/enso-org/ide/issues/1698)
// In the user interface, this is not a big problem, since icons are usually shown at lower zoom
// levels. But it is a problem during development of icons when it becomes necessary to inspect them
// closely. In those situations, one can apply `.shrink(0.35)` to shapes to compensate for the bug
// and make them appear at the correct size while the camera is zoomed in. But that work-around will
// make them appear too thin on the default zoom level.
//
// To make it easy to turn this shrinking on and off before and after working on icons, we define
// the constant `SHRINK_AMOUNT` and apply `.shrink(SHRINK_AMOUNT.px())` to all icons. In every
// commit, `SHRINK_AMOUNT` should be set to 0.0 to make icons look best in the user interface. But
// during work on the icons, it can temporarily be set to 0.35.
const SHRINK_AMOUNT: f32 = 0.0;



// ========================
// === Shape Components ===
// ========================

/// An arrow shape consisting of a straight line and a triangular head. The arrow points upwards and
/// the tip is positioned at the origin.
fn arrow(length: f32, width: f32, head_length: f32, head_width: f32) -> AnyShape {
    // We overlap the line with the head by this amount to make sure that the renderer does not
    // display a gap between them.
    const OVERLAP: f32 = 1.0;
    let line_length = length - head_length + OVERLAP;
    let line = Rect((width.px(), line_length.px()));
    let line = line.translate_y((-line_length / 2.0 - head_length + OVERLAP).px());
    let head = Triangle(head_width, head_length).translate_y((-head_length / 2.0).px());
    (line + head).into()
}

/// An infinite grid of horizontal and vertical lines. The width of each line is given by
/// `stroke_width`, the distance between horizontal lines and the distance between vertical lines
/// are both given by `cell_size`. The origin is at the intersection of a horizontal and a vertical
/// line, touching the horizontal line from the top and the vertical line from the left.
fn grid(stroke_width: f32, cell_size: f32) -> AnyShape {
    let horizontal = HalfPlane();
    let horizontal = horizontal.translate_y(stroke_width.px()) - horizontal;
    let vertical = HalfPlane().rotate((PI / 2.0).radians());
    let vertical = vertical.translate_x(stroke_width.px()) - vertical;
    (horizontal + vertical).repeat((cell_size.px(), cell_size.px())).into()
}

/// A cursor shape, looking roughly like a capital "I".
fn cursor() -> AnyShape {
    let middle = Rect((1.0.px(), 15.0.px()));
    let top = Rect((5.0.px(), 1.0.px())).translate_y(7.5.px());
    let bottom = Rect((5.0.px(), 1.0.px())).translate_y((-7.5).px());
    (middle + top + bottom).into()
}

/// An arc around the origin. `outer_radius` determines the distance from the origin to the outer
/// edge of the arc, `stroke_width` the width of the arc. The arc starts at `start_angle`, relative
/// to the origin and ends at `end_angle`. The ends are flat not rounded, as in [`RoundedArc`].
fn arc(outer_radius: f32, stroke_width: f32, start_angle: f32, end_angle: f32) -> AnyShape {
    let circle = Circle(outer_radius.px()) - Circle((outer_radius - stroke_width).px());
    let inner_angle = (end_angle - start_angle).rem_euclid(2.0 * PI);
    let mid_angle = start_angle + inner_angle / 2.0;
    let angle = PlaneAngleFast(inner_angle.radians()).rotate(mid_angle.radians());
    // The implementation of `PlaneAngleFast` adds 0.5 px to the actual distance to avoid artifacts
    // in corner cases. We apply `grow` to compensate for that and get the shape that we really
    // want.
    let angle = angle.grow(0.5.px());
    (circle * angle).into()
}

/// The shape of a table, given by a grid with size `columns` x `rows`. The stroke width is 1.0 and
/// the cell size 4.0. The origin is at the lower left corner.
fn table(columns: i32, rows: i32) -> AnyShape {
    const STROKE_WIDTH: f32 = 1.0;
    const CELL_SIZE: f32 = 4.0;

    let width = columns as f32 * CELL_SIZE + STROKE_WIDTH;
    let height = rows as f32 * CELL_SIZE + STROKE_WIDTH;
    let bounds =
        Rect((width.px(), height.px())).translate(((width / 2.0).px(), (height / 2.0).px()));
    let grid = grid(STROKE_WIDTH, CELL_SIZE);
    (grid * bounds).into()
}

/// A plus, consisting of two strokes of length `size` and width `stroke_width`, intersecting at the
/// origin.
fn plus(size: f32, stroke_width: f32) -> AnyShape {
    let horizontal = Rect((size.px(), stroke_width.px()));
    let vertical = Rect((stroke_width.px(), size.px()));
    (horizontal + vertical).into()
}

/// A shape resembling a lightning bolt, centered at the origin.
fn lightning_bolt() -> AnyShape {
    let top = Triangle(3.0.px(), 6.0.px()).translate(((-1.0).px(), 1.9.px()));
    let bottom = Triangle(3.0.px(), 6.0.px()).rotate(PI.radians());
    let bottom = bottom.translate((1.0.px(), (-1.9).px()));
    let lightning = (top + bottom).rotate((PI / 6.0).radians());
    lightning.into()
}



// =================
// === Favorites ===
// =================

/// A five-pointed star.
mod star {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let shape = FiveStar(7.0.px(),0.447);
            let shape = shape.fill(style.get_color(theme::favorites));
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}



// ==========
// === IO ===
// ==========

/// A rounded rectangle with an arrow pointing in from the left.
mod data_input {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {

            // === Border ===

            let border = Rect((10.0.px(),13.0.px())).corners_radius(1.5.px()).translate_x(1.5.px());
            // Taking just an outline.
            let border = &border - border.shrink(1.0.px());
            // Creating a gap for the arrow to pass through.
            let gap  = Rect((2.0.px(),4.0.px())).translate_x((-3.0).px());
            let border = border - gap;


            // === Arrow ===

            let arrow = arrow(11.0,1.0,4.0,5.0).rotate((PI/2.0).radians()).translate_x(4.0.px());


            // === Shape ===

            let shape = border + arrow;
            let shape = shape.fill(style.get_color(theme::io::strong));
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}

/// A rounded rectangle with an arrow pointing out to the right.
mod data_output {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {

            // === Border ===

            let border = Rect((9.0.px(),13.0.px())).corners_radius(1.5.px());
            let border = border.translate_x((-2.5).px());
            // Taking just an outline.
            let border = &border - border.shrink(1.0.px());
            // Creating a gap for the arrow to pass through.
            let gap  = Rect((2.0.px(),4.0.px())).translate_x((1.5).px());
            let border = border - gap;


            // === Arrow ===

            let arrow = arrow(11.0,1.0,4.0,5.0).rotate((PI/2.0).radians()).translate_x(8.0.px());


            // === Shape ===

            let shape = border + arrow;
            let shape = shape.fill(style.get_color(theme::io::strong));
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}

/// A rounded rectangle with the letter "A" and a text cursor.
mod text_input {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {

            // === Border ===

            let border = Rect((16.0.px(),11.0.px())).corners_radius(1.5.px());
            // Using just the outline.
            let border = &border - border.shrink(1.0.px());
            // Creating a gap for the cursor.
            let gap = Rect((3.0.px(),13.0.px())).translate_x((3.5).px());
            let border = border - gap;


            // === Cursor ===

            let cursor = cursor().translate_x(3.5.px());


            // === Letter ===

            // We construct the letter "A", consisting of a diagonal stroke on the left, a diagonal
            // stroke on the right and a horizontal bar in the middle.
            let left_stroke   = Segment((0.0.px(),2.5.px()),((-2.5).px(),(-2.5).px()),1.0.px());
            let right_stroke  = Segment((0.0.px(),2.5.px()),(2.5.px(),(-2.5).px()),1.0.px());
            let bar           = Rect((4.0.px(),1.0.px())).translate_y((-1.0).px());
            let letter        = left_stroke + right_stroke + bar;
            let letter        = letter.translate_x((-2.5).px());


            // === Shape ===

            let shape = border + cursor + letter;
            let shape = shape.fill(style.get_color(theme::io::strong));
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}

/// A rounded rectangle with the number "5" and a text cursor.
mod number_input {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {

            // === Border ===

            let border = Rect((16.0.px(),11.0.px())).corners_radius(5.5.px());
            // Using just the outline.
            let border = &border - border.shrink(1.0.px());
            // Creating a gap for the cursor.
            let gap  = Rect((3.0.px(),13.0.px())).translate_x((3.5).px());
            let border = border - gap;


            // === Cursor ===

            let cursor = cursor().translate_x(3.5.px());


            // === Number 5 ===

            // The number "5" consists of a short horizontal bar at the top, a vertical bar
            // connected to it on the left and a big arc below, connected to the vertical bar.
            let top  = Rect((3.0.px(),1.0.px()));
            let left = Rect((1.0.px(),3.0.px())).translate_x((-1.0).px()).translate_y((-1.0).px());


            // == Number 5 Arc ==

            let arc_center = Vector2(-0.25_f32,-3.5_f32);
            // The point where the inner side of the arc connects with the vertical bar.
            let arc_connection = Vector2(-0.5_f32,-2.5_f32);
            // Offset from the arc center to the connection.
            let connection_offset = arc_connection - arc_center;
            let stroke_width = 1.0;
            // The outer radius of the arc.
            let radius: f32          = connection_offset.norm() + stroke_width;
            let connection_direction = connection_offset.x.atan2(connection_offset.y);

            let arc = arc(radius,stroke_width,connection_direction,228_f32.to_radians());
            let arc = arc.translate((arc_center.x.px(),arc_center.y.px()));

            let number = (top + left + arc).translate_x((-2.0).px()).translate_y(2.5.px());


            // === Shape ===

            let shape = border + cursor + number;
            let shape = shape.fill(style.get_color(theme::io::strong));
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}

/// A table with 4x2 cells and a cursor shape in front of it.
mod table_edit {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            // We need to create the table in two parts, left and right of the cursor to achieve the
            // right cell arangement.
            let left_table  = table(2,2).translate(((-8.0).px(),(-4.5).px()));
            let right_table = table(2,2).translate(((-1.0).px(),(-4.5).px()));
            let gap         = Rect((3.0.px(),13.0.px()));
            let cursor      = cursor();

            let shape = left_table + right_table - gap + cursor;
            let shape = shape.fill(style.get_color(theme::io::strong));
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}

/// An arrow to the left on top and an arrow to the right below.
mod convert {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let upper_arrow = arrow(10.0,1.0,4.5,6.0).rotate((-PI/2.0).radians());
            let upper_arrow = upper_arrow.translate(((-8.0).px(),1.0.px()));
            let lower_arrow = arrow(10.0,1.0,4.5,6.0).rotate((PI/2.0).radians());
            let lower_arrow = lower_arrow.translate((8.0.px(),(-1.5).px()));

            let shape = upper_arrow + lower_arrow;
            let shape = shape.fill(style.get_color(theme::io::strong));
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}

/// A table with an eraser in front.
mod dataframe_clean {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let table_color = style.get_color(theme::preparation::weak);
            let table       = table(2,3).translate(((-8.0).px(),(-6.5).px())).fill(table_color);
            let bottom_line = Rect((13.0.px(),1.0.px())).corners_radius(1.0.px()).fill(table_color);
            let bottom_line = bottom_line.translate_y((-6.0).px());

            let eraser       = Rect((9.0.px(),5.0.px())).corners_radius(1.0.px());
            let eraser_bg    = eraser.grow(1.5.px());
            let eraser_bg    = eraser_bg.rotate((-0.25 * std::f32::consts::PI).radians());
            let eraser_bg    = eraser_bg.translate((3.5.px(),(-1.5).px()));
            let eraser_inner = Rect((7.0.px(),3.0.px()));
            let eraser_bar   = Rect((1.0.px(),4.0.px())).translate_x((-1.0).px());
            let eraser       = eraser - eraser_inner + eraser_bar;
            let eraser       = eraser.fill(style.get_color(theme::preparation::strong));
            let eraser       = eraser.rotate((-0.25 * std::f32::consts::PI).radians());
            let eraser       = eraser.translate((3.5.px(),(-1.5).px()));

            let shape = table - eraser_bg + eraser + bottom_line;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A light column on the left, a dark column in the middle and a plus on the right.
mod add_column {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let old_color = style.get_color(theme::preparation::weak);
            let new_color = style.get_color(theme::preparation::strong);

            let old_column = table(1,3).translate(((-8.0).px(),(-6.5).px())).fill(old_color);
            let new_column = table(1,3).translate(((-4.0).px(),(-6.5).px())).fill(new_color);
            let plus       = plus(5.0,1.0).fill(new_color).translate_x(5.0.px());

            let shape = old_column + new_column + plus;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A light row at the top, a dark row in the middle and a plus at the bottom.
mod add_row {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let old_color = style.get_color(theme::preparation::weak);
            let new_color = style.get_color(theme::preparation::strong);

            let old_row = table(3,1).translate(((-6.5).px(),3.0.px())).fill(old_color);
            let new_row = table(3,1).translate(((-6.5).px(),(-1.0).px())).fill(new_color);
            let plus = plus(5.0,1.0).fill(new_color).translate_y((-5.0).px());

            let shape = old_row + new_row + plus;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// Two light columns on the left and one dark column detached on the right.
mod select_column {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let unselected = table(2,3).translate(((-8.0).px(),(-6.5).px()));
            let unselected = unselected.fill(style.get_color(theme::preparation::weak));
            let selected   = table(1,3).translate((3.0.px(),(-6.5).px()));
            let selected   = selected.fill(style.get_color(theme::preparation::strong));

            let shape = unselected + selected;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// Two light rows at the top and one dark row detached at the bottom.
mod select_row {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let unselected = table(3,2).translate(((-6.5).px(),(-1.0).px()));
            let unselected = unselected.fill(style.get_color(theme::preparation::weak));
            let selected   = table(3,1).translate(((-6.5).px(),(-8.0).px()));
            let selected   = selected.fill(style.get_color(theme::preparation::strong));

            let shape = unselected + selected;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A light column, a dark column and a lightning bolt on the right.
mod dataframe_map_column {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let weak_color   = style.get_color(theme::preparation::weak);
            let strong_color = style.get_color(theme::preparation::strong);

            let weak_column   = table(1,3).translate(((-8.0).px(),(-6.5).px())).fill(weak_color);
            let strong_column = table(1,3).translate(((-4.0).px(),(-6.5).px())).fill(strong_color);
            let lightning     = lightning_bolt().translate_x(5.25.px()).fill(strong_color);

            let shape = weak_column + strong_column + lightning;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A light row, a dark row and a lightning bolt below.
mod dataframe_map_row {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let weak_color   = style.get_color(theme::preparation::weak);
            let strong_color = style.get_color(theme::preparation::strong);

            let weak_row   = table(3,1).translate(((-6.5).px(),3.0.px())).fill(weak_color);
            let strong_row = table(3,1).translate(((-6.5).px(),(-1.0).px())).fill(strong_color);
            let lightning  = lightning_bolt().rotate((PI/2.0).radians());
            let lightning  = lightning.translate_y((-5.25).px()).fill(strong_color);

            let shape = weak_row + strong_row + lightning;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// Two columns with a plus in-between.
mod dataframes_join {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let column_color = style.get_color(theme::join::weak);
            let plus_color   = style.get_color(theme::join::strong);

            let left_column  = table(1,3).translate(((-8.0).px(),(-6.5).px())).fill(column_color);
            let right_column = table(1,3).translate((3.0.px(),(-6.5).px())).fill(column_color);
            let plus         = plus(5.0,1.0).fill(plus_color);

            let shape = left_column + right_column + plus;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// Two rows with a plus in-between.
mod dataframes_union {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let row_color  = style.get_color(theme::join::weak);
            let plus_color = style.get_color(theme::join::strong);

            let top_row    = table(3,1).translate(((-6.5).px(),3.0.px())).fill(row_color);
            let bottom_row = table(3,1).translate(((-6.5).px(),(-8.0).px())).fill(row_color);
            let plus       = plus(5.0,1.0).fill(plus_color);

            let shape = top_row + bottom_row + plus;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A capital "Σ".
mod sigma {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let shape = path(2.0,&[
                ( 4.0 ,  4.0),
                ( 4.0 ,  5.5),
                (-5.0 ,  5.5),
                ( 0.5 ,  0.0),
                (-5.0 , -5.5),
                ( 4.0 , -5.5),
                ( 5.0 , -3.5),
            ]);
            let shape = shape.fill(style.get_color(theme::transform));
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// The shape of a sheet of paper that has been ripped apart with a vertical crack through the
/// middle. Both pieces contain two thin rectangles as a simple representation of lines of text.
mod split_text {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {

            // === Page border ===

            let page = Rect((16.0.px(),14.0.px())).corners_radius(2.0.px());
            let page = &page - page.shrink(1.0.px());
            let gap  = Rect((3.0.px(),15.0.px())).translate_x(0.5.px());
            let page = page - gap;


            // === Lines ===

            let line1 = Rect((3.0.px(),1.0.px())).translate_x((-4.5).px());
            let line2 = Rect((2.0.px(),1.0.px())).translate(((-5.0).px(),(-3.0).px()));
            let line3 = Rect((2.0.px(),1.0.px())).translate_x(5.0.px());
            let line4 = Rect((3.0.px(),1.0.px())).translate((4.5.px(),(-3.0).px()));
            let page  = page + line1 + line2 + line3 + line4;
            let page  = page.fill(style.get_color(theme::text::weak));


            // === Crack ===

            let crack = path(1.0,&[
                ( 0.0  ,  6.5),
                (-1.25 ,  3.25),
                ( 0.0  ,  0.0),
                (-1.25 , -3.25),
                ( 0.0  , -6.5),
            ]);
            let crack = crack.fill(style.get_color(theme::text::strong));

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
mod data_science {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let blue = style.get_color(theme::data_science::blue);
            let rect1 = Rect((4.0.px(),4.0.px())).translate(((-5.5).px(),3.0.px())).fill(blue);
            let rect2 = Rect((4.0.px(),4.0.px())).translate_y((-5.5).px()).fill(blue);

            let gray = style.get_color(theme::data_science::gray);
            let circle1 = Circle(2.0.px()).translate_y(5.5.px()).fill(gray);
            let circle2 = Circle(2.0.px()).translate(((-5.5).px(),(-3.0).px())).fill(gray);
            let circle3 = Circle(2.0.px()).translate((5.5.px(),(-3.0).px())).fill(gray);

            let red = style.get_color(theme::data_science::red);
            let circle4 = Circle(2.0.px()).fill(red);

            let shape = rect1 + rect2 + circle1 + circle2 + circle3 + circle4;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A WiFi symbol, consisting of a small circle and three arcs of increasing size above it.
mod network {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let circle = Circle(1.0.px())
                .fill(style.get_color(theme::network::_0));
            let arc1 = RoundedArc((10.5/3.0*1.0).px(),(PI/2.0).radians(),1.5.px())
                .fill(style.get_color(theme::network::_1));
            let arc2 = RoundedArc((10.5/3.0*2.0).px(),(PI/2.0).radians(),1.5.px())
                .fill(style.get_color(theme::network::_2));
            let arc3 = RoundedArc((10.5/3.0*3.0).px(),(PI/2.0).radians(),1.5.px())
                .fill(style.get_color(theme::network::_3));

            let shape = circle + arc1 + arc2 + arc3;
            let shape = shape.translate_y((-5.5).px());
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A dark rectangle containing the simple terminal prompt ">_".
mod system {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let background = Rect((14.0.px(),14.0.px())).corners_radius(2.0.px());
            let background = background.translate_y((-0.5).px());
            let background = background.fill(style.get_color(theme::system::background));
            let greater    = path(1.5,&[
                (-3.75 ,  2.25),
                (-1.25 , -0.25),
                (-3.75 , -2.25),
            ]);
            let bar = Rect((4.0.px(),1.5.px())).translate((2.5.px(),(-2.75).px()));
            let content = greater + bar;
            let content = content.fill(style.get_color(theme::system::content));

            let shape = background + content;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// Four rounded rectangles in different colors aranged in a grid.
mod libraries {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let rect0 = Rect((6.5.px(),6.5.px())).corners_radius(1.0.px());
            let rect0 = rect0.fill(style.get_color(theme::libraries::_0));
            let rect0 = rect0.translate(((-3.75).px(),3.75.px()));

            let rect1 = Rect((6.5.px(),6.5.px())).corners_radius(1.0.px());
            let rect1 = rect1.fill(style.get_color(theme::libraries::_1));
            let rect1 = rect1.translate(((-3.75).px(),(-3.75).px()));

            let rect2 = Rect((6.5.px(),6.5.px())).corners_radius(1.0.px());
            let rect2 = rect2.fill(style.get_color(theme::libraries::_2));
            let rect2 = rect2.translate((3.75.px(),(-3.75).px()));

            let rect3 = Rect((6.5.px(),6.5.px())).corners_radius(1.0.px());
            let rect3 = rect3.fill(style.get_color(theme::libraries::_3));
            let rect3 = rect3.translate((3.75.px(),3.75.px()));

            let shape = rect0 + rect1 + rect2 + rect3;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A plus and three rounded rectangles in different colors aranged in a grid.
mod marketplace {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let plus = plus(6.5,1.5);
            let plus = plus.fill(style.get_color(theme::libraries::_0));
            let plus = plus.translate(((-3.75).px(),3.75.px()));

            let rect1 = Rect((6.5.px(),6.5.px())).corners_radius(1.0.px());
            let rect1 = rect1.fill(style.get_color(theme::libraries::_1));
            let rect1 = rect1.translate(((-3.75).px(),(-3.75).px()));

            let rect2 = Rect((6.5.px(),6.5.px())).corners_radius(1.0.px());
            let rect2 = rect2.fill(style.get_color(theme::libraries::_2));
            let rect2 = rect2.translate((3.75.px(),(-3.75).px()));

            let rect3 = Rect((6.5.px(),6.5.px())).corners_radius(1.0.px());
            let rect3 = rect3.fill(style.get_color(theme::libraries::_3));
            let rect3 = rect3.translate((3.75.px(),3.75.px()));

            let shape = plus + rect1 + rect2 + rect3;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// Two half arrow, one on top and pointing to the right, one at the bottom and pointing to the
/// left. The shape has an outline in a darker color.
mod io {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let half_arrow = arrow(14.0,5.0,7.0,11.0).rotate((PI/2.0).radians()) - HalfPlane();
            let upper      = half_arrow.translate((7.0.px(),0.5.px()));
            let lower      = half_arrow.rotate(PI.radians()).translate(((-7.0).px(),(-1.0).px()));

            let base  = upper + lower;
            let outer = base.fill(style.get_color(theme::io::strong));
            let inner = base.shrink(0.5.px()).fill(style.get_color(theme::io::weak));

            let shape = outer + inner;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// The shape of a funnel, consisting of a big upside-down triangle at the top connected with a thin
/// rectangular tube shape below with a triangular end piece. The whole shape has an outline.
mod preparation {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {

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
            let outline = outline.fill(style.get_color(theme::preparation::strong));


            // === Fill ===

            let big_triangle   = Triangle(13.5.px(),6.75.px()).rotate(PI.radians());
            let big_triangle   = big_triangle.translate(((-0.25).px(),2.625.px()));
            let pipe           = Rect((2.5.px(),6.0.px())).translate(((-0.25).px(),(-1.5).px()));
            let small_triangle = Triangle(5.0.px(),2.5.px()).rotate((-PI/2.0).radians());
            let small_triangle = small_triangle.translate(((-0.25).px(),(-4.5).px()));
            let fill           = big_triangle + pipe + small_triangle;
            let fill           = fill.fill(style.get_color(theme::preparation::weak));


            // === Shape ===

            let shape = fill.shrink(SHRINK_AMOUNT.px()) + outline.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// Two intersecting circles. The circles, their outlines and the intersection are displayed in
/// different colors.
mod join {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let left_circle   = Circle(5.0.px()).translate_x((-3.0).px());
            let right_circle  = Circle(5.0.px()).translate_x(3.0.px());
            let left_outline  = &left_circle - left_circle.shrink(0.5.px());
            let right_outline = &right_circle - right_circle.shrink(0.5.px());
            let intersection  = &left_circle * &right_circle;

            let left_circle   = left_circle.fill(style.get_color(theme::join::weak));
            let right_circle  = right_circle.fill(style.get_color(theme::join::weak));
            let intersection  = intersection.fill(style.get_color(theme::join::medium));
            let left_outline  = left_outline.fill(style.get_color(theme::join::strong));
            let right_outline = right_outline.fill(style.get_color(theme::join::strong));

            let shape = left_circle + right_circle + intersection + left_outline + right_outline;
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A page with three lines representing text. The upper line is part of an arrow pointing out to
/// the right.
mod text {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let page = Rect((10.0.px(),14.0.px())).corners_radius(2.0.px());
            let page = page.translate_x((-2.0).px());
            let page = &page - page.shrink(1.0.px());

            let arrow = arrow(13.0,1.0,3.0,6.0)
                .rotate((PI/2.0).radians())
                .translate((8.0.px(),3.0.px()));

            let line1 = Rect((6.0.px(),1.0.px())).translate_x((-2.0).px());
            let line2 = line1.translate_y((-3.0).px());

            let shape = page + arrow + line1 + line2;
            let shape = shape.fill(style.get_color(theme::text::strong));
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A clock shape.
mod date_and_time {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let circle = Circle(7.75.px());
            let circle = &circle - circle.shrink(1.0.px());

            let big_hand   = Segment((0.0.px(),0.0.px()),(3.0.px(),(-2.0).px()),1.5.px());
            let small_hand = Segment((0.0.px(),0.0.px()),(0.0.px(),2.5.px()),1.5.px());

            let shape = circle + big_hand + small_hand;
            let shape = shape.translate((0.25.px(),0.25.px()));
            let shape = shape.fill(style.get_color(theme::date_and_time));
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// The shape location marker. A thick circle outline going over into a triangle that poins down.
/// Around the tip there is an ellipse outline.
mod spatial {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let circle   = Circle(4.5.px()).translate_y(3.5.px());
            let circle   = &circle - circle.shrink(2.0.px());
            let triangle = Triangle(7.0,5.75).rotate(PI.radians()).translate_y((-2.125).px());
            let marker   = circle + &triangle;

            let ellipse     = Ellipse(6.5.px(),2.5.px()).translate_y((-5.0).px());
            let ellipse     = &ellipse - ellipse.shrink(1.0.px());
            // If we used just the triangle for the gap then it would also cut into the lower part
            // of the ellipse.
            let ellipse_gap = triangle.grow(1.5.px()) - HalfPlane().translate_y((-5.0).px());
            let ellipse     = ellipse - ellipse_gap;

            let shape = marker + ellipse;
            let shape = shape.fill(style.get_color(theme::spatial));
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// The shape of a christal ball with a bas below.
mod predictive {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let circle = Circle(5.5.px());
            let sphere = &circle - circle.shrink(1.0.px());

            let reflection1 = arc(3.5,1.0,-114.0_f32.to_radians(),-95.0_f32.to_radians());
            let reflection2 = arc(3.5,1.0,276.0_f32.to_radians(),13.0_f32.to_radians());
            let sphere      = sphere + reflection1 + reflection2;
            let sphere      = sphere.translate_y(1.5.px());

            let base = Triangle(21.0,8.0).translate_y((-4.0).px());
            let base = base * Rect((13.0.px(),5.0.px())).translate_y((-5.0).px());
            let base = base - circle.translate_y(1.5.px()).grow(2.0.px());

            let shape = sphere + base;
            let shape = shape.fill(style.get_color(theme::predictive));
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// The shape of an android.
mod machine_learning {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
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
            let shape = shape.fill(style.get_color(theme::machine_learning));
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// The simplified shape of a camera. It consists of a small red circle in a bigger circle outline,
/// representing the lens and a base above that the camera is mounted on.
mod computer_vision {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let lens    = Circle(2.0.px()).fill(style.get_color(theme::computer_vision::highlight));
            let outline = Circle(4.5.px()) - Circle(3.5.px());
            let outline = outline.fill(style.get_color(theme::computer_vision::strong));

            let base = Circle(7.0.px()).translate_y(6.0.px()) * HalfPlane().translate_y(7.0.px());
            let base = base + Rect((14.0.px(),2.0.px())).translate_y(7.0.px());
            let base = base - Circle(5.5.px());
            let base = base.fill(style.get_color(theme::computer_vision::weak));

            let shape = lens + outline + base;
            let shape = shape.translate_y((-2.0).px());
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}



// ===================
// === Debug Scene ===
// ===================

/// A rectangular frame to mark the edges of icons.
mod frame {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let inner = Rect((ICON_SIZE.px(),ICON_SIZE.px()));
            let outer = inner.grow(0.2.px());
            let shape = (outer - inner).fill(color::Rgba::black());
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}

/// An entry point that displays all icons on a grid.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_searcher_icons() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();

    let logger = Logger::new("Icons example");
    let app = Application::new(&web::get_html_element_by_id("root").unwrap());
    ensogl_theme::builtin::dark::register(&app);
    ensogl_theme::builtin::light::register(&app);
    ensogl_theme::builtin::light::enable(&app);
    let world = app.display.clone();
    mem::forget(app);
    let scene = world.scene();
    mem::forget(Navigator::new(scene, &scene.camera()));


    // === Grid ===

    let grid_div = web::create_div();
    grid_div.set_style_or_panic("width", "1000px");
    grid_div.set_style_or_panic("height", "16px");
    grid_div.set_style_or_panic("background-size", "1.0px 1.0px");
    grid_div.set_style_or_panic(
        "background-image",
        "linear-gradient(to right,  grey 0.05px, transparent 0.05px),
                                 linear-gradient(to bottom, grey 0.05px, transparent 0.05px)",
    );

    let grid = DomSymbol::new(&grid_div);
    scene.dom.layers.back.manage(&grid);
    world.add_child(&grid);
    grid.set_size(Vector2(1000.0, ICON_SIZE));
    mem::forget(grid);


    // === Frame ===

    let frame = frame::View::new(&logger);
    world.add_child(&frame);
    frame.size.set(Vector2(ICON_SIZE + 20.0, ICON_SIZE + 20.0));
    mem::forget(frame);


    // === Star ===

    let star = star::View::new(&logger);
    world.add_child(&star);
    star.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    star.set_position_x(20.0);
    mem::forget(star);


    // === Data Input ===

    let data_input = data_input::View::new(&logger);
    world.add_child(&data_input);
    data_input.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    data_input.set_position_x(40.0);
    mem::forget(data_input);


    // === Data Output ===

    let data_output = data_output::View::new(&logger);
    world.add_child(&data_output);
    data_output.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    data_output.set_position_x(60.0);
    mem::forget(data_output);


    // === Text Input ===

    let text_input = text_input::View::new(&logger);
    world.add_child(&text_input);
    text_input.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    text_input.set_position_x(80.0);
    mem::forget(text_input);


    // === Number Input ===

    let number_input = number_input::View::new(&logger);
    world.add_child(&number_input);
    number_input.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    number_input.set_position_x(100.0);
    mem::forget(number_input);


    // === Table Edit ===

    let table_edit = table_edit::View::new(&logger);
    world.add_child(&table_edit);
    table_edit.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    table_edit.set_position_x(120.0);
    mem::forget(table_edit);


    // === Convert ===

    let convert = convert::View::new(&logger);
    world.add_child(&convert);
    convert.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    convert.set_position_x(140.0);
    mem::forget(convert);


    // === Dataframe Clean ===

    let dataframe_clean = dataframe_clean::View::new(&logger);
    world.add_child(&dataframe_clean);
    dataframe_clean.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframe_clean.set_position_x(160.0);
    mem::forget(dataframe_clean);


    // === Add Column ===

    let add_column = add_column::View::new(&logger);
    world.add_child(&add_column);
    add_column.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    add_column.set_position_x(180.0);
    mem::forget(add_column);


    // === Add Row ===

    let add_row = add_row::View::new(&logger);
    world.add_child(&add_row);
    add_row.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    add_row.set_position_x(200.0);
    mem::forget(add_row);


    // === Select Column ===

    let select_column = select_column::View::new(&logger);
    world.add_child(&select_column);
    select_column.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    select_column.set_position_x(220.0);
    mem::forget(select_column);


    // === Select Row ===

    let select_row = select_row::View::new(&logger);
    world.add_child(&select_row);
    select_row.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    select_row.set_position_x(240.0);
    mem::forget(select_row);


    // === Dataframe Map Column ===

    let dataframe_map_column = dataframe_map_column::View::new(&logger);
    world.add_child(&dataframe_map_column);
    dataframe_map_column.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframe_map_column.set_position_x(260.0);
    mem::forget(dataframe_map_column);


    // === Dataframe Map Row ===

    let dataframe_map_row = dataframe_map_row::View::new(&logger);
    world.add_child(&dataframe_map_row);
    dataframe_map_row.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframe_map_row.set_position_x(280.0);
    mem::forget(dataframe_map_row);


    // === Dataframes Join ===

    let dataframes_join = dataframes_join::View::new(&logger);
    world.add_child(&dataframes_join);
    dataframes_join.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframes_join.set_position_x(300.0);
    mem::forget(dataframes_join);


    // === Dataframes Union ===

    let dataframes_union = dataframes_union::View::new(&logger);
    world.add_child(&dataframes_union);
    dataframes_union.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframes_union.set_position_x(320.0);
    mem::forget(dataframes_union);


    // === Sigma ===

    let sigma = sigma::View::new(&logger);
    world.add_child(&sigma);
    sigma.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    sigma.set_position_x(340.0);
    mem::forget(sigma);


    // === Split Text ===

    let split_text = split_text::View::new(&logger);
    world.add_child(&split_text);
    split_text.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    split_text.set_position_x(360.0);
    mem::forget(split_text);


    // === Data Science ===

    let data_science = data_science::View::new(&logger);
    world.add_child(&data_science);
    data_science.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    data_science.set_position_x(380.0);
    mem::forget(data_science);


    // === Network ===

    let network = network::View::new(&logger);
    world.add_child(&network);
    network.size.set(Vector2(ICON_SIZE + 1.0, ICON_SIZE));
    network.set_position_x(400.0);
    mem::forget(network);


    // === System ===

    let system = system::View::new(&logger);
    world.add_child(&system);
    system.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    system.set_position_x(420.0);
    mem::forget(system);


    // === Libraries ===

    let libraries = libraries::View::new(&logger);
    world.add_child(&libraries);
    libraries.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    libraries.set_position_x(440.0);
    mem::forget(libraries);


    // === Marketplace ===

    let marketplace = marketplace::View::new(&logger);
    world.add_child(&marketplace);
    marketplace.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    marketplace.set_position_x(460.0);
    mem::forget(marketplace);


    // === IO ===

    let io = io::View::new(&logger);
    world.add_child(&io);
    io.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    io.set_position_x(480.0);
    mem::forget(io);


    // === Preparation ===

    let preparation = preparation::View::new(&logger);
    world.add_child(&preparation);
    preparation.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    preparation.set_position_x(500.0);
    mem::forget(preparation);


    // === Join ===

    let join = join::View::new(&logger);
    world.add_child(&join);
    join.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    join.set_position_x(520.0);
    mem::forget(join);


    // === Text ===

    let text = text::View::new(&logger);
    world.add_child(&text);
    text.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    text.set_position_x(540.0);
    mem::forget(text);


    // === Date and Time ===

    let date_and_time = date_and_time::View::new(&logger);
    world.add_child(&date_and_time);
    date_and_time.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    date_and_time.set_position_x(560.0);
    mem::forget(date_and_time);


    // === Spatial ===

    let spatial = spatial::View::new(&logger);
    world.add_child(&spatial);
    spatial.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    spatial.set_position_x(580.0);
    mem::forget(spatial);


    // === Predictive ===

    let predictive = predictive::View::new(&logger);
    world.add_child(&predictive);
    predictive.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    predictive.set_position_x(600.0);
    mem::forget(predictive);


    // === Machine Learning ===

    let machine_learning = machine_learning::View::new(&logger);
    world.add_child(&machine_learning);
    machine_learning.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    machine_learning.set_position_x(620.0);
    mem::forget(machine_learning);


    // === Computer Vision ===

    let computer_vision = computer_vision::View::new(&logger);
    world.add_child(&computer_vision);
    computer_vision.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    computer_vision.set_position_x(640.0);
    mem::forget(computer_vision);
}
