//! All icons that are used in the Component Browser.

use crate::prelude::*;
use ide_view_component_list_panel_icons::common_part::*;

use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::compound::path::path;
use ensogl_grid_view as grid_view;
use ensogl_hardcoded_theme::application::searcher::icons as theme;
use ide_view_component_list_panel_icons::define_icons;
use ide_view_component_list_panel_icons::SHRINK_AMOUNT;
use std::f32::consts::PI;


// ==============
// === Export ===
// ==============

pub use ide_view_component_list_panel_icons::Any;
pub use ide_view_component_list_panel_icons::SIZE;



// =============
// === Icons ===
// =============

define_icons! {
    /// A rounded rectangle with an arrow pointing in from the left.
    pub mod data_input(DataInput) {
        ensogl_core::define_shape_system! {
            above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {

                // === Rectangle ===

                let rect = Rect((11.0.px(),12.0.px())).corners_radius(2.0.px());
                let rect = rect.translate_x(2.5.px());
                let rect = rect.fill(weak_color);


                // === Arrow ===

                let arrow = arrow(11.0,2.0,4.0,6.0).rotate((PI/2.0).radians());
                let arrow = arrow.translate_x(4.0.px());
                let arrow = arrow.fill(strong_color);


                // === Shape ===

                let shape = rect + arrow;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A rounded rectangle with an arrow pointing out to the right.
    pub mod data_output(DataOutput) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {

                // === Rect ===

                let rect = Rect((10.0.px(),12.0.px())).corners_radius(2.0.px());
                let rect = rect.translate_x((-3.0).px());
                let rect = rect.fill(weak_color);


                // === Arrow ===

                let arrow =
                    arrow(11.0,2.0,4.0,6.0).rotate((PI/2.0).radians()).translate_x(8.0.px());
                let arrow = arrow.fill(strong_color);


                // === Shape ===

                let shape = rect + arrow;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A rounded rectangle with the letter "A" and a text cursor.
    pub mod text_input(TextInput) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {

                // === Rect ===

                let rect = Rect((16.0.px(),10.0.px())).corners_radius(2.0.px());
                let rect = rect.fill(weak_color);


                // === Cursor ===

                let cursor = cursor().translate_x(3.0.px()).fill(strong_color.clone());


                // === Letter ===

                // We construct the letter "A", consisting of a diagonal stroke on the left,
                // a diagonal stroke on the right and a horizontal bar in the middle.
                let left_stroke   = Segment((0.0.px(),2.5.px()),((-2.5).px(),(-2.5).px()),1.0.px());
                let right_stroke  = Segment((0.0.px(),2.5.px()),(2.5.px(),(-2.5).px()),1.0.px());
                let bar           = Rect((4.0.px(),1.0.px())).translate_y((-1.0).px());
                let letter        = left_stroke + right_stroke + bar;
                let letter        = letter.translate_x((-3.0).px());
                let letter = letter.fill(strong_color);


                // === Shape ===

                let shape = rect + cursor + letter;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A rounded rectangle with the number "5" and a text cursor.
    pub mod number_input(NumberInput) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {

                // === Rect ===

                let rect = Rect((16.0.px(),10.0.px())).corners_radius(2.0.px());
                let rect = rect.fill(weak_color);


                // === Cursor ===

                let cursor = cursor().translate_x(3.0.px()).fill(strong_color.clone());


                // === Number 5 ===

                // The number "5" consists of a short horizontal bar at the top, a vertical bar
                // connected to it on the left and a big arc below, connected to the vertical bar.
                let top  = Rect((3.0.px(),1.0.px()));
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
                let radius: f32          = connection_offset.norm() + stroke_width;
                let connection_direction = connection_offset.x.atan2(connection_offset.y);

                let arc = arc(radius,stroke_width,connection_direction,228_f32.to_radians());
                let arc = arc.translate((arc_center.x.px(),arc_center.y.px()));

                let number = (top + left + arc).translate_x((-3.0).px()).translate_y(2.5.px());
                let number = number.fill(strong_color);


                // === Shape ===

                let shape = rect + cursor + number;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A table with 4x2 cells and a cursor shape in front of it.
    pub mod table_edit(TableEdit) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                // We need to create the table in two parts, left and right of the cursor to achieve
                // the right cell arangement.
                let left_table  = table(2,2, 4.0).translate(((-8.0).px(), (-4.0).px()));
                let right_table = table(2,2, 4.0).translate(((-1.0).px(),(-4.0).px()));
                let gap         = Rect((4.0.px(),16.0.px()));
                let table = left_table + right_table - gap;
                let table = table.fill(weak_color);
                let cursor      = cursor().fill(strong_color);

                let shape = table + cursor;
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// An arrow to the left on top and an arrow to the right below.
    pub mod convert(Convert) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let upper_arrow = arrow(11.0,2.0,4.0,6.0).rotate((-PI/2.0).radians());
                let upper_arrow = upper_arrow.translate(((-8.0).px(),2.0.px()));
                let lower_arrow = arrow(11.0,2.0,4.0,6.0).rotate((PI/2.0).radians());
                let lower_arrow = lower_arrow.translate((8.0.px(),(-1.0).px()));

                let shape = upper_arrow + lower_arrow;
                let shape = shape.fill(strong_color);
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// A table with an eraser in front.
    pub mod dataframe_clean(DataframeClean) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let table_color = weak_color;
                let table = table(2,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let table = table.fill(table_color);

                let eraser_x = 3.5;
                let eraser_y = -2.0;
                let eraser_rotation = -0.25 * std::f32::consts::PI;

                let eraser = Rect((9.0.px(),5.0.px()));
                let eraser = eraser.corners_radius(1.0.px());
                let eraser_bar = Rect((1.0.px(),6.0.px())).translate_x((-2.0).px());
                let eraser = eraser - eraser_bar;
                let eraser = eraser.rotate(eraser_rotation.radians());
                let eraser = eraser.translate((eraser_x.px(), eraser_y.px()));
                let eraser = eraser.fill(strong_color);
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
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let old_color = weak_color;
                let new_color = strong_color;

                let old_column = table(1,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let old_column = old_column.fill(old_color);
                let new_column = table(1,3, 4.0).translate(((-3.0).px(),(-7.0).px()));
                let new_column = new_column.fill(new_color.clone());
                let plus = plus(6.0,2.0).fill(new_color).translate_x(5.0.px());

                let shape = old_column + new_column + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A light row at the top, a dark row in the middle and a plus at the bottom.
    pub mod add_row(AddRow) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let old_color = weak_color;
                let new_color = strong_color;

                let old_row = table(3,1, 4.0).translate(((-7.0).px(),4.0.px())).fill(old_color);
                let new_row =
                    table(3,1, 4.0).translate(((-7.0).px(),(-1.0).px())).fill(new_color.clone());
                let plus = plus(6.0,2.0).fill(new_color).translate_y((-5.0).px());

                let shape = old_row + new_row + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two light columns on the left and one dark column detached on the right.
    pub mod select_column(SelectColumn) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let unselected = table(2,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let unselected = unselected.fill(weak_color);
                let selected   = table(1,3, 4.0).translate((4.0.px(),(-7.0).px()));
                let selected   = selected.fill(strong_color);

                let shape = unselected + selected;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two light rows at the top and one dark row detached at the bottom.
    pub mod select_row(SelectRow) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let unselected = table(3,2, 4.0).translate(((-7.0).px(),(-1.0).px()));
                let unselected = unselected.fill(weak_color);
                let selected   = table(3,1, 4.0).translate(((-7.0).px(),(-8.0).px()));
                let selected   = selected.fill(strong_color);

                let shape = unselected + selected;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A light column, a dark column and a lightning bolt on the right.
    pub mod dataframe_map_column(DataframeMapColumn) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let weak_color   = weak_color;
                let strong_color = strong_color;

                let weak_column   = table(1,3, 4.0).translate(((-8.0).px(),(-7.0).px()));
                let weak_column = weak_column.fill(weak_color);
                let strong_column = table(1,3, 4.0).translate(((-3.0).px(),(-7.0).px()));
                let strong_column = strong_column.fill(strong_color.clone());
                let lightning = lightning_bolt().translate_x(5.25.px()).fill(strong_color);

                let shape = weak_column + strong_column + lightning;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A light row, a dark row and a lightning bolt below.
    pub mod dataframe_map_row(DataframeMapRow) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let weak_color   = weak_color;
                let strong_color = strong_color;

                let weak_row   = table(3,1, 4.0).translate(((-7.0).px(),4.0.px())).fill(weak_color);
                let strong_row =
                    table(3,1, 4.0).translate(((-7.0).px(),(-1.0).px())).fill(strong_color.clone());
                let lightning  = lightning_bolt().rotate((PI/2.0).radians());
                let lightning  = lightning.translate_y((-5.25).px()).fill(strong_color);

                let shape = weak_row + strong_row + lightning;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two columns with a plus in-between.
    pub mod dataframes_join(DataframesJoin) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let column_color = weak_color.clone();
                let plus_color = weak_color;

                let left_column = table(1,3, 3.0).translate(((-8.0).px(),(-5.5).px()));
                let left_column = left_column.fill(column_color.clone());
                let right_column = table(1,3, 3.0).translate((5.0.px(),(-5.5).px()));
                let right_column = right_column.fill(column_color);
                let plus = plus(6.0,1.0).fill(plus_color);

                let shape = left_column + right_column + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two rows with a plus in-between.
    pub mod dataframes_union(DataframesUnion) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let row_color  = weak_color.clone();
                let plus_color = weak_color;

                let top_row = table(3,1, 3.0).translate(((-5.5).px(),5.0.px()));
                let top_row = top_row.fill(row_color.clone());
                let bottom_row = table(3,1, 3.0).translate(((-5.5).px(),(-8.0).px()));
                let bottom_row = bottom_row.fill(row_color);
                let plus = plus(6.0,1.0).fill(plus_color);

                let shape = top_row + bottom_row + plus;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A capital "Σ".
    pub mod sigma(Sigma) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let shape = path(2.0,&[
                    ( 4.0 ,  4.0),
                    ( 4.0 ,  5.5),
                    (-5.0 ,  5.5),
                    ( 0.5 ,  0.0),
                    (-5.0 , -5.5),
                    ( 4.0 , -5.5),
                    ( 5.0 , -3.5),
                ]);
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape of a sheet of paper that has been ripped apart with a vertical crack through the
    /// middle. Both pieces contain two thin rectangles as a simple representation of lines of text.
    pub mod split_text(SplitText) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {

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
                let page  = page.fill(weak_color);


                // === Crack ===

                let crack = path(1.0,&[
                    ( 0.0  ,  6.5),
                    (-1.25 ,  3.25),
                    ( 0.0  ,  0.0),
                    (-1.25 , -3.25),
                    ( 0.0  , -6.5),
                ]);
                let crack = crack.fill(strong_color);

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
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let circle = Circle(2.0.px());
                let circle1 = circle.translate_y(5.5.px()).fill(weak_color.clone());
                let circle2 = circle.translate(((-5.5).px(),(-3.0).px())).fill(weak_color.clone());
                let circle3 = circle.translate((5.5.px(),(-3.0).px())).fill(weak_color);

                let circle4 = circle.fill(strong_color.clone());
                let rect = Rect((4.0.px(),4.0.px()));
                let rect1 = rect.translate(((-5.5).px(),3.0.px())).fill(strong_color.clone());
                let rect2 = rect.translate_y((-5.5).px()).fill(strong_color);

                let shape = rect1 + rect2 + circle1 + circle2 + circle3 + circle4;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A WiFi symbol, consisting of a small circle and three arcs of increasing size above it.
    pub mod network(Network) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let circle = Circle(1.0.px()).fill(strong_color.clone());
                let arc1 = RoundedArc((10.5/3.0*1.0).px(),(PI/2.0).radians(),1.5.px());
                let arc1= arc1.fill(strong_color.clone());
                let arc2 = RoundedArc((10.5/3.0*2.0).px(),(PI/2.0).radians(),1.5.px());
                let arc2 = arc2.fill(strong_color);
                let arc3 = RoundedArc((10.5/3.0*3.0).px(),(PI/2.0).radians(),1.5.px());
                let arc3 = arc3.fill(weak_color);

                let shape = circle + arc1 + arc2 + arc3;
                let shape = shape.translate_y((-5.5).px());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A dark rectangle containing the simple terminal prompt ">_".
    pub mod system(System) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
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

    /// Two half arrow, one on top and pointing to the right, one at the bottom and pointing to the
    /// left. The shape has an outline in a darker color.
    pub mod io(IO) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let half_arrow = arrow(14.0,5.0,7.0,11.0).rotate((PI/2.0).radians()) - HalfPlane();
                let upper = half_arrow.translate((7.0.px(),0.5.px()));
                let lower = half_arrow.rotate(PI.radians()).translate(((-7.0).px(),(-1.0).px()));

                let base  = upper + lower;
                let outer = base.fill(strong_color);
                let inner = base.shrink(0.5.px()).fill(weak_color);

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
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {

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
                let outline = outline.fill(strong_color);


                // === Fill ===

                let big_triangle = Triangle(13.5.px(),6.75.px()).rotate(PI.radians());
                let big_triangle = big_triangle.translate(((-0.25).px(),2.625.px()));
                let pipe = Rect((2.5.px(),6.0.px())).translate(((-0.25).px(),(-1.5).px()));
                let small_triangle = Triangle(5.0.px(),2.5.px()).rotate((-PI/2.0).radians());
                let small_triangle = small_triangle.translate(((-0.25).px(),(-4.5).px()));
                let fill = big_triangle + pipe + small_triangle;
                let fill = fill.fill(weak_color);


                // === Shape ===

                let shape = fill.shrink(SHRINK_AMOUNT.px()) + outline.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Two intersecting circles. The circles, their outlines and the intersection are displayed in
    /// different colors.
    pub mod join(Join) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let left_circle = Circle(4.5.px()).translate_x((-2.5).px());
                let right_circle = Circle(4.5.px()).translate_x(2.5.px());
                let intersection = &left_circle * &right_circle;
                let left_outline = left_circle.grow(1.0.px()) - &left_circle;
                let left_outline = left_outline.fill(strong_color.clone());
                let right_outline = right_circle.grow(1.0.px()) - &right_circle;
                let right_outline = right_outline.fill(strong_color.clone());

                let left_circle = left_circle.fill(weak_color.clone());
                let right_circle = right_circle.fill(weak_color.clone());
                let intersection = intersection.fill(weak_color);

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
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let page = Rect((10.0.px(),14.0.px())).corners_radius(2.0.px());
                let page = page.translate_x((-2.0).px());
                let page = &page - page.shrink(1.0.px());

                let arrow = arrow(13.0,1.0,3.0,6.0)
                    .rotate((PI/2.0).radians())
                    .translate((8.0.px(),3.0.px()));

                let line1 = Rect((6.0.px(),1.0.px())).translate_x((-2.0).px());
                let line2 = line1.translate_y((-3.0).px());

                let shape = page + arrow + line1 + line2;
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A clock shape.
    pub mod date_and_time(DateAndTime) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let circle = Circle(7.75.px());
                let circle = &circle - circle.shrink(1.0.px());

                let big_hand   = Segment((0.0.px(),0.0.px()),(3.0.px(),(-2.0).px()),1.5.px());
                let small_hand = Segment((0.0.px(),0.0.px()),(0.0.px(),2.5.px()),1.5.px());

                let shape = circle + big_hand + small_hand;
                let shape = shape.translate((0.25.px(),0.25.px()));
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape location marker. A thick circle outline going over into a triangle that poins
    /// down. Around the tip there is an ellipse outline.
    pub mod spatial(Spatial) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let circle   = Circle(4.5.px()).translate_y(3.5.px());
                let circle   = &circle - circle.shrink(2.0.px());
                let triangle = Triangle(7.0,5.75).rotate(PI.radians()).translate_y((-2.125).px());
                let marker   = circle + &triangle;

                let ellipse     = Ellipse(6.5.px(),2.5.px()).translate_y((-5.0).px());
                let ellipse     = &ellipse - ellipse.shrink(1.0.px());
                // If we used just the triangle for the gap then it would also cut into the lower
                // part of the ellipse.
                let ellipse_gap = triangle.grow(1.5.px()) - HalfPlane().translate_y((-5.0).px());
                let ellipse     = ellipse - ellipse_gap;

                let shape = marker + ellipse;
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape of a christal ball with a bas below.
    pub mod predictive(Predictive) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
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
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The shape of an android.
    pub mod machine_learning(MachineLearning) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
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
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// The simplified shape of a camera. It consists of a small red circle in a bigger circle
    /// outline, representing the lens and a base above that the camera is mounted on.
    pub mod computer_vision(ComputerVision) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let lens =
                    Circle(2.0.px()).fill(style.get_color(theme::computer_vision::highlight));
                let outline = Circle(4.5.px()) - Circle(3.5.px());
                let outline = outline.fill(strong_color);

                let base =
                    Circle(7.0.px()).translate_y(6.0.px()) * HalfPlane().translate_y(7.0.px());
                let base = base + Rect((14.0.px(),2.0.px())).translate_y(7.0.px());
                let base = base - Circle(5.5.px());
                let base = base.fill(weak_color);

                let shape = lens + outline + base;
                let shape = shape.translate_y((-2.0).px());
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Outline of a circle. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Atom`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod atom(Atom) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let circle = Circle(5.5.px()) - Circle(4.0.px());
                let shape = circle.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A filled triangle pointing to the right. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Function`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod function(Function) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let triangle = Triangle(12.0, 12.0).rotate((PI/2.0).radians());
                let shape = triangle.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A small filled circle. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Local`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod local(Local) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let dot = Circle(4.0.px());
                let shape = dot.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A rectangle rotated by 45 degrees. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Method`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod method(Method) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let rhomb = path(1.5, &[
                    (6.0, 0.0),
                    (0.0, -6.0),
                    (-6.0, 0.0),
                    (0.0, 6.0),
                    (6.0, 0.0),
                ]);
                let shape = rhomb.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// An outline of a square with rounded corners. A placeholder icon for
    /// [`enso_gui::model::suggestion_database::entry::Kind::Module`] components. Planned to be
    /// replaced by a carefully designed icon in the future.
    pub mod module(Module) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::entry::background];
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let rect = Rect((14.0.px(), 14.0.px())).corners_radius(3.0.px());
                let rect = &rect - rect.shrink(1.5.px());
                let shape = rect.fill(strong_color);
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
