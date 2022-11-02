use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;
use ide_view_component_list_panel_icons::common_part::*;

use ensogl_grid_view as grid_view;
use ide_view_component_list_panel_icons::define_icons;
use ide_view_component_list_panel_icons::SHRINK_AMOUNT;



define_icons! {
    /// A five-pointed star.
    pub mod star(Star) {
        ensogl_core::define_shape_system! {
            above = [grid_view::selectable::highlight::shape, crate::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let shape = FiveStar(8.0.px(),0.447);
                let shape = shape.fill(strong_color);
                shape.shrink(SHRINK_AMOUNT.px()).into()
            }
        }
    }

    /// Local scope section button. A dot inside a circle.
    pub mod local_scope(LocalScope) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let dot = Circle(3.0.px());
                let outer = Circle(8.0.px()) - Circle(7.0.px());
                let shape = dot + outer;
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Sub-modules section button. Three rectangles placed behind each other with perspective.
    pub mod sub_modules(SubModules) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                let corners_radius = 1.5;
                let top = Rect((8.0.px(), 1.5.px()));
                let top = top.corners_radius(corners_radius.px()).translate_y(4.75.px());
                let middle = Rect((12.0.px(), 1.5.px()));
                let middle = middle.corners_radius(corners_radius.px()).translate_y(2.25.px());
                let bottom = Rect((16.0.px(), 6.0.px()));
                let bottom = bottom.corners_radius(corners_radius.px()).translate_y((-2.5).px());
                let shape = top + middle + bottom;
                let shape = shape.fill(strong_color);
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// Four rounded rectangles in different colors aranged in a grid.
    pub mod libraries(Libraries) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                // FIXME: only weak colors are used atm
                let size = 7.0;
                let half = size / 2.0;
                let corners_radius = 1.0;
                let rect0 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect0 = rect0.fill(weak_color.clone());
                let rect0 = rect0.translate(((-half - 0.5).px(),(half + 0.5).px()));

                let rect1 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect1 = rect1.fill(weak_color.clone());
                let rect1 = rect1.translate(((-half - 0.5).px(),(-half - 0.5).px()));

                let rect2 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect2 = rect2.fill(weak_color.clone());
                let rect2 = rect2.translate(((half + 0.5).px(),(-half - 0.5).px()));

                let rect3 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect3 = rect3.fill(weak_color);
                let rect3 = rect3.translate(((half + 0.5).px(),(half + 0.5).px()));

                let shape = rect0 + rect1 + rect2 + rect3;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }

    /// A plus and three rounded rectangles in different colors aranged in a grid.
    pub mod marketplace(Marketplace) {
        ensogl_core::define_shape_system! {
           above = [grid_view::selectable::highlight::shape, crate::background];
            pointer_events = false;
            (style: Style, strong_color: Vector4, weak_color: Vector4) {
                // FIXME: only weak colors are used atm
                let size = 7.0;
                let half = size / 2.0;
                let corners_radius = 1.0;
                let plus = plus(size,1.5);
                let plus = plus.fill(weak_color.clone());
                let plus = plus.translate(((-half - 0.5).px(),(half + 0.5).px()));

                let rect1 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect1 = rect1.fill(weak_color.clone());
                let rect1 = rect1.translate(((-half - 0.5).px(),(-half - 0.5).px()));

                let rect2 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect2 = rect2.fill(weak_color.clone());
                let rect2 = rect2.translate(((half + 0.5).px(),(-half - 0.5).px()));

                let rect3 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
                let rect3 = rect3.fill(weak_color);
                let rect3 = rect3.translate(((half + 0.5).px(),(half + 0.5).px()));

                let shape = plus + rect1 + rect2 + rect3;
                let shape = shape.shrink(SHRINK_AMOUNT.px());
                shape.into()
            }
        }
    }
}

impl Default for Id {
    fn default() -> Self {
        Self::Star
    }
}
