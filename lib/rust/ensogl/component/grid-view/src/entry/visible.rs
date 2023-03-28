//! A module with content related to entries visible in GridView.

use crate::prelude::*;

use crate::entry;
use crate::Col;
use crate::ColumnWidths;
use crate::Entry;
use crate::Margins;
use crate::Row;

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_scroll_area::Viewport;



// =================
// === Constants ===
// =================

/// The distance the mouse should be moved in last couple of frames to highlight entry as hovered.
///
/// We avoid hovering the entry when mouse does not move for better UX (e.g. during scrolling with
/// mouse wheel).
const MOUSE_MOVEMENT_NEEDED_TO_HOVER_PX: f32 = 1.5;



// ====================
// === VisibleEntry ===
// ====================

/// An `Entry` instance visible inside Grid View.
#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "Entry: CloneRef")]
pub struct VisibleEntry<Entry> {
    /// The entry instance.
    pub entry:   Entry,
    /// The overlay shape, catching the mouse events for the entry.
    pub overlay: entry::overlay::View,
}

impl<E: display::Object> display::Object for VisibleEntry<E> {
    fn display_object(&self) -> &display::object::Instance {
        self.entry.display_object()
    }
}



// ===================
// === CreationCtx ===
// ===================

/// A structure gathering all data required for creating new entry instance.
#[allow(missing_docs)]
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct CreationCtx<EntryParams> {
    pub app:                   Application,
    pub network:               frp::WeakNetwork,
    pub set_entry_size:        frp::Stream<Vector2>,
    pub set_entry_params:      frp::Stream<EntryParams>,
    pub entry_contour:         frp::Any<(Row, Col, entry::Contour)>,
    pub entry_hovered:         frp::Any<Option<(Row, Col)>>,
    pub entry_selected:        frp::Any<Option<(Row, Col)>>,
    pub entry_accepted:        frp::Any<(Row, Col)>,
    pub override_column_width: frp::Any<(Col, f32)>,
    pub minimum_column_width:  frp::Any<(Col, f32)>,
}

impl<EntryParams> CreationCtx<EntryParams>
where EntryParams: frp::node::Data
{
    /// Create new entry instance.
    ///
    /// The new instance will have all its FRP endpoints connected to appropriate endpoints of
    /// `self` and overlay mouse events.
    ///
    /// The second value in the returned tuple is an `init` FRP endpoint that must be triggered
    /// as soon as the entry is created. It is not triggered automatically to allow calling
    /// `create_entry` without the risk of multiple RefCell borrows. Some FRP endpoints of the
    /// entry may cause double borrows, as they are already connected to the grid view handlers
    /// at the time of the `create_entry` call.
    pub fn create_entry<E: Entry<Params = EntryParams>>(
        &self,
        text_layer: Option<&Layer>,
    ) -> (VisibleEntry<E>, Option<frp::Source<()>>) {
        let entry = E::new(&self.app, text_layer);
        let overlay = entry::overlay::View::new();
        entry.add_child(&overlay);
        let init = if let Some(network) = self.network.upgrade_or_warn() {
            let entry_frp = entry.frp();
            let entry_network = entry_frp.network();
            let mouse = &self.app.display.default_scene.mouse.frp_deprecated;
            frp::new_bridge_network! { [network, entry_network] grid_view_entry_bridge
                init <- source_();
                entry_frp.set_size <+ all(init, self.set_entry_size)._1();
                entry_frp.set_params <+ all(init, self.set_entry_params)._1();
                contour <- all(init, entry_frp.contour)._1();
                eval contour ((c) overlay.set_contour(*c));
                contour_offset <- all(init, entry_frp.contour_offset)._1();
                eval contour_offset ((off) overlay.set_xy(*off));

                let events = &overlay.events_deprecated;
                let disabled = &entry_frp.disabled;
                let location = entry_frp.set_location.clone_ref();
                self.entry_contour <+ all_with(&location, &contour, |&(r, c), &cont| (r, c, cont));
                column <- location._1();

                // We make a distinction between "hovered" state and "mouse_in" state, because
                // we want to highlight entry as hovered only when mouse moves a bit.
                hovering <- any(...);
                hovering <+ events.mouse_out.constant(false);
                mouse_in <- bool(&events.mouse_out, &events.mouse_over);
                // We can receive `mouse_over` event a couple of frames after actual hovering.
                // Therefore, we count our "mouse move" from a couple of frames before.
                mouse_pos_some_time_ago <- mouse.prev_position.previous().previous().previous();
                mouse_over_movement_start <- mouse_pos_some_time_ago.sample(&events.mouse_over);
                mouse_over_with_start_pos <- all(mouse.position, mouse_over_movement_start).gate(&mouse_in);
                mouse_move_which_hovers <- mouse_over_with_start_pos.filter(
                    |(pos, start_pos)| (pos - start_pos).norm() > MOUSE_MOVEMENT_NEEDED_TO_HOVER_PX
                );
                hovered <- mouse_move_which_hovers.gate_not(&hovering).gate_not(disabled);
                hovering <+ hovered.constant(true);
                selected <- events.mouse_down.gate_not(disabled);
                accepted <- events.mouse_down_primary.gate_not(disabled);
                self.entry_hovered <+ location.sample(&hovered).map(|l| Some(*l));
                self.entry_selected <+ location.sample(&selected).map(|l| Some(*l));
                self.entry_accepted <+ location.sample(&accepted);
                self.override_column_width <+ entry_frp.override_column_width.map2(
                    &column,
                    |width, col| (*col, *width)
                );
                self.minimum_column_width <+ entry_frp.minimum_column_width.map2(
                    &column,
                    |width, col| (*col, *width)
                );
            }
            Some(init)
        } else {
            None
        };
        (VisibleEntry { entry, overlay }, init)
    }
}



// ================
// === Position ===
// ================

/// Get base X position of entry at given column.
pub fn position_x(col: Col, entry_size: Vector2, column_widths: &ColumnWidths) -> f32 {
    let x_offset = column_widths.pos_offset(col) + column_widths.width_diff(col) / 2.0;
    (col as f32 + 0.5) * entry_size.x + x_offset
}

/// Get base Y position of entry at given row.
pub fn position_y(row: Row, entry_size: Vector2) -> f32 {
    (row as f32 + 0.5) * -entry_size.y
}

/// Get base position of entry at given row and column.
pub fn position(row: Row, col: Col, entry_size: Vector2, column_widths: &ColumnWidths) -> Vector2 {
    Vector2(position_x(col, entry_size, column_widths), position_y(row, entry_size))
}

/// Set the proper position of entry at given row and column.
pub fn set_position<E: Entry>(
    entry: &E,
    row: Row,
    col: Col,
    entry_size: Vector2,
    column_widths: &ColumnWidths,
) {
    let pos = position(row, col, entry_size, column_widths);
    entry.set_xy(pos);
    entry.frp().position_set(pos);
}

/// Get size of entry at given row and column.
pub fn size(
    _row: Row,
    col: Col,
    base_entry_size: Vector2,
    column_widths: &ColumnWidths,
) -> Vector2 {
    Vector2(base_entry_size.x + column_widths.width_diff(col), base_entry_size.y)
}

/// Return the position of the top-left corner of a viewport containing the area around the entry
/// at given row and column. The area around an entry is defined as the bounding box of the entry
/// enlarged by given margins. If there is more than one such viewport possible, return the one
/// closest to the given viewport. The returned viewport has the same size as given viewport.
///
/// In the picture below, the dashed border represents the viewport, while the solid border
/// represents the entry contained in the viewport.
/// ```text
/// ┌┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┐
/// ┆                     ▲             ┆
/// ┆         margins.top │             ┆
/// ┆                     ▼             ┆
/// ┆ margins ┌───────────────┐ margins ┆
/// ┆  .left  │  entry        │  .right ┆
/// ┆ ◀─────▶ └───────────────┘ ◀─────▶ ┆
/// ┆                     ▲             ┆
/// ┆      margins.bottom │             ┆
/// ┆                     ▼             ┆
/// └┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┘
/// ```
pub fn position_of_viewport_containing_entry(
    row: Row,
    col: Col,
    entry_size: Vector2,
    column_widths: &ColumnWidths,
    viewport: Viewport,
    margins: Margins,
) -> Vector2 {
    let pos = position(row, col, entry_size, column_widths);
    let size = size(row, col, entry_size, column_widths);
    let entry = Viewport::from_center_point_and_size(pos, size);
    let entry_plus_margins = Viewport {
        top:    entry.top + margins.top,
        bottom: entry.bottom - margins.bottom,
        left:   entry.left - margins.left,
        right:  entry.right + margins.right,
    };
    let moved_viewport = viewport.moved_to_contain(entry_plus_margins);
    Vector2(moved_viewport.left, moved_viewport.top)
}

#[cfg(test)]
mod tests {
    use super::*;

    const ENTRY_SIZE: Vector2 = Vector2(20.0, 10.0);
    const COL_COUNT: usize = 100;
    const MARGINS: Margins = Margins { top: 1.0, bottom: 2.0, left: 3.0, right: 4.0 };
    const VIEWPORT_SIZE: Vector2 = Vector2(200.0, 100.0);
    const ROW: Row = 3;
    const COL: Col = 5;

    fn sample_column_widths() -> ColumnWidths {
        ColumnWidths::new(COL_COUNT)
    }

    fn pos_of_viewport_centered_and_then_repositioned_to_contain_sample_entry(
        center: Vector2,
    ) -> Vector2 {
        let viewport = Viewport::from_center_point_and_size(center, VIEWPORT_SIZE);
        let column_widths = sample_column_widths();
        position_of_viewport_containing_entry(
            ROW,
            COL,
            ENTRY_SIZE,
            &column_widths,
            viewport,
            MARGINS,
        )
    }

    #[test]
    fn position_of_viewport_scrolled_up_and_left_to_contain_entry() {
        let center = Vector2(1000.0, -1000.0);
        let pos = pos_of_viewport_centered_and_then_repositioned_to_contain_sample_entry(center);
        assert_approx_eq!(pos.x, 97.0);
        assert_approx_eq!(pos.y, -29.0);
    }

    #[test]
    fn position_of_viewport_scrolled_down_and_right_to_contain_entry() {
        let center = Vector2(-1000.0, 1000.0);
        let pos = pos_of_viewport_centered_and_then_repositioned_to_contain_sample_entry(center);
        assert_approx_eq!(pos.x, -76.0);
        assert_approx_eq!(pos.y, 58.0);
    }

    #[test]
    fn position_of_viewport_not_modified_because_it_already_contains_entry() {
        let center = Vector2(100.0, -30.0);
        let pos = pos_of_viewport_centered_and_then_repositioned_to_contain_sample_entry(center);
        assert_approx_eq!(pos.x, center.x - VIEWPORT_SIZE.x / 2.0);
        assert_approx_eq!(pos.y, center.y + VIEWPORT_SIZE.y / 2.0);
    }
}
