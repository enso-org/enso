//! A module with content related to entries visible in GridView.

use crate::prelude::*;

use crate::entry;
use crate::Col;
use crate::ColumnWidths;
use crate::Entry;
use crate::Row;

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;



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
}

impl<EntryParams> CreationCtx<EntryParams>
where EntryParams: frp::node::Data
{
    /// Create new entry instance.
    ///
    /// The new instance will have all its FRP endpoints connected to appropriate endpoints of
    /// `self` and overlay mouse events.
    pub fn create_entry<E: Entry<Params = EntryParams>>(
        &self,
        text_layer: Option<&Layer>,
    ) -> VisibleEntry<E> {
        let entry = E::new(&self.app, text_layer);
        let overlay = entry::overlay::View::new(Logger::new("EntryOverlay"));
        entry.add_child(&overlay);
        if let Some(network) = self.network.upgrade_or_warn() {
            let entry_frp = entry.frp();
            let entry_network = entry_frp.network();
            let mouse = &self.app.display.default_scene.mouse.frp;
            frp::new_bridge_network! { [network, entry_network] grid_view_entry_bridge
                init <- source_();
                entry_frp.set_size <+ all(init, self.set_entry_size)._1();
                entry_frp.set_params <+ all(init, self.set_entry_params)._1();
                contour <- all(init, entry_frp.contour)._1();
                eval contour ((c) overlay.set_contour(*c));

                let events = &overlay.events;
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
            }
            init.emit(());
        }
        VisibleEntry { entry, overlay }
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
pub fn set_position<E: display::Object>(
    entry: &E,
    row: Row,
    col: Col,
    entry_size: Vector2,
    column_widths: &ColumnWidths,
) {
    entry.set_position_xy(position(row, col, entry_size, column_widths));
}
