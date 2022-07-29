use crate::prelude::*;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_scroll_area::Viewport;

use crate::entry;
use crate::entry::visible::VisibleEntry;
use crate::visible_area;
use crate::Col;
use crate::Entry;
use crate::Properties;
use crate::Row;



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! { <HeaderModel: (frp::node::Data)>
    Input {
        section_info(Range<Row>, Col, HeaderModel)
    }
    Output {
        section_info_needed(Row, Col)
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
pub struct VisibleHeader<HeaderEntry> {
    section_rows: Range<Row>,
    entry:        VisibleEntry<HeaderEntry>,
}

#[derive(Clone, Debug)]
pub struct Model<HeaderEntry, HeaderParams> {
    grid:               display::object::Instance,
    visible_headers:    RefCell<HashMap<Col, VisibleHeader<HeaderEntry>>>,
    free_headers:       RefCell<Vec<VisibleEntry<HeaderEntry>>>,
    entry_creation_ctx: entry::visible::CreationCtx<HeaderParams>,
    layer:              Layer,
    text_layer:         Option<Layer>,
}

impl<HeaderEntry, HeaderParams> Model<HeaderEntry, HeaderParams> {
    fn new(
        grid: &impl display::Object,
        entry_creation_ctx: entry::visible::CreationCtx<HeaderParams>,
        layer: Layer,
        text_layer: Option<Layer>,
    ) -> Self {
        let grid = grid.display_object().clone_ref();
        let visible_headers = default();
        let free_headers = default();
        Self { grid, visible_headers, free_headers, entry_creation_ctx, layer, text_layer }
    }
}

impl<HeaderEntry: display::Object, HeaderParams> Model<HeaderEntry, HeaderParams> {
    fn update_visible_headers(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { row_count, col_count, viewport, entries_size } = properties;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let cols_range = visible_area::visible_columns(viewport, entries_size, col_count);
        let highest_visible_row =
            visible_area::visible_rows(viewport, entries_size, row_count).start;
        let freed = visible_headers.drain_filter(|col, header| {
            !cols_range.contains(col) || !header.section_rows.contains(&highest_visible_row)
        });
        let detached = freed.map(|(_, header)| {
            header.entry.entry.unset_parent();
            header.entry
        });
        free_headers.extend(detached);
        for (col, header) in &*visible_headers {
            header.entry.set_position_xy(header_position(*col, entries_size, viewport));
        }
        let uncovered = cols_range.filter_map(|col| {
            (!visible_headers.contains_key(&col)).as_some((highest_visible_row, col))
        });
        uncovered.collect()
    }

    fn reset_entries(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { row_count, col_count, viewport, entries_size } = properties;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let highest_visible_row =
            visible_area::visible_rows(viewport, entries_size, row_count).start;
        let detached = visible_headers.drain().map(|(_, header)| {
            header.entry.entry.unset_parent();
            header.entry
        });
        free_headers.extend(detached);
        let visible_columns = visible_area::visible_columns(viewport, entries_size, col_count);
        visible_columns.map(|col| (highest_visible_row, col)).collect()
    }

    fn drop_all_entries(&self, properties: Properties) -> Vec<(Row, Col)> {
        let to_section_info_request = self.reset_entries(properties);
        self.free_headers.borrow_mut().clear();
        to_section_info_request
    }
}

fn header_position(col: Col, entry_size: Vector2, viewport: Viewport) -> Vector2 {
    let x = viewport.top - entry_size.y / 2.0;
    Vector2(x, entry::visible::position_y(col, entry_size))
}

impl<HeaderEntry: Entry> Model<HeaderEntry, HeaderEntry::Params> {
    fn update_section(
        &self,
        rows: Range<Row>,
        col: Col,
        model: HeaderEntry::Model,
        entry_size: Vector2,
        viewport: Viewport,
    ) {
        use std::collections::hash_map::Entry::*;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let create_new_entry = || self.entry_creation_ctx.create_entry(self.text_layer.as_ref());
        let entry = match visible_headers.entry(col) {
            Occupied(mut entry) => {
                entry.get_mut().section_rows = rows;
                entry.into_mut()
            }
            Vacant(lack_of_entry) => {
                let new_entry = free_headers.pop().unwrap_or_else(create_new_entry);
                self.grid.add_child(&new_entry);
                let new_header_entry =
                    VisibleHeader { section_rows: rows, entry: new_entry };
                lack_of_entry.insert(new_header_entry)
            }
        };
        let entry_frp = entry.entry.entry.frp();
        entry_frp.set_model(model);
        entry_frp.set_location((entry.section_rows.start, col));
        entry.entry.set_position_xy(header_position(col, entry_size, viewport));
    }
}



// ===============
// === Handler ===
// ===============

#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct HandlerTemplate<HeaderEntry, HeaderModel: frp::node::Data, HeaderParams: frp::node::Data>
{
    #[deref]
    frp:   Frp<HeaderModel>,
    model: Rc<Model<HeaderEntry, HeaderParams>>,
}

pub type Handler<HeaderEntry> =
    HandlerTemplate<HeaderEntry, <HeaderEntry as Entry>::Model, <HeaderEntry as Entry>::Params>;

impl<HeaderEntry: Entry> HandlerTemplate<HeaderEntry, HeaderEntry::Model, HeaderEntry::Params> {
    fn new<E>(grid: &crate::GridView<E>, layer: Layer, text_layer: Option<Layer>) -> Self
    where E: Entry<Params = HeaderEntry::Params> {
        let frp = Frp::new();
        let entry_creation_ctx = grid.model().entry_creation_ctx.clone_ref();
        let model = Rc::new(Model::new(grid, entry_creation_ctx, layer, text_layer));
        let network = frp.network();
        let input = &frp.private.input;
        let out = &frp.private.output;
        let grid_frp = grid.frp();
        frp::extend! { network
            request_sections_after_viewport_change <=
                grid_frp.viewport.map2(&grid_frp.properties, f!((_, props) model.update_visible_headers(*props)));
            request_sections_after_entry_size_change <=
                grid_frp.entries_size.map2(&grid_frp.properties, f!((_, props) model.update_visible_headers(*props)));
            request_sections_after_reset <=
                grid_frp.reset_entries.map2(&grid_frp.properties, f!((_, props) model.reset_entries(*props)));
            request_section_after_text_layer_change <=
                grid_frp.set_text_layer.map2(&grid_frp.properties, f!((_, props) model.reset_entries(*props)));
            out.section_info_needed <+ request_sections_after_viewport_change;
            out.section_info_needed <+ request_sections_after_entry_size_change;
            out.section_info_needed <+ request_sections_after_reset;
            out.section_info_needed <+ request_section_after_text_layer_change;

            section_and_props <- all(input.section_info, grid_frp.properties);
            eval section_and_props ((((rows, col, m), props): &((Range<Row>, Col, HeaderEntry::Model), Properties)) model.update_section(rows.clone(), *col, m.clone(), props.entries_size, props.viewport));
        }

        Self { frp, model }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestEntry;
    use crate::tests::TestEntryParams;
    use ensogl_core::application::Application;

    #[test]
    fn initializing_headers_in_column() {
        let app = Application::new("root");
        let headers_layer = app.display.default_scene.layers.main.create_sublayer();
        let grid = crate::GridView::<TestEntry>::new(&app);
        let headers = Handler::<TestEntry>::new(&grid, headers_layer, None);
        let network = frp::Network::new("tests::initializing_headers_in_column");
        frp::extend! { network
            grid.model_for_entry <+ grid.model_for_entry_needed.map(|&(row, col)| (row, col, Immutable(row + col)));
            headers.section_info <+ headers.section_info_needed.map(|&(row, col)| match col {
                0 | 2 => (0..4, col, Immutable(11 + col)),
                1 if row > 0 => (1..4, col, Immutable(12)),
                _ => (row..(row + 1), col, Immutable(999)),
            });
        }
        let params = TestEntryParams { param: Immutable(4) };
        grid.set_entries_size(Vector2(10.0, 10.0));
        grid.set_entries_params(params);
        grid.set_viewport(Viewport { left: 5.0, right: 26.0, top: -12.0, bottom: -25.0 });
        grid.reset_entries(4, 3);
    }
}
