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
        section_info(Range<Row>, Col, HeaderModel),
        reset_sections(),
    }
    Output {
        section_info_needed(Row, Col),
        header_position_changed(Row, Col, Vector2),
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

impl<HeaderEntry> VisibleHeader<HeaderEntry> {
    fn header_position(&self, col: Col, entry_size: Vector2, viewport: Viewport) -> Vector2 {
        let next_section_y = entry::visible::position_y(self.section_rows.end, entry_size);
        let min_y = next_section_y + entry_size.y;
        let y = (viewport.top - entry_size.y / 2.0).max(min_y);
        Vector2(entry::visible::position_x(col, entry_size), y)
    }
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

    fn header_position(
        &self,
        row: Row,
        col: Col,
        entry_size: Vector2,
        viewport: Viewport,
    ) -> Option<Vector2> {
        let visible_headers = self.visible_headers.borrow();
        let header = visible_headers.get(&col).filter(|h| h.section_rows.start == row);
        header.map(|h| h.header_position(col, entry_size, viewport))
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
        let uncovered = cols_range.filter_map(|col| {
            (!visible_headers.contains_key(&col)).as_some((highest_visible_row, col))
        });
        uncovered.collect()
    }

    fn update_headers_positions(&self, properties: Properties) -> Vec<(Row, Col, Vector2)> {
        let Properties { row_count, col_count, viewport, entries_size } = properties;
        let visible_headers = self.visible_headers.borrow();
        visible_headers
            .iter()
            .filter_map(|(col, header)| {
                let new_position = header.header_position(*col, entries_size, viewport);
                (header.entry.position().xy() != new_position).as_some_from(|| {
                    header.entry.set_position_xy(new_position);
                    (header.section_rows.start, *col, new_position)
                })
            })
            .collect()
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

impl<HeaderEntry: Entry> Model<HeaderEntry, HeaderEntry::Params> {
    fn update_section(
        &self,
        rows: Range<Row>,
        col: Col,
        model: HeaderEntry::Model,
        entry_size: Vector2,
        viewport: Viewport,
    ) -> (Row, Col, Vector2) {
        use std::collections::hash_map::Entry::*;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let create_new_entry = || {
            let entry = self.entry_creation_ctx.create_entry(self.text_layer.as_ref());
            self.layer.add_exclusive(&entry);
            entry
        };
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
        let position = entry.header_position(col, entry_size, viewport);
        entry.entry.set_position_xy(position);
        (entry.section_rows.start, col, position)
    }
}



// ===============
// === Handler ===
// ===============

#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
pub struct HandlerTemplate<HeaderEntry, HeaderModel: frp::node::Data, HeaderParams> {
    #[deref]
    frp:   Frp<HeaderModel>,
    model: Rc<RefCell<Option<Rc<Model<HeaderEntry, HeaderParams>>>>>,
}

pub type Handler<HeaderEntry> =
    HandlerTemplate<HeaderEntry, <HeaderEntry as Entry>::Model, <HeaderEntry as Entry>::Params>;

impl<HeaderEntry, HeaderModel, HeaderParams> HandlerTemplate<HeaderEntry, HeaderModel, HeaderParams>
where
    HeaderModel: frp::node::Data,
    HeaderParams: frp::node::Data,
{
    pub fn new() -> Self {
        default()
    }
}

impl<HeaderEntry: Entry> HandlerTemplate<HeaderEntry, HeaderEntry::Model, HeaderEntry::Params> {
    pub fn setup_with_grid<E>(
        &self,
        grid: &crate::GridView<E>,
        layer: Layer,
        text_layer: Option<Layer>,
    ) where
        E: Entry<Params = HeaderEntry::Params>,
    {
        let frp = &self.frp;
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
            request_sections_after_sections_reset <=
                frp.reset_sections.map2(&grid_frp.properties, f!((_, props) model.reset_entries(*props)));
            request_section_after_text_layer_change <=
                grid_frp.set_text_layer.map2(&grid_frp.properties, f!((_, props) model.reset_entries(*props)));
            out.section_info_needed <+ request_sections_after_viewport_change;
            out.section_info_needed <+ request_sections_after_entry_size_change;
            out.section_info_needed <+ request_sections_after_reset;
            out.section_info_needed <+ request_section_after_text_layer_change;

            position_update <= grid_frp.viewport.map2(&grid_frp.properties, f!((_, props) model.update_headers_positions(*props)));
            out.header_position_changed <+ position_update;
            out.header_position_changed <+ input.section_info.map2(&grid_frp.properties,
                f!(((rows, col, m), props) model.update_section(rows.clone(), *col, m.clone(), props.entries_size, props.viewport))
            );
        }

        *self.model.borrow_mut() = Some(model);
    }
}

impl<HeaderEntry, HeaderModel, HeaderParams> HandlerTemplate<HeaderEntry, HeaderModel, HeaderParams>
where
    HeaderModel: frp::node::Data,
    HeaderParams: frp::node::Data,
{
    pub fn get_entry(&self, row: Row, col: Col) -> Option<HeaderEntry>
    where HeaderEntry: CloneRef {
        self.model.borrow().as_ref().and_then(|model| {
            let headers = model.visible_headers.borrow();
            let header = headers.get(&col).filter(|h| h.section_rows.start == row);
            header.map(|e| e.entry.entry.clone_ref())
        })
    }

    pub fn header_position(
        &self,
        row: Row,
        col: Col,
        entry_size: Vector2,
        viewport: Viewport,
    ) -> Option<Vector2> {
        self.model.borrow().as_ref().and_then(|m| m.header_position(row, col, entry_size, viewport))
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
    use enso_frp::web::binding::wasm::console::assert;
    use ensogl_core::application::Application;

    fn setup_section_handler<const COL_COUNT: Col>(
        app: &Application,
        network: &frp::Network,
        row_count: Row,
        grid: &crate::GridView<TestEntry>,
        headers: [impl IntoIterator<Item = (Row, <TestEntry as Entry>::Model)>; COL_COUNT],
    ) -> Handler<TestEntry> {
        let headers: [BTreeMap<_, _>; COL_COUNT] = headers.map(|i| i.into_iter().collect());
        let headers_layer = app.display.default_scene.layers.main.create_sublayer();
        let handler = Handler::<TestEntry>::new();
        handler.setup_with_grid(&grid, headers_layer, None);
        frp::extend! { network
            grid.model_for_entry <+ grid.model_for_entry_needed.map(|&(row, col)| (row, col, Immutable(row + col)));
            handler.section_info <+ handler.section_info_needed.map(move |&(row, col)| {
                let (&section_start, model) = headers[col].range(..=row).last().unwrap();
                let section_end = headers[col].range((row+1)..).next().map_or(row_count, |(row, _)| *row);
                (section_start..section_end, col, model.clone())
            });
        }
        grid.set_entries_size(Vector2(10.0, 10.0));
        handler
    }

    fn check_headers_positions<const COL_COUNT: Col>(
        handler: &Handler<TestEntry>,
        expected: [Vector2; COL_COUNT],
    ) {
        let visible_headers = get_sorted_headers(handler);
        assert_eq!(visible_headers.len(), COL_COUNT);
        for ((_, header), expected_pos) in visible_headers.into_iter().zip(expected.into_iter()) {
            assert_eq!(header.entry.position().xy(), expected_pos);
        }
    }

    fn check_headers_models<const COL_COUNT: Col>(
        handler: &Handler<TestEntry>,
        expected: [usize; COL_COUNT],
    ) {
        let visible_headers = get_sorted_headers(handler);
        assert_eq!(visible_headers.len(), COL_COUNT);
        for ((_, header), expected_model) in visible_headers.into_iter().zip(expected.into_iter()) {
            assert_eq!(header.entry.entry.model_set.get(), expected_model);
        }
    }

    fn check_headers_sections<const COL_COUNT: Col>(
        handler: &Handler<TestEntry>,
        expected: [(Range<Row>, Col); COL_COUNT],
    ) {
        let visible_headers = get_sorted_headers(handler);
        assert_eq!(visible_headers.len(), COL_COUNT);
        for ((col, header), (expected_rows, expected_col)) in
            visible_headers.into_iter().zip(expected.into_iter())
        {
            assert_eq!(col, expected_col);
            assert_eq!(header.section_rows, expected_rows);
        }
    }

    fn get_sorted_headers(handler: &Handler<TestEntry>) -> Vec<(Col, VisibleHeader<TestEntry>)> {
        let model = handler.model.borrow();
        let visible_headers = model.as_ref().unwrap().visible_headers.borrow();
        visible_headers
            .iter()
            .map(|(col, header)| (*col, header.clone()))
            .sorted_by_key(|(col, _)| *col)
            .collect()
    }

    #[test]
    fn initializing_headers_and_pushing_them_down() {
        let network = frp::Network::new("tests::initializing_headers_in_column");
        let app = Application::new("root");
        let grid = crate::GridView::<TestEntry>::new(&app);
        let headers_info = [[(0, Immutable(11))], [(1, Immutable(12))], [(0, Immutable(13))]];
        let headers = setup_section_handler(&app, &network, 4, &grid, headers_info);
        grid.set_viewport(Viewport { left: 5.0, right: 26.0, top: -12.0, bottom: -25.0 });
        grid.reset_entries(4, 3);

        check_headers_sections(&headers, [(0..4, 0), (1..4, 1), (0..4, 2)]);
        check_headers_models(&headers, [11, 12, 13]);
        check_headers_positions(&headers, [
            Vector2(5.0, -17.0),
            Vector2(15.0, -17.0),
            Vector2(25.0, -17.0),
        ]);

        grid.set_viewport(Viewport { left: 5.0, right: 28.0, top: -17.0, bottom: -30.0 });
        check_headers_sections(&headers, [(0..4, 0), (1..4, 1), (0..4, 2)]);
        check_headers_models(&headers, [11, 12, 13]);
        check_headers_positions(&headers, [
            Vector2(5.0, -22.0),
            Vector2(15.0, -22.0),
            Vector2(25.0, -22.0),
        ]);
    }

    #[test]
    fn pushing_headers_down() {
        let app = Application::new("root");
        let grid = crate::GridView::<TestEntry>::new(&app);
        let network = frp::Network::new("tests::initializing_headers_in_column");
        let sections = [
            vec![(0, Immutable(1)), (1, Immutable(4)), (4, Immutable(6))],
            vec![(0, Immutable(2)), (2, Immutable(5)), (4, Immutable(7))],
            vec![(0, Immutable(3)), (3, Immutable(8))],
        ];
        let handler = setup_section_handler(&app, &network, 8, &grid, sections);
        grid.set_viewport(Viewport { left: 0.0, right: 10.0, top: -0.0, bottom: -20.0 });
        grid.reset_entries(8, 3);

        check_headers_positions(&handler, [Vector2(5.0, -5.0)]);
        check_headers_models(&handler, [1]);
        check_headers_sections(&handler, [(0..1, 0)]);

        grid.set_viewport(Viewport { left: 5.0, right: 15.0, top: -5.0, bottom: -25.0 });
        check_headers_positions(&handler, [Vector2(5.0, -5.0), Vector2(15.0, -10.0)]);
        check_headers_models(&handler, [1, 2]);
        check_headers_sections(&handler, [(0..1, 0), (0..2, 1)]);

        grid.set_viewport(Viewport { left: 5.0, right: 15.0, top: -10.0, bottom: -30.0 });
        check_headers_positions(&handler, [Vector2(5.0, -15.0), Vector2(15.0, -15.0)]);
        check_headers_models(&handler, [4, 2]);
        check_headers_sections(&handler, [(1..4, 0), (0..2, 1)]);

        grid.set_viewport(Viewport { left: 15.0, right: 25.0, top: -10.0, bottom: -30.0 });
        check_headers_positions(&handler, [Vector2(15.0, -15.0), Vector2(25.0, -15.0)]);
        check_headers_models(&handler, [2, 3]);
        check_headers_sections(&handler, [(0..2, 1), (0..3, 2)]);

        grid.set_viewport(Viewport { left: 5.0, right: 15.0, top: -35.0, bottom: -45.0 });
        check_headers_positions(&handler, [Vector2(5.0, -35.0), Vector2(15.0, -35.0)]);
        check_headers_models(&handler, [4, 5]);
        check_headers_sections(&handler, [(1..4, 0), (2..4, 1)]);
    }
}
