//! The module containing [`GridView`] with headers.

use crate::prelude::*;

use crate::entry;
use crate::entry::visible::VisibleEntry;
use crate::visible_area;
use crate::Col;
use crate::Entry;
use crate::Properties;
use crate::Row;

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::display::scene::Layer;
use ensogl_scroll_area::Viewport;



// ===========
// === FRP ===
// ===========

/// A structure with layers where the headers are displayed.
///
/// The references are weak so the structure may be safely propagated through FRP networks.
#[derive(Clone, Debug)]
pub struct WeakLayers {
    pub header: WeakLayer,
    pub text:   Option<WeakLayer>,
}

impl WeakLayers {
    /// Constructor.
    pub fn new(header: &Layer, text: Option<&Layer>) -> Self {
        Self { header: header.downgrade(), text: text.map(|l| l.downgrade()) }
    }

    fn upgrade_header(this: &Option<Self>) -> Option<Layer> {
        this.as_ref()?.header.upgrade()
    }

    fn upgrade_text(this: &Option<Self>) -> Option<Layer> {
        this.as_ref()?.text.as_ref()?.upgrade()
    }
}

ensogl_core::define_endpoints_2! { <HeaderModel: (frp::node::Data)>
    Input {
        set_layers(Option<WeakLayers>),
        section_info(Range<Row>, Col, HeaderModel),
        reset_sections(),
    }
    Output {
        section_info_needed(Row, Col),
        header_shown(Row, Col),
        header_position_changed(Row, Col, Vector2),
        header_hidden(Row, Col),
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

impl<HeaderEntry: Entry> VisibleHeader<HeaderEntry> {
    fn header_position(&self, col: Col, entry_size: Vector2, viewport: Viewport) -> Vector2 {
        let contour = self.entry.entry.frp().contour.value();
        let max_y = entry::visible::position_y(self.section_rows.start, entry_size);
        let next_section_y = entry::visible::position_y(self.section_rows.end, entry_size);
        let min_y = next_section_y + entry_size.y / 2.0 + contour.size.y / 2.0;
        let y = (viewport.top - contour.size.y / 2.0).min(max_y).max(min_y);
        Vector2(entry::visible::position_x(col, entry_size), y)
    }
}

/// A structure containing data of [`GridView`] with headers.
#[derive(Clone, Debug)]
pub struct Model<InnerGrid, HeaderEntry, HeaderParams> {
    grid:               InnerGrid,
    visible_headers:    RefCell<HashMap<Col, VisibleHeader<HeaderEntry>>>,
    free_headers:       RefCell<Vec<VisibleEntry<HeaderEntry>>>,
    entry_creation_ctx: entry::visible::CreationCtx<HeaderParams>,
}

impl<InnerGrid, HeaderEntry, HeaderParams> Model<InnerGrid, HeaderEntry, HeaderParams> {
    fn new(grid: InnerGrid, entry_creation_ctx: entry::visible::CreationCtx<HeaderParams>) -> Self {
        let visible_headers = default();
        let free_headers = default();
        Self { grid, visible_headers, free_headers, entry_creation_ctx }
    }
}

impl<InnerGrid, HeaderEntry: display::Object, HeaderParams>
    Model<InnerGrid, HeaderEntry, HeaderParams>
{
    fn hide_no_longer_visible_headers(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { row_count, col_count, viewport, entries_size } = properties;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let cols_range = visible_area::visible_columns(viewport, entries_size, col_count);
        let highest_visible_row =
            visible_area::visible_rows(viewport, entries_size, row_count).start;
        let freed = visible_headers.drain_filter(|col, header| {
            !cols_range.contains(col) || !header.section_rows.contains(&highest_visible_row)
        });
        let detached = freed.map(|(col, header)| {
            header.entry.entry.unset_parent();
            ((header.section_rows.start, col), header.entry)
        });
        let (locations, entries): (Vec<_>, Vec<_>) = detached.unzip();
        free_headers.extend(entries);
        locations
    }

    fn needed_info_for_uncovered_sections(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { row_count, col_count, viewport, entries_size } = properties;
        let visible_headers = self.visible_headers.borrow();
        let cols_range = visible_area::visible_columns(viewport, entries_size, col_count);
        let highest_visible_row =
            visible_area::visible_rows(viewport, entries_size, row_count).start;
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

impl<InnerGrid, HeaderEntry: Entry> Model<InnerGrid, HeaderEntry, HeaderEntry::Params> {
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

    fn header_separator(&self, col: Col, entry_size: Vector2, viewport: Viewport) -> f32 {
        let visible_headers = self.visible_headers.borrow();
        let header = visible_headers.get(&col);
        header
            .map(|h| {
                h.header_position(col, entry_size, viewport).y
                    - h.entry.entry.frp().contour.value().size.y / 2.0
            })
            .unwrap_or(viewport.top)
    }

    fn update_headers_positions(&self, properties: Properties) -> Vec<(Row, Col, Vector2)> {
        let Properties { viewport, entries_size, .. } = properties;
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

    fn update_section(
        &self,
        rows: Range<Row>,
        col: Col,
        model: HeaderEntry::Model,
        entry_size: Vector2,
        viewport: Viewport,
        layers: &Option<WeakLayers>,
    ) -> (Row, Col, Vector2)
    where
        InnerGrid: display::Object,
    {
        use std::collections::hash_map::Entry::*;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let create_new_entry = || {
            let text_layer = WeakLayers::upgrade_text(layers);
            let entry = self.entry_creation_ctx.create_entry(text_layer.as_ref());
            if let Some(layer) = WeakLayers::upgrade_header(layers) {
                layer.add_exclusive(&entry);
            }
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



// ================
// === GridView ===
// ================


#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct GridViewTemplate<
    Entry,
    InnerGridView,
    HeaderEntry,
    HeaderModel: frp::node::Data,
    HeaderParams,
> {
    frp:        Frp<HeaderModel>,
    model:      Rc<Model<InnerGridView, HeaderEntry, HeaderParams>>,
    entry_type: PhantomData<Entry>,
}

impl<Entry, InnerGridView, HeaderEntry, HeaderModel: frp::node::Data, HeaderParams> Deref
    for GridViewTemplate<Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams>
{
    type Target = InnerGridView;

    fn deref(&self) -> &InnerGridView {
        &self.model.grid
    }
}

pub type GridView<Entry, HeaderEntry> = GridViewTemplate<
    Entry,
    crate::GridView<Entry>,
    HeaderEntry,
    <HeaderEntry as crate::Entry>::Model,
    <HeaderEntry as crate::Entry>::Params,
>;

impl<E: Entry, HeaderEntry: Entry<Params = E::Params>> GridView<E, HeaderEntry> {
    pub fn new(app: &Application) -> Self {
        let grid = crate::GridView::<E>::new(app);
        Self::new_wrapping(grid)
    }
}

impl<E, InnerGridView, HeaderEntry>
    GridViewTemplate<E, InnerGridView, HeaderEntry, HeaderEntry::Model, HeaderEntry::Params>
where
    E: Entry<Params = HeaderEntry::Params>,
    InnerGridView: AsRef<crate::GridView<E>> + display::Object + 'static,
    HeaderEntry: Entry,
{
    pub fn new_wrapping(grid: InnerGridView) -> Self {
        let frp = Frp::new();
        let entry_creation_ctx = grid.as_ref().model().entry_creation_ctx.clone_ref();
        let model = Rc::new(Model::new(grid, entry_creation_ctx));
        let grid_frp = model.grid.as_ref().frp();
        let network = frp.network();
        let input = &frp.private.input;
        let out = &frp.private.output;
        frp::extend! { network
            headers_hidden_after_viewport_change <=
                grid_frp.viewport.map2(&grid_frp.properties, f!((_, props) model.hide_no_longer_visible_headers(*props)));
            headers_hidden_after_entry_size_change <=
                grid_frp.entries_size.map2(&grid_frp.properties, f!((_, props) model.hide_no_longer_visible_headers(*props)));
            out.header_hidden <+ headers_hidden_after_viewport_change;
            out.header_hidden <+ headers_hidden_after_entry_size_change;

            request_sections_after_viewport_change <=
                grid_frp.viewport.map2(&grid_frp.properties, f!((_, props) model.needed_info_for_uncovered_sections(*props)));
            request_sections_after_entry_size_change <=
                grid_frp.entries_size.map2(&grid_frp.properties, f!((_, props) model.needed_info_for_uncovered_sections(*props)));
            request_sections_after_reset <=
                grid_frp.reset_entries.map2(&grid_frp.properties, f!((_, props) model.reset_entries(*props)));
            request_sections_after_sections_reset <=
                frp.reset_sections.map2(&grid_frp.properties, f!((_, props) model.reset_entries(*props)));
            request_sections_after_layer_change <=
                frp.set_layers.map2(&grid_frp.properties, f!((_, props) model.drop_all_entries(*props)));
            out.section_info_needed <+ request_sections_after_viewport_change;
            out.section_info_needed <+ request_sections_after_entry_size_change;
            out.section_info_needed <+ request_sections_after_reset;
            out.section_info_needed <+ request_sections_after_sections_reset;
            out.section_info_needed <+ request_sections_after_layer_change;

            position_update <= grid_frp.viewport.map2(&grid_frp.properties, f!((_, props) model.update_headers_positions(*props)));
            out.header_position_changed <+ position_update;
            section_update <- input.section_info.map3(&grid_frp.properties, &frp.set_layers,
                f!(((rows, col, m): &(Range<usize>, usize, HeaderEntry::Model), props, layers) model.update_section(rows.clone(), *col, m.clone(), props.entries_size, props.viewport, layers))
            );
            out.header_shown <+ section_update.map(|&(row, col, _)| (row, col));
            out.header_position_changed <+ section_update;
        }
        let entry_type = PhantomData;
        Self { frp, model, entry_type }
    }

    pub fn header_position(&self, row: Row, col: Col) -> Option<Vector2> {
        let entry_size = self.model.grid.as_ref().entries_size.value();
        let viewport = self.model.grid.as_ref().viewport.value();
        self.model.header_position(row, col, entry_size, viewport)
    }

    pub fn header_separator(&self, col: Col) -> f32 {
        let entry_size = self.model.grid.as_ref().entries_size.value();
        let viewport = self.model.grid.as_ref().viewport.value();
        self.model.header_separator(col, entry_size, viewport)
    }

    pub fn header_or_entry_position(&self, row: Row, column: Col) -> Vector2 {
        let header_pos = self.header_position(row, column);
        header_pos.unwrap_or_else(|| self.model.grid.as_ref().entry_position(row, column))
    }
}

impl<Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams>
    GridViewTemplate<Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams>
where
    HeaderModel: frp::node::Data,
    HeaderParams: frp::node::Data,
{
    pub fn get_header(&self, row: Row, col: Col) -> Option<HeaderEntry>
    where HeaderEntry: CloneRef {
        let headers = self.model.visible_headers.borrow();
        let header = headers.get(&col).filter(|h| h.section_rows.start == row);
        header.map(|e| e.entry.entry.clone_ref())
    }

    pub fn header_frp(&self) -> &Frp<HeaderModel> {
        &self.frp
    }
}

impl<E: Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams> AsRef<crate::GridView<E>>
    for GridViewTemplate<E, InnerGridView, HeaderEntry, HeaderModel, HeaderParams>
where
    InnerGridView: AsRef<crate::GridView<E>>,
    HeaderModel: frp::node::Data,
    HeaderParams: frp::node::Data,
{
    fn as_ref(&self) -> &crate::GridView<E> {
        self.model.grid.as_ref()
    }
}

impl<Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams> display::Object
    for GridViewTemplate<Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams>
where
    InnerGridView: display::Object,
    HeaderModel: frp::node::Data,
    HeaderParams: frp::node::Data,
{
    fn display_object(&self) -> &display::object::Instance {
        self.model.grid.display_object()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entry::Contour;
    use crate::tests::TestEntry as ParentEntry;
    use crate::EntryFrp;
    use ensogl_core::application::frp::API;
    use ensogl_core::application::Application;

    #[derive(Clone, CloneRef, Debug)]
    struct TestEntry {
        parent: ParentEntry,
    }

    impl Entry for TestEntry {
        type Model = <ParentEntry as Entry>::Model;
        type Params = <ParentEntry as Entry>::Params;

        fn new(app: &Application, text_layer: Option<&Layer>) -> Self {
            let parent = <ParentEntry as Entry>::new(app, text_layer);
            let frp = &parent.frp;
            frp.private()
                .output
                .contour
                .emit(Contour { size: Vector2(9.0, 9.0), corners_radius: 0.0 });
            TestEntry { parent }
        }

        fn frp(&self) -> &EntryFrp<Self> {
            self.parent.frp()
        }
    }

    impl display::Object for TestEntry {
        fn display_object(&self) -> &display::object::Instance {
            self.parent.display_object()
        }
    }

    fn setup_grid_view<const COL_COUNT: Col>(
        app: &Application,
        network: &frp::Network,
        row_count: Row,
        headers: [impl IntoIterator<Item = (Row, <TestEntry as Entry>::Model)>; COL_COUNT],
    ) -> GridView<TestEntry, TestEntry> {
        let headers: [BTreeMap<_, _>; COL_COUNT] = headers.map(|i| i.into_iter().collect());
        let headers_layer = app.display.default_scene.layers.main.create_sublayer();
        let grid_view = GridView::<TestEntry, TestEntry>::new(app);
        let header_frp = grid_view.header_frp();
        header_frp.set_layers(WeakLayers::new(&headers_layer, None));
        frp::extend! { network
            grid_view.model_for_entry <+ grid_view.model_for_entry_needed.map(|&(row, col)| (row, col, Immutable(row + col)));
            header_frp.section_info <+ header_frp.section_info_needed.map(move |&(row, col)| {
                let (&section_start, model) = headers[col].range(..=row).last().unwrap();
                let section_end = headers[col].range((row+1)..).next().map_or(row_count, |(row, _)| *row);
                (section_start..section_end, col, model.clone())
            });
        }
        grid_view.set_entries_size(Vector2(10.0, 10.0));
        grid_view
    }

    fn check_headers_positions<const COL_COUNT: Col>(
        grid_view: &GridView<TestEntry, TestEntry>,
        expected: [Vector2; COL_COUNT],
    ) {
        let visible_headers = get_sorted_headers(grid_view);
        assert_eq!(visible_headers.len(), COL_COUNT);
        for ((_, header), expected_pos) in visible_headers.into_iter().zip(expected.into_iter()) {
            assert_eq!(header.entry.position().xy(), expected_pos);
        }
    }

    fn check_headers_models<const COL_COUNT: Col>(
        grid_view: &GridView<TestEntry, TestEntry>,
        expected: [usize; COL_COUNT],
    ) {
        let visible_headers = get_sorted_headers(grid_view);
        assert_eq!(visible_headers.len(), COL_COUNT);
        for ((_, header), expected_model) in visible_headers.into_iter().zip(expected.into_iter()) {
            assert_eq!(header.entry.entry.parent.model_set.get(), expected_model);
        }
    }

    fn check_headers_sections<const COL_COUNT: Col>(
        grid_view: &GridView<TestEntry, TestEntry>,
        expected: [(Range<Row>, Col); COL_COUNT],
    ) {
        let visible_headers = get_sorted_headers(grid_view);
        assert_eq!(visible_headers.len(), COL_COUNT);
        for ((col, header), (expected_rows, expected_col)) in
            visible_headers.into_iter().zip(expected.into_iter())
        {
            assert_eq!(col, expected_col);
            assert_eq!(header.section_rows, expected_rows);
        }
    }

    fn get_sorted_headers(
        grid_view: &GridView<TestEntry, TestEntry>,
    ) -> Vec<(Col, VisibleHeader<TestEntry>)> {
        let visible_headers = grid_view.model.visible_headers.borrow();
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
        let headers_info = [[(0, Immutable(11))], [(1, Immutable(12))], [(0, Immutable(13))]];
        let grid = setup_grid_view(&app, &network, 4, headers_info);
        grid.set_viewport(Viewport { left: 5.0, right: 26.0, top: -12.0, bottom: -25.0 });
        grid.reset_entries(4, 3);

        check_headers_sections(&grid, [(0..4, 0), (1..4, 1), (0..4, 2)]);
        check_headers_models(&grid, [11, 12, 13]);
        check_headers_positions(&grid, [
            Vector2(5.0, -16.5),
            Vector2(15.0, -16.5),
            Vector2(25.0, -16.5),
        ]);

        grid.set_viewport(Viewport { left: 5.0, right: 28.0, top: -17.0, bottom: -30.0 });
        check_headers_sections(&grid, [(0..4, 0), (1..4, 1), (0..4, 2)]);
        check_headers_models(&grid, [11, 12, 13]);
        check_headers_positions(&grid, [
            Vector2(5.0, -21.5),
            Vector2(15.0, -21.5),
            Vector2(25.0, -21.5),
        ]);
    }

    #[test]
    fn pushing_headers_down() {
        let app = Application::new("root");
        let network = frp::Network::new("tests::initializing_headers_in_column");
        let sections = [
            vec![(0, Immutable(1)), (1, Immutable(4)), (4, Immutable(6))],
            vec![(0, Immutable(2)), (2, Immutable(5)), (4, Immutable(7))],
            vec![(0, Immutable(3)), (3, Immutable(8))],
        ];
        let grid = setup_grid_view(&app, &network, 8, sections);
        grid.set_viewport(Viewport { left: 0.0, right: 10.0, top: -0.0, bottom: -20.0 });
        grid.reset_entries(8, 3);

        check_headers_positions(&grid, [Vector2(5.0, -5.0)]);
        check_headers_models(&grid, [1]);
        check_headers_sections(&grid, [(0..1, 0)]);

        grid.set_viewport(Viewport { left: 5.0, right: 15.0, top: -5.0, bottom: -25.0 });
        check_headers_positions(&grid, [Vector2(5.0, -5.5), Vector2(15.0, -9.5)]);
        check_headers_models(&grid, [1, 2]);
        check_headers_sections(&grid, [(0..1, 0), (0..2, 1)]);

        grid.set_viewport(Viewport { left: 5.0, right: 15.0, top: -10.0, bottom: -30.0 });
        check_headers_positions(&grid, [Vector2(5.0, -15.0), Vector2(15.0, -14.5)]);
        check_headers_models(&grid, [4, 2]);
        check_headers_sections(&grid, [(1..4, 0), (0..2, 1)]);

        grid.set_viewport(Viewport { left: 15.0, right: 25.0, top: -10.0, bottom: -30.0 });
        check_headers_positions(&grid, [Vector2(15.0, -14.5), Vector2(25.0, -14.5)]);
        check_headers_models(&grid, [2, 3]);
        check_headers_sections(&grid, [(0..2, 1), (0..3, 2)]);

        grid.set_viewport(Viewport { left: 5.0, right: 15.0, top: -35.0, bottom: -45.0 });
        check_headers_positions(&grid, [Vector2(5.0, -35.5), Vector2(15.0, -35.5)]);
        check_headers_models(&grid, [4, 5]);
        check_headers_sections(&grid, [(1..4, 0), (2..4, 1)]);
    }
}
