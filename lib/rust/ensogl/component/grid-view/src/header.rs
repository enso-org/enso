//! The module containing [`GridView`] with headers.

use crate::prelude::*;

use crate::entry;
use crate::entry::visible::VisibleEntry;
use crate::visible_area;
use crate::Col;
use crate::ColumnWidths;
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

/// A structure with layers where the headers are displayed. Designed to be used in FRP networks.
///
/// To avoid unpredictable layer lifetime, all the references are weak.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct WeakLayers {
    pub header: Option<WeakLayer>,
    pub text:   Option<WeakLayer>,
}

impl WeakLayers {
    /// Constructor.
    pub fn new(header: &Layer, text: Option<&Layer>) -> Self {
        Self { header: Some(header.downgrade()), text: text.map(|l| l.downgrade()) }
    }

    /// Upgrade the main layer for headers.
    pub fn upgrade_header(&self) -> Option<Layer> {
        self.header.as_ref()?.upgrade()
    }

    /// Upgrade the text layer for headers.
    pub fn upgrade_text(&self) -> Option<Layer> {
        self.text.as_ref()?.upgrade()
    }
}

ensogl_core::define_endpoints_2! { <HeaderModel: (frp::node::Data)>
    Input {
        set_layers(WeakLayers),
        /// The information about section, should be provided when the [`section_info_needed`]
        /// output is emitted.
        ///
        /// The first row in the range is considered a header of the section. The model is an
        /// [`Entry::Model`] for the header instance (displayed in case when the section is scrolled
        /// down).
        section_info(Range<Row>, Col, HeaderModel),
        reset_sections(),
    }
    Output {
        /// Emitted when the information of the section where given location belongs is needed.
        ///
        /// As a response, the `section_info` input should be called for given column, until then
        /// the header won't be displayed.
        section_info_needed(Row, Col),
        header_shown(Row, Col),
        header_position_changed(Row, Col, Vector2),
        header_hidden(Row, Col),
    }
}



// =============
// === Model ===
// =============

/// A pushed-down header visible on the top of the viewport.
///
/// We push down headers of scrolled-down sections to have them always visible. See the
/// [main component documentation](GridView).
#[derive(Clone, Debug)]
pub struct VisibleHeader<HeaderEntry> {
    section_rows: Range<Row>,
    entry:        VisibleEntry<HeaderEntry>,
}

impl<HeaderEntry: Entry> VisibleHeader<HeaderEntry> {
    fn header_position(
        &self,
        col: Col,
        entry_size: Vector2,
        viewport: Viewport,
        column_widths: &ColumnWidths,
    ) -> entry::MovedHeaderPosition {
        let contour = self.entry.entry.frp().contour.value();
        let contour_offset = self.entry.entry.frp().contour_offset.value();
        let max_y = entry::visible::position_y(self.section_rows.start, entry_size);
        let next_section_y = entry::visible::position_y(self.section_rows.end, entry_size);
        let min_y = next_section_y + entry_size.y / 2.0 + contour.size.y / 2.0 - contour_offset.y;
        let x = entry::visible::position_x(col, entry_size, column_widths);
        let y = (viewport.top - contour.size.y / 2.0 - contour_offset.y).min(max_y).max(min_y);
        entry::MovedHeaderPosition { position: Vector2(x, y), y_range: min_y..=max_y }
    }
}

/// A structure containing data of [`GridView`] with headers.
#[derive(Clone, Debug)]
pub struct Model<InnerGrid, HeaderEntry, HeaderParams> {
    grid:               InnerGrid,
    /// The cloned-ref instance of ColumnWidths structure from `grid`.
    column_widths:      ColumnWidths,
    visible_headers:    RefCell<HashMap<Col, VisibleHeader<HeaderEntry>>>,
    free_headers:       RefCell<Vec<VisibleEntry<HeaderEntry>>>,
    entry_creation_ctx: entry::visible::CreationCtx<HeaderParams>,
}

impl<InnerGrid, HeaderEntry, HeaderParams> Model<InnerGrid, HeaderEntry, HeaderParams> {
    fn new<E: Entry>(
        grid: InnerGrid,
        entry_creation_ctx: entry::visible::CreationCtx<HeaderParams>,
    ) -> Self
    where
        InnerGrid: AsRef<crate::GridView<E>>,
    {
        let visible_headers = default();
        let free_headers = default();
        let column_widths = grid.as_ref().model().column_widths.clone_ref();
        Self { grid, column_widths, visible_headers, free_headers, entry_creation_ctx }
    }
}

impl<InnerGrid, HeaderEntry: display::Object, HeaderParams>
    Model<InnerGrid, HeaderEntry, HeaderParams>
{
    fn hide_no_longer_visible_headers(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { row_count, col_count, viewport, entries_size } = properties;
        let widths = &self.column_widths;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let cols_range = visible_area::visible_columns(viewport, entries_size, col_count, widths);
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
        let widths = &self.column_widths;
        let visible_headers = self.visible_headers.borrow();
        let cols_range = visible_area::visible_columns(viewport, entries_size, col_count, widths);
        let highest_visible_row =
            visible_area::visible_rows(viewport, entries_size, row_count).start;
        let uncovered = cols_range.filter_map(|col| {
            (!visible_headers.contains_key(&col)).as_some((highest_visible_row, col))
        });
        uncovered.collect()
    }

    fn reset_entries(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { row_count, col_count, viewport, entries_size } = properties;
        let widths = &self.column_widths;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let highest_visible_row =
            visible_area::visible_rows(viewport, entries_size, row_count).start;
        let detached = visible_headers.drain().map(|(_, header)| {
            header.entry.entry.unset_parent();
            header.entry
        });
        free_headers.extend(detached);
        let visible_columns =
            visible_area::visible_columns(viewport, entries_size, col_count, widths);
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
        let widths = &self.column_widths;
        let header = visible_headers.get(&col).filter(|h| h.section_rows.start == row);
        header.map(|h| h.header_position(col, entry_size, viewport, widths).position)
    }

    /// The y position of line between the header displayed on the top of the viewport and the rest
    /// of entries. If no header is displayed in given column, the top of the viewport is returned.
    fn header_separator(&self, col: Col, entry_size: Vector2, viewport: Viewport) -> f32 {
        let visible_headers = self.visible_headers.borrow();
        let widths = &self.column_widths;
        let header = visible_headers.get(&col);
        let separator_if_header_visible = header.map(|h| {
            let base_pos = h.header_position(col, entry_size, viewport, widths).position;
            let contour = h.entry.entry.frp().contour.value();
            let offset = h.entry.entry.frp().contour_offset.value();
            base_pos.y + offset.y - contour.size.y / 2.0
        });
        separator_if_header_visible.unwrap_or(viewport.top)
    }

    fn update_headers_positions(&self, properties: Properties) -> Vec<(Row, Col, Vector2)> {
        let Properties { viewport, entries_size, .. } = properties;
        let visible_headers = self.visible_headers.borrow();
        let widths = &self.column_widths;
        let updated_positions = visible_headers.iter().filter_map(|(col, header)| {
            let new_position = header.header_position(*col, entries_size, viewport, widths);
            (header.entry.position().xy() != new_position.position).as_some_from(|| {
                header.entry.set_position_xy(new_position.position);
                header.entry.entry.frp().moved_as_header(&new_position);
                (header.section_rows.start, *col, new_position.position)
            })
        });
        updated_positions.collect()
    }

    fn update_header_size(&self, col: Col, properties: Properties) {
        let entries_size = properties.entries_size;
        let width_diff = self.column_widths.width_diff(col);
        let header = self.visible_headers.borrow().get(&col).map(|h| h.entry.clone_ref());
        if let Some(header) = header {
            header.entry.frp().set_size(entries_size + Vector2(width_diff, 0.0))
        }
    }

    fn update_section(
        &self,
        rows: Range<Row>,
        col: Col,
        model: HeaderEntry::Model,
        entry_size: Vector2,
        viewport: Viewport,
        layers: &WeakLayers,
    ) -> (Row, Col, Vector2)
    where
        InnerGrid: display::Object,
    {
        use std::collections::hash_map::Entry::*;
        let mut visible_headers = self.visible_headers.borrow_mut();
        let mut free_headers = self.free_headers.borrow_mut();
        let widths = &self.column_widths;
        let create_new_entry = || {
            let text_layer = layers.upgrade_text();
            let (entry, init) = self.entry_creation_ctx.create_entry(text_layer.as_ref());
            if let Some(layer) = layers.upgrade_header() {
                layer.add_exclusive(&entry);
            }
            (entry, init)
        };
        let (entry, init) = match visible_headers.entry(col) {
            Occupied(mut entry) => {
                entry.get_mut().section_rows = rows;
                (entry.into_mut(), None)
            }
            Vacant(lack_of_entry) => {
                let (new_entry, init) =
                    free_headers.pop().map(|entry| (entry, None)).unwrap_or_else(create_new_entry);
                self.grid.add_child(&new_entry);
                let new_header_entry =
                    VisibleHeader { section_rows: rows, entry: new_entry };
                (lack_of_entry.insert(new_header_entry), init)
            }
        };
        if let Some(init) = init {
            init.emit(());
        }
        let entry_frp = entry.entry.entry.frp();
        entry_frp.set_model(model);
        entry_frp.set_location((entry.section_rows.start, col));
        let width_offset = self.column_widths.width_diff(col);
        entry_frp.set_size(entry_size + Vector2(width_offset, 0.0));
        let position = entry.header_position(col, entry_size, viewport, widths);
        entry.entry.set_position_xy(position.position);
        entry_frp.moved_as_header(&position);
        (entry.section_rows.start, col, position.position)
    }
}



// ================
// === GridView ===
// ================

/// A template for [`GridView`] structure, wrapping any version of Grid View, and where entry
/// parameters and model are separate generic arguments.
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

/// Grid View with Headers.
///
/// This is an extended version of [the base Grid View](crate::GridView). Here each column is
/// assumed to be arranged in sections. The first entry of each section is treated as header. When
/// the section is scrolled down, the header remains visible.
///
/// The structure derefs to the [base GridView FRP API](crate::Frp). To access the API specific
/// to headers use [`Self::header_frp`] method.
///
/// # Headers
///
/// Those headers which are pushed down can (and should) be instantiated in a separate layer, passed
/// with `set_layers` input. User should ensure, that the layer is displayed over the normal
/// entries. Those headers can be the same type as main entries, but does not have to. They should
/// have the same [`Entry::Params`] type, however.
///
/// ## Requesting for Models
///
/// Only the pushed-down, currently visible headers are instantiated. The models for the header
/// instances and information about their section should be provided on demand, as a reaction
/// for [`Frp::section_info_needed`] event, by emitting [`Frp::section_info`] with proper data.
///
/// **Important**. The [`Frp::section_info_needed`] are emitted once when needed and not repeated
/// anymore, after adding connections to this FRP node in particular. Therefore, be sure, that you
/// connect providing models logic before emitting any of [`crate::Frp::set_entries_size`] or
/// [`crate::Frp::set_viewport`].
pub type GridView<Entry, HeaderEntry> = GridViewTemplate<
    Entry,
    crate::GridView<Entry>,
    HeaderEntry,
    <HeaderEntry as crate::Entry>::Model,
    <HeaderEntry as crate::Entry>::Params,
>;

impl<E: Entry, HeaderEntry: Entry<Params = E::Params>> GridView<E, HeaderEntry> {
    /// Create new Grid View with headers.
    pub fn new(app: &Application) -> Self {
        let grid = app.new_view::<crate::GridView<E>>();
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
    /// Add the "headers" feature to an arbitrary `InnerGridView` and returns as a new component.
    pub fn new_wrapping(grid: InnerGridView) -> Self {
        let frp = Frp::new();
        let entry_creation_ctx = grid.as_ref().model().entry_creation_ctx.clone_ref();
        let model = Rc::new(Model::new(grid, entry_creation_ctx));
        let grid_frp = model.grid.as_ref().frp();
        let network = frp.network();
        let input = &frp.private.input;
        let out = &frp.private.output;
        frp::extend! { network
            viewport_changed <- grid_frp.viewport.constant(());
            entries_size_changed <- grid_frp.entries_size.constant(());
            column_resized <- grid_frp.column_resized.constant(());
            headers_may_be_hidden <- any(viewport_changed, entries_size_changed, column_resized);
            header_hidden <= headers_may_be_hidden.map2(
                &grid_frp.properties,
                f!(((), props) model.hide_no_longer_visible_headers(*props))
            );
            out.header_hidden <+ header_hidden;

            request_sections_after_viewport_change <= grid_frp.viewport.map2(
                &grid_frp.properties,
                f!((_, props) model.needed_info_for_uncovered_sections(*props))
            );
            request_sections_after_entry_size_change <= grid_frp.entries_size.map2(
                &grid_frp.properties,
                f!((_, props) model.needed_info_for_uncovered_sections(*props))
            );
            request_sections_after_reset <= grid_frp.reset_entries.map2(
                &grid_frp.properties,
                f!((_, props) model.reset_entries(*props))
            );
            request_sections_after_sections_reset <= frp.reset_sections.map2(
                &grid_frp.properties,
                f!((_, props) model.reset_entries(*props))
            );
            request_sections_after_layer_change <= frp.set_layers.map2(
                &grid_frp.properties,
                f!((_, props) model.drop_all_entries(*props))
            );
            request_sections_after_column_resize <= grid_frp.column_resized.map2(
                &grid_frp.properties,
                f!((_, props) model.needed_info_for_uncovered_sections(*props))
            );
            out.section_info_needed <+ request_sections_after_viewport_change;
            out.section_info_needed <+ request_sections_after_entry_size_change;
            out.section_info_needed <+ request_sections_after_reset;
            out.section_info_needed <+ request_sections_after_sections_reset;
            out.section_info_needed <+ request_sections_after_layer_change;
            out.section_info_needed <+ request_sections_after_column_resize;

            pos_should_be_updated <- any(viewport_changed, entries_size_changed, column_resized);
            position_update <= pos_should_be_updated.map2(
                &grid_frp.properties,
                f!(((), props) model.update_headers_positions(*props))
            );
            out.header_position_changed <+ position_update;
            section_update <- input.section_info.map3(
                &grid_frp.properties,
                &frp.set_layers,
                f!(((rows, col, m): &(Range<usize>, usize, HeaderEntry::Model), props, layers)
                    model.update_section(rows.clone(), *col, m.clone(), props.entries_size, props.viewport, layers))
            );
            out.header_shown <+ section_update.map(|&(row, col, _)| (row, col));
            out.header_position_changed <+ section_update;

            column_resize_params <- all(&grid_frp.column_resized, &grid_frp.properties);
            eval column_resize_params ((&((col, _), props)) model.update_header_size(col, props));
        }
        let entry_type = PhantomData;
        Self { frp, model, entry_type }
    }

    /// Returns the current header position by its row and column index.
    ///
    /// If the entry at given location is not a pushed-down header, the method returns `None`.
    /// You may be also interested in [`header_or_entry_position`] method.
    pub fn header_position(&self, row: Row, col: Col) -> Option<Vector2> {
        let entry_size = self.model.grid.as_ref().entries_size.value();
        let viewport = self.model.grid.as_ref().viewport.value();
        self.model.header_position(row, col, entry_size, viewport)
    }

    /// Returns the y position where pushed-down header ends, and below which the standard entries
    /// are visible.
    ///
    /// If the column does not have pushed-down header visible, the top of the viewport is returned.
    pub fn header_separator(&self, col: Col) -> f32 {
        let entry_size = self.model.grid.as_ref().entries_size.value();
        let viewport = self.model.grid.as_ref().viewport.value();
        self.model.header_separator(col, entry_size, viewport)
    }

    /// If the entry at given location is a pushed-down header, return its current (pushed-down
    /// position), otherwise returns the base position for that entry (same as
    /// [`crate::GridView::entry_position`]).
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
    /// Return the pushed-down header instance at given position, or `None` if the entry at this
    /// position is not a pushed-down header.
    pub fn get_header(&self, row: Row, col: Col) -> Option<HeaderEntry>
    where HeaderEntry: CloneRef {
        let headers = self.model.visible_headers.borrow();
        let header = headers.get(&col).filter(|h| h.section_rows.start == row);
        header.map(|e| e.entry.entry.clone_ref())
    }

    /// Return the FRP API related to headers.
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

impl<Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams> AsRef<Self>
    for GridViewTemplate<Entry, InnerGridView, HeaderEntry, HeaderModel, HeaderParams>
where HeaderModel: frp::node::Data
{
    fn as_ref(&self) -> &Self {
        self
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
                (section_start..section_end, col, *model)
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
