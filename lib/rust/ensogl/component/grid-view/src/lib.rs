//! Grid View EnsoGL Component.
//!
//! The main structure is [`GridView`] - see its docs for details.

#![recursion_limit = "1024"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(hash_drain_filter)]
#![feature(bool_to_option)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod column_widths;
pub mod entry;
pub mod scrollable;
pub mod selectable;
pub mod simple;
pub mod visible_area;

pub use ensogl_scroll_area::Viewport;



/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::display::shape::*;
    pub use ensogl_core::prelude::*;

    pub use crate::entry::ShapeWithEntryContour;
    pub use crate::selectable::highlight::shape::AttrSetter as TRAIT_AttrSetter;

    pub use enso_frp as frp;
    pub use ensogl_core::application::command::FrpNetworkProvider;
}

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::display::scene::Layer;
use ensogl_core::gui::Widget;

use crate::column_widths::ColumnWidths;
use crate::entry::EntryFrp;
use crate::visible_area::all_visible_locations;
use crate::visible_area::visible_columns;
use crate::visible_area::visible_rows;
pub use entry::Entry;


// =================
// === Constants ===
// =================

const MOUSE_MOVEMENT_NEEDED_TO_HOVER_PX: f32 = 1.5;



// ===========
// === FRP ===
// ===========

/// A row index in [`GridView`].
pub type Row = usize;
/// A column index  in [`GridView`].
pub type Col = usize;

ensogl_core::define_endpoints_2! {
    <EntryModel: (frp::node::Data), EntryParams: (frp::node::Data)>
    Input {
        /// Declare what area of the GridView is visible. The area position is relative to left-top
        /// corner of the Grid View.
        set_viewport(Viewport),
        /// Set new size of the grid. If the number of rows or columns is reduced, the entries are
        /// removed from the view. If it is extended, new model for entries may be requested if
        /// needed.
        resize_grid(Row, Col),
        /// Reset entries, providing new number of rows and columns. All currently displayed entries
        /// will be detached and their models re-requested.
        reset_entries(Row, Col),
        /// Provide model for specific entry. Should be called only after `model_for_entry_needed`
        /// event for given row and column. After that the entry will be visible.
        model_for_entry(Row, Col, EntryModel),
        /// Emit `model_for_entry_needed` signal for each visible entry. In contrary to
        /// [`reset_entries`], it does not detach any entry.
        request_model_for_visible_entries(),
        /// Set the entries size. All entries have the same size.
        set_entries_size(Vector2),
        /// Set the width of the specified column.
        set_column_width((Col, f32)),
        /// Set the entries parameters.
        set_entries_params(EntryParams),
        /// Set the layer for any texts rendered by entries. The layer will be passed to entries'
        /// constructors. **Performance note**: This will re-instantiate all entries.
        set_text_layer(Option<WeakLayer>),
        select_entry(Option<(Row, Col)>),
        hover_entry(Option<(Row, Col)>),
        accept_entry(Row, Col),
    }

    Output {
        grid_size(Row, Col),
        viewport(Viewport),
        entries_size(Vector2),
        entries_params(EntryParams),
        content_size(Vector2),
        /// Event emitted when the Grid View needs model for an uncovered entry.
        model_for_entry_needed(Row, Col),
        entry_shown(Row, Col),
        entry_hovered(Option<(Row, Col)>),
        entry_selected(Option<(Row, Col)>),
        entry_accepted(Row, Col),
    }
}



// ====================
// === VisibleEntry ===
// ====================

#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "Entry: CloneRef")]
struct VisibleEntry<Entry> {
    entry:   Entry,
    overlay: entry::overlay::View,
}

impl<E: display::Object> display::Object for VisibleEntry<E> {
    fn display_object(&self) -> &display::object::Instance {
        self.entry.display_object()
    }
}



// =============
// === Model ===
// =============

// === EntryCreationCtx ===

/// A structure gathering all data required for creating new entry instance.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
struct EntryCreationCtx<EntryParams> {
    app:                   Application,
    network:               frp::WeakNetwork,
    set_entry_size:        frp::Stream<Vector2>,
    set_entry_params:      frp::Stream<EntryParams>,
    entry_hovered:         frp::Any<Option<(Row, Col)>>,
    entry_selected:        frp::Any<Option<(Row, Col)>>,
    entry_accepted:        frp::Any<(Row, Col)>,
    override_column_width: frp::Any<(Col, f32)>,
}

impl<EntryParams> EntryCreationCtx<EntryParams>
where EntryParams: frp::node::Data
{
    fn create_entry<E: Entry<Params = EntryParams>>(
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

fn entry_position(
    row: Row,
    col: Col,
    entry_size: Vector2,
    column_widths: &ColumnWidths,
) -> Vector2 {
    let x_offset = column_widths.pos_offset(col) + column_widths.width_diff(col) / 2.0;
    let x = (col as f32 + 0.5) * entry_size.x + x_offset;
    let y = (row as f32 + 0.5) * -entry_size.y;
    Vector2(x, y)
}

fn set_entry_position<E: display::Object>(
    entry: &E,
    row: Row,
    col: Col,
    entry_size: Vector2,
    column_widths: &ColumnWidths,
) {
    entry.set_position_xy(entry_position(row, col, entry_size, column_widths));
}


// === Properties ===

#[derive(Copy, Clone, Debug, Default)]
struct Properties {
    row_count:    usize,
    col_count:    usize,
    viewport:     Viewport,
    entries_size: Vector2,
}

impl Properties {
    fn all_visible_locations(
        &self,
        column_widths: &ColumnWidths,
    ) -> impl Iterator<Item = (Row, Col)> {
        let rows = self.row_count;
        let cols = self.col_count;
        all_visible_locations(&self.viewport, self.entries_size, rows, cols, column_widths)
    }
}


// === Model ===

/// The Model of [`GridView`].
#[derive(Clone, Debug)]
pub struct Model<Entry, EntryParams> {
    display_object:     display::object::Instance,
    visible_entries:    RefCell<HashMap<(Row, Col), VisibleEntry<Entry>>>,
    free_entries:       RefCell<Vec<VisibleEntry<Entry>>>,
    entry_creation_ctx: EntryCreationCtx<EntryParams>,
    column_widths:      ColumnWidths,
}

impl<Entry, EntryParams> Model<Entry, EntryParams> {
    fn new(entry_creation_ctx: EntryCreationCtx<EntryParams>) -> Self {
        let logger = Logger::new("GridView");
        let display_object = display::object::Instance::new(&logger);
        let visible_entries = default();
        let free_entries = default();
        let column_widths = ColumnWidths::new(0);
        Model { display_object, visible_entries, free_entries, entry_creation_ctx, column_widths }
    }
}

impl<Entry: display::Object, EntryParams> Model<Entry, EntryParams> {
    fn update_entries_visibility(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { viewport, entries_size, row_count: rows, col_count: cols } = properties;
        let widths = &self.column_widths;
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let visible_rows = visible_rows(&viewport, entries_size, rows);
        let visible_cols = visible_columns(&viewport, entries_size, cols, widths);
        let no_longer_visible = visible_entries.drain_filter(|(row, col), _| {
            !visible_rows.contains(row) || !visible_cols.contains(col)
        });
        let detached = no_longer_visible.map(|(_, entry)| {
            entry.unset_parent();
            entry
        });
        free_entries.extend(detached);
        let uncovered = all_visible_locations(&viewport, entries_size, rows, cols, widths)
            .filter(|loc| !visible_entries.contains_key(loc));
        uncovered.collect_vec()
    }

    fn update_after_entries_size_change(&self, properties: Properties) -> Vec<(Row, Col)> {
        let to_model_request = self.update_entries_visibility(properties);
        for ((row, col), visible_entry) in &*self.visible_entries.borrow() {
            let size = properties.entries_size;
            let widths = &self.column_widths;
            set_entry_position(visible_entry, *row, *col, size, widths);
        }
        to_model_request
    }

    fn reset_entries(&self, properties: Properties) -> Vec<(Row, Col)> {
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let detached = visible_entries.drain().map(|(_, entry)| {
            entry.unset_parent();
            entry
        });
        free_entries.extend(detached);
        properties.all_visible_locations(&self.column_widths).collect_vec()
    }

    fn drop_all_entries(&self, properties: Properties) -> Vec<(Row, Col)> {
        let to_model_request = self.reset_entries(properties);
        self.free_entries.borrow_mut().clear();
        to_model_request
    }

    fn resize_column(&self, col: Col, width: f32, properties: Properties) {
        let current_width = properties.entries_size.x;
        let width_diff = width - current_width;
        self.column_widths.set_width_diff(col, width_diff);
    }

    fn content_size(&self, row_count: Row, col_count: Col, entries_size: Vector2) -> Vector2 {
        self.column_widths.resize(col_count);
        let columns_offset = self.column_widths.pos_offset(col_count);
        let x = col_count as f32 * entries_size.x + columns_offset;
        let y = row_count as f32 * entries_size.y;
        Vector2(x, y)
    }
}

impl<E: Entry> Model<E, E::Params> {
    fn update_entry(
        &self,
        row: Row,
        col: Col,
        model: E::Model,
        entry_size: Vector2,
        text_layer: &Option<WeakLayer>,
    ) {
        use std::collections::hash_map::Entry::*;
        let create_new_entry = || {
            let text_layer = text_layer.as_ref().and_then(|l| l.upgrade());
            self.entry_creation_ctx.create_entry(text_layer.as_ref())
        };
        // We must not emit FRP events when some state is borrowed to avoid double borrows.
        // So the following code block isolates operations with borrowed fields from emitting
        // FRP events.
        let (entry_frp, should_set_location) = {
            let mut visible_entries = self.visible_entries.borrow_mut();
            let mut free_entries = self.free_entries.borrow_mut();
            let (entry, should_set_location) = match visible_entries.entry((row, col)) {
                Occupied(entry) => (entry.into_mut(), false),
                Vacant(lack_of_entry) => {
                    let new_entry = free_entries.pop().unwrap_or_else(create_new_entry);
                    set_entry_position(&new_entry, row, col, entry_size, &self.column_widths);
                    self.display_object.add_child(&new_entry);
                    (lack_of_entry.insert(new_entry), true)
                }
            };
            (entry.entry.frp().clone_ref(), should_set_location)
        };
        // The location should be updated first, because computing entry position after column width
        // change uses information about it. And column width may be change as a reaction of any
        // other event, depending of Entry implementation.
        if should_set_location {
            entry_frp.set_location((row, col));
        }
        let width_offset = self.column_widths.width_diff(col);
        entry_frp.set_size(entry_size + Vector2(width_offset, 0.0));
        entry_frp.set_model(model);
    }

    fn update_after_column_resize(
        &self,
        resized_column: Col,
        properties: Properties,
    ) -> Vec<(Row, Col)> {
        let to_model_request = self.update_entries_visibility(properties);
        // We must not emit FRP events when some state is borrowed to avoid double borrows.
        // So the following code block isolates operations with borrowed fields from emitting
        // FRP events.
        let entries_and_sizes = {
            let borrowed = self.visible_entries.borrow();
            // We are not interested in columns to the left of the resized column.
            let entries = borrowed.iter().filter(|((_, col), _)| *col >= resized_column);
            let entries_and_sizes = entries.map(|((row, col), entry)| {
                let size = properties.entries_size;
                set_entry_position(entry, *row, *col, size, &self.column_widths);
                let width_diff = self.column_widths.width_diff(*col);
                (entry.clone_ref(), size + Vector2(width_diff, 0.0))
            });
            entries_and_sizes.collect_vec()
        };
        for (visible_entry, size) in entries_and_sizes {
            visible_entry.entry.frp().set_size(size);
        }
        to_model_request
    }
}



// ================
// === GridView ===
// ================

/// A template for [`GridView`] structure, where entry parameters and model are separate generic
/// arguments.
///
/// It may be useful when using GridView in parametrized structs, where we want to avoid rewriting
/// `Entry` bound in each place. Otherwise, it's better to use [`GridView`].
///
/// Note that some bounds are still required, as we use [`Widget`] and [`Frp`] nodes.
#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct GridViewTemplate<
    Entry: 'static,
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
> {
    widget: Widget<Model<Entry, EntryParams>, Frp<EntryModel, EntryParams>>,
}

/// Grid View Component.
///
/// This Component displays any kind of entry `E` in a grid. To have it working, you need to
/// * Set entries size ([`Frp::set_entries_size`]),
/// * Declare (and keep up-to-date) the visible area ([`Frp::set_viewport`]),
/// * Set up logic for providing models (see _Requesting for Models_ section).
/// * Optionally: entries parameters, if given entry does not have sensible default.
/// * Finally, reset the content, providing number of rows and columns ([`Frp::reset_entries`]).
///
/// # Positioning
///
/// Please mark, that this structure has its left-top corner docked to (0, 0) point of parent
/// display object, as this is a more intuitive way with handling grids.
///
/// # Entries Instantiation
///
/// The entry should implement [`Entry`] trait. Entries are instantiated lazily, only those visible
/// in provided [`Frp::view_area`]. Once entries are no longer visible, are detached, but not
/// dropped and may be re-used to display new entries when needed. This way we can achieve very
/// efficient scrolling.
///
/// ## Requesting for Models
///
/// Once an entry is uncovered, the Grid View emits [`Frp::model_for_entry_needed`]. Then the proper
/// model should be provided using [`Frp::model_for_entry`] endpoint - only then the entry will be
/// displayed.
///
/// **Important**. The [`Frp::model_for_entry_needed`] are emitted once when needed and not repeated
/// anymore, after adding connections to this FRP node in particular. Therefore, be sure, that you
/// connect providing models logic before emitting any of [`Frp::set_entries_size`] or
/// [`Frp::set_viewport`].
///
/// # Hovering, Selecting and Accepting Entries
///
/// The support for hovering, selecting or accepting entries is limited in this component - it will
/// react for mouse events and emit appropriate event when an entry is hovered/selected or accepted.
/// It does not set `is_selected/is_hovered` flag on entry nor highlight any of those components. If
/// you   want to have full support, use [`selectable::GridView`] instead.
///
/// The entries are both selected accepted with LMB-click, and selected with any other mouse click.
///
/// # Resizing Columns
///
/// By default, each column has a width specified by [`Frp::set_entry_size`]. However, you can
/// override this size in two ways:
/// 1. Using the [`Frp::set_column_width`] input.
/// 2. Using the [`EntryFrp::override_column_width`] output.
///
/// The resizing is permanent and can only be canceled using either method to return the width
/// to the original value. Both ways have the same priority; the last one applied wins.
/// [`EntryFrp::override_column_width`] can be called from different entries in the same column,
/// but only the last one has the effect. Each resize has performance implications proportional
/// to the number of visible entries.
///
/// After either method of resizing, each visible entry in the affected column will receive
/// the [`EntryFrp::set_size`] event. It is up to the entry implementation to avoid loops between
/// [`EntryFrp::set_size`] and [`EntryFrp::override_column_width`].
///
/// **Important**: The current implementation has performance implications for large amounts of
/// entries. A more effective implementation is possible and may be implemented using a
/// specialized tree-like data structure.
/// See https://www.pivotaltracker.com/story/show/183046885 for more details.
pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

impl<E: Entry> GridView<E> {
    /// Create new Grid View.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let input = &frp.private.input;
        let out = &frp.private.output;
        frp::extend! { network
            set_entry_size <- input.set_entries_size.sampler();
            set_entry_params <- input.set_entries_params.sampler();
            override_column_width <- any(...);
        }
        let entry_creation_ctx = EntryCreationCtx {
            app:                   app.clone_ref(),
            network:               network.downgrade(),
            set_entry_size:        set_entry_size.into(),
            set_entry_params:      set_entry_params.into(),
            entry_hovered:         out.entry_hovered.clone_ref(),
            entry_selected:        out.entry_selected.clone_ref(),
            entry_accepted:        out.entry_accepted.clone_ref(),
            override_column_width: override_column_width.clone_ref(),
        };
        let model = Rc::new(Model::new(entry_creation_ctx));
        frp::extend! { network
            out.grid_size <+ input.resize_grid;
            out.grid_size <+ input.reset_entries;
            out.viewport <+ input.set_viewport;
            out.entries_size <+ input.set_entries_size;
            out.entries_params <+ input.set_entries_params;
            prop <- all_with3(
                &out.grid_size, &out.viewport, &out.entries_size,
                |&(row_count, col_count), &viewport, &entries_size| {
                    Properties { row_count, col_count, viewport, entries_size }
                }
            );

            set_column_width <- any(&input.set_column_width, &override_column_width);
            resized_column <- set_column_width.map2(
                &prop,
                f!(((col, width), prop) {
                    model.resize_column(*col,*width, *prop);
                    *col
                })
            );

            column_resized <- resized_column.constant(());
            content_size_params <- all(out.grid_size, input.set_entries_size, column_resized);
            out.content_size <+ content_size_params.map(f!((&((rows, cols), esz, _)) model.content_size(rows, cols, esz)));

            request_models_after_vis_area_change <=
                input.set_viewport.map2(&prop, f!((_, p) model.update_entries_visibility(*p)));
            request_model_after_grid_size_change <=
                input.resize_grid.map2(&prop, f!((_, p) model.update_entries_visibility(*p)));
            request_models_after_entry_size_change <= input.set_entries_size.map2(
                &prop,
                f!((_, p) model.update_after_entries_size_change(*p))
            );
            request_models_after_reset <=
                input.reset_entries.map2(&prop, f!((_, p) model.reset_entries(*p)));
            request_models_after_column_resize <=
                resized_column.map2(&prop, f!((col, p) model.update_after_column_resize(*col, *p)));
            request_models_after_text_layer_change <=
                input.set_text_layer.map2(&prop, f!((_, p) model.drop_all_entries(*p)));
            request_models_for_request <= input.request_model_for_visible_entries.map2(
                &prop,
                f!([model](_, p) p.all_visible_locations(&model.column_widths).collect_vec()),
            );
            out.model_for_entry_needed <+ request_models_after_vis_area_change;
            out.model_for_entry_needed <+ request_model_after_grid_size_change;
            out.model_for_entry_needed <+ request_models_after_entry_size_change;
            out.model_for_entry_needed <+ request_models_after_column_resize;
            out.model_for_entry_needed <+ request_models_after_reset;
            out.model_for_entry_needed <+ request_models_after_text_layer_change;
            out.model_for_entry_needed <+ request_models_for_request;

            out.entry_hovered <+ input.hover_entry;
            out.entry_selected <+ input.select_entry;
            out.entry_accepted <+ input.accept_entry;

            // The ordering here is important: we want to first call [`update_entry`] and only then
            // inform everyone that the entry is visible. They may want to immediately get the entry
            // with [`get_entry`] method.
            model_prop_and_layer <-
                input.model_for_entry.map3(&prop, &input.set_text_layer, |model, prop, layer| (model.clone(), *prop, layer.clone()));
            eval model_prop_and_layer
                ((((row, col, entry_model), prop, layer): &((Row, Col, E::Model), Properties, Option<WeakLayer>))
                    model.update_entry(*row, *col, entry_model.clone(), prop.entries_size, layer)
                );
            out.entry_shown <+ input.model_for_entry.map(|(row, col, _)| (*row, *col));
        }
        let display_object = model.display_object.clone_ref();
        let widget = Widget::new(app, frp, model, display_object);
        Self { widget }
    }
}

impl<Entry, EntryModel, EntryParams> GridViewTemplate<Entry, EntryModel, EntryParams>
where
    Entry: CloneRef,
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    fn get_entry(&self, row: Row, column: Col) -> Option<Entry> {
        let entries = self.widget.model().visible_entries.borrow();
        let entry = entries.get(&(row, column));
        entry.map(|e| e.entry.clone_ref())
    }
}

impl<Entry, EntryModel: frp::node::Data, EntryParams: frp::node::Data> AsRef<Self>
    for GridViewTemplate<Entry, EntryModel, EntryParams>
{
    fn as_ref(&self) -> &Self {
        self
    }
}

impl<Entry, EntryModel: frp::node::Data, EntryParams: frp::node::Data> display::Object
    for GridViewTemplate<Entry, EntryModel, EntryParams>
{
    fn display_object(&self) -> &display::object::Instance {
        self.widget.display_object()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Copy, Clone, CloneRef, Debug, Default)]
    struct TestEntryParams {
        param: Immutable<usize>,
    }

    #[derive(Clone, CloneRef, Debug)]
    struct TestEntry {
        frp:            EntryFrp<Self>,
        param_set:      Rc<Cell<usize>>,
        model_set:      Rc<Cell<usize>>,
        display_object: display::object::Instance,
    }

    impl Entry for TestEntry {
        type Model = Immutable<usize>;
        type Params = TestEntryParams;

        fn new(_app: &Application, _: Option<&Layer>) -> Self {
            let frp = entry::EntryFrp::<Self>::new();
            let network = frp.network();
            let param_set = Rc::new(Cell::new(0));
            let model_set = Rc::new(Cell::new(0));
            let display_object = display::object::Instance::new(Logger::new("TestEntry"));
            frp::extend! { network
                eval frp.input.set_model ((model) model_set.set(**model));
                eval frp.input.set_params ((param) param_set.set(*param.param));
            }
            Self { frp, param_set, model_set, display_object }
        }

        fn frp(&self) -> &EntryFrp<Self> {
            &self.frp
        }
    }

    impl display::Object for TestEntry {
        fn display_object(&self) -> &display::object::Instance {
            &self.display_object
        }
    }

    #[test]
    fn initializing_grid_view() {
        let app = Application::new("root");
        let grid_view = GridView::<TestEntry>::new(&app);
        let network = grid_view.network();
        frp::extend! { network
            updates_requested <- grid_view.model_for_entry_needed.count().sampler();
        }

        let vis_area = Viewport { left: 0.0, top: 0.0, right: 100.0, bottom: -100.0 };
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        grid_view.reset_entries(100, 100);
        grid_view.set_viewport(vis_area);
        grid_view.set_entries_params(TestEntryParams { param: Immutable(13) });

        assert_eq!(grid_view.model().visible_entries.borrow().len(), 0);
        assert_eq!(updates_requested.value(), 25);

        for i in 0..5 {
            for j in 0..5 {
                grid_view.model_for_entry(i, j, Immutable(i * 200 + j));
            }
        }

        {
            let created_entries = grid_view.model().visible_entries.borrow();
            assert_eq!(created_entries.len(), 25);
            for ((row, col), entry) in created_entries.iter() {
                assert_eq!(entry.entry.model_set.get(), row * 200 + col);
                assert_eq!(entry.entry.param_set.get(), 13);
            }
        }
    }

    #[test]
    fn updating_entries_after_viewport_change() {
        let app = Application::new("root");
        let grid_view = GridView::<TestEntry>::new(&app);
        let network = grid_view.network();
        let initial_vis_area = Viewport { left: 0.0, top: 0.0, right: 100.0, bottom: -100.0 };
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        grid_view.reset_entries(100, 100);
        grid_view.set_viewport(initial_vis_area);
        grid_view.set_entries_params(TestEntryParams { param: Immutable(13) });

        for i in 0..5 {
            for j in 0..5 {
                grid_view.model_for_entry(i, j, Immutable(i * 200 + j));
            }
        }

        frp::extend! { network
            updates_requested <- grid_view.model_for_entry_needed.count().sampler();
        }

        let uncovering_new_entries =
            Viewport { left: 5.0, top: -5.0, right: 105.0, bottom: -105.0 };
        grid_view.set_viewport(uncovering_new_entries);
        assert_eq!(updates_requested.value(), 11);
        assert_eq!(grid_view.model().visible_entries.borrow().len(), 25);

        for i in 0..6 {
            grid_view.model_for_entry(5, i, Immutable(200 * 5 + i));
        }
        for i in 0..5 {
            grid_view.model_for_entry(i, 5, Immutable(200 * i + 5));
        }
        assert_eq!(grid_view.model().visible_entries.borrow().len(), 36);

        let hiding_old_entries =
            Viewport { left: 20.0, top: -20.0, right: 120.0, bottom: -120.0 };
        grid_view.set_viewport(hiding_old_entries);
        assert_eq!(updates_requested.value(), 11); // Count should not change.
        assert_eq!(grid_view.model().visible_entries.borrow().len(), 25);
        assert_eq!(grid_view.model().free_entries.borrow().len(), 11);
    }

    #[test]
    fn overriding_column_width() {
        let app = Application::new("root");
        let grid_view = GridView::<TestEntry>::new(&app);
        let network = grid_view.network();
        frp::extend! { network
            updates_requested <- grid_view.model_for_entry_needed.count().sampler();
        }

        let initial_vis_area = Viewport { left: 0.0, top: 0.0, right: 100.0, bottom: -100.0 };
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        grid_view.reset_entries(100, 100);
        grid_view.set_viewport(initial_vis_area);

        assert_eq!(grid_view.model().visible_entries.borrow().len(), 0);
        assert_eq!(updates_requested.value(), 25);

        for i in 0..5 {
            for j in 0..5 {
                grid_view.model_for_entry(i, j, Immutable(i * 200 + j));
            }
        }
        assert_eq!(grid_view.model().visible_entries.borrow().len(), 25);

        assert_eq!(updates_requested.value(), 25);
        grid_view.set_column_width((0, 5.0));
        assert_eq!(updates_requested.value(), 30);
        for i in 0..5 {
            for j in 0..6 {
                grid_view.model_for_entry(i, j, Immutable(200 * i + 5));
            }
        }

        assert_eq!(grid_view.model().visible_entries.borrow().len(), 30);
    }
}
