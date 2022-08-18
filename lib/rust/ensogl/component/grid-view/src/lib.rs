//! Grid View EnsoGL Component.
//!
//! There are many variants of the Grid View component:
//! * the basic one: [`GridView`],
//! * with scroll bars: [`scrollable::GridView`],
//! * with selection and mouse hover highlights: [`selectable::GridView`],
//! * with sections and headers visible even when scrolled down: [`header::GridView`].
//! * The combinations of the features of above three, like [`selectable::GridViewWithHeaders`] or
//!   [`scrollable::SeleectableGridViewWithHeraders`].
//!
//! Each variant expose the [`FRP`] structure as its main API - additional APIs (like for handling
//! highlight or headers) are available through accessors (like `header_frp` or
//! `selection_highlight_frp`). Also, as every variant is based on the basic [`GridView`], they
//! implement `AsRef<GridView>`.

#![recursion_limit = "1024"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(hash_drain_filter)]
#![feature(type_alias_impl_trait)]
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

pub mod entry;
pub mod header;
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
use ensogl_core::gui::Widget;

use crate::entry::EntryFrp;
use crate::visible_area::all_visible_locations;
use crate::visible_area::visible_columns;
use crate::visible_area::visible_rows;
pub use entry::Entry;



// ===========
// === FRP ===
// ===========

// === Row and Col Aliases ===

/// A row index in [`GridView`].
pub type Row = usize;
/// A column index  in [`GridView`].
pub type Col = usize;


// === Properties ===

/// A set of GridView properties used in many operations.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default)]
pub struct Properties {
    pub row_count:    usize,
    pub col_count:    usize,
    pub viewport:     Viewport,
    pub entries_size: Vector2,
}

impl Properties {
    /// Return iterator over all visible locations (row-column pairs).
    pub fn all_visible_locations(&self) -> impl Iterator<Item = (Row, Col)> {
        all_visible_locations(self.viewport, self.entries_size, self.row_count, self.col_count)
    }
}


// === The Endpoints ===

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
        properties(Properties),
        /// Event emitted when the Grid View needs model for an uncovered entry.
        model_for_entry_needed(Row, Col),
        entry_shown(Row, Col),
        entry_contour(Row, Col, entry::Contour),
        entry_hovered(Option<(Row, Col)>),
        entry_selected(Option<(Row, Col)>),
        entry_accepted(Row, Col),
    }
}



// =============
// === Model ===
// =============

// === Model ===

/// The Model of [`GridView`].
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Model<Entry, EntryParams> {
    display_object:         display::object::Instance,
    visible_entries:        RefCell<HashMap<(Row, Col), entry::visible::VisibleEntry<Entry>>>,
    free_entries:           RefCell<Vec<entry::visible::VisibleEntry<Entry>>>,
    pub entry_creation_ctx: entry::visible::CreationCtx<EntryParams>,
}

impl<Entry, EntryParams> Model<Entry, EntryParams> {
    fn new(entry_creation_ctx: entry::visible::CreationCtx<EntryParams>) -> Self {
        let logger = Logger::new("GridView");
        let display_object = display::object::Instance::new(&logger);
        let visible_entries = default();
        let free_entries = default();
        Model { display_object, visible_entries, free_entries, entry_creation_ctx }
    }
}

impl<Entry: display::Object, EntryParams> Model<Entry, EntryParams> {
    fn update_entries_visibility(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { viewport, entries_size, row_count, col_count } = properties;
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let visible_rows = visible_rows(viewport, entries_size, row_count);
        let visible_cols = visible_columns(viewport, entries_size, col_count);
        let no_longer_visible = visible_entries.drain_filter(|(row, col), _| {
            !visible_rows.contains(row) || !visible_cols.contains(col)
        });
        let detached = no_longer_visible.map(|(_, entry)| {
            entry.unset_parent();
            entry
        });
        free_entries.extend(detached);
        let uncovered = all_visible_locations(viewport, entries_size, row_count, col_count)
            .filter(|loc| !visible_entries.contains_key(loc));
        uncovered.collect_vec()
    }

    fn update_after_entries_size_change(&self, properties: Properties) -> Vec<(Row, Col)> {
        let to_model_request = self.update_entries_visibility(properties);
        for ((row, col), visible_entry) in &*self.visible_entries.borrow() {
            entry::visible::set_position(visible_entry, *row, *col, properties.entries_size);
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
        properties.all_visible_locations().collect_vec()
    }

    fn drop_all_entries(&self, properties: Properties) -> Vec<(Row, Col)> {
        let to_model_request = self.reset_entries(properties);
        self.free_entries.borrow_mut().clear();
        to_model_request
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
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let create_new_entry = || {
            let text_layer = text_layer.as_ref().and_then(|l| l.upgrade());
            self.entry_creation_ctx.create_entry(text_layer.as_ref())
        };
        let entry = match visible_entries.entry((row, col)) {
            Occupied(entry) => entry.get().entry.clone_ref(),
            Vacant(lack_of_entry) => {
                let new_entry = free_entries.pop().unwrap_or_else(create_new_entry);
                entry::visible::set_position(&new_entry, row, col, entry_size);
                self.display_object.add_child(&new_entry);
                lack_of_entry.insert(new_entry).entry.clone_ref()
            }
        };
        drop(visible_entries);
        drop(free_entries);
        entry.frp().set_location((row, col));
        entry.frp().set_model(model);
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
pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

impl<E: Entry> GridView<E> {
    /// Create new Grid View.
    fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let input = &frp.private.input;
        let out = &frp.private.output;
        frp::extend! { network
            set_entry_size <- input.set_entries_size.sampler();
            set_entry_params <- input.set_entries_params.sampler();
        }
        let entry_creation_ctx = entry::visible::CreationCtx {
            app:              app.clone_ref(),
            network:          network.downgrade(),
            set_entry_size:   set_entry_size.into(),
            set_entry_params: set_entry_params.into(),
            entry_contour:    out.entry_contour.clone_ref(),
            entry_hovered:    out.entry_hovered.clone_ref(),
            entry_selected:   out.entry_selected.clone_ref(),
            entry_accepted:   out.entry_accepted.clone_ref(),
        };
        let model = Rc::new(Model::new(entry_creation_ctx));
        frp::extend! { network
            grid_size <- any(input.resize_grid, input.reset_entries);
            // We want to update `properties` output first, as some could listen for specific
            // event (e.g. the `viewport` and expect the `properties` output is up to date.
            out.properties <+ all_with3(
                &grid_size, &input.set_viewport, &input.set_entries_size,
                |&(row_count, col_count), &viewport, &entries_size| {
                    Properties { row_count, col_count, viewport, entries_size }
                }
            );
            out.grid_size <+ grid_size;
            out.grid_size <+ input.reset_entries;
            out.viewport <+ input.set_viewport;
            out.entries_size <+ input.set_entries_size;
            out.entries_params <+ input.set_entries_params;

            content_size_params <- all(out.grid_size, input.set_entries_size);
            out.content_size <+ content_size_params.map(|&((rows, cols), esz)| Self::content_size(rows, cols, esz));

            request_models_after_vis_area_change <=
                input.set_viewport.map2(&out.properties, f!((_, p) model.update_entries_visibility(*p)));
            request_model_after_grid_size_change <=
                input.resize_grid.map2(&out.properties, f!((_, p) model.update_entries_visibility(*p)));
            request_models_after_entry_size_change <= input.set_entries_size.map2(
                &out.properties,
                f!((_, p) model.update_after_entries_size_change(*p))
            );
            request_models_after_reset <=
                input.reset_entries.map2(&out.properties, f!((_, p) model.reset_entries(*p)));
            request_models_after_text_layer_change <=
                input.set_text_layer.map2(&out.properties, f!((_, p) model.drop_all_entries(*p)));
            request_models_for_request <= input.request_model_for_visible_entries.map2(
                &out.properties,
                |_, p| p.all_visible_locations().collect_vec()
            );
            out.model_for_entry_needed <+ request_models_after_vis_area_change;
            out.model_for_entry_needed <+ request_model_after_grid_size_change;
            out.model_for_entry_needed <+ request_models_after_entry_size_change;
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
                input.model_for_entry.map3(&out.properties, &input.set_text_layer, |m, p, l| (m.clone(), *p, l.clone()));
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

    fn content_size(row_count: Row, col_count: Col, entries_size: Vector2) -> Vector2 {
        let x = col_count as f32 * entries_size.x;
        let y = row_count as f32 * entries_size.y;
        Vector2(x, y)
    }
}

impl<Entry, EntryModel, EntryParams> GridViewTemplate<Entry, EntryModel, EntryParams>
where
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    /// Get the entry instance for given row and column, or `None` if no entry is instantiated at
    /// given location.
    pub fn get_entry(&self, row: Row, column: Col) -> Option<Entry>
    where Entry: CloneRef {
        let entries = self.widget.model().visible_entries.borrow();
        let entry = entries.get(&(row, column));
        entry.map(|e| e.entry.clone_ref())
    }

    /// Return the position of the Entry instance for given row and column.
    pub fn entry_position(&self, row: Row, column: Col) -> Vector2 {
        entry::visible::position(row, column, self.entries_size.value())
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
pub(crate) mod tests {
    use super::*;
    use ensogl_core::display::scene::Layer;

    #[derive(Copy, Clone, CloneRef, Debug, Default)]
    pub struct TestEntryParams {
        pub param: Immutable<usize>,
    }

    #[derive(Clone, CloneRef, Debug)]
    pub struct TestEntry {
        pub frp:            EntryFrp<Self>,
        pub param_set:      Rc<Cell<usize>>,
        pub model_set:      Rc<Cell<usize>>,
        pub display_object: display::object::Instance,
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
}
