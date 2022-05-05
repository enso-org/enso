//! A multi-column [Component Group] without header.
//!
//! The widget is defined by the [`View`].
//!
//! To learn more about component groups, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
//!
//! [Component Group]: crate::component_group::View

use ensogl_core::application::traits::*;
use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::EntryId;

use enso_frp as frp;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_gui_component::component;
use ensogl_label::Label;
use ensogl_list_view as list_view;
use list_view::entry::AnyModelProvider;


// =================
// === Constants ===
// =================

const NO_ITEMS_LABEL_TEXT: &str = "No local variables";
const ENTRY_HEIGHT: f32 = list_view::entry::HEIGHT;
const MINIMAL_HEIGHT: f32 = ENTRY_HEIGHT;
const COLUMNS: usize = 3;



// ===============
// === Aliases ===
// ===============

/// Type of the component group items.
type Entry = list_view::entry::Label;
/// An index of the column.
type ColumnId = usize;



// ========================
// === Background Shape ===
// ========================

/// The background of the Wide Component Group.
pub mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        below = [list_view::background];
        (style:Style, color:Vector4) {
            let color = Var::<Rgba>::from(color);
            let shape = Plane().fill(color);
            shape.into()
        }
    }
}



// =====================
// === ModelProvider ===
// =====================

/// A [`list_view::entry::ModelProvider`] wrapper that splits entries into `COLUMNS` lists.
///
/// Entries are distributed evenly between lists. If the entry count is not divisible by `COLUMNS` -
/// the lists with lower indices will have more entries.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct ModelProvider {
    inner:     AnyModelProvider<Entry>,
    column_id: Immutable<ColumnId>,
}

impl ModelProvider {
    /// Wrap [`AnyModelProvider`] and split its entries into `COLUMNS` lists. The returned instance
    /// provides entries for column with `column_id`.
    fn wrap(inner: &AnyModelProvider<Entry>, column_id: ColumnId) -> AnyModelProvider<Entry> {
        AnyModelProvider::new(Self {
            inner:     inner.clone_ref(),
            column_id: Immutable(column_id),
        })
    }
}

impl list_view::entry::ModelProvider<Entry> for ModelProvider {
    fn entry_count(&self) -> usize {
        let total_entry_count = self.inner.entry_count();
        entry_count_in_column(*self.column_id, total_entry_count)
    }

    fn get(&self, id: EntryId) -> Option<String> {
        let total_entry_count = self.inner.entry_count();
        let idx = local_idx_to_global(*self.column_id, id, total_entry_count);
        self.inner.get(idx)
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        /// Accept the currently selected suggestion. Should be bound to "Suggestion Acceptance Key"
        /// described in
        /// [Component Browser Design Doc](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#key-binding-dictionary)
        accept_suggestion(),
        select_entry(ColumnId, EntryId),
        set_entries(AnyModelProvider<Entry>),
        set_background_color(Rgba),
        set_width(f32),
    }
    Output {
        selected_entry(Option<EntryId>),
        suggestion_accepted(EntryId),
        expression_accepted(EntryId),
        selection_position_target(Vector2<f32>),
        entry_count(usize),
        size(Vector2<f32>),
    }
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, _style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        let out = &api.output;
        frp::extend! { network
            entry_count <- input.set_entries.map(|p| p.entry_count());
            out.entry_count <+ entry_count;

            eval input.select_entry([model]((column, entry)) {
                if let Some(column) = model.columns.get(*column) {
                    let real_entry_id = column.list_view_entry_id(*entry);
                    column.select_entry(real_entry_id);
                }
            });

            eval input.set_background_color((c) model.background.color.set(c.into()));


            // === Background size ===

            background_height <- any(...);
            let background_width = input.set_width.clone_ref();
            size <- all_with(&background_width, &background_height,
                             |width, height| Vector2(*width, *height));
            eval size((size) model.background.size.set(*size));
            out.size <+ size;

            // === "No items" label ===

            no_entries_provided <- entry_count.map(|c| *c == 0);
            show_no_items_label <- no_entries_provided.on_true();
            hide_no_items_label <- no_entries_provided.on_false();
            eval_ show_no_items_label(model.show_no_items_label());
            eval_ hide_no_items_label(model.hide_no_items_label());
        }

        for (idx, column) in model.columns.iter().enumerate() {
            frp::extend! { network
                // === Accepting suggestions ===

                accepted_entry <- column.selected_entry.sample(&input.accept_suggestion).filter_map(|e| *e);
                chosen_entry <- column.chosen_entry.filter_map(|e| *e);
                out.suggestion_accepted <+ accepted_entry.map2(&entry_count, f!(
                        [](&e, &total) local_idx_to_global(idx, e, total)
                ));
                out.expression_accepted <+ chosen_entry.map2(&entry_count, f!(
                        [](&e, &total) local_idx_to_global(idx, e, total)
                ));


                // === Selection position ===

                on_column_selected <- column.selected_entry.map(|e| e.is_some()).on_true();
                eval_ on_column_selected(model.on_column_selected(idx));
                selection_pos <- column.selection_position_target.sample(&on_column_selected);
                out.selection_position_target <+ selection_pos.map(f!((pos) column.selection_position(*pos)));


                // === set_entries ===

                out.selected_entry <+ column.selected_entry.map2(&entry_count, move |entry, total| {
                    entry.map(|e| local_idx_to_global(idx, e, *total))
                });
                entries <- input.set_entries.map(move |p| ModelProvider::wrap(p, idx));
                background_height <+ entries.map(f_!(model.background_height()));
                eval entries((e) column.set_entries(e));
                _eval <- all_with(&entries, &out.size, f!((_, size) column.resize_and_place(*size)));
            }
        }
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl_core::application::shortcut::ActionType::*;
        (&[(Press, "tab", "accept_suggestion")])
            .iter()
            .map(|(a, b, c)| View::self_shortcut(*a, *b, *c))
            .collect()
    }
}



// ==============
// === Column ===
// ==============

/// An internal representation of the column.
#[derive(Debug, Clone, CloneRef)]
struct Column {
    index:     Immutable<ColumnId>,
    provider:  Rc<CloneRefCell<AnyModelProvider<Entry>>>,
    list_view: list_view::ListView<Entry>,
}

impl Deref for Column {
    type Target = list_view::ListView<Entry>;
    fn deref(&self) -> &Self::Target {
        &self.list_view
    }
}

impl Column {
    /// Constructor.
    fn new(app: &Application, index: ColumnId) -> Self {
        Self {
            index:     Immutable(index),
            provider:  default(),
            list_view: app.new_view::<list_view::ListView<Entry>>(),
        }
    }

    /// An entry count for this column.
    fn len(&self) -> usize {
        self.provider.get().entry_count()
    }

    /// Transforms `entry_id` into the actual EntryId for the underlying [`list_view::ListView`].
    ///
    /// EntryId of the Wide Component Group count from the bottom (the bottom most entry has an id
    /// of 0), but the underlying [`list_view::ListView`] starts its ids from the top (so that
    /// the top most entry has an id of 0). This function converts the former to the latter.
    fn list_view_entry_id(&self, entry_id: EntryId) -> EntryId {
        self.len() - entry_id - 1
    }

    /// Update entries list, a setter for [`list_view::ListView`]`::set_entries`.
    fn set_entries(&self, provider: &AnyModelProvider<Entry>) {
        self.provider.set(provider.clone_ref());
        self.list_view.set_entries(provider);
    }

    /// Resize the column and update its position.
    fn resize_and_place(&self, size: Vector2) {
        let width = size.x / COLUMNS as f32;
        let bg_height = size.y;
        let height = self.len() as f32 * ENTRY_HEIGHT;
        self.list_view.resize(Vector2(width, height));

        let left_border = -(COLUMNS as f32 * width / 2.0) + width / 2.0;
        let pos_x = left_border + width * *self.index as f32;
        let half_height = height / 2.0;
        let background_bottom = -bg_height / 2.0;
        let pos_y = background_bottom + half_height;
        self.list_view.set_position_x(pos_x);
        self.list_view.set_position_y(pos_y);
    }

    /// Transform Column-space position to WideComponentGroup-space one.
    fn selection_position(&self, pos: Vector2) -> Vector2 {
        self.position().xy() + pos
    }
}



// =============
// === Model ===
// =============

/// The Model of the [`View`] component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object: display::object::Instance,
    background:     background::View,
    columns:        Rc<[Column; COLUMNS]>,
    no_items_label: Label,
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "WideComponentGroupView"
    }

    fn new(app: &Application, logger: &Logger) -> Self {
        let display_object = display::object::Instance::new(&logger);
        let background = background::View::new(&logger);
        display_object.add_child(&background);
        let columns = Rc::new([Column::new(app, 0), Column::new(app, 1), Column::new(app, 2)]);
        for column in columns.iter() {
            column.hide_selection();
            column.set_background_color(Rgba::transparent());
            column.show_background_shadow(false);
            column.set_background_corners_radius(0.0);
            display_object.add_child(&**column);
        }

        let no_items_label = Label::new(app);
        no_items_label.set_content(NO_ITEMS_LABEL_TEXT);

        Model { no_items_label, display_object, background, columns }
    }
}

impl Model {
    /// Make the "no items" label visible.
    fn show_no_items_label(&self) {
        self.display_object.add_child(&self.no_items_label);
    }

    /// Hide the "no items" label.
    fn hide_no_items_label(&self) {
        self.display_object.remove_child(&self.no_items_label);
    }

    /// Deselect entries in all columns except the one with provided `column_index`. We ensure that
    /// at all times only a single entry across all columns is selected.
    fn on_column_selected(&self, column_id: ColumnId) {
        let other_columns = self.columns.iter().enumerate().filter(|(i, _)| *i != column_id);
        for (_, column) in other_columns {
            column.deselect_entries();
        }
    }

    /// Calculate the height of the component. It can't be less than [`MINIMAL_HEIGHT`].
    fn background_height(&self) -> f32 {
        if let Some(largest_column) = self.columns.first() {
            let entry_count_in_largest_column = largest_column.len();
            let background_height = entry_count_in_largest_column as f32 * ENTRY_HEIGHT;
            background_height.max(MINIMAL_HEIGHT)
        } else {
            WARNING!("Wide Component Group does not have any columns.");
            MINIMAL_HEIGHT
        }
    }
}



// ============
// === View ===
// ============

/// The implementation of the visual component described in the module's documentation.
pub type View = component::ComponentView<Model, Frp>;



// ===============
// === Helpers ===
// ===============

/// Return the number of entries in column with `index`.
fn entry_count_in_column(column_id: ColumnId, total_entry_count: usize) -> usize {
    let evenly_distributed_count = total_entry_count / COLUMNS;
    let remainder = total_entry_count % COLUMNS;
    let has_remainder = remainder > 0;
    let column_contains_remaining_entries = column_id < remainder;
    if has_remainder && column_contains_remaining_entries {
        evenly_distributed_count + 1
    } else {
        evenly_distributed_count
    }
}

/// Transform the index inside column to the "global" index inside the Wide Component Group.
///
/// The entry #1 in column with index 2 is the second item from the bottom in the third column and
/// has a "global" index of `COLUMNS + 2 = 5`.
fn local_idx_to_global(column: ColumnId, entry: EntryId, total_entry_count: usize) -> EntryId {
    let upside_down_index = entry_count_in_column(column, total_entry_count) - entry - 1;
    COLUMNS * upside_down_index + column
}
