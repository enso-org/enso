//! A multi-column [Component Group] without header.
//!
//! Almost every type in this module is parametrized with `COLUMNS` const generic, that represents
//! the count of columns the widget will have. The default value is `3`. (see
//! [`DEFAULT_COLUMNS_COUNT`])
//!
//! The widget is defined by the [`View`].
//!
//! To learn more about component groups, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
//!
//! [Component Group]: crate::component_group::View

use crate::prelude::*;

use crate::background;
use crate::entry;
use crate::theme;
use crate::Colors;
use crate::Selected;
use crate::SelectionStyle;

use enso_frp as frp;
use ensogl::application::shortcut::Shortcut;
use ensogl::application::Application;
use ensogl::data::color::Rgba;
use ensogl::display;
use ensogl_gui_component::component;
use ensogl_label::Label;
use ensogl_list_view as list_view;
use list_view::entry::AnyModelProvider;



// =================
// === Constants ===
// =================

/// The default count of columns to display.
pub const DEFAULT_COLUMNS_COUNT: usize = 3;
const ENTRY_HEIGHT: f32 = list_view::entry::HEIGHT;
const MINIMAL_HEIGHT: f32 = ENTRY_HEIGHT;



// ===============
// === Aliases ===
// ===============

/// Type of the component group items.
type Entry = crate::Entry;

newtype_prim! {
    /// An index of the column.
    ColumnId(usize);
}



// =====================
// === ModelProvider ===
// =====================

/// A [`list_view::entry::ModelProvider`] wrapper that splits entries into `COLUMNS` lists.
///
/// Entries are distributed evenly between lists. If the entry count is not divisible by `COLUMNS` -
/// the lists with lower indices will have more entries.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct ModelProvider<const COLUMNS: usize> {
    inner:     AnyModelProvider<Entry>,
    column_id: Immutable<ColumnId>,
}

impl<const COLUMNS: usize> ModelProvider<COLUMNS> {
    /// Wrap [`AnyModelProvider`] and split its entries into `COLUMNS` lists. The returned instance
    /// provides entries for the column with `column_id`.
    fn wrap(inner: &AnyModelProvider<Entry>, column_id: ColumnId) -> AnyModelProvider<Entry> {
        AnyModelProvider::new(Self {
            inner:     inner.clone_ref(),
            column_id: Immutable(column_id),
        })
    }
}

impl<const COLUMNS: usize> list_view::entry::ModelProvider<Entry> for ModelProvider<COLUMNS> {
    fn entry_count(&self) -> usize {
        let total_entry_count = self.inner.entry_count();
        entry_count_in_column::<COLUMNS>(*self.column_id, total_entry_count)
    }

    fn get(&self, id: entry::Id) -> Option<entry::Model> {
        let total_entry_count = self.inner.entry_count();
        let idx = local_idx_to_global::<COLUMNS>(*self.column_id, id, total_entry_count);
        self.inner.get(idx)
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints_2! {
    Input {
        /// Accept the currently selected suggestion. Should be bound to "Suggestion Acceptance Key"
        /// described in
        /// [Component Browser Design Doc](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#key-binding-dictionary)
        accept_suggestion(),
        select_entry(ColumnId, entry::Id),
        set_entries(AnyModelProvider<Entry>),
        set_color(Rgba),
        set_dimmed(bool),
        set_width(f32),
        set_no_items_label_text(String),
    }
    Output {
        is_mouse_over(bool),
        selected(Option<Selected>),
        suggestion_accepted(entry::Id),
        expression_accepted(entry::Id),
        /// While resizing the list of entries, the selection will follow the selected entry if
        /// possible. If the entry disappears, the selection will move to some visible entry in the same
        /// column if possible. If there are no more entries in this column, the selection will move to
        /// the next non-empty column to the left.
        selection_position_target(Vector2<f32>),
        selection_size(Vector2<f32>),
        selection_corners_radius(f32),
        module_entered(entry::Id),
        entry_count(usize),
        size(Vector2<f32>),
    }
}

impl<const COLUMNS: usize> component::Frp<Model<COLUMNS>> for Frp {
    fn init(
        network: &frp::Network,
        api: &Self::Private,
        _app: &Application,
        model: &Model<COLUMNS>,
        style: &StyleWatchFrp,
    ) {
        let input = &api.input;
        let out = &api.output;
        let colors = Colors::from_main_color(network, style, &input.set_color, &input.set_dimmed);
        let padding = style.get_number(theme::entry_list::padding);
        let column_padding = style.get_number(theme::selection::wide_group_column_padding);
        frp::extend! { network
            init <- source_();
            entry_count <- input.set_entries.map(|p| p.entry_count());
            out.entry_count <+ entry_count;
            is_mouse_over <- init.constant(false);

            selected_column_and_entry <- any(...);
            update_selected_entry <- selected_column_and_entry.sample(&out.size);
            select_entry <- any(&input.select_entry, &update_selected_entry);
            eval select_entry([model]((column, entry)) {
                let column = model.non_empty_column(*column);
                if let Some(column) = column {
                    let real_entry_id = column.reverse_index(*entry);
                    column.select_entry(real_entry_id);
                }
            });

            eval colors.background((c) model.background.color.set(c.into()));
            eval colors.selected.background((c) model.selection_background.color.set(c.into()));

            eval input.set_no_items_label_text((text) model.set_no_items_label_text(text));

            // === Background size ===

            background_height <- any(...);
            let background_width = input.set_width.clone_ref();
            size <- all_with3(&background_width, &background_height, &padding,
                             |width, height, padding| Vector2(*width, *height + *padding));
            eval size((size) model.background.size.set(*size));
            eval size((size) model.selection_background.size.set(*size));
            out.size <+ size;
            let selection_style = SelectionStyle::from_style(style, network);
            out.selection_size <+ background_width.all_with4(
                &selection_style,
                &column_padding,
                &out.focused,
                |&width,&style,&column_padding,_| {
                    let width = (width - 2.0 * column_padding) / COLUMNS as f32;
                    let height = style.height;
                    Vector2(width, height)
                }
            );
            out.selection_corners_radius <+ all_with(&selection_style, &out.focused,
                |style,_| style.corners_radius
            );

            // === "No items" label ===

            no_entries_provided <- entry_count.map(|c| *c == 0);
            show_no_items_label <- no_entries_provided.on_true();
            hide_no_items_label <- no_entries_provided.on_false();
            eval_ show_no_items_label(model.show_no_items_label());
            eval_ hide_no_items_label(model.hide_no_items_label());
        }
        init.emit(());

        let mut is_mouse_over = is_mouse_over.clone_ref();
        for column in model.columns.iter() {
            let col_id = column.id.clone_ref();
            frp::extend! { network
                // === Focus propagation ===

                eval_ input.defocus(model.defocus_columns());


                // === Mouse hovering ===

                // We connect `is_mouse_over` events from all columns into a single event stream
                // using `or` combinator.
                new_is_mouse_over <- is_mouse_over.or(&column.is_mouse_over);
                is_mouse_over = new_is_mouse_over;

                // === Accepting suggestions ===

                accepted_entry <- column.selected_entry.sample(&input.accept_suggestion).filter_map(|e| *e);
                chosen_entry <- column.chosen_entry.filter_map(|e| *e);
                out.suggestion_accepted <+ accepted_entry.map2(&entry_count, f!(
                        [](&e, &total) local_idx_to_global::<COLUMNS>(col_id, e, total)
                ));
                chosen_entry <- chosen_entry.map2(&entry_count, f!(
                        [](&e, &total) local_idx_to_global::<COLUMNS>(col_id, e, total)
                ));
                chosen_entry <- chosen_entry.map2(&input.set_entries, |id, p| {
                    (*id, p.get(*id).map_or(false, |e| e.is_enterable))
                });
                out.module_entered <+ chosen_entry.filter(|(_, is_enterable)| *is_enterable)._0();
                out.expression_accepted <+ chosen_entry.filter(|(_, is_enterable)| !is_enterable)._0();


                // === Selected entry ===

                is_column_selected <- column.selected_entry.map(|e| e.is_some());
                selected_entry <- column.selected_entry.gate(&is_column_selected);
                out.selected <+ selected_entry.map2(&entry_count, move |entry, total| {
                    entry.map(|e| Selected::Entry(local_idx_to_global::<COLUMNS>(col_id,e,*total)))
                });


                // === Selection position ===

                entry_selected <- column.selected_entry.filter_map(|e| *e);
                selected_column_and_entry <+ entry_selected.map(f!([column](e) (col_id, column.reverse_index(*e))));
                on_column_selected <- column.selected_entry.map(|e| e.is_some()).on_true();
                eval_ on_column_selected(model.on_column_selected(col_id));
                eval_ on_column_selected(column.focus());
                selection_pos <- column.selection_position_target.sample(&on_column_selected);
                out.selection_position_target <+ selection_pos.map(f!((pos) column.selection_position(*pos)));


                // === set_entries ===

                entries <- input.set_entries.map(move |p| ModelProvider::<COLUMNS>::wrap(p, col_id));
                background_height <+ entries.map(f_!(model.background_height()));
                eval entries((e) column.set_entries(e));
                _eval <- all_with3(&entries, &out.size, &padding,
                    f!((_, size, padding) column.resize_and_place(*size, *padding))
                );
            }
            frp::extend! { network
                out.is_mouse_over <+ is_mouse_over;
            }

            let selection_layer = default();
            let params = entry::Params { colors: colors.clone_ref(), selection_layer };
            column.list_view.set_entry_params_and_recreate_entries(params);
        }
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl::application::shortcut::ActionType::*;
        [(Press, "tab", "accept_suggestion")]
            .iter()
            .map(|(a, b, c)| View::<COLUMNS>::self_shortcut(*a, *b, *c))
            .collect()
    }
}



// ==============
// === Column ===
// ==============

/// An internal representation of the column.
///
/// `COLUMNS` is the total count of columns in the widget.
#[derive(Debug, Clone, CloneRef, Deref)]
struct Column<const COLUMNS: usize> {
    id:        ColumnId,
    provider:  Rc<CloneRefCell<AnyModelProvider<Entry>>>,
    #[deref]
    list_view: list_view::ListView<Entry>,
}

impl<const COLUMNS: usize> Column<COLUMNS> {
    /// Constructor.
    fn new(app: &Application, id: ColumnId) -> Self {
        let list_view = app.new_view::<list_view::ListView<Entry>>();
        list_view.set_style_prefix(entry::STYLE_PATH);
        Self { id, provider: default(), list_view }
    }

    /// An entry count for this column.
    fn len(&self) -> usize {
        self.provider.get().entry_count()
    }

    /// Transforms `entry_id` into the actual [`entry::Id`] for the underlying
    /// [`list_view::ListView`].
    ///
    /// [`entry::Id`] of the Wide Component Group counts from the bottom (the bottom most entry has
    /// an id of 0), but the underlying [`list_view::ListView`] starts its ids from the top (so
    /// that the top most entry has an id of 0). This function converts the former to the latter.
    fn reverse_index(&self, entry_id: entry::Id) -> entry::Id {
        reverse_index(entry_id, self.len())
    }

    /// Update the entries list, a setter for [`list_view::ListView::set_entries`].
    fn set_entries(&self, provider: &AnyModelProvider<Entry>) {
        self.provider.set(provider.clone_ref());
        self.list_view.set_entries(provider);
    }

    /// Resize the column and update its position.
    fn resize_and_place(&self, size: Vector2, padding: f32) {
        let width = size.x / COLUMNS as f32;
        let bg_height = size.y;
        let height = self.len() as f32 * ENTRY_HEIGHT;
        self.list_view.resize(Vector2(width, height + padding));

        let left_border = -(COLUMNS as f32 * width / 2.0) + width / 2.0;
        let pos_x = left_border + width * *self.id as f32;
        let half_height = height / 2.0;
        let background_bottom = -bg_height / 2.0;
        let pos_y = background_bottom + half_height + padding;
        self.list_view.set_position_x(pos_x);
        self.list_view.set_position_y(pos_y);
    }

    /// Transform the position relative to the column into the position relative to the whole
    /// widget.
    fn selection_position(&self, pos: Vector2) -> Vector2 {
        self.position().xy() + pos
    }
}



// =============
// === Model ===
// =============

/// The Model of the [`View`] component. Consists of `COLUMNS` columns.
#[derive(Clone, CloneRef, Debug)]
pub struct Model<const COLUMNS: usize> {
    display_object:       display::object::Instance,
    background:           background::View,
    selection_background: background::View,
    columns:              Rc<Vec<Column<COLUMNS>>>,
    no_items_label:       Label,
}

impl<const COLUMNS: usize> display::Object for Model<COLUMNS> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl<const COLUMNS: usize> component::Model for Model<COLUMNS> {
    fn label() -> &'static str {
        "WideComponentGroupView"
    }

    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let background = background::View::new();
        display_object.add_child(&background);
        let selection_background = background::View::new();
        display_object.add_child(&selection_background);
        let columns: Vec<_> = (0..COLUMNS).map(|i| Column::new(app, ColumnId::new(i))).collect();
        let columns = Rc::new(columns);
        for column in columns.iter() {
            column.set_style_prefix(entry::STYLE_PATH);
            column.hide_selection();
            column.set_background_color(Rgba::transparent());
            column.show_background_shadow(false);
            column.set_background_corners_radius(0.0);
            display_object.add_child(&**column);
        }
        let no_items_label = Label::new(app);

        Model { no_items_label, display_object, background, selection_background, columns }
    }
}

impl<const COLUMNS: usize> Model<COLUMNS> {
    /// Assign a set of layers to render the component group. Must be called after constructing
    /// the [`View`].
    pub fn set_layers(&self, layers: &crate::Layers) {
        layers.normal.background.add_exclusive(&self.background);
        layers.selection.background.add_exclusive(&self.selection_background);
        self.no_items_label.set_layers(&layers.normal.text, &layers.normal.text);
        let layer = &layers.selection.text;
        for column in self.columns.iter() {
            let mut params = column.list_view.entry_params();
            params.selection_layer = Rc::new(Some(layer.downgrade()));
            column.list_view.set_entry_params_and_recreate_entries(params);
            layers.normal.background.add_exclusive(&column.list_view);
            column.list_view.set_label_layer(&layers.normal.text);
        }
    }

    /// Set the text content of the "no items" label.
    fn set_no_items_label_text(&self, text: &str) {
        self.no_items_label.set_content(text);
    }

    /// Make the "no items" label visible.
    fn show_no_items_label(&self) {
        self.display_object.add_child(&self.no_items_label);
    }

    /// Hide the "no items" label.
    fn hide_no_items_label(&self) {
        self.display_object.remove_child(&self.no_items_label);
    }

    /// Returns the rightmost non-empty column with index less or equal to `index`.
    fn non_empty_column(&self, index: ColumnId) -> Option<&Column<COLUMNS>> {
        let indexes_to_the_right = (0..=*index).rev();
        let mut columns_to_the_right = indexes_to_the_right.flat_map(|i| self.columns.get(i));
        columns_to_the_right.find(|col| col.len() > 0)
    }

    /// Send `defocus()` event to each column.
    fn defocus_columns(&self) {
        for column in self.columns.iter() {
            column.defocus();
        }
    }

    /// Deselect entries in all columns except the one with provided `column_index`. We ensure that
    /// at all times only a single entry across all columns is selected.
    fn on_column_selected(&self, column_id: ColumnId) {
        let other_columns = self.columns.iter().enumerate().filter(|(i, _)| *i != *column_id);
        for (_, column) in other_columns {
            column.deselect_entries();
            column.defocus();
        }
    }

    /// Calculate the height of the component. It can't be less than [`MINIMAL_HEIGHT`].
    fn background_height(&self) -> f32 {
        if let Some(largest_column) = self.columns.first() {
            let entry_count_in_largest_column = largest_column.len();
            let background_height = entry_count_in_largest_column as f32 * ENTRY_HEIGHT;
            background_height.max(MINIMAL_HEIGHT)
        } else {
            MINIMAL_HEIGHT
        }
    }
}



// ============
// === View ===
// ============

/// The implementation of the visual component described in the module's documentation.
pub type View<const COLUMNS: usize = DEFAULT_COLUMNS_COUNT> =
    component::ComponentView<Model<COLUMNS>, Frp>;



// ===============
// === Helpers ===
// ===============

/// Return the number of entries in the column with `index`.
fn entry_count_in_column<const COLUMNS: usize>(
    column_id: ColumnId,
    total_entry_count: usize,
) -> usize {
    let evenly_distributed_count = total_entry_count / COLUMNS;
    let remainder = total_entry_count % COLUMNS;
    let has_remainder = remainder > 0;
    let column_contains_remaining_entries = *column_id < remainder;
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
fn local_idx_to_global<const COLUMNS: usize>(
    column: ColumnId,
    entry: entry::Id,
    total_entry_count: usize,
) -> entry::Id {
    let reversed_index =
        reverse_index(entry, entry_count_in_column::<COLUMNS>(column, total_entry_count));
    COLUMNS * reversed_index + *column
}

/// "Reverse" the index in such a way that the first entry becomes the last, and the last becomes
/// the first.
fn reverse_index(index: entry::Id, entries_count: usize) -> entry::Id {
    entries_count.saturating_sub(index).saturating_sub(1)
}
