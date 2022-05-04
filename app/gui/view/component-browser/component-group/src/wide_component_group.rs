//! A multi-column [Component Group] without header.
//!
//! The widget is defined by the [`View`].
//!
//! To learn more about component groups, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
//!
//! [Component Group]: crate::component_group::View

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::application::traits::*;
use ensogl_core::data::color::Rgba;
use ensogl_core::display;
use ensogl_core::Animation;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_gui_component::component;
use ensogl_label::Label;
use ensogl_list_view as list_view;
use list_view::entry::AnyModelProvider;
use crate::EntryId;



// =================
// === Constants ===
// =================

const NO_ITEMS_LABEL_TEXT: &str = "No local variables";
const MINIMAL_HEIGHT: f32 = list_view::entry::HEIGHT;
const COLUMNS: usize = 3;



// ==========================
// === Shapes Definitions ===
// ==========================


// === Background ===

/// The background of the Wide Component Group.
pub mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        below = [list_view::background];
        (style:Style, color:Vector4) {
            let sprite_width: Var<Pixels> = "input_size.x".into();
            let sprite_height: Var<Pixels> = "input_size.y".into();
            let color = Var::<Rgba>::from(color);
            let shape = Rect((&sprite_width, &sprite_height)).fill(color);
            shape.into()
        }
    }
}



// =====================
// === ModelProvider ===
// =====================

type Entry = list_view::entry::Label;

/// A special [`list_view::entry::ModelProvider`] wrapper that splits the entries into `COLUMNS`
/// lists.
///
/// Entries are distributed evenly between lists. If the entry count is not divisible by `COLUMNS` -
/// the lists with lower indices will have more entries.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct ModelProvider {
    inner: AnyModelProvider<Entry>,
    index: Immutable<usize>,
}

impl ModelProvider {
    /// Wrap [`AnyModelProvider`] and split its entries into `COLUMNS` lists. The returned instance
    /// provides entries for column with `index`.
    fn wrap(inner: &AnyModelProvider<Entry>, index: usize) -> AnyModelProvider<Entry> {
        AnyModelProvider::new(Self { inner: inner.clone_ref(), index: Immutable(index) })
    }
}

impl list_view::entry::ModelProvider<Entry> for ModelProvider {
    fn entry_count(&self) -> usize {
        let total_count = self.inner.entry_count();
        Model::entry_count_in_column(*self.index, total_count)
    }

    fn get(&self, id: list_view::entry::Id) -> Option<String> {
        let idx = (self.entry_count() - 1 - id) * COLUMNS + *self.index;
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
        set_header_text(String),
        set_entries(AnyModelProvider<Entry>),
        set_background_color(Rgba),
        set_width(f32),
    }
    Output {
        selected_entry(Option<EntryId>),
        suggestion_accepted(EntryId),
        expression_accepted(EntryId),
        selection_size(Vector2<f32>),
        selection_position_target(Vector2<f32>),
        size(Vector2<f32>)
    }
}

impl component::Frp<Model> for Frp {
    fn init(api: &Self::Private, _app: &Application, model: &Model, _style: &StyleWatchFrp) {
        let network = &api.network;
        let input = &api.input;
        let out = &api.output;
        let background_height = Animation::new(network);
        frp::extend! { network
            eval input.set_background_color((c) model.background.color.set(c.into()));
            selected_entry <- any_mut();
            some_selected_entry <- selected_entry.filter_map(|&(entry, group): &(Option<usize>, usize)| if entry.is_some() { Some(group) } else { None });
            eval some_selected_entry ((group) model.on_selected_entry(*group));
            out.selected_entry <+ selected_entry.map(|(entry, _)| *entry);


            // === Background size ===

            let background_width = input.set_width.clone_ref();
            column_width <- background_width.map(|w| *w / COLUMNS as f32);

            entry_count <- input.set_entries.map(|p| p.entry_count());
            background_height.target <+ entry_count.map(|c| Model::background_height(*c));
            size <- all_with(&background_width, &background_height.value,
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
                accepted_entry <- column.selected_entry.sample(&input.accept_suggestion);
                out.suggestion_accepted <+ accepted_entry.filter_map(|e| *e);
                out.expression_accepted <+ column.chosen_entry.filter_map(|e| *e);
            }

            frp::extend! { network
                column_is_active <- column.selected_entry.map(|e| e.is_some());
                selection_position <- column.selection_position_target.gate(&column_is_active);
                out.selection_position_target <+ selection_position.map(f!((pos) model.selection_position(idx, *pos)));
            }
            frp::extend! { network
                selected_entry <+ column.frp.output.selected_entry.map(move |entry| (*entry, idx));

                let column = column.clone_ref();
                entries <- input.set_entries.map(move |p| ModelProvider::wrap(p, idx));
                entry_count_in_column <- entries.map(|p| p.entry_count());
                column.set_entries <+ entries;

                // === Columns size and position ===

                column_height <- entry_count_in_column.map(|count| *count as f32 * list_view::entry::HEIGHT);
                _eval <- all_with3(&column_width, &column_height, &background_height.value, f!(
                        [column](&width, &height, &bg_height) {
                            column.resize(Vector2(width, height));
                            let left_border = -(COLUMNS as f32 * width / 2.0) + width / 2.0;
                            column.set_position_x(left_border + width * idx as f32);
                            let half_height = height / 2.0;
                            let background_bottom = -bg_height / 2.0;
                            column.set_position_y(background_bottom + half_height);
                        }));

                // === Clear default list view background ===

                column.set_background_color(Rgba(1.0, 1.0, 1.0, 0.0));
                column.show_background_shadow(false);
                column.set_background_corners_radius(0.0);
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



// =============
// === Model ===
// =============

/// The Model of the [`View`] component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object: display::object::Instance,
    background:     background::View,
    columns:        Rc<[list_view::ListView<list_view::entry::Label>; COLUMNS]>,
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
        let columns = Rc::new([
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
            app.new_view::<list_view::ListView<list_view::entry::Label>>(),
        ]);
        for column in columns.iter() {
            column.hide_selection();
            display_object.add_child(column);
        }
        let no_items_label = Label::new(app);
        no_items_label.set_content(NO_ITEMS_LABEL_TEXT);

        Model { no_items_label, display_object, background, columns }
    }
}

impl Model {
    fn show_no_items_label(&self) {
        self.display_object.add_child(&self.no_items_label);
    }

    fn hide_no_items_label(&self) {
        self.display_object.remove_child(&self.no_items_label);
    }

    /// Deselect entries in all columns except the one with provided `column_index`. We ensure that
    /// at all times only one entry across all columns will be selected.
    fn on_selected_entry(&self, column_index: usize) {
        let other_columns = self.columns.iter().enumerate().filter(|(i, _)| *i != column_index);
        for (_, column) in other_columns {
            column.deselect_entries();
        }
    }

    /// Calculate the height of the component. It can't be less than [`MINIMAL_HEIGHT`].
    fn background_height(total_entry_count: usize) -> f32 {
        let entry_count_in_largest_column = Self::entry_count_in_column(0, total_entry_count);
        let background_height = entry_count_in_largest_column as f32 * list_view::entry::HEIGHT;
        background_height.max(MINIMAL_HEIGHT)
    }

    /// Return the number of entries in column with `index`.
    fn entry_count_in_column(index: usize, total_entry_count: usize) -> usize {
        let evenly_distributed_count = total_entry_count / COLUMNS;
        let remainder = total_entry_count % COLUMNS;
        let has_remainder = remainder > 0;
        let column_contains_remaining_entries = index < remainder;
        if has_remainder && column_contains_remaining_entries {
            evenly_distributed_count + 1
        } else {
            evenly_distributed_count
        }
    }

    fn selection_position(
        &self,
        index: usize,
        entries_selection_position: Vector2,
    ) -> Vector2 {
        let column = self.columns.get(index);
        if let Some(column) = column {
            column.position().xy() + entries_selection_position
        } else {
            WARNING!("Attempt to access nonexisting column {index}.");
            Vector2::zero()
        }
    }
}



// ============
// === View ===
// ============

/// The implementation of the visual component described in the module's documentation.
pub type View = component::ComponentView<Model, Frp>;
