//! A single block component that is used to build up a flame graph.

use std::f32::consts::E;

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::application::View;
use ensogl_core::data::color;
use ensogl_core::data::color::Lcha;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::event::FocusIn;
use ensogl_core::event::FocusOut;
use ensogl_core::frp;
use ensogl_grid_view as grid_view;
use ensogl_gui_component::component;
use ensogl_gui_component::component::ComponentView;


// =================
// === Constants ===
// =================

/// Size of single entry in pixels.
const ENTRY_HEIGHT: f32 = 25.0;
/// Default maximum external size of the whole dropdown.
const DEFAULT_SIZE: Vector2 = Vector2(150.0, 400.0);
const DEFAULT_COLOR: color::Lcha = color::Lcha::new(0.44682, 0.30307, 0.9668195, 0.7);
/// Total height of search bar element.
const SEARCH_HEIGHT: f32 = 0.0; // ENTRY_HEIGHT; // TODO: add to FRP and default
/// Dropdown corner radius.
const CORNER_RADIUS: f32 = 12.0;



// =========================
// === Shape Definition ===
// =========================

mod background {
    use super::*;
    ensogl_core::shape! {
        (style:Style, color_rgba: Vector4<f32>) {
            let size          = Var::canvas_size();
            let color         = Var::<color::Rgba>::from(color_rgba);
            let overlay       = Rect(size).corners_radius(CORNER_RADIUS.px());
            let overlay       = overlay.fill(color);
            let out           = overlay;
            out.into()
        }
    }
}


// ===========
// === FRP ===
// ===========

pub trait DropdownValue = Debug + Default + Clone + PartialEq + Eq + Hash + 'static;

ensogl_core::define_endpoints_2! { <T: (DropdownValue)>
    Input {
        set_max_size(Vector2),
        set_color(Lcha),

        set_current_values(Vec<DropdownEntry<T>>),

        model_for_entry(usize, DropdownEntry<T>),
        set_number_of_entries(usize),
        /// Force a request for models of all currently displayed entries. Useful to update the
        /// displayed values after the model has been changed.
        request_model_for_visible_entries(),

        set_max_selected(usize), // TODO, now assumed 1

        // shortcuts
        focus_previous_entry(),
        focus_next_entry(),
        accept_focused_entry(),
    }
    Output {
        model_for_entry_needed(usize),
        selected_values(Vec<DropdownEntry<T>>),
        is_focused(bool),
    }
}


impl<T: DropdownValue> component::Frp<Model<T>> for Frp<T> {
    fn init(
        network: &frp::Network,
        api: &Self::Private,
        _app: &Application,
        model: &Model<T>,
        _style: &StyleWatchFrp,
    ) {
        let focus_in = model.background.on_event::<FocusIn>();
        let focus_out = model.background.on_event::<FocusOut>();

        frp::extend! { network
            init <- source_();
            default_size <- init.constant(DEFAULT_SIZE);
            default_color <- init.constant(DEFAULT_COLOR);
            default_max_selection <- init.constant(1);
            max_selection <- any(default_max_selection, api.input.set_max_selected);
            max_size <- any(default_size, api.input.set_max_size);
            dimensions <- all(api.input.set_number_of_entries, max_size);
            color <- any(api.input.set_color, default_color);

            eval_ model.background.events.mouse_down (model.background.focus());
            is_focused <- bool(&focus_out, &focus_in);
            api.output.is_focused <+ is_focused;
            eval dimensions((&(num_entries, max_size)) model.set_dimensions(num_entries, max_size));
            eval color((color) model.set_color(*color));
        }
        frp::extend! { network

            requested_entries <- model.grid.model_for_entry_needed._0();
            requested_entries <- requested_entries.map(f!([model] (row) {
                let cache = model.cache.borrow();
                (*row, cache.get(*row).map(|(e, dirty)| (e.clone(), dirty)))
            }));
        }
        frp::extend! { network

        api.output.model_for_entry_needed <+ requested_entries
            .filter_map(|(row, entry)| entry.as_ref().map_or(true, |(_, dirty)| *dirty).then_some(*row));
        }
        frp::extend! { network
            model.grid.model_for_entry <+ requested_entries.filter_map(f!([model] ((row, entry)) {
                entry.as_ref().map(|(entry, _)| (*row, 0, model_for_entry(entry, model.is_selected(entry))))
            }));
        }
        frp::extend! { network

            visible_range <- model.grid.viewport.map(|viewport| {
                let start = (-viewport.top / ENTRY_HEIGHT).floor() as usize;
                let end = (-viewport.bottom / ENTRY_HEIGHT).ceil() as usize;
                (start, end)
            });


            model.grid.model_for_entry <+ api.input.model_for_entry.map2(
                &visible_range,
                f!([model] ((row, entry), &(start, end)) {
                    model.cache.borrow_mut().insert(*row, entry.clone(), start..=end);
                    (*row, 0, model_for_entry(entry, model.is_selected(&entry)))
                })
            );
        }
        frp::extend! { network

            eval api.input.request_model_for_visible_entries((()) model.cache.borrow_mut().mark_all_dirty());
            model.grid.request_model_for_visible_entries <+ api.input.request_model_for_visible_entries;

        }
        frp::extend! { network

            model.grid.accept_selected_entry <+ api.input.accept_focused_entry;
            model.grid.move_selection_up <+ api.input.focus_previous_entry;
            model.grid.move_selection_down <+ api.input.focus_next_entry;
        }
        frp::extend! { network
            entry_hovered <- model.grid.entry_hovered.filter_map(|l| *l);
            entry_selected <- model.grid.entry_selected.filter_map(|l| *l);
        }
        frp::extend! { network
            pruned <- max_selection.map(f!((max) model.set_max_selection(*max))).on_true();
            selected_values <- model.grid.entry_accepted.map2(&max_selection, f!(((row, _), max) model.select_entry(*row, *max)));
            selection_set <- api.input.set_current_values.map2(&max_selection, f!((values, max) model.set_selection(values, *max)));
        }
        frp::extend! { network

            selection_changed <- any3(&selected_values, &selection_set, &pruned);
            model.grid.request_model_for_visible_entries <+ selection_changed;
        }
        frp::extend! { network
            selected_entries <- selection_changed.map(f!((()) model.get_selected_entries()));
            positions_to_update <- selected_entries.map(f!([model] (entries) {
                let cache = model.cache.borrow();
                entries.iter().filter_map(|entry| {
                    let pos = cache.get_position(&entry.value)?;
                    let entry = model_for_entry(entry, model.is_selected(entry));
                    Some((pos, 0, entry))
                }).collect_vec()
            }));
        }
        frp::extend! { network
            model.grid.model_for_entry <+ positions_to_update.iter();
        }
        frp::extend! { network
            api.output.selected_values <+ selected_entries;
        }
        frp::extend! { network

            eval entry_hovered ([]((row, _col)) warn!("Hovered entry ({row})."));
            eval entry_selected ([]((row, _col)) warn!("Selected entry ({row})."));
            eval model.grid.entry_accepted ([]((row, _col)) warn!("ACCEPTED entry ({row})."));
        }

        init.emit(());
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        [
            (Press, "is_focused", "down", "focus_next_entry"),
            (Press, "is_focused", "up", "select_previous_entry"),
            (Press, "is_focused", "enter", "accept_focused_entry"),
        ]
        .iter()
        .map(|(a, b, c, d)| Dropdown::<T>::self_shortcut_when(*a, *c, *d, *b))
        .collect()
    }
}



// =============
// === Model ===
// =============

#[derive(Derivative, CloneRef, Debug)]
#[derivative(Clone(bound = ""))]
pub struct Model<T> {
    background:       background::View,
    // search_bar: search_bar::Text,
    grid:             Grid,
    display_object:   display::object::Instance,
    selected_entries: Rc<RefCell<HashSet<DropdownEntry<T>>>>,
    cache:            Rc<RefCell<EntryCache<T>>>,
}

impl<T> component::Model for Model<T> {
    fn label() -> &'static str {
        "Dropdown"
    }

    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();

        let background = background::View::new();
        let grid = Grid::new(app);
        // grid.display_object().add_child(&background);
        display_object.add_child(&background);
        display_object.add_child(&grid);

        Model { background, grid, display_object, selected_entries: default(), cache: default() }
    }
}


impl<T: DropdownValue> Model<T> {
    fn set_dimensions(&self, num_entries: usize, max_size: Vector2) {
        let grid_inner_height = num_entries as f32 * ENTRY_HEIGHT;

        let height = (grid_inner_height + SEARCH_HEIGHT).min(max_size.y);
        let width = max_size.x;

        self.background.size.set(Vector2(width, height));
        self.background.set_xy(Vector2(width, -height) / 2.0);
        self.grid.set_y(-SEARCH_HEIGHT);
        self.grid.scroll_frp().resize(Vector2(width, height - SEARCH_HEIGHT));
        self.grid.scroll_frp().set_corner_radius_bottom_right(CORNER_RADIUS);
        self.grid.scroll_frp().set_corner_radius_bottom_left(CORNER_RADIUS);
        self.grid.set_entries_size(Vector2(width, ENTRY_HEIGHT));
        self.grid.resize_grid(num_entries, 1);
    }

    fn set_selection(&self, selected: &[DropdownEntry<T>], max_selected: usize) {
        let mut entries = self.selected_entries.borrow_mut();
        for entry in &selected[..max_selected.min(selected.len())] {
            entries.insert(entry.clone());
        }
    }

    fn select_entry(&self, index: usize, max_selected: usize) {
        let Some((entry, _)) = self.cache.borrow().get(index) else { return };
        let mut entries = self.selected_entries.borrow_mut();
        if entries.len() < max_selected {
            entries.insert(entry);
        } else if max_selected == 1 {
            entries.clear();
            entries.insert(entry);
        }
    }

    fn set_max_selection(&self, max_selected: usize) -> bool {
        let mut entries = self.selected_entries.borrow_mut();
        if entries.len() > max_selected {
            let mut to_drop = entries.len() - max_selected;
            entries.retain(|_| {
                let retain = to_drop == 0;
                to_drop = to_drop.saturating_sub(1);
                retain
            });
            true
        } else {
            false
        }
    }

    fn is_selected(&self, entry: &DropdownEntry<T>) -> bool {
        self.selected_entries.borrow().contains(entry)
    }

    fn get_selected_entries(&self) -> Vec<DropdownEntry<T>> {
        self.selected_entries.borrow().iter().cloned().collect()
    }

    fn set_color(&self, color: Lcha) {
        self.background.color_rgba.set(color::Rgba::from(color).into());
    }
}

impl<T: PartialEq + Hash> display::Object for Model<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

#[derive(Debug)]
struct EntryCache<T> {
    current_generation: usize,
    position_to_entry:  HashMap<usize, DropdownEntryInCache<T>>,
    entry_to_position:  HashMap<T, usize>,
}

#[derive(Debug)]
struct DropdownEntryInCache<T> {
    generation: usize,
    in_flight:  bool,
    entry:      DropdownEntry<T>,
}

impl<T> Default for EntryCache<T> {
    fn default() -> Self {
        Self { current_generation: 0, position_to_entry: default(), entry_to_position: default() }
    }
}

const MAX_CACHE_SIZE: usize = 100;

impl<T> EntryCache<T> {
    fn mark_all_dirty(&mut self) {
        self.current_generation = self.current_generation.wrapping_add(1);
    }

    fn position_is_dirty(&self, position: usize) -> bool {
        self.position_to_entry
            .get(&position)
            .map(|e| e.generation != self.current_generation)
            .unwrap_or(true)
    }

    fn mark_in_flight(&mut self, position: usize) {
        self.position_to_entry.entry(position).and_modify(|e| e.in_flight = true);
    }

    fn insert(
        &mut self,
        position: usize,
        entry: DropdownEntry<T>,
        visible_range: RangeInclusive<usize>,
    ) where
        T: Clone + Hash + Eq,
    {
        if self.position_to_entry.len()
            > MAX_CACHE_SIZE.max(visible_range.end() - visible_range.start())
        {
            self.prune_cache(visible_range);
        }

        self.entry_to_position.insert(entry.value.clone(), position);
        self.position_to_entry.insert(position, DropdownEntryInCache {
            generation: self.current_generation,
            entry,
            in_flight: false,
        });
    }

    fn prune_cache(&mut self, visible_range: RangeInclusive<usize>) {
        self.position_to_entry.retain(|k, _| visible_range.contains(k));
        self.entry_to_position.retain(|_, v| visible_range.contains(v));
    }

    /// Get entry value and dirty status.
    fn get(&self, position: usize) -> Option<(DropdownEntry<T>, bool)>
    where T: Clone {
        self.position_to_entry
            .get(&position)
            .map(|e| (e.entry.clone(), e.generation != self.current_generation))
    }

    fn get_position(&self, value: &T) -> Option<usize>
    where T: Hash + Eq {
        self.entry_to_position.get(value).copied()
    }
}


// =================
// === Component ===
// =================

#[allow(missing_docs)]
pub type Dropdown<T> = ComponentView<Model<T>, Frp<T>>;

// GRID
type DropdownGridEntry = grid_view::simple::Entry;
type Grid = grid_view::scrollable::SelectableGridView<DropdownGridEntry>; // selectable is probably wrong, need multiselect

type InnerEntryModel = <DropdownGridEntry as grid_view::entry::Entry>::Model;


// POD data struct for dropdown selection
#[derive(Debug, Clone, Default)]
pub struct DropdownEntry<T> {
    pub value: T, // any generic "id" value, can be just position
    pub label: ImString,
}

impl<T> DropdownEntry<T> {
    pub fn new(value: T, label: ImString) -> Self {
        Self { value, label }
    }
}

impl<T: Hash> Hash for DropdownEntry<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl<T: PartialEq> PartialEq for DropdownEntry<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for DropdownEntry<T> {}

fn model_for_entry<T>(input: &DropdownEntry<T>, selected: bool) -> InnerEntryModel {
    if selected {
        InnerEntryModel::new(&format!("[{}]", input.label))
    } else {
        InnerEntryModel::new(&input.label)
    }
}

struct Layout {
    entry_position: Vector2,
}
