//! Breadcrumbs of the Component Browser. It displays a stack of entered module names.
//!
//! To learn more about the Component Browser and its components, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
//!
//! The Breadcrumbs component is displayed as a horizontal list composed of individual breadcrumb
//! entries (simple text labels) separated by [`entry::Model::Separator`] icons and an optional
//! [`entry::Model::Ellipsis`] icon at the end of the list. The ellipsis icon shows that the last
//! module has ancestors and can be further expanded.
//!
//! The selection of the breadcrumbs can be controlled by both the mouse and the keyboard.
//! After switching to a higher-level name, the lower-level names should become grayed out, letting
//! the user to switch back fast.
//!
//! The implementation is based on the [`grid_view::GridView`] with a single row and variable
//! number of columns. A custom entry type for the Grid View is implemented in the [`entry`]
//! module. Each entry has three different representations: a text label, a separator icon and an
//! ellipsis icon, and can switch between these representations if needed.

#![recursion_limit = "1024"]
// === Features ===
#![feature(option_result_contains)]
#![feature(derive_default_enum)]
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

use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::layer::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::gui::Widget;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::Viewport;
use ensogl_hardcoded_theme::application::component_browser::searcher::list_panel::breadcrumbs as theme;
use entry::Entry;
use grid_view::Col;



mod entry;



// ====================
// === Type Aliases ===
// ====================

type GridView = grid_view::selectable::GridView<Entry>;
type Entries = Rc<RefCell<Vec<Breadcrumb>>>;
type BreadcrumbId = usize;



// ============
// === Mask ===
// ============

/// A rectangular mask used to crop the content of the breadcrumbs when it doesn't fit in the
/// designated space.
mod mask {
    use super::*;
    use ensogl_core::display::shape::*;
    ensogl_core::define_shape_system! {
        pointer_events = false;
        (style: Style, corners_radius: f32) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            Rect((width, height)).corners_radius(corners_radius).fill(color::Rgba::black()).into()
        }
    }
}



// ==============
// === Layers ===
// ==============

/// A set of layers used by the breadcrumbs.
#[derive(Debug, Clone, CloneRef)]
struct Layers {
    main: Layer,
    text: Layer,
    mask: Layer,
}

impl Layers {
    /// Constructor.
    pub fn new(app: &Application, base_layer: &Layer) -> Self {
        let mask = Layer::new(app.logger.sub("BreadcrumbsMask"));
        let main = base_layer.create_sublayer();
        let text = main.create_sublayer();
        main.set_mask(&mask);
        Layers { main, text, mask }
    }
}



// =============
// === Model ===
// =============

/// A breadcrumbs model.
#[derive(Debug, Clone, CloneRef)]
pub struct Model {
    display_object: display::object::Instance,
    grid:           GridView,
    entries:        Entries,
    network:        frp::Network,
    mask:           mask::View,
    layers:         Layers,
    show_ellipsis:  Rc<Cell<bool>>,
    params:         Rc<RefCell<entry::Params>>,
}

impl Model {
    /// Constructor.
    pub fn new(app: &Application, layer: &Layer) -> Self {
        let layers = Layers::new(app, layer);
        let display_object = display::object::Instance::new(&app.logger);
        let mask = mask::View::new(&app.logger);
        display_object.add_child(&mask);
        layers.mask.add_exclusive(&mask);
        layers.main.add_exclusive(&display_object);
        let grid = GridView::new(app);
        grid.set_text_layer(Some(layers.text.downgrade()));
        let params = entry::Params::default();
        grid.set_entries_params(params.clone());
        grid.reset_entries(1, 0);
        display_object.add_child(&grid);
        let entries: Entries = default();
        let show_ellipsis = Rc::new(Cell::new(false));
        frp::new_network! { network
            requested_entry <- grid.model_for_entry_needed.map2(&grid.grid_size,
                f!([entries, show_ellipsis]((row, col), grid_size) {
                    let (_, cols) = grid_size;
                    (*row, *col, Self::entry_model(&entries, *col, show_ellipsis.get(), *cols))
                })
            );
            grid.model_for_entry <+ requested_entry;
        }
        Self {
            display_object,
            grid,
            entries,
            network,
            mask,
            layers,
            show_ellipsis,
            params: Rc::new(RefCell::new(params)),
        }
    }

    /// Update the displayed height of the breadcrumbs entries.
    fn update_entries_height(&self, height: f32) {
        // The width of the entries is not important, because each entry will set it according to
        // its own size.
        let width = 0.0;
        self.grid.set_entries_size(Vector2(width, height));
    }

    /// Update the position and the viewport of the underlying Grid View. If the content does not
    /// fit into [`size`], it is cropped by the rectangular [`mask`] and shifted left, so that
    /// the user always sees the right (most-important) part of the breadcrumbs.
    fn update_layout(&self, content_size: Vector2, size: Vector2) {
        self.mask.size.set(size);
        let grid_view_center = Vector2(size.x / 2.0, -size.y / 2.0);
        self.mask.set_position_xy(grid_view_center);
        let offset = (content_size.x - size.x).max(0.0);
        self.grid.set_position_x(-offset);
        let right = offset + size.x;
        let vp = Viewport { top: 0.0, bottom: -size.y, left: offset, right };
        self.grid.set_viewport(vp);
    }

    /// A model for the specific entry. Every second entry of the grid view is an actual
    /// breadcrumb. They are separated by the [`entry::Model::Separator`] entries and can have an
    /// optional [`entry::Model::Ellipsis`] icon as the last entry (if [`show_ellipsis`] is true).
    fn entry_model(
        entries: &Entries,
        col: Col,
        show_ellipsis: bool,
        number_of_cols: Col,
    ) -> entry::Model {
        let is_last = col == number_of_cols - 1;
        let is_not_first = col != 0;
        let is_separator_index = col % 2 == 1;
        if show_ellipsis && is_last && is_not_first {
            entry::Model::Ellipsis
        } else if is_separator_index {
            entry::Model::Separator
        } else if let Some(entry) = entries.borrow().get(col / 2) {
            entry::Model::Text(entry.0.clone_ref())
        } else {
            tracing::error!("Requested entry is missing in the breadcrumbs ({col})");
            entry::Model::default()
        }
    }

    /// A number of columns in the grid view. It depends on the number of entries and whether the
    /// ellipsis icon is displayed.
    fn grid_columns(&self) -> Col {
        let entries_count = self.entries.borrow().len();
        let is_not_empty = entries_count != 0;
        let ellipsis = if self.show_ellipsis.get() && is_not_empty { 2 } else { 0 };
        (entries_count * 2).saturating_sub(1) + ellipsis
    }

    /// The column index of the last right-most displayed breadcrumb. Returns [`None`] if there
    /// are no breadcrumbs displayed.
    fn column_of_the_last_entry(&self) -> Option<Col> {
        if self.entries.borrow().is_empty() {
            None
        } else {
            if self.show_ellipsis.get() {
                Some(self.grid_columns().saturating_sub(3))
            } else {
                Some(self.grid_columns().saturating_sub(1))
            }
        }
    }

    /// Enable or disable showing of the ellipsis icon at the end of breadcrumbs stack.
    pub fn show_ellipsis(&self, show: bool) {
        if self.show_ellipsis.get() != show {
            self.show_ellipsis.set(show);
            let new_cols = self.grid_columns();
            self.grid.resize_grid(1, new_cols);
            // TODO: An API for partial update in the Grid View?
            self.grid.request_model_for_visible_entries();
        }
    }

    /// Mark entries as greyed out starting from supplied column index. Cancel greying out if
    /// [`None`] is supplied.
    pub fn grey_out(&self, from: Option<Col>) {
        let params = {
            let mut borrowed = self.params.borrow_mut();
            borrowed.greyed_out_start = from;
            borrowed.clone()
        };
        self.grid.set_entries_params(params);
    }

    /// Push a new breadcrumb to the top of the stack. Immediately selects added breadcrumb.
    pub fn push(&self, breadcrumb: &Breadcrumb) {
        self.entries.borrow_mut().push(breadcrumb.clone_ref());
        let new_col_count = self.grid_columns();
        self.grid.resize_grid(1, new_col_count);
        // TODO: An API for partial update in the Grid View?
        self.grid.request_model_for_visible_entries();
        if let Some(last_entry) = self.column_of_the_last_entry() {
            self.grid.select_entry(Some((0, last_entry)));
        }
    }

    /// Move the selection to the previous breadcrumb. Stops at the first one, there is always at
    /// least one breadcrumb selected.
    pub fn move_up(&self) {
        if let Some((row, col)) = self.grid.entry_selected.value() {
            if col != 0 {
                self.grid.select_entry(Some((row, col.saturating_sub(2))));
            }
        } else {
            if let Some(last) = self.column_of_the_last_entry() {
                self.grid.select_entry(Some((0, last)));
            }
        }
    }

    /// Move the selection to the next breadcrumb. Stops at the last one, there is always at
    /// least one breadcrumb selected.
    pub fn move_down(&self) {
        if let Some((row, col)) = self.grid.entry_selected.value() {
            if let Some(last) = self.column_of_the_last_entry() {
                if col < last {
                    self.grid.select_entry(Some((row, col + 2)));
                }
            }
        }
    }

    /// Clear the breadcrumbs list.
    pub fn clear(&self) {
        self.entries.borrow_mut().clear();
        self.grey_out(None);
        self.grid.resize_grid(1, 0);
    }
}


// === Breadcrumb ===

/// The breadcrumb type.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct Breadcrumb(ImString);

impl Breadcrumb {
    /// Constructor.
    pub fn new(label: &str) -> Self {
        Self(ImString::new(label))
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        /// Select a specific breadcrumb, greying out breadcrumbs after it.
        select(BreadcrumbId),
        /// Add a new breadcrumb to the end of the list.
        push(Breadcrumb),
        /// Set a list of displayed breadcrumbs, rewriting any previously added breadcrumbs.
        set_entries(Vec<Breadcrumb>),
        /// Enable or disable displaying of the ellipsis icon at the end of the list.
        show_ellipsis(bool),
        /// Remove all breadcrumbs.
        clear(),
        /// Set a size of the visible portion of the breadcrumbs. The widget will crop the
        /// breadcrumbs to this size and will prioritize showing the right part of the list if it
        /// can't fit in completely.
        set_size(Vector2),
        /// Move the selection to the previous breadcrumb in the list.
        move_up(),
        /// Move the selection to the next breadcrumb in the list.
        move_down(),
    }
    Output {
        /// Currently selected breadcrumb.
        selected(BreadcrumbId)
    }
}



/// ==============
/// === Widget ===
/// ==============

#[derive(Debug, Clone, CloneRef, Deref)]
pub struct Breadcrumbs {
    #[deref]
    widget: Widget<Model, Frp>,
}

impl Breadcrumbs {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app, &app.display.default_scene.layers.node_searcher));
        let display_object = model.display_object.clone_ref();
        let frp = Frp::new();
        let network = frp.network();
        let input = &frp.private().input;
        let out = &frp.private().output;
        let grid = &model.grid;
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let entries_height = style.get_number(theme::height);
        frp::extend! { network
            init <- source_();
            eval input.push((breadcrumb) model.push(breadcrumb));
            entries <= input.set_entries.map(f!((e) { model.clear(); e.clone() }));
            eval entries((entry) model.push(entry));
            eval_ input.clear(model.clear());
            eval input.show_ellipsis((b) model.show_ellipsis(*b));
            entry_selected <- grid.entry_selected.filter_map(|l| *l);
            eval entry_selected([model]((_row, col)) {
                model.grey_out(Some(col + 1));
            });
            selected <- entry_selected.map(|(_, col)| col / 2);
            eval selected([](id) tracing::debug!("Selected breadcrumb: {id}"));
            out.selected <+ selected;
            _eval <- all_with(&model.grid.content_size, &input.set_size,
                f!((content_size, size) model.update_layout(*content_size, *size))
            );
            eval_ input.move_up(model.move_up());
            eval_ input.move_down(model.move_down());
            entries_height <- all(&entries_height, &init)._0();
            eval entries_height((height) model.update_entries_height(*height));
        }
        init.emit(());

        let widget = Widget::new(app, frp, model, display_object);
        Self { widget }
    }
}

impl ensogl_core::application::View for Breadcrumbs {
    fn label() -> &'static str {
        "Breadcrumbs"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }

    fn app(&self) -> &Application {
        self.widget.app()
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl_core::application::shortcut::ActionType::*;
        (&[(Press, "shift enter", "move_up"), (Press, "ctrl shift enter", "move_down")])
            .iter()
            .map(|(a, b, c)| Self::self_shortcut(*a, *b, *c))
            .collect()
    }
}

impl FrpNetworkProvider for Breadcrumbs {
    fn network(&self) -> &frp::Network {
        self.widget.frp().network()
    }
}

impl display::Object for Breadcrumbs {
    fn display_object(&self) -> &display::object::Instance {
        self.widget.display_object()
    }
}
