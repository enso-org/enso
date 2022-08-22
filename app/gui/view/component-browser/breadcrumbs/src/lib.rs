//! Breadcrumbs EnsoGL Component.

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

use enso_frp as frp;
use enso_frp::Network;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::frp::API;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::layer::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::gui::Widget;
use ensogl_core::prelude::*;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::Viewport;
use ensogl_hardcoded_theme::application::component_browser::searcher::list_panel::breadcrumbs as theme;
use grid_view::Col;


type GridView = grid_view::selectable::GridView<Entry>;
type Entries = Rc<RefCell<Vec<Breadcrumb>>>;

mod entry;

use entry::Entry;

#[derive(Debug, Clone, CloneRef)]
struct Layers {
    main: Layer,
    text: Layer,
    mask: Layer,
}

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
    pub fn new(app: &Application, layer: &Layer) -> Self {
        let mask = Layer::new(app.logger.sub("BreadcrumbsMask"));
        let main = layer.create_sublayer();
        let text = main.create_sublayer();
        main.set_mask(&mask);
        let layers = Layers { main, text, mask };
        let display_object = display::object::Instance::new(&app.logger);
        let mask = mask::View::new(&app.logger);
        display_object.add_child(&mask);
        layers.mask.add_exclusive(&mask);
        layers.main.add_exclusive(&display_object);
        let grid = GridView::new(app);
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
        grid.set_text_layer(Some(layers.text.downgrade()));
        let params = entry::Params::default();
        grid.set_entries_params(params.clone());
        grid.reset_entries(1, 0);
        display_object.add_child(&grid);
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

    fn update_entries_height(&self, height: f32) {
        // The width of the entries is not important, because each entry will set it according to
        // its own size.
        let width = 0.0;
        self.grid.set_entries_size(Vector2(width, height));
    }

    fn entry_model(
        entries: &Entries,
        col: Col,
        show_ellipsis: bool,
        number_of_cols: Col,
    ) -> entry::Model {
        let is_last = col == number_of_cols - 1;
        let is_not_first = col != 0;
        let is_separator_index = col % 2 == 1;
        let model = if show_ellipsis && is_last && is_not_first {
            entry::Model::Ellipsis
        } else if is_separator_index {
            entry::Model::Separator
        } else if let Some(entry) = entries.borrow().get(col / 2) {
            entry::Model::Text(entry.label.clone_ref())
        } else {
            tracing::error!("Requested entry is missing in the breadcrumbs ({col})");
            entry::Model::default()
        };
        model
    }

    fn grid_columns(&self) -> Col {
        let entries_count = self.entries.borrow().len();
        let is_not_empty = entries_count != 0;
        let ellipsis = if self.show_ellipsis.get() && is_not_empty { 2 } else { 0 };
        (entries_count * 2).saturating_sub(1) + ellipsis
    }

    fn last_entry(&self) -> Option<BreadcrumbId> {
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

    pub fn show_ellipsis(&self, show: bool) {
        if self.show_ellipsis.get() != show {
            self.show_ellipsis.set(show);
            let new_cols = self.grid_columns();
            self.grid.resize_grid(1, new_cols);
            self.grid.reset_entries(1, new_cols);
        }
    }

    pub fn grey_out(&self, from: Option<usize>) {
        let params = {
            let mut borrowed = self.params.borrow_mut();
            borrowed.greyed_out_start = from;
            borrowed.clone()
        };
        self.grid.set_entries_params(params);
    }

    pub fn push(&self, breadcrumb: &Breadcrumb) {
        self.entries.borrow_mut().push(breadcrumb.clone_ref());
        let new_col_count = self.grid_columns();
        self.grid.resize_grid(1, new_col_count);
        self.grid.reset_entries(1, new_col_count);
        if let Some(last_entry) = self.last_entry() {
            self.grid.select_entry(Some((0, last_entry)));
        }
    }

    pub fn move_up(&self) {
        let selected = self.grid.entry_selected.value();
        if let Some((row, col)) = selected {
            if col != 0 {
                self.grid.select_entry(Some((row, col.saturating_sub(2))));
            }
        } else {
            if let Some(last) = self.last_entry() {
                self.grid.select_entry(Some((0, last)));
            }
        }
    }

    pub fn move_down(&self) {
        let selected = self.grid.entry_selected.value();
        if let Some((row, col)) = selected {
            if let Some(last) = self.last_entry() {
                if col < last {
                    self.grid.select_entry(Some((row, col + 2)));
                }
            }
        }
    }

    pub fn clear(&self) {
        self.entries.borrow_mut().clear();
        self.grey_out(None);
        self.grid.resize_grid(1, 0);
        self.grid.reset_entries(1, 0);
    }
}

#[derive(Debug, Clone, CloneRef, Default)]
pub struct Breadcrumb {
    label: ImString,
}

impl Breadcrumb {
    pub fn new(label: &str) -> Self {
        Self { label: ImString::new(label) }
    }
}

type BreadcrumbId = usize;

ensogl_core::define_endpoints_2! {
    Input {
        select(BreadcrumbId),
        push(Breadcrumb),
        set_entries(Vec<Breadcrumb>),
        show_ellipsis(bool),
        clear(),
        set_size(Vector2),
        move_up(),
        move_down(),
    }
    Output {
        selected(BreadcrumbId)
    }
}

#[derive(Debug, Clone, CloneRef, Deref)]
pub struct Breadcrumbs {
    #[deref]
    widget: Widget<Model, Frp>,
}

impl Breadcrumbs {
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
            eval entry_selected([model]((row, col)) {
                model.grey_out(Some(col + 1));
            });
            selected <- entry_selected.map(|(_, col)| col / 2);
            eval selected([](id) tracing::debug!("Selected breadcrumb: {id}"));
            out.selected <+ selected;
            _eval <- all_with(&model.grid.content_size, &input.set_size,
                f!([model](content_size, size) {
                    model.mask.size.set(*size);
                    model.mask.set_position_xy(Vector2(size.x / 2.0, -size.y / 2.0));
                    let offset_left = (content_size.x - size.x).max(0.0);
                    model.grid.set_position_x(-offset_left);
                    let left = offset_left;
                    let right = offset_left + size.x;
                    let vp = Viewport { top: 0.0, bottom: -size.y, left, right };
                    model.grid.set_viewport(vp);
                })
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
    fn network(&self) -> &Network {
        self.widget.frp().network()
    }
}

impl display::Object for Breadcrumbs {
    fn display_object(&self) -> &display::object::Instance {
        self.widget.display_object()
    }
}
