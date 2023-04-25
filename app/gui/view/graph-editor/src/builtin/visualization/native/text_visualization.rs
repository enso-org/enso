//! Text visualisation that can show text based data from the backend. If the text is larger than
//! the available space, it will use lazy loading to request only a subset of the data to
//! display. This is useful for large texts to avoid overwhelming the visualisation.
//!
//! The visualisation is made up of text `chunks` that are cached and will be requested form the
//! backend. The size of the chunks is determined by the `chunk_size` parameter and each hunk is
//! shown as a cell in a grid.
//!
//! Example:
//! ```text
//! ┌──────────┬──────────┬────
//! │chunk(0,0)│chunk(1,0)│ ...
//! ├──────────┼──────────┼───
//! │chunk(0,1 │chunk(1,1)│ ...
//! ├──────────┼──────────┼───
//! │ ...      │ ....     │ ...
//! ```



mod grid_cache;
mod grid_view_entry;
pub mod text_provider;

use crate::prelude::*;
use ensogl::system::web::traits::*;

use crate::component::visualization;
use crate::StyleWatchFrp;
use enso_frp as frp;
use enso_prelude::serde_reexports::Deserialize;
use enso_prelude::serde_reexports::Serialize;
use ensogl::application::frp::API;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::DomSymbol;
use ensogl::prelude::FrpNetworkProvider;
use ensogl::system::web;
use ensogl::system::web::CanvasRenderingContext2d;
use ensogl_component::grid_view;
use ensogl_component::grid_view::GridView;
use ensogl_component::scrollbar;
use ensogl_component::scrollbar::Scrollbar;
use ensogl_hardcoded_theme as theme;



// =================
// === Constants ===
// =================

/// Number of characters that can be displayed in one grid cell and will be requested together from
/// the engine. Also referred to as `chunk`. This value can be changed to tweak the size of the
/// messages sent to the visualisation as well as the caching performance. A larger value will
/// result in fewer, smaller messages, but the visualisation might have to load more data that is
/// not needed, as it will be cropped. For example, a value of 100, would mean that the
/// visualisation would request 100 characters per chunk, even if it can only show 10 characters at
/// once in the available viewport.
const CHARS_PER_CHUNK: usize = 20;
/// Extra chunks to load around the visible grid to ensure smooth scrolling. Extra chunks are
/// loaded in each direction around the visible grid. So a value of 5 with a base grid of 20x10 will
/// load 25x15 grid.
const CACHE_PADDING: u32 = 15;
const PADDING_TEXT: f32 = 10.0;



// =============================
// === Grid Coordinate Types ===
// =============================

type GridPosition = Vector2<i32>;
type GridSize = Vector2<i32>;
type GridVector = Vector2<i32>;

/// Position and size of a sub-grid within a karger grid.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct GridWindow {
    position: GridPosition,
    size:     GridSize,
}

use text_provider::BackendTextProvider;
use text_provider::TextProvider;



// =============
// === Model ===
// =============

#[derive(Debug)]
#[allow(missing_docs)]
pub struct Model<T> {
    app:                   Application,
    size:                  Rc<Cell<Vector2>>,
    root:                  display::object::Instance,
    // Note that we are using a simple `GridView` and our own scrollbar, instead of the
    // `scrollable::GridView` to avoid adding `ScrollAreas` to the scene, as the clipping they
    // provide though the `mask::View` is not free in terms of performance (they add a draw call
    // cost) and we don't need it here because we need to clip DOM elements anyway.
    text_grid:             GridView<grid_view_entry::Entry>,
    dom_entry_root:        web::HtmlDivElement,
    clipping_div:          DomSymbol,
    scroll_bar_horizontal: Scrollbar,
    scroll_bar_vertical:   Scrollbar,
    text_provider:         Rc<RefCell<Option<T>>>,
}

impl<T: 'static> Model<T> {
    /// Constructor.
    fn new(app: Application) -> Self {
        let root = display::object::Instance::new();
        let clipping_div = web::document.create_div_or_panic();
        let clipping_div = DomSymbol::new(&clipping_div);
        let dom_entry_root = web::document.create_div_or_panic();
        let size = default();
        let text_provider = default();

        let text_grid: GridView<grid_view_entry::Entry> = GridView::new(&app);
        root.add_child(&text_grid);

        let scroll_bar_horizontal = Scrollbar::new(&app);
        let scroll_bar_vertical = Scrollbar::new(&app);

        Model {
            app,
            text_grid,
            clipping_div,
            size,
            dom_entry_root,
            scroll_bar_horizontal,
            scroll_bar_vertical,
            root,
            text_provider,
        }
        .init()
    }

    fn init(self) -> Self {
        self.init_dom();
        self.init_scrollbars();
        self
    }

    fn init_dom(&self) {
        let scene = &self.app.display.default_scene;

        // The `clipping_div` needs to cut of its content.
        self.clipping_div.set_style_or_warn("overflow", "hidden");
        self.clipping_div.set_style_or_warn("z-index", "2");
        scene.dom.layers.back.manage(&self.clipping_div);
        self.root.add_child(&self.clipping_div);

        // The `dom_entry_root` is a container for the elements and its position is not managed
        // through the display object hierarchy to a avoid issues with mixing the DOM and EnsoGL
        // object hierarchies. See https://www.pivotaltracker.com/n/projects/2539304/stories/184051310
        // for more ramification.
        self.dom_entry_root.set_style_or_warn("position", "absolute");
        self.clipping_div.append_or_warn(&self.dom_entry_root);
    }

    fn init_scrollbars(&self) {
        self.root.add_child(&self.scroll_bar_horizontal);
        self.root.add_child(&self.scroll_bar_vertical);
        self.scroll_bar_vertical.set_rotation_z(-90.0_f32.to_radians());

        self.app.display.default_scene.layers.main.add(&self.scroll_bar_horizontal);
        self.app.display.default_scene.layers.main.add(&self.scroll_bar_vertical);
    }

    fn set_size(&self, size: Vector2) {
        self.scroll_bar_horizontal.set_y(-size.y / 2.0);
        self.scroll_bar_horizontal.set_length(size.x);
        let scrollbar_width = scrollbar::WIDTH - scrollbar::PADDING;
        self.scroll_bar_horizontal.modify_y(|y| *y += scrollbar_width / 2.0);
        self.scroll_bar_vertical.set_x(size.x / 2.0);
        self.scroll_bar_vertical.set_length(size.y);
        self.scroll_bar_vertical.modify_x(|x| *x -= scrollbar_width / 2.0);
        let text_padding = Vector2::new(PADDING_TEXT, PADDING_TEXT);
        self.clipping_div.set_dom_size(size - 2.0 * text_padding);
        self.size.set(size);
    }

    fn set_text_provider(&self, text_provider: T) {
        self.text_provider.replace(Some(text_provider));
    }
}

impl<T: TextProvider> Model<T> {
    fn get_string_for_cell(&self, row: usize, column: usize) -> String {
        self.text_provider
            .borrow()
            .as_ref()
            .and_then(|text_provider| text_provider.get_slice(row, column))
            .unwrap_or_default()
    }
}


// ================
// === TextGrid ===
// ================

/// Sample visualization that renders the given data as text. Useful for debugging and testing.
#[derive(Debug, Deref)]
#[allow(missing_docs)]
pub struct TextGrid<T> {
    #[deref]
    model:                 Rc<Model<T>>,
    pub frp:               visualization::instance::Frp,
    network:               frp::Network,
    fonts_loaded_notifier: FontLoadedNotifier,
}

impl<T: TextProvider + 'static> TextGrid<T> {
    /// Constructor.
    pub fn new(app: Application) -> Self {
        let network = frp::Network::new("visualization_text_grid");
        let frp = visualization::instance::Frp::new(&network);
        let model = Rc::new(Model::new(app));
        let fonts_loaded_notifier = FontLoadedNotifier::new(&network);


        Self { model, frp, network, fonts_loaded_notifier }
    }

    fn init_frp(&self, text_provider: &dyn TextProvider) {
        let network = &self.network;
        let frp = &self.frp;
        let text_provider = text_provider.frp();
        let model = &self.model;

        frp::extend! { network
            init <- source::<()>();
            on_data_update <- frp.send_data.constant(());
            eval frp.set_size  ((size) model.set_size(*size));
        }

        self.init_text_grid_api(&init, &on_data_update);

        self.init_scrolling(&init, text_provider, &on_data_update);
        self.init_style(&init);

        init.emit(());
    }

    fn init_style(&self, init: &frp::Source) {
        let init = init.clone_ref();

        let scene = &self.model.app.display.default_scene;
        let style_watch = StyleWatchFrp::new(&scene.style_sheet);
        use theme::graph_editor::visualization::text_grid as text_grid_style;
        let font_name = style_watch.get_text(text_grid_style::font);
        let font_size = style_watch.get_number(text_grid_style::font_size);
        let network = &self.network;
        let fonts_loaded = self.fonts_loaded_notifier.on_fonts_loaded.clone_ref();

        let dom_entry_root = &self.dom_entry_root;
        let text_grid = &self.text_grid;

        frp::extend! { network

            theme_update <- all(init, font_name, font_size);
            text_grid.set_entries_params <+  theme_update.map(
                f!([dom_entry_root]((_, font_name, font_size)) {

                    dom_entry_root.set_style_or_warn("font-family", font_name.clone());
                    dom_entry_root.set_style_or_warn("font-size", format!("{}px", *font_size as u32));

                    let parent = Some(dom_entry_root.clone_ref());
                    grid_view_entry::Params { parent }
                })
            );

            item_width_update <- all(init, fonts_loaded, font_name, font_size);
            item_width <- item_width_update.map(f!([]((_, _, font_name, font_size)) {
                let font_size = *font_size;
                let char_width = measure_character_width(font_name, font_size);
                CHARS_PER_CHUNK as f32 * char_width
            })).on_change();
            item_update <- all(init, item_width, font_size);
            text_grid.set_entries_size <+ item_update.map(f!([]((_, item_width, item_height)) {
                Vector2::new(*item_width, *item_height)
            }));
        }
    }

    fn init_text_grid_api(&self, init: &frp::Source, on_data_update: &frp::Stream) {
        let network = &self.network;
        let text_grid = &self.text_grid;
        let model = &self.model;
        let init = init.clone_ref();
        let on_data_update = on_data_update.clone_ref();

        frp::extend! { network
            text_grid.request_model_for_visible_entries <+ any(on_data_update,init);


            requested_entry <- text_grid.model_for_entry_needed.map2(&text_grid.grid_size,
                 f!([model]((row, col), _grid_size) {
                    let text = model.get_string_for_cell(*row,*col);
                    let model = grid_view_entry::Model{text};
                    (*row, *col, model)
                })
            );
            text_grid.model_for_entry <+ requested_entry;
        }
    }

    fn init_scrolling(
        &self,
        init: &frp::Source,
        text_provider: &text_provider::Frp,
        on_data_update: &frp::Stream,
    ) {
        let network = &self.network;
        let text_grid = &self.text_grid;
        let scrollbar_h = &self.model.scroll_bar_horizontal;
        let scrollbar_v = &self.model.scroll_bar_vertical;
        let dom_entry_root = &self.model.dom_entry_root;
        let on_data_update = on_data_update.clone_ref();
        let frp = &self.frp;
        let init = init.clone_ref();

        frp::extend! { network

            scroll_position <- all(&scrollbar_h.thumb_position, &scrollbar_v.thumb_position);

            longest_line_with_init <- all(&init, &text_provider.longest_line)._1();
            lines_with_init        <- all(&init, &text_provider.line_count)._1();

            longest_line <- longest_line_with_init.on_change();
            line_count <- lines_with_init.on_change();

            content_size <- all(on_data_update, longest_line, line_count).map(
                |(_, width,height)| {
                let columns = (*width as usize / CHARS_PER_CHUNK) + 1;
                let rows = *height as usize;
                ( rows.max(1), columns.max(1) )
            }).on_change();

            text_grid.resize_grid <+ content_size;
            text_grid.reset_entries <+ content_size;
            text_grid.request_model_for_visible_entries <+ text_provider.data_refresh;

            text_grid_content_size_x <- text_grid.content_size.map(|size| size.x).on_change();
            text_grid_content_size_x_previous <- text_grid_content_size_x.previous();

            text_grid_content_size_y <- text_grid.content_size.map(|size| size.y).on_change();
            text_grid_content_size_y_previous <- text_grid_content_size_y.previous();

            scrollbar_h.set_max <+ text_grid_content_size_x;
            scrollbar_v.set_max <+ text_grid_content_size_y;

            scrollbar_h.set_thumb_size <+ frp.set_size.map(|size| size.x - 2.0 * PADDING_TEXT);
            scrollbar_v.set_thumb_size <+ frp.set_size.map(|size| size.y - 2.0 * PADDING_TEXT);

            horizontal_scrollbar_change_args <- all(
                text_grid_content_size_x,
                text_grid_content_size_x_previous,
                scrollbar_h.thumb_position
            );
            on_content_size_x_change <- horizontal_scrollbar_change_args
                .sample(&text_grid_content_size_x);
            scrollbar_h.jump_to <+ on_content_size_x_change.map(
                |(content_size_x, content_size_x_previous, thumb_position)| {
                thumb_position * content_size_x_previous / content_size_x
            });

            vertical_scrollbar_change_args <- all(text_grid_content_size_y,
                text_grid_content_size_y_previous,
                scrollbar_v.thumb_position
            );
            on_content_size_y_change <- vertical_scrollbar_change_args
                .sample(&text_grid_content_size_y);
            scrollbar_v.jump_to <+ on_content_size_y_change.map(
                |(content_size_y, content_size_y_previous, thumb_position)| {
                thumb_position * content_size_y_previous / content_size_y
            });

            viewport <- all_with3(
                &init,
                &scroll_position,
                &frp.set_size,
                f!([dom_entry_root] (_, scroll_position, vis_size) {
                    let (scroll_x, scroll_y) = *scroll_position;
                    let top = -scroll_y;
                    let bottom = top - vis_size.y;
                    let left = scroll_x;
                    let right = left + vis_size.x;

                    // Set DOM element size.
                    dom_entry_root.set_style_or_warn("top", format!("{top}px"));
                    dom_entry_root.set_style_or_warn("left", format!("{}px", -left));

                    // Output viewport.
                    let viewport = grid_view::Viewport {top, bottom, left, right};
                    viewport
                })
            );
            text_grid.set_viewport <+ viewport;
        }
    }

    /// Set the text provider.
    pub fn set_text_provider(&self, text_provider: T) {
        self.model.set_text_provider(text_provider);
        self.init_frp(self.model.text_provider.borrow().as_ref().unwrap());
    }
}

impl<T> From<TextGrid<T>> for visualization::Instance {
    fn from(t: TextGrid<T>) -> Self {
        Self::new(&t, &t.frp, &t.network, Some(t.model.clipping_div.clone_ref()))
    }
}

impl<T> display::Object for TextGrid<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}



// ========================
// === Font Measurement ===
// ========================

// Return the width of a character in the default monospaced font defined in `FONT_NAME`.
fn measure_character_width(font_name: &str, font_size: f32) -> f32 {
    // We expect the font to be monospaced, so we can measure the width of any character.
    let sample_text = "█";

    let canvas = web::document.create_canvas_or_panic();
    let context = canvas.get_context("2d").unwrap().unwrap();
    let context: CanvasRenderingContext2d = context.dyn_into().unwrap();
    let font = format!("{font_size}px {font_name}");
    context.set_font(&font);
    let text_metrics = context.measure_text(sample_text).unwrap();
    let text_length = sample_text.chars().count() as f32;
    let width = text_metrics.width();
    let result = width as f32 / text_length;
    result
}


// =============================
// === Font Loading Notifier ===
// =============================

#[derive(Debug)]
struct FontLoadedNotifier {
    pub on_fonts_loaded: enso_frp::Source,
}

impl FontLoadedNotifier {
    fn new(network: &enso_frp::Network) -> Self {
        enso_frp::extend! { network
                on_fonts_loaded <- source::<()>();
        }

        let callback: Rc<RefCell<Option<web::Closure<_>>>> = default();

        let _closure = web::Closure::new(f_!([on_fonts_loaded, callback]{
            on_fonts_loaded.emit(());
            // Release the self-reference after being called, so the closure can be dropped.
            *callback.borrow_mut() = None;
        }));

        callback.set(_closure);

        let _promise = match web::document.fonts().ready() {
            Ok(promise) => callback.borrow().as_ref().map(|closure| promise.then(closure)),
            Err(e) => {
                warn!("Could not set up font loading event because of error: {:?}.", e);
                None
            }
        };

        Self { on_fonts_loaded }
    }
}


// ===========================
// === Visualisation Types ===
// ===========================

/// A text grid backed by a `String`. Used for testing and backend agnostic development and demos.
/// Should not be used in production as it is not optimized for performance.
pub type DebugTextGridVisualisation = TextGrid<String>;
/// A text grid backed by a the engine. Requests data from the engine on demand and renders it.
pub type TextVisualisation = TextGrid<BackendTextProvider>;

/// Return definition of a lazy text visualisation.
pub fn text_visualisation() -> visualization::Definition {
    let path = visualization::Path::builtin("JSON");
    visualization::Definition::new(
        visualization::Signature::new_for_any_type(path, visualization::Format::Json),
        |app| {
            let grid = TextVisualisation::new(app.clone_ref());
            grid.set_text_provider(BackendTextProvider::new(
                grid.frp.inputs.send_data.clone_ref(),
                grid.frp.preprocessor_change.clone_ref(),
            ));
            Ok(grid.into())
        },
    )
}
