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
use ensogl::application::command::FrpNetworkProvider;
use ensogl::application::frp::API;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::system::web::CanvasRenderingContext2d;
use ensogl_component::grid_view;
use ensogl_component::grid_view::GridView;
use ensogl_component::scrollbar;
use ensogl_component::scrollbar::Scrollbar;
use ensogl_hardcoded_theme as theme;

pub use entry::Entry;



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
    dom_entry_root:        DomSymbol,
    clipping_div:          DomSymbol,
    scroll_bar_horizontal: Scrollbar,
    scroll_bar_vertical:   Scrollbar,
    text_provider:         Rc<RefCell<Option<T>>>,
}

impl<T: 'static> Model<T> {
    /// Constructor.
    fn new(app: Application) -> Self {
        let scene = &app.display.default_scene;
        let root = display::object::Instance::new();
        let clipping_div = web::document.create_div_or_panic();
        let clipping_div = DomSymbol::new(&clipping_div);
        let dom_entry_root = web::document.create_div_or_panic();
        let dom_entry_root = DomSymbol::new(&dom_entry_root);
        let size = default();
        let text_provider = default();

        clipping_div.set_style_or_warn("overflow", "hidden");
        clipping_div.set_style_or_warn("class", "dom_root");
        scene.dom.layers.front.manage(&clipping_div);
        scene.dom.layers.front.manage(&dom_entry_root);
        root.add_child(&clipping_div);
        clipping_div.append_or_warn(&dom_entry_root);

        let text_grid: GridView<grid_view_entry::Entry> = GridView::new(&app);
        dom_entry_root.add_child(&text_grid);

        let scroll_bar_horizontal = Scrollbar::new(&app);
        root.add_child(&scroll_bar_horizontal);
        let scroll_bar_vertical = Scrollbar::new(&app);
        root.add_child(&scroll_bar_vertical);
        scroll_bar_vertical.set_rotation_z(-90.0_f32.to_radians());

        app.display.default_scene.layers.above_nodes_text.add(&scroll_bar_horizontal);
        app.display.default_scene.layers.above_nodes_text.add(&scroll_bar_vertical);

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
    }

    fn set_size(&self, size: Vector2) {
        self.scroll_bar_horizontal.set_y(-size.y / 2.0);
        self.scroll_bar_horizontal.set_length(size.x);
        let scrollbar_width = scrollbar::WIDTH - scrollbar::PADDING;
        self.scroll_bar_horizontal.mod_y(|y| y + scrollbar_width / 2.0);
        self.scroll_bar_vertical.set_x(size.x / 2.0);
        self.scroll_bar_vertical.set_length(size.y);
        self.scroll_bar_vertical.mod_x(|x| x - scrollbar_width / 2.0);
        self.clipping_div.set_size(size);
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
#[derive(Debug, Shrinkwrap)]
#[allow(missing_docs)]
pub struct TextGrid<T> {
    #[shrinkwrap(main_field)]
    model:   Rc<Model<T>>,
    pub frp: visualization::instance::Frp,
    network: frp::Network,
}

impl<T: TextProvider + 'static> TextGrid<T> {
    /// Constructor.
    pub fn new(app: Application) -> Self {
        let network = frp::Network::new("visualization_text_grid");
        let frp = visualization::instance::Frp::new(&network);
        let model = Rc::new(Model::new(app));
        Self { model, frp, network }
    }

    fn init_frp(&self, text_provider: &dyn TextProvider) {
        let network = &self.network;
        let frp = &self.frp;
        let text_provider = text_provider.frp();
        let text_grid = &self.text_grid;
        let model = &self.model;
        let scrollbar_h = &self.model.scroll_bar_horizontal;
        let scrollbar_v = &self.model.scroll_bar_vertical;
        let dom_entry_root = &self.model.dom_entry_root;

        let scene = &self.model.app.display.default_scene;
        let style_watch = StyleWatchFrp::new(&scene.style_sheet);
        use theme::graph_editor::visualization::text_grid as text_grid_style;
        let font_name = style_watch.get_text(text_grid_style::font);
        let font_size = style_watch.get_number(text_grid_style::font_size);

        let longest_observed_line: Rc<Cell<u32>> = default();
        let max_observed_lines: Rc<Cell<u32>> = default();

        frp::extend! { network
            init <- source::<()>();

            on_data_update <- frp.send_data.constant(());
            text_grid.request_model_for_visible_entries <+ any(on_data_update,init);

            // === Visualisation API Inputs ===

            eval frp.set_size  ((size) model.set_size(*size));


            // === Text Grid API ===

            requested_entry <- text_grid.model_for_entry_needed.map2(&text_grid.grid_size,
                f!([model]((row, col), _grid_size) {
                    let text = model.get_string_for_cell(*row,*col);
                    let model = grid_view_entry::Model{text};
                    (*row, *col, model)
                })
            );
            text_grid.model_for_entry <+ requested_entry;

            // === Scrolling ===

            scroll_positition <- all(&scrollbar_h.thumb_position, &scrollbar_v.thumb_position);

            longest_line <- text_provider.longest_line.map(
                f!([longest_observed_line](longest_line) {
                    let observed_value = longest_observed_line.get();
                    let longest_line = observed_value.max(*longest_line);
                    longest_observed_line.set(longest_line);
                    longest_line
                })
            ).on_change();

            line_count <- text_provider.line_count.map(f!([max_observed_lines](line_count) {
                let observed_value = max_observed_lines.get();
                let max_lines = observed_value.max(*line_count);
                max_observed_lines.set(max_lines);
                max_lines
            })).on_change();

            content_size <- all(init, on_data_update, longest_line, line_count).map(
                |(_, _, width,height)| {
                let columns = (*width as usize / CHARS_PER_CHUNK) + 1;
                let rows = *height as usize;
                ( rows.max(1), columns.max(1) )
            }).on_change();

            text_grid.resize_grid <+ content_size;
            text_grid.reset_entries <+ content_size;

            text_grid_content_size_x <- text_grid.content_size.map(|size| size.x).on_change();
            text_grid_content_size_x_previous <- text_grid_content_size_x.previous();

            text_grid_content_size_y <- text_grid.content_size.map(|size| size.y).on_change();
            text_grid_content_size_y_previous <- text_grid_content_size_y.previous();

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

            size_update <- all(&frp.set_size, &text_grid.content_size);
            scrollbar_sizes <- size_update.map(|(vis_size, content_size)| {
                vis_size.iter().zip(content_size.iter()).map(|(vis_size, content_size)| {
                    if *content_size > 0.0 {
                        (vis_size / content_size).clamp(0.0,1.0)
                    } else {
                        0.0
                    }
                }).collect_tuple::<(f32,f32)>()
            }).unwrap();
            scrollbar_h.set_thumb_size <+ scrollbar_sizes._0();
            scrollbar_v.set_thumb_size <+ scrollbar_sizes._1();

            viewport <- all_with4(
                &init,
                &scroll_positition,
                &frp.set_size,
                &text_grid.content_size,
                f!([dom_entry_root](_, scroll_position, vis_size, content_size) {
                    let (scroll_x, scroll_y) = *scroll_position;
                    let top = -scroll_y * content_size.y;
                    let bottom = top - vis_size.y;
                    let left = scroll_x * content_size.x;
                    let right = left +  vis_size.x;

                    // Set DOM element size.
                    dom_entry_root.set_style_or_warn("top", format!("{}px", top));
                    dom_entry_root.set_style_or_warn("left", format!("{}px", -left));

                    // Output viewport.
                    let viewport = grid_view::Viewport {top, bottom, left, right};
                    viewport
                })
            );
            text_grid.set_viewport <+ viewport;
        }


        // === Style ===

        frp::extend! { network

            theme_update <- all(init, font_name, font_size);
            text_grid.set_entries_params <+  theme_update.map(
                f!([dom_entry_root]((_, font_name, font_size)) {

                    dom_entry_root.set_style_or_warn("font-family", font_name.clone());
                    dom_entry_root.set_style_or_warn("font-size", format!("{}px", *font_size as u32));

                    let parent = Some(dom_entry_root.clone_ref());
                    let font_name = font_name.clone();
                    let font_size = *font_size;
                    grid_view_entry::Params { parent, font_name, font_size}
                })
            );

            item_width_update <- all(init, font_name, font_size);
            item_width <- item_width_update.map(f!([]((_, font_name, font_size)) {
                let font_size = *font_size;
                let char_width = measure_character_width(font_name, font_size);
                CHARS_PER_CHUNK as f32 * char_width
            })).on_change();
            item_update <- all(init, item_width, font_size);
            text_grid.set_entries_size <+ item_update.map(f!([]((_, item_width, item_height)) {
                Vector2::new(*item_width, *item_height)
            }));

        }

        init.emit(());
    }

    /// Set the text provider.
    pub fn set_text_provider(&self, text_provider: T) {
        self.model.set_text_provider(text_provider);
        self.init_frp(self.model.text_provider.borrow().as_ref().unwrap());
    }
}

impl<T> From<TextGrid<T>> for visualization::Instance {
    fn from(t: TextGrid<T>) -> Self {
        Self::new(&t, &t.frp, &t.network, Some(t.model.dom_entry_root.clone_ref()))
    }
}

impl<T> display::Object for TextGrid<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.root
    }
}



// =================================
// === Text Processing Utilities ===
// =================================

// Return the width of a character in the default monospaced font defined in `FONT_NAME`.
fn measure_character_width(font_name: &str, font_size: f32) -> f32 {
    // We expect the font to be monospaced, so we can measure the width of any character.
    let sample_text = "█";

    let canvas = web::document.create_canvas_or_panic();
    let context = canvas.get_context("2d").unwrap().unwrap();
    let context: CanvasRenderingContext2d = context.dyn_into().unwrap();
    context.set_font(&format!("{}px {}", font_size as u32, font_name));
    context.fill_text(sample_text, 0.0, 0.0).unwrap();
    let text_metrics = context.measure_text(sample_text).unwrap();
    let width = text_metrics.actual_bounding_box_right() + text_metrics.actual_bounding_box_left();

    2.4 * width as f32 / sample_text.len() as f32
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
pub fn lazy_text_visualisation() -> visualization::Definition {
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
