//! Example visualisation showing the provided data as text.

use crate::prelude::*;
use ensogl::system::web::traits::*;

use crate::component::visualization;
use crate::StyleWatchFrp;

use enso_frp as frp;
use ensogl::application::command::FrpNetworkProvider;
use ensogl::application::frp::API;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::system::web::CanvasRenderingContext2d;
use ensogl_component::grid_view;
use ensogl_component::grid_view::GridView;
use ensogl_component::scrollbar::Scrollbar;
use ensogl_hardcoded_theme as theme;



// =================
// === Constants ===
// =================

const CHARS_PER_CELL: f32 = 15.0;



// =============
// === Entry ===
// =============

mod entry {
    use super::*;

    use crate::display;
    use crate::display::DomSymbol;
    use crate::web;
    use crate::Application;
    use ensogl::prelude::*;
    use ensogl_component::grid_view;
    use ensogl_component::grid_view::entry::EntryFrp;

    /// Model that contains the data that is required to populate the data in an `Entry`.
    #[derive(Clone, Debug, Default)]
    pub struct Model {
        pub text: String,
    }

    /// Parameters that are required to set up an `Entry`.
    #[derive(Clone, Debug, Default)]
    pub struct Params {
        /// DOM parent of the Entry. The text element in the `Entry` must be a child of the
        /// `parent` to appear correctly.
        pub parent:    Option<DomSymbol>,
        /// Name of the font to be used in the `Entry`.
        pub font_name: ImString,
        /// Font size in pixels.
        pub font_size: f32,
    }

    /// Entry for use in GridView. Contains a dom element with a text, the Entry frp, and a dummy
    /// display object for compatibility with `GridView`. The `dummy_root` is not used for
    /// displaying anything, all that is visible is the `text` element, which is updates through
    /// the FRP.
    #[derive(Clone, CloneRef, Debug)]
    pub struct Entry {
        text: Rc<DomSymbol>,
        frp:  Rc<EntryFrp<Self>>,
    }

    impl Entry {
        fn set_model(&self, model: &Model) {
            self.text.set_inner_text(&model.text);
        }

        fn set_params(&self, params: &Params) {
            if let Some(parent) = &params.parent {
                parent.append_or_warn(self.text.dom());
            }
            self.text.set_style_or_warn("font-family", params.font_name.clone());
            self.text.set_style_or_warn("font-size", format!("{}px", params.font_size as u32));
        }

        fn set_position_and_size(&self, pos: &Vector2, size: &Vector2) {
            self.text.set_xy(*pos);

            self.text.set_style_or_warn("left", format!("{}px", pos.x - size.x / 2.0));
            self.text.set_style_or_warn("top", format!("{}px", -pos.y - size.y / 2.0));

            self.text.set_style_or_warn("width", format!("{}px", size.x as u32));
            self.text.set_style_or_warn("height", format!("{}px", size.y as u32));
        }
    }

    impl display::Object for Entry {
        fn display_object(&self) -> &display::object::Instance {
            self.text.display_object()
        }
    }

    impl grid_view::Entry for Entry {
        type Model = Model;
        type Params = Params;

        fn new(app: &Application, _text_layer: Option<&display::scene::Layer>) -> Self {
            let scene = &app.display.default_scene;
            let text_div = web::document.create_div_or_panic();
            let text = DomSymbol::new(&text_div);
            scene.dom.layers.back.manage(&text);
            text.set_style_or_warn("white-space", "nowrap");
            text.set_style_or_warn("pointer-events", "auto");
            text.set_style_or_warn("white-space", "pre");

            let new_entry = Self { text: Rc::new(text), frp: default() };

            let input = &new_entry.frp.private().input;
            let network = new_entry.frp.network();
            enso_frp::extend! { network
                init <- source_();
                eval input.set_model((model) new_entry.set_model(model));
                eval input.set_params((params) new_entry.set_params(params));

                pos_size <- all(&input.position_set, &input.set_size);
                eval pos_size (((pos, size)) new_entry.set_position_and_size(pos, size));
            }
            init.emit(());
            new_entry
        }

        fn frp(&self) -> &EntryFrp<Self> {
            &self.frp
        }
    }
}

pub use entry::Entry;



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
    text_grid:             GridView<Entry>,
    dom_entry_root:        DomSymbol,
    clipping_div:          DomSymbol,
    scroll_bar_horizontal: Scrollbar,
    scroll_bar_vertical:   Scrollbar,
    text_provider:         Rc<RefCell<Option<T>>>,
}

impl<T: TextProvider> Model<T> {
    /// Constructor.
    fn new(app: Application) -> Self {
        let scene = &app.display.default_scene;
        let root = display::object::Instance::new();
        let clipping_div = web::document.create_div_or_panic();
        let clipping_div = DomSymbol::new(&clipping_div);
        let dom_entry_root = web::document.create_div_or_panic();
        let dom_entry_root = DomSymbol::new(&dom_entry_root);
        let size = Rc::new(Cell::new(Vector2(200.0, 200.0)));
        let text_provider = default();

        clipping_div.set_style_or_warn("overflow", "hidden");
        clipping_div.set_style_or_warn("class", "dom_root");
        scene.dom.layers.back.manage(&clipping_div);
        scene.dom.layers.back.manage(&dom_entry_root);
        root.add_child(&clipping_div);
        clipping_div.append_or_warn(&dom_entry_root);

        let text_grid: GridView<Entry> = GridView::new(&app);
        dom_entry_root.add_child(&text_grid);

        let scroll_bar_horizontal = Scrollbar::new(&app);
        root.add_child(&scroll_bar_horizontal);
        let scroll_bar_vertical = Scrollbar::new(&app);
        root.add_child(&scroll_bar_vertical);
        scroll_bar_vertical.set_rotation_z(-90.0_f32.to_radians());

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

        self.scroll_bar_vertical.set_x(size.x / 2.0);
        self.scroll_bar_vertical.set_length(size.y);

        self.clipping_div.set_dom_size(size);
        self.size.set(size);
    }

    fn receive_data(&self, _data: &visualization::Data) -> Result<(), visualization::DataError> {
        // TODO[MM]: Will be implemented as part of https://www.pivotaltracker.com/story/show/183453466
        Ok(())
    }

    fn get_string_for_cell(&self, row: usize, column: usize) -> String {
        let width = CHARS_PER_CELL as usize;
        self.text_provider
            .borrow()
            .as_ref()
            .and_then(|text| text.get_slice(row, width, column))
            .unwrap_or_default()
    }

    /// Set the text provider. Updates the grid according to text dimensions.
    pub fn set_text_provider(&self, text_provider: T) {
        let lines = text_provider.line_count();
        let max_line_length = text_provider.longest_line();
        self.text_provider.replace(Some(text_provider));

        let max_columns = (max_line_length / CHARS_PER_CELL as usize) + 1;
        // self.text_grid.set_entries_size(Vector2(60.0, 8.0));
        self.text_grid.reset_entries(lines, max_columns);
        self.text_grid.resize_grid(lines, max_columns);
        self.text_grid.request_model_for_visible_entries();
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

impl<T: 'static + TextProvider> TextGrid<T> {
    /// Definition of this visualization.
    pub fn definition() -> visualization::Definition {
        let path = visualization::Path::builtin("JSON");
        visualization::Definition::new(
            visualization::Signature::new_for_any_type(path, visualization::Format::Json),
            |app| Ok(Self::new(app.clone_ref()).into()),
        )
    }

    /// Constructor.
    pub fn new(app: Application) -> Self {
        let network = frp::Network::new("visualization_text_grid");
        let frp = visualization::instance::Frp::new(&network);
        let model = Rc::new(Model::new(app));
        Self { model, frp, network }.init()
    }

    fn init(self) -> Self {
        let network = &self.network;
        let model = self.model.clone_ref();
        let frp = self.frp.clone_ref();
        let text_grid = &self.model.text_grid;
        let dom_entry_root = &self.model.dom_entry_root;
        let scrollbar_h = &self.model.scroll_bar_horizontal;
        let scrollbar_v = &self.model.scroll_bar_vertical;

        let scene = &model.app.display.default_scene;
        let style_watch = StyleWatchFrp::new(&scene.style_sheet);
        use theme::graph_editor::visualization::text_grid as text_grid_style;
        let font_name = style_watch.get_text(text_grid_style::font);
        let font_size = style_watch.get_number(text_grid_style::font_size);

        frp::extend! { network

            // === Visualisation API Inputs ===

            eval frp.set_size ((size) model.set_size(*size));
            eval frp.send_data ([frp,model](data) {
                if let Err(e) = model.receive_data(data) {
                    frp.data_receive_error.emit(Some(e));
                }
             });

            // === Text Grid API ===

            requested_entry <- text_grid.model_for_entry_needed.map2(&text_grid.grid_size,
                f!([model]((row, col), _grid_size) {
                    let text = model.get_string_for_cell(*row,*col);
                    let model = entry::Model{text};
                    (*row, *col, model)
                })
            );
            text_grid.model_for_entry <+ requested_entry;

            // === Scrolling ===

            scroll_positition <- all(&scrollbar_h.thumb_position, &scrollbar_v.thumb_position);
            trace scroll_positition;
            viewport <- all_with3(&scroll_positition, &frp.set_size, &text_grid.content_size, f!([dom_entry_root](scroll_position, vis_size, content_size) {
                let (scroll_x, scroll_y) = *scroll_position;
                let top = -scroll_y * content_size.y;
                let bottom = top - vis_size.y;
                let left = scroll_x * content_size.x;
                let right = left +  vis_size.x;
                dom_entry_root.set_style_or_warn("top", format!("{}px", top));
                dom_entry_root.set_style_or_warn("left", format!("{}px", -left));
                grid_view::Viewport {top,bottom,left,right}
            }));
            text_grid.set_viewport <+ viewport;

        }


        // === Style ===

        frp::extend! { network
            init <- source::<()>();

            theme_update <- all(init, font_name, font_size);
            text_grid.set_entries_params <+  theme_update.map(f!([dom_entry_root]((_, font_name, font_size)) {
                let parent = Some(dom_entry_root.clone_ref());
                let font_name = font_name.clone();
                let font_size = *font_size;
                entry::Params { parent, font_name, font_size}
            }));

            item_width_update <- all(init, font_name, font_size);
            item_width <- item_width_update.map(f!([]((_, font_name, font_size)) {
                let font_size = *font_size;
                let char_width = measure_character_width(font_name, font_size);
                CHARS_PER_CELL * char_width
            })).on_change();
            item_update <- all(init, item_width, font_size);
            text_grid.set_entries_size <+ item_update.map(f!([]((_, item_width, item_height)) {
                Vector2::new(*item_width, *item_height)
            }));

        }

        init.emit(());

        self
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

// ====================
// === TextProvider ===
// ====================

/// Trait for providing text for the TextGrid.
pub trait TextProvider {
    /// Return a slice of the text.
    ///
    /// The slice is indexed by the line and the "chunk", where a chunk is a sequence of characters
    /// of fixed length, into which the line is divided. For example, for "abcdef" there could
    /// be chunks of size two: ["ab", "cd", "ef"], or of size three ["abc", "def"].
    fn get_slice(&self, line: usize, chunk_size: usize, chunk_index: usize) -> Option<String>;

    /// Return the number of lines.
    fn line_count(&self) -> usize;

    /// Return the length of the longest line in characters.
    fn longest_line(&self) -> usize;
}

impl TextProvider for String {
    fn get_slice(&self, line: usize, chunk_size: usize, chunk_index: usize) -> Option<String> {
        self.lines().nth(line).and_then(|line| {
            line.chars()
                .chunks(chunk_size)
                .into_iter()
                .nth(chunk_index)
                .map(|chunk| chunk.collect::<String>())
        })
    }

    /// Return the number of lines in the text.
    fn line_count(&self) -> usize {
        self.lines().count()
    }

    /// Return the number of characters in the longest line.
    fn longest_line(&self) -> usize {
        self.lines().map(|line| line.chars().count()).max().unwrap_or(0)
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
