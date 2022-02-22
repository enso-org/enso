//! An EnsoGL implementation of a basic flame graph.

use ensogl_core::prelude::*;

use enso_frp as frp;
use enso_profiler as profiler;
use ensogl_core::application;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::*;
use ensogl_core::Animation;
use ensogl_text as text;



// ==========================
// === Shapes Definitions ===
// ==========================

mod background {
    use super::*;
    ensogl_core::define_shape_system! {
        (style:Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let zoom                 = Var::<f32>::from("1.0/zoom()");
            let base_color           = style.get_color("flame_graph_color");

            let shape = Rect((&width,&height));

            let border_width: Var<Pixels> = (zoom * 2.0).into();

            let right = Rect((&border_width,&height));
            let right = right.translate_x(&width/2.0);

            let left = Rect((&border_width,&height));
            let left = left.translate_x(-&width/2.0);

            let shape = shape - left;
            let shape = shape - right;
            let shape = shape.fill(base_color);

            (shape).into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_content(String),
        set_position(Vector2),
        set_size(Vector2)
    }
    Output {}
}



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
struct Model {
    app:            Application,
    background:     background::View,
    label:          text::Area,
    display_object: display::object::Instance,
}

impl Model {
    fn new(app: Application) -> Self {
        let scene = app.display.scene();
        let logger = Logger::new("FlameGraphBlock");
        let display_object = display::object::Instance::new(&logger);

        let label = app.new_view::<text::Area>();

        let background = background::View::new(&logger);

        display_object.add_child(&background);
        display_object.add_child(&label);

        let app = app.clone_ref();
        let model = Model { app, background, label, display_object };
        model.set_layers(&scene.layers.tooltip, &scene.layers.tooltip_text);
        model
    }


    /// Set scene layers for background and text respectively.
    pub fn set_layers(&self, background_layer: &Layer, text_layer: &Layer) {
        // FIXME[MM/WD]: Depth sorting of labels to in front of everything else in the scene.
        //  Temporary solution. The depth management needs to allow defining relative position of
        //  the text and background and let the whole component to be set to am an arbitrary layer.
        background_layer.add_exclusive(&self.background);
        self.label.add_to_scene_layer(text_layer);
    }

    fn set_size(&self, size: Vector2) {
        self.background.size.set(size);
        let text_size = self.label.height.value();
        const TEXT_OFFSET_X: f32 = 4.0;
        const TEXT_OFFSET_Y: f32 = -2.0;
        let text_origin =
            Vector2(TEXT_OFFSET_X - size.x / 2.0, TEXT_OFFSET_Y + text_size as f32 / 2.0);
        self.label.set_position_xy(text_origin);
    }

    fn set_content(&self, t: &str) {
        self.label.set_content(t)
    }

    fn set_label_opacity(&self, opacity: f32) {
        let color = self.label.default_color.value();
        let color: color::Rgba = color.opaque.with_alpha(opacity);
        self.label.set_color_all.emit(color);
        self.label.set_default_color.emit(color);
    }
}



// =======================
// === Block Component ===
// =======================

#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Block {
    model:   Rc<Model>,
    pub frp: Rc<Frp>,
}

impl Block {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Rc::new(Frp::new());
        let model = Rc::new(Model::new(app.clone_ref()));
        Block { model, frp }.init()
    }

    fn init(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;

        let label_opacity = Animation::<f32>::new(network);

        frp::extend! { network
           eval frp.set_content((t) model.set_content(t));
           eval frp.set_size((size) model.set_size(*size));

            label_opacity.target <+ model.background.events.mouse_over.constant(1.0);
            label_opacity.target <+ model.background.events.mouse_out.constant(0.0);
            eval label_opacity.value ((t) model.set_label_opacity(*t));

        }

        label_opacity.target.emit(0.0);
        model.set_label_opacity(0.0);

        self
    }
}

impl Deref for Block {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl display::Object for Block {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl application::command::FrpNetworkProvider for Block {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for Block {
    fn label() -> &'static str {
        "FlameGraphBlock"
    }

    fn new(app: &Application) -> Self {
        Block::new(app)
    }

    fn app(&self) -> &Application {
        &self.model.app
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        default()
    }
}

// ===================
// === Flame Graph ===
// ===================


// === Layout Constants ===

const ROW_HEIGHT: f64 = 20.0;
const ROW_PADDING: f64 = 5.0;


// === Implementation ===


pub struct FlameGraph {
    display_object: display::object::Instance,
    blocks:         Vec<Block>,
}

fn shape_from_block(block: profiler::flame_graph::Block, app: &Application) -> Block {
    let component = app.new_view::<Block>();

    let size = Vector2::new(block.width() as f32, ROW_HEIGHT as f32);
    let x = block.start + block.width() / 2.0;
    let y = block.row as f64 * (ROW_HEIGHT + ROW_PADDING);
    let pos = Vector2::new(x as f32, y as f32);

    component.set_content.emit(block.label);
    component.set_size.emit(size);
    component.set_position_xy(pos);

    component
}

impl FlameGraph {
    pub fn from_data(data: profiler::flame_graph::FlameGraph, app: &Application) -> Self {
        let logger = Logger::new("FlameGraph");
        let display_object = display::object::Instance::new(&logger);

        let min_time =
            data.blocks.iter().map(|block| block.start.floor() as u32).min().unwrap_or_default();

        let blocks_zero_aligned = data.blocks.into_iter().map(|mut block| {
            block.start -= min_time as f64;
            block.end -= min_time as f64;
            block
        });

        let blocks = blocks_zero_aligned.map(|block| shape_from_block(block, app)).collect_vec();
        blocks.iter().for_each(|item| display_object.add_child(item));
        Self { display_object, blocks }
    }

    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }
}

impl display::Object for FlameGraph {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
