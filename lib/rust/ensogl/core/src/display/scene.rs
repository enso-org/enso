// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::dirty::traits::*;
use crate::prelude::*;
use web::traits::*;

use crate::animation;
use crate::control::callback;
use crate::control::io::mouse;
use crate::control::io::mouse::MouseManager;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display;
use crate::display::camera::Camera2d;
use crate::display::render;
use crate::display::scene::dom::DomScene;
use crate::display::shape::system::ShapeSystemOf;
use crate::display::shape::system::StaticShapeSystemInstance;
use crate::display::shape::ShapeSystemInstance;
use crate::display::style;
use crate::display::style::data::DataMatch;
use crate::display::symbol::registry::SymbolRegistry;
use crate::display::symbol::Symbol;
use crate::display::symbol::SymbolId;
use crate::system;
use crate::system::gpu::data::attribute;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::web;
use crate::system::web::EventListenerHandle;
use crate::system::Context;
use crate::system::ContextLostHandler;

use enso_frp as frp;
use enso_frp::io::js::CurrentJsEvent;
use enso_shapely::shared;
use std::any::TypeId;
use web::HtmlElement;


// ==============
// === Export ===
// ==============

#[warn(missing_docs)]
pub mod dom;
#[warn(missing_docs)]
pub mod layer;

pub use crate::system::web::dom::Shape;
pub use layer::Layer;



pub trait MouseTarget: Debug + 'static {
    fn mouse_down(&self) -> &frp::Source;
    fn mouse_up(&self) -> &frp::Source;
    fn mouse_over(&self) -> &frp::Source;
    fn mouse_out(&self) -> &frp::Source;
}



// =====================
// === ShapeRegistry ===
// =====================

shared! { ShapeRegistry
#[derive(Debug,Default)]
pub struct ShapeRegistryData {
    // FIXME[WD]: The only valid field here is the `mouse_target_map`. The rest should be removed
    //            after proper implementation of text depth sorting, which is the only component
    //            using the obsolete fields now.
    scene            : Option<Scene>,
    shape_system_map : HashMap<TypeId,Box<dyn Any>>,
    mouse_target_map : HashMap<(SymbolId,attribute::InstanceIndex),Rc<dyn MouseTarget>>,
}

impl {
    fn get<T:ShapeSystemInstance>(&self) -> Option<T> {
        let id = TypeId::of::<T>();
        self.shape_system_map.get(&id).and_then(|t| t.downcast_ref::<T>()).map(|t| t.clone_ref())
    }

    fn register<T:ShapeSystemInstance>(&mut self) -> T {
        let id     = TypeId::of::<T>();
        let system = <T as ShapeSystemInstance>::new(self.scene.as_ref().unwrap());
        let any    = Box::new(system.clone_ref());
        self.shape_system_map.insert(id,any);
        system
    }

    fn get_or_register<T:ShapeSystemInstance>(&mut self) -> T {
        self.get().unwrap_or_else(|| self.register())
    }

    pub fn shape_system<T:display::shape::system::Shape>
    (&mut self, _phantom:PhantomData<T>) -> ShapeSystemOf<T> {
        self.get_or_register::<ShapeSystemOf<T>>()
    }

    pub fn new_instance<T:display::shape::system::Shape>(&mut self) -> T {
        let system = self.get_or_register::<ShapeSystemOf<T>>();
        system.new_instance()
    }

    pub fn insert_mouse_target<T:MouseTarget>
    (&mut self, symbol_id:SymbolId, instance_id:attribute::InstanceIndex, target:T) {
        let target = Rc::new(target);
        self.mouse_target_map.insert((symbol_id,instance_id),target);
    }

    pub fn remove_mouse_target
    (&mut self, symbol_id:SymbolId, instance_id:attribute::InstanceIndex) {
        self.mouse_target_map.remove(&(symbol_id,instance_id));
    }

    pub fn get_mouse_target(&mut self, target:PointerTarget) -> Option<Rc<dyn MouseTarget>> {
        match target {
            PointerTarget::Background => None,
            PointerTarget::Symbol {symbol_id,instance_id} => {
                self.mouse_target_map.get(&(symbol_id,instance_id)).cloned()
            }
        }
    }
}}



// ==============
// === Target ===
// ==============

/// Result of a Decoding operation in the Target.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum DecodingResult {
    /// Values had to be truncated.
    Truncated(u8, u8, u8),
    /// Values have been encoded successfully.
    Ok(u8, u8, u8),
}

/// Mouse target. Contains a path to an object pointed by mouse.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum PointerTarget {
    Background,
    Symbol { symbol_id: SymbolId, instance_id: attribute::InstanceIndex },
}

impl PointerTarget {
    /// Encode two u32 values into three u8 values.
    ///
    /// This is the same encoding that is used in the `fragment_runner`. This encoding is lossy and
    /// can only encode values up to 4096 (2^12) each.
    ///
    /// We use 12 bits from each value and pack them into the 3 output bytes like described in the
    /// following diagram.
    ///
    /// ```text
    ///  Input
    ///
    ///    value1 (v1) as bytes               value2 (v2) as bytes
    ///   +-----+-----+-----+-----+           +-----+-----+-----+-----+
    ///   |     |     |     |     |           |     |     |     |     |
    ///   +-----+-----+-----+-----+           +-----+-----+-----+-----+
    ///  32    24    16     8     0          32    24    16     8     0   <- Bit index
    ///
    ///
    /// Output
    ///
    /// byte1            byte2                     byte3
    /// +-----------+    +----------------------+     +------------+
    /// | v ]12..4] |    | v1 ]4..0]  v2 ]4..0] |     | v2 ]12..4] |
    /// +-----------+    +----------------------+     +------------+
    ///
    /// Ranges use mathematical notation for inclusion/exclusion.
    /// ```
    fn encode(value1: u32, value2: u32) -> DecodingResult {
        let chunk1 = (value1 >> 4u32) & 0x00FFu32;
        let chunk2 = (value1 & 0x000Fu32) << 4u32;
        let chunk2 = chunk2 | ((value2 & 0x0F00u32) >> 8u32);
        let chunk3 = value2 & 0x00FFu32;

        if value1 > 2u32.pow(12) || value2 > 2u32.pow(12) {
            DecodingResult::Truncated(chunk1 as u8, chunk2 as u8, chunk3 as u8)
        } else {
            DecodingResult::Ok(chunk1 as u8, chunk2 as u8, chunk3 as u8)
        }
    }

    /// Decode the symbol_id and instance_id that was encoded in the `fragment_runner`.
    ///
    /// See the `encode` method for more information on the encoding.
    fn decode(chunk1: u32, chunk2: u32, chunk3: u32) -> (u32, u32) {
        let value1 = (chunk1 << 4) + (chunk2 >> 4);
        let value2 = chunk3 + ((chunk2 & 0x000F) << 8);
        (value1, value2)
    }

    fn to_internal(self, logger: &Logger) -> Vector4<u32> {
        match self {
            Self::Background => Vector4::new(0, 0, 0, 0),
            Self::Symbol { symbol_id, instance_id } => {
                match Self::encode(*symbol_id, (*instance_id) as u32) {
                    DecodingResult::Truncated(pack0, pack1, pack2) => {
                        warning!(
                            logger,
                            "Target values too big to encode: \
                                         ({symbol_id},{instance_id})."
                        );
                        Vector4::new(pack0.into(), pack1.into(), pack2.into(), 1)
                    }
                    DecodingResult::Ok(pack0, pack1, pack2) =>
                        Vector4::new(pack0.into(), pack1.into(), pack2.into(), 1),
                }
            }
        }
    }

    fn from_internal(v: Vector4<u32>) -> Self {
        if v.w == 0 {
            Self::Background
        } else if v.w == 255 {
            let decoded = Self::decode(v.x, v.y, v.z);
            let symbol_id = SymbolId::new(decoded.0);
            let instance_id = attribute::InstanceIndex::new(decoded.1 as usize);
            Self::Symbol { symbol_id, instance_id }
        } else {
            panic!("Wrong internal format alpha for mouse target.")
        }
    }

    pub fn is_background(self) -> bool {
        self == Self::Background
    }

    pub fn is_symbol(self) -> bool {
        !self.is_background()
    }
}

impl Default for PointerTarget {
    fn default() -> Self {
        Self::Background
    }
}


// === Target Tests ===

#[cfg(test)]
mod target_tests {
    use super::*;

    /// Asserts that decoding encoded the given values returns the correct initial values again.
    /// That means that `decode(encode(value1,value2)) == (value1,value2)`.
    fn assert_valid_roundtrip(value1: u32, value2: u32) {
        let pack = PointerTarget::encode(value1, value2);
        match pack {
            DecodingResult::Truncated { .. } => {
                panic!("Values got truncated. This is an invalid test case: {}, {}", value1, value1)
            }
            DecodingResult::Ok(pack0, pack1, pack2) => {
                let unpack = PointerTarget::decode(pack0.into(), pack1.into(), pack2.into());
                assert_eq!(unpack.0, value1);
                assert_eq!(unpack.1, value2);
            }
        }
    }

    #[test]
    fn test_roundtrip_coding() {
        assert_valid_roundtrip(0, 0);
        assert_valid_roundtrip(0, 5);
        assert_valid_roundtrip(512, 0);
        assert_valid_roundtrip(1024, 64);
        assert_valid_roundtrip(1024, 999);
    }

    #[test]
    fn test_encoding() {
        let pack = PointerTarget::encode(0, 0);
        assert_eq!(pack, DecodingResult::Ok(0, 0, 0));

        let pack = PointerTarget::encode(3, 7);
        assert_eq!(pack, DecodingResult::Ok(0, 48, 7));

        let pack = PointerTarget::encode(3, 256);
        assert_eq!(pack, DecodingResult::Ok(0, 49, 0));

        let pack = PointerTarget::encode(255, 356);
        assert_eq!(pack, DecodingResult::Ok(15, 241, 100));

        let pack = PointerTarget::encode(256, 356);
        assert_eq!(pack, DecodingResult::Ok(16, 1, 100));

        let pack = PointerTarget::encode(31256, 0);
        assert_eq!(pack, DecodingResult::Truncated(161, 128, 0));
    }
}



// =============
// === Mouse ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Mouse {
    pub mouse_manager: MouseManager,
    pub last_position: Rc<Cell<Vector2<i32>>>,
    pub position:      Uniform<Vector2<i32>>,
    pub hover_ids:     Uniform<Vector4<u32>>,
    pub target:        Rc<Cell<PointerTarget>>,
    pub handles:       Rc<[callback::Handle; 4]>,
    pub frp:           enso_frp::io::Mouse,
    pub scene_frp:     Frp,
    pub logger:        Logger,
}

impl Mouse {
    pub fn new(
        scene_frp: &Frp,
        root: &web::dom::WithKnownShape<web::HtmlDivElement>,
        variables: &UniformScope,
        current_js_event: &CurrentJsEvent,
        logger: Logger,
    ) -> Self {
        let scene_frp = scene_frp.clone_ref();
        let target = PointerTarget::default();
        let last_position = Rc::new(Cell::new(Vector2::new(0, 0)));
        let position = variables.add_or_panic("mouse_position", Vector2::new(0, 0));
        let hover_ids = variables.add_or_panic("mouse_hover_ids", target.to_internal(&logger));
        let target = Rc::new(Cell::new(target));
        let mouse_manager = MouseManager::new_separated(&root.clone_ref().into(), &web::window);
        let frp = frp::io::Mouse::new();
        let on_move = mouse_manager.on_move.add(current_js_event.make_event_handler(
            f!([frp,scene_frp,position,last_position] (event:&mouse::OnMove) {
                    let shape       = scene_frp.shape.value();
                    let pixel_ratio = shape.pixel_ratio;
                    let screen_x    = event.client_x();
                    let screen_y    = event.client_y();

                    let new_pos     = Vector2::new(screen_x,screen_y);
                    let pos_changed = new_pos != last_position.get();
                    if pos_changed {
                        last_position.set(new_pos);
                        let new_canvas_position = new_pos.map(|v| (v as f32 *  pixel_ratio) as i32);
                        position.set(new_canvas_position);
                        let position = Vector2(new_pos.x as f32,new_pos.y as f32) - shape.center();
                        frp.position.emit(position);
                    }
                }
            ),
        ));
        let on_down = mouse_manager.on_down.add(
            current_js_event
                .make_event_handler(f!((event:&mouse::OnDown) frp.down.emit(event.button()))),
        );
        let on_up = mouse_manager.on_up.add(
            current_js_event
                .make_event_handler(f!((event:&mouse::OnUp) frp.up.emit(event.button()))),
        );
        let on_wheel = mouse_manager
            .on_wheel
            .add(current_js_event.make_event_handler(f_!(frp.wheel.emit(()))));
        let handles = Rc::new([on_move, on_down, on_up, on_wheel]);
        Self {
            mouse_manager,
            last_position,
            position,
            hover_ids,
            target,
            handles,
            frp,
            scene_frp,
            logger,
        }
    }

    /// Re-emits FRP mouse changed position event with the last mouse position value.
    ///
    /// The immediate question that appears is why it is even needed. The reason is tightly coupled
    /// with how the rendering engine works and it is important to understand it properly. When
    /// moving a mouse the following events happen:
    /// - `MouseManager` gets notification and fires callbacks.
    /// - Callback above is run. The value of `screen_position` uniform changes and FRP events are
    ///   emitted.
    /// - FRP events propagate trough the whole system.
    /// - The rendering engine renders a frame and waits for the pixel read pass to report symbol ID
    ///   under the cursor. This is normally done the next frame but sometimes could take even few
    ///   frames.
    /// - When the new ID are received, we emit `over` and `out` FRP events for appropriate
    ///   elements.
    /// - After emitting `over` and `out `events, the `position` event is re-emitted.
    ///
    /// The idea is that if your FRP network listens on both `position` and `over` or `out` events,
    /// then you do not need to think about the whole asynchronous mechanisms going under the hood,
    /// and you can assume that it is synchronous. Whenever mouse moves, it is discovered what
    /// element it hovers, and its position change event is emitted as well.
    pub fn re_emit_position_event(&self) {
        let shape = self.scene_frp.shape.value();
        let new_pos = self.last_position.get();
        let position = Vector2(new_pos.x as f32, new_pos.y as f32) - shape.center();
        self.frp.position.emit(position);
    }
}



// ================
// === Keyboard ===
// ================

#[derive(Clone, CloneRef, Debug)]
pub struct Keyboard {
    pub frp:  enso_frp::io::keyboard::Keyboard,
    bindings: Rc<enso_frp::io::keyboard::DomBindings>,
}

impl Keyboard {
    pub fn new(current_event: &CurrentJsEvent) -> Self {
        let frp = enso_frp::io::keyboard::Keyboard::default();
        let bindings = Rc::new(enso_frp::io::keyboard::DomBindings::new(&frp, current_event));
        Self { frp, bindings }
    }
}



// ===========
// === Dom ===
// ===========

/// DOM element manager
#[derive(Clone, CloneRef, Debug)]
pub struct Dom {
    /// Root DOM element of the scene.
    pub root:   web::dom::WithKnownShape<web::HtmlDivElement>,
    /// DomLayers of the scene.
    pub layers: DomLayers,
}

impl Dom {
    /// Constructor.
    pub fn new(logger: &Logger) -> Self {
        let root = web::document.create_div_or_panic();
        let layers = DomLayers::new(logger, &root);
        root.set_class_name("scene");
        root.set_style_or_warn("height", "100vh");
        root.set_style_or_warn("width", "100vw");
        root.set_style_or_warn("display", "block");
        let root = web::dom::WithKnownShape::new(&root);
        Self { root, layers }
    }

    pub fn shape(&self) -> Shape {
        self.root.shape()
    }

    pub fn recompute_shape_with_reflow(&self) {
        self.root.recompute_shape_with_reflow();
    }
}



// =================
// === DomLayers ===
// =================

/// DOM DomLayers of the scene. It contains a 2 CSS 3D layers and a canvas layer in the middle. The
/// CSS layers are used to manage DOM elements and to simulate depth-sorting of DOM and canvas
/// elements.
///
/// Each DomLayer is created with `pointer-events: none` CSS property to avoid "stealing" mouse
/// events from other layers.
/// See https://github.com/enso-org/enso/blob/develop/lib/rust/ensogl/doc/mouse-handling.md for more info.
#[derive(Clone, CloneRef, Debug)]
pub struct DomLayers {
    /// Bottom-most DOM scene layer with disabled panning. This layer is placed at the bottom
    /// because mouse cursor is drawn on `canvas` layer, and would be covered by Welcome Screen
    /// elements otherwise.
    pub welcome_screen: DomScene,
    /// Back DOM scene layer.
    pub back:           DomScene,
    /// Back DOM scene layer with fullscreen visualization. Kept separately from `back`, because
    /// the fullscreen visualizations should not share camera with main view. This layer is placed
    /// behind `canvas` because mouse cursor is drawn on `canvas` layer, and would be covered by
    /// `fullscreen_vis` elements otherwise.
    pub fullscreen_vis: DomScene,
    /// Front DOM scene layer.
    pub front:          DomScene,
    /// The WebGL scene layer.
    pub canvas:         web::HtmlCanvasElement,
}

impl DomLayers {
    /// Constructor.
    pub fn new(logger: &Logger, dom: &web::HtmlDivElement) -> Self {
        let welcome_screen = DomScene::new(logger);
        welcome_screen.dom.set_class_name("welcome_screen");
        welcome_screen.dom.set_style_or_warn("z-index", "0");
        dom.append_or_warn(&welcome_screen.dom);

        let back = DomScene::new(logger);
        back.dom.set_class_name("back");
        back.dom.set_style_or_warn("z-index", "1");
        dom.append_or_warn(&back.dom);

        let fullscreen_vis = DomScene::new(logger);
        fullscreen_vis.dom.set_class_name("fullscreen_vis");
        fullscreen_vis.dom.set_style_or_warn("z-index", "2");
        dom.append_or_warn(&fullscreen_vis.dom);

        let canvas = web::document.create_canvas_or_panic();
        canvas.set_style_or_warn("display", "block");
        canvas.set_style_or_warn("z-index", "3");
        // These properties are set by `DomScene::new` constuctor for other layers.
        // See its documentation for more info.
        canvas.set_style_or_warn("position", "absolute");
        canvas.set_style_or_warn("height", "100vh");
        canvas.set_style_or_warn("width", "100vw");
        canvas.set_style_or_warn("pointer-events", "none");
        dom.append_or_warn(&canvas);

        let front = DomScene::new(logger);
        front.dom.set_class_name("front");
        front.dom.set_style_or_warn("z-index", "4");
        dom.append_or_warn(&front.dom);

        Self { back, welcome_screen, fullscreen_vis, front, canvas }
    }
}



// ================
// === Uniforms ===
// ================

/// Uniforms owned by the scene.
#[derive(Clone, CloneRef, Debug)]
pub struct Uniforms {
    /// Pixel ratio of the screen used to display the scene.
    pub pixel_ratio: Uniform<f32>,
}

impl Uniforms {
    /// Constructor.
    pub fn new(scope: &UniformScope) -> Self {
        let pixel_ratio = scope.add_or_panic("pixel_ratio", 1.0);
        Self { pixel_ratio }
    }
}



// =============
// === Dirty ===
// =============

pub type ShapeDirty = dirty::SharedBool<Box<dyn Fn()>>;
pub type SymbolRegistryDirty = dirty::SharedBool<Box<dyn Fn()>>;

#[derive(Clone, CloneRef, Debug)]
pub struct Dirty {
    symbols: SymbolRegistryDirty,
    shape:   ShapeDirty,
}

impl Dirty {
    pub fn new<OnMut: Fn() + Clone + 'static>(logger: &Logger, on_mut: OnMut) -> Self {
        let sub_logger = Logger::new_sub(logger, "shape_dirty");
        let shape = ShapeDirty::new(sub_logger, Box::new(on_mut.clone()));
        let sub_logger = Logger::new_sub(logger, "symbols_dirty");
        let symbols = SymbolRegistryDirty::new(sub_logger, Box::new(on_mut));
        Self { symbols, shape }
    }
}



// ================
// === Renderer ===
// ================

/// Scene renderer. Manages the initialization and lifetime of both [`render::Pipeline`] and the
/// [`render::Composer`].
///
/// Please note that the composer can be empty if the context was either not provided yet or it was
/// lost.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Renderer {
    pub logger:   Logger,
    dom:          Dom,
    variables:    UniformScope,
    pub pipeline: Rc<CloneCell<render::Pipeline>>,
    pub composer: Rc<RefCell<Option<render::Composer>>>,
}

impl Renderer {
    fn new(logger: impl AnyLogger, dom: &Dom, variables: &UniformScope) -> Self {
        let logger = Logger::new_sub(logger, "renderer");
        let dom = dom.clone_ref();
        let variables = variables.clone_ref();
        let pipeline = default();
        let composer = default();
        Self { logger, dom, variables, pipeline, composer }
    }

    fn set_context(&self, context: Option<&Context>) {
        let composer = context.map(|context| {
            // To learn more about the blending equations used here, please see the following
            // articles:
            // - http://www.realtimerendering.com/blog/gpus-prefer-premultiplication
            // - https://www.khronos.org/opengl/wiki/Blending#Colors
            context.enable(Context::BLEND);
            context.blend_equation_separate(Context::FUNC_ADD, Context::FUNC_ADD);
            context.blend_func_separate(
                Context::ONE,
                Context::ONE_MINUS_SRC_ALPHA,
                Context::ONE,
                Context::ONE_MINUS_SRC_ALPHA,
            );

            let (width, height) = self.view_size();
            let pipeline = self.pipeline.get();
            render::Composer::new(&pipeline, context, &self.variables, width, height)
        });
        *self.composer.borrow_mut() = composer;
        self.update_composer_pipeline();
    }

    /// Set the pipeline of this renderer.
    pub fn set_pipeline(&self, pipeline: render::Pipeline) {
        self.pipeline.set(pipeline);
        self.update_composer_pipeline()
    }

    /// Reload the composer pipeline.
    fn update_composer_pipeline(&self) {
        if let Some(composer) = &mut *self.composer.borrow_mut() {
            composer.set_pipeline(&self.pipeline.get());
        }
    }

    /// Reload the composer after scene shape change.
    fn resize_composer(&self) {
        if let Some(composer) = &mut *self.composer.borrow_mut() {
            let (width, height) = self.view_size();
            composer.resize(width, height);
        }
    }

    // The width and height in device pixels should be integers. If they are not then this is due to
    // rounding errors. We round to the nearest integer to compensate for those errors.
    fn view_size(&self) -> (i32, i32) {
        let shape = self.dom.shape().device_pixels();
        let width = shape.width.round() as i32;
        let height = shape.height.round() as i32;
        (width, height)
    }

    /// Run the renderer.
    pub fn run(&self) {
        if let Some(composer) = &mut *self.composer.borrow_mut() {
            debug!(self.logger, "Running.", || {
                composer.run();
            })
        }
    }
}



// ==============
// === Layers ===
// ==============

/// Please note that currently the `Layers` structure is implemented in a hacky way. It assumes the
/// existence of several layers, which are needed for the GUI to display shapes properly. This
/// should be abstracted away in the future.
#[derive(Clone, CloneRef, Debug)]
pub struct HardcodedLayers {
    pub root:               Layer,
    pub viz:                Layer,
    pub below_main:         Layer,
    pub main:               Layer,
    pub port_selection:     Layer,
    pub label:              Layer,
    pub above_nodes:        Layer,
    pub above_nodes_text:   Layer,
    /// Layer containing all panels with fixed position (not moving with the panned scene)
    /// like status bar, breadcrumbs or similar.
    pub panel:              Layer,
    pub panel_text:         Layer,
    pub node_searcher:      Layer,
    pub node_searcher_mask: Layer,
    pub tooltip:            Layer,
    pub tooltip_text:       Layer,
    pub cursor:             Layer,
    pub mask:               Layer,
}

impl Deref for HardcodedLayers {
    type Target = Layer;
    fn deref(&self) -> &Self::Target {
        &self.root
    }
}

impl HardcodedLayers {
    pub fn new(logger: impl AnyLogger) -> Self {
        let root = Layer::new(logger.sub("root"));
        let main = Layer::new(logger.sub("main"));
        let main_cam = &main.camera();
        let viz = Layer::new_with_cam(logger.sub("viz"), main_cam);
        let below_main = Layer::new_with_cam(logger.sub("below_main"), main_cam);
        let port_selection = Layer::new(logger.sub("port_selection"));
        let label = Layer::new_with_cam(logger.sub("label"), main_cam);
        let above_nodes = Layer::new_with_cam(logger.sub("above_nodes"), main_cam);
        let above_nodes_text = Layer::new_with_cam(logger.sub("above_nodes_text"), main_cam);
        let panel = Layer::new(logger.sub("panel"));
        let panel_text = Layer::new(logger.sub("panel_text"));
        let node_searcher = Layer::new(logger.sub("node_searcher"));
        let node_searcher_mask = Layer::new(logger.sub("node_searcher_mask"));
        let tooltip = Layer::new_with_cam(logger.sub("tooltip"), main_cam);
        let tooltip_text = Layer::new_with_cam(logger.sub("tooltip_text"), main_cam);
        let cursor = Layer::new(logger.sub("cursor"));

        let mask = Layer::new_with_cam(logger.sub("mask"), main_cam);
        node_searcher.set_mask(&node_searcher_mask);
        root.set_sublayers(&[
            &viz,
            &below_main,
            &main,
            &port_selection,
            &label,
            &above_nodes,
            &above_nodes_text,
            &panel,
            &panel_text,
            &node_searcher,
            &tooltip,
            &tooltip_text,
            &cursor,
        ]);
        Self {
            root,
            viz,
            below_main,
            main,
            port_selection,
            label,
            above_nodes,
            above_nodes_text,
            panel,
            panel_text,
            node_searcher,
            node_searcher_mask,
            tooltip,
            tooltip_text,
            cursor,
            mask,
        }
    }
}



// ===========
// === FRP ===
// ===========

/// FRP Scene interface.
#[derive(Clone, CloneRef, Debug)]
pub struct Frp {
    pub network:           frp::Network,
    pub shape:             frp::Sampler<Shape>,
    pub camera_changed:    frp::Stream,
    pub frame_time:        frp::Stream<f32>,
    camera_changed_source: frp::Source,
    frame_time_source:     frp::Source<f32>,
}

impl Frp {
    /// Constructor
    pub fn new(shape: &frp::Sampler<Shape>) -> Self {
        frp::new_network! { network
            camera_changed_source <- source();
            frame_time_source     <- source();
        }
        let shape = shape.clone_ref();
        let camera_changed = camera_changed_source.clone_ref().into();
        let frame_time = frame_time_source.clone_ref().into();
        Self {
            network,
            shape,
            camera_changed,
            frame_time,
            camera_changed_source,
            frame_time_source,
        }
    }
}



// =================
// === Extension ===
// =================

pub trait Extension: 'static + CloneRef {
    fn init(scene: &Scene) -> Self;
}

#[derive(Clone, CloneRef, Debug, Default)]
pub struct Extensions {
    map: Rc<RefCell<HashMap<TypeId, Box<dyn Any>>>>,
}

impl Extensions {
    pub fn get<T: Extension>(&self, scene: &Scene) -> T {
        let type_id = TypeId::of::<T>();
        let map_mut = &mut self.map.borrow_mut();
        let entry = map_mut.entry(type_id).or_insert_with(|| Box::new(T::init(scene)));
        entry.downcast_ref::<T>().unwrap().clone_ref()
    }
}



// =================
// === SceneData ===
// =================

#[derive(Clone, CloneRef, Debug)]
pub struct SceneData {
    pub display_object:       display::object::Instance,
    pub dom:                  Dom,
    pub context:              Rc<RefCell<Option<Context>>>,
    pub context_lost_handler: Rc<RefCell<Option<ContextLostHandler>>>,
    pub symbols:              SymbolRegistry,
    pub variables:            UniformScope,
    pub current_js_event:     CurrentJsEvent,
    pub mouse:                Mouse,
    pub keyboard:             Keyboard,
    pub uniforms:             Uniforms,
    pub shapes:               ShapeRegistry,
    pub stats:                Stats,
    pub dirty:                Dirty,
    pub logger:               Logger,
    pub renderer:             Renderer,
    pub layers:               HardcodedLayers,
    pub style_sheet:          style::Sheet,
    pub bg_color_var:         style::Var,
    pub bg_color_change:      callback::Handle,
    pub frp:                  Frp,
    extensions:               Extensions,
    disable_context_menu:     Rc<EventListenerHandle>,
}

impl SceneData {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<OnMut: Fn() + Clone + 'static>(
        logger: Logger,
        stats: &Stats,
        on_mut: OnMut,
    ) -> Self {
        debug!(logger, "Initializing.");

        let dom = Dom::new(&logger);
        let display_object = display::object::Instance::new(&logger);
        display_object.force_set_visibility(true);
        let var_logger = Logger::new_sub(&logger, "global_variables");
        let variables = UniformScope::new(var_logger);
        let dirty = Dirty::new(&logger, on_mut);
        let symbols_dirty = &dirty.symbols;
        let symbols = SymbolRegistry::mk(&variables, stats, &logger, f!(symbols_dirty.set()));
        let layers = HardcodedLayers::new(&logger);
        let stats = stats.clone();
        let shapes = ShapeRegistry::default();
        let uniforms = Uniforms::new(&variables);
        let renderer = Renderer::new(&logger, &dom, &variables);
        let style_sheet = style::Sheet::new();
        let current_js_event = CurrentJsEvent::new();
        let frp = Frp::new(&dom.root.shape);
        let mouse_logger = Logger::new_sub(&logger, "mouse");
        let mouse = Mouse::new(&frp, &dom.root, &variables, &current_js_event, mouse_logger);
        let disable_context_menu = Rc::new(web::ignore_context_menu(&dom.root));
        let keyboard = Keyboard::new(&current_js_event);
        let network = &frp.network;
        let extensions = Extensions::default();
        let bg_color_var = style_sheet.var("application.background");
        let bg_color_change = bg_color_var.on_change(f!([dom](change){
            change.color().for_each(|color| {
                let color = color.to_javascript_string();
                dom.root.set_style_or_warn("background-color",color);
            })
        }));

        layers.main.add_exclusive(&display_object);
        frp::extend! { network
            eval_ frp.shape (dirty.shape.set());
        }

        uniforms.pixel_ratio.set(dom.shape().pixel_ratio);
        let context = default();
        let context_lost_handler = default();
        Self {
            display_object,
            dom,
            context,
            context_lost_handler,
            symbols,
            variables,
            current_js_event,
            mouse,
            keyboard,
            uniforms,
            shapes,
            stats,
            dirty,
            logger,
            renderer,
            layers,
            style_sheet,
            bg_color_var,
            bg_color_change,
            frp,
            extensions,
            disable_context_menu,
        }
    }

    pub fn set_context(&self, context: Option<&Context>) {
        self.symbols.set_context(context);
        *self.context.borrow_mut() = context.cloned();
        self.dirty.shape.set();
        self.renderer.set_context(context);
    }

    pub fn shape(&self) -> &frp::Sampler<Shape> {
        &self.dom.root.shape
    }

    pub fn camera(&self) -> Camera2d {
        self.layers.main.camera()
    }

    pub fn new_symbol(&self) -> Symbol {
        self.symbols.new()
    }

    pub fn symbols(&self) -> &SymbolRegistry {
        &self.symbols
    }

    fn handle_mouse_events(&self) {
        let new_target = PointerTarget::from_internal(self.mouse.hover_ids.get());
        let current_target = self.mouse.target.get();
        if new_target != current_target {
            self.mouse.target.set(new_target);
            self.shapes.get_mouse_target(current_target).for_each(|t| t.mouse_out().emit(()));
            self.shapes.get_mouse_target(new_target).for_each(|t| t.mouse_over().emit(()));
            self.mouse.re_emit_position_event(); // See docs to learn why.
        }
    }

    fn update_shape(&self) {
        if self.dirty.shape.check_all() {
            let screen = self.dom.shape();
            self.resize_canvas(screen);
            self.layers.iter_sublayers_and_masks_nested(|layer| {
                layer.camera().set_screen(screen.width, screen.height)
            });
            self.renderer.resize_composer();
            self.dirty.shape.unset_all();
        }
    }

    fn update_symbols(&self) {
        if self.dirty.symbols.check_all() {
            self.symbols.update();
            self.dirty.symbols.unset_all();
        }
    }

    fn update_camera(&self, scene: &Scene) {
        // Updating camera for DOM layers. Please note that DOM layers cannot use multi-camera
        // setups now, so we are using here the main camera only.
        let camera = self.camera();
        let fullscreen_vis_camera = self.layers.panel.camera();
        // We are using unnavigable camera to disable panning behavior.
        let welcome_screen_camera = self.layers.panel.camera();
        let changed = camera.update(scene);
        if changed {
            self.frp.camera_changed_source.emit(());
            self.symbols.set_camera(&camera);
            self.dom.layers.front.update_view_projection(&camera);
            self.dom.layers.back.update_view_projection(&camera);
        }
        let fs_vis_camera_changed = fullscreen_vis_camera.update(scene);
        if fs_vis_camera_changed {
            self.dom.layers.fullscreen_vis.update_view_projection(&fullscreen_vis_camera);
            self.dom.layers.welcome_screen.update_view_projection(&welcome_screen_camera);
        }

        // Updating all other cameras (the main camera was already updated, so it will be skipped).
        self.layers.iter_sublayers_and_masks_nested(|layer| {
            layer.camera().update(scene);
        });
    }

    /// Resize the underlying canvas. This function should rather not be called
    /// directly. If you want to change the canvas size, modify the `shape` and
    /// set the dirty flag.
    fn resize_canvas(&self, screen: Shape) {
        let canvas = screen.device_pixels();
        // The width and height in device pixels should be integers. If they are not then this is
        // due to rounding errors. We round to the nearest integer to compensate for those errors.
        let width = canvas.width.round() as i32;
        let height = canvas.height.round() as i32;
        debug!(self.logger, "Resized to {screen.width}px x {screen.height}px.", || {
            self.dom.layers.canvas.set_attribute_or_warn("width", &width.to_string());
            self.dom.layers.canvas.set_attribute_or_warn("height", &height.to_string());
            if let Some(context) = &*self.context.borrow() {
                context.viewport(0, 0, width, height);
            }
        });
    }

    pub fn render(&self) {
        self.renderer.run()
    }

    pub fn screen_to_scene_coordinates(&self, position: Vector3<f32>) -> Vector3<f32> {
        let position = position / self.camera().zoom();
        let position = Vector4::new(position.x, position.y, position.z, 1.0);
        (self.camera().inversed_view_matrix() * position).xyz()
    }

    /// Transforms screen position to the object (display object) coordinate system.
    pub fn screen_to_object_space(
        &self,
        object: &impl display::Object,
        screen_pos: Vector2,
    ) -> Vector2 {
        let origin_world_space = Vector4(0.0, 0.0, 0.0, 1.0);
        let layer = object.display_layers().first().and_then(|t| t.upgrade());
        let camera = layer.map_or(self.camera(), |l| l.camera());
        let origin_clip_space = camera.view_projection_matrix() * origin_world_space;
        let inv_object_matrix = object.transform_matrix().try_inverse().unwrap();

        let shape = self.frp.shape.value();
        let clip_space_z = origin_clip_space.z;
        let clip_space_x = origin_clip_space.w * 2.0 * screen_pos.x / shape.width;
        let clip_space_y = origin_clip_space.w * 2.0 * screen_pos.y / shape.height;
        let clip_space = Vector4(clip_space_x, clip_space_y, clip_space_z, origin_clip_space.w);
        let world_space = camera.inversed_view_projection_matrix() * clip_space;
        (inv_object_matrix * world_space).xy()
    }
}

impl display::Object for SceneData {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =============
// === Scene ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Scene {
    no_mut_access: SceneData,
}

impl Scene {
    pub fn new<OnMut: Fn() + Clone + 'static>(
        logger: impl AnyLogger,
        stats: &Stats,
        on_mut: OnMut,
    ) -> Self {
        let logger = Logger::new_sub(logger, "scene");
        let no_mut_access = SceneData::new(logger, stats, on_mut);
        let this = Self { no_mut_access };

        // FIXME MEMORY LEAK:
        this.no_mut_access.shapes.rc.borrow_mut().scene = Some(this.clone_ref());
        this
    }

    pub fn display_in(&self, parent_dom: impl DomPath) {
        match parent_dom.try_into_dom_element() {
            None => error!(&self.logger, "The scene host element could not be found."),
            Some(parent_dom) => {
                parent_dom.append_or_warn(&self.dom.root);
                self.dom.recompute_shape_with_reflow();
                self.uniforms.pixel_ratio.set(self.dom.shape().pixel_ratio);
                self.init();
            }
        }
    }

    fn init(&self) {
        let context_loss_handler = crate::system::context::init_webgl_2_context(self);
        match context_loss_handler {
            Err(err) => error!(self.logger, "{err}"),
            Ok(handler) => *self.context_lost_handler.borrow_mut() = Some(handler),
        }
    }

    pub fn extension<T: Extension>(&self) -> T {
        self.extensions.get(self)
    }
}

impl system::context::Display for Scene {
    fn device_context_handler(&self) -> &system::context::DeviceContextHandler {
        &self.dom.layers.canvas
    }

    fn set_context(&self, context: Option<&Context>) {
        self.no_mut_access.set_context(context)
    }
}

impl AsRef<SceneData> for Scene {
    fn as_ref(&self) -> &SceneData {
        &self.no_mut_access
    }
}

impl std::borrow::Borrow<SceneData> for Scene {
    fn borrow(&self) -> &SceneData {
        &self.no_mut_access
    }
}

impl Deref for Scene {
    type Target = SceneData;
    fn deref(&self) -> &Self::Target {
        &self.no_mut_access
    }
}

impl Scene {
    pub fn update(&self, t: animation::TimeInfo) {
        if self.context.borrow().is_some() {
            debug!(self.logger, "Updating.", || {
                self.frp.frame_time_source.emit(t.local);
                // Please note that `update_camera` is called first as it may trigger FRP events
                // which may change display objects layout.
                self.update_camera(self);
                self.display_object.update(self);
                self.layers.update();
                self.update_shape();
                self.update_symbols();
                self.handle_mouse_events();
            })
        }
    }
}

impl AsRef<Scene> for Scene {
    fn as_ref(&self) -> &Scene {
        self
    }
}

impl display::Object for Scene {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ===============
// === DomPath ===
// ===============

/// Abstraction for DOM path. It can be either a specific HTML element or a DOM id, a string used
/// to query the DOM structure to find the corresponding HTML node.
#[allow(missing_docs)]
pub trait DomPath {
    fn try_into_dom_element(self) -> Option<HtmlElement>;
}

impl DomPath for HtmlElement {
    fn try_into_dom_element(self) -> Option<HtmlElement> {
        Some(self)
    }
}

impl<'t> DomPath for &'t HtmlElement {
    fn try_into_dom_element(self) -> Option<HtmlElement> {
        Some(self.clone())
    }
}

impl DomPath for String {
    fn try_into_dom_element(self) -> Option<HtmlElement> {
        web::document.get_html_element_by_id(&self)
    }
}

impl<'t> DomPath for &'t String {
    fn try_into_dom_element(self) -> Option<HtmlElement> {
        web::document.get_html_element_by_id(self)
    }
}

impl<'t> DomPath for &'t str {
    fn try_into_dom_element(self) -> Option<HtmlElement> {
        web::document.get_html_element_by_id(self)
    }
}
