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
use crate::display::scene::layer::LayerSymbolPartition;
use crate::display::shape::compound::rectangle;
use crate::display::shape::primitive::glsl;
use crate::display::style;
use crate::display::style::data::DataMatch;
use crate::display::symbol::Symbol;
use crate::display::world;
use crate::system;
use crate::system::gpu::context::profiler::Results;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::gpu::shader;
use crate::system::gpu::Context;
use crate::system::gpu::ContextLostHandler;
use crate::system::web;
use crate::system::web::EventListenerHandle;

use enso_frp as frp;
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
#[warn(missing_docs)]
pub mod pointer_target;

pub use crate::system::web::dom::Shape;
pub use layer::Layer;
pub use pointer_target::PointerTargetId;
pub use pointer_target::PointerTarget_DEPRECATED;



// =====================
// === ShapeRegistry ===
// =====================

shared! { PointerTargetRegistry
#[derive(Debug)]
pub struct ShapeRegistryData {
    mouse_target_map : HashMap<PointerTargetId, (PointerTarget_DEPRECATED, display::object::Instance)>,
}

impl {
    fn new(background_pointer_target: &PointerTarget_DEPRECATED, background: &display::object::Instance) -> Self {
        let mouse_target_map = default();
        Self {mouse_target_map} . init(background_pointer_target, background)
    }

    pub fn insert
    (&mut self, id:impl Into<PointerTargetId>, target:impl Into<PointerTarget_DEPRECATED>, display_object:&display::object::Instance) {
        self.mouse_target_map.insert(id.into(),(target.into(), display_object.clone_ref()));
    }

    pub fn remove
    (&mut self, id:impl Into<PointerTargetId>) {
        self.mouse_target_map.remove(&id.into());
    }

    pub fn get(&self, target:PointerTargetId) -> Option<(PointerTarget_DEPRECATED, display::object::Instance)> {
        self.mouse_target_map.get(&target).cloned()
    }
}}

impl ShapeRegistryData {
    fn init(
        mut self,
        background: &PointerTarget_DEPRECATED,
        display_object: &display::object::Instance,
    ) -> Self {
        self.mouse_target_map.insert(
            PointerTargetId::Background,
            (background.clone_ref(), display_object.clone_ref()),
        );
        self
    }
}

impl PointerTargetRegistry {
    /// Runs the provided function on the [`PointerTarget_DEPRECATED`] associated with the provided
    /// [`PointerTargetId`]. Please note that the [`PointerTarget_DEPRECATED`] will be cloned
    /// because during evaluation of the provided function this registry might be changed, which
    /// would result in double borrow mut otherwise.
    pub fn with_mouse_target<T>(
        &self,
        target_id: PointerTargetId,
        f: impl FnOnce(&PointerTarget_DEPRECATED, &display::object::Instance) -> T,
    ) -> Option<T> {
        match self.get(target_id) {
            Some(t) => Some(f(&t.0, &t.1)),
            None => {
                warn!("Internal error. Symbol ID {target_id:?} is not registered.");
                None
            }
        }
    }
}



// =============
// === Mouse ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Mouse {
    pub mouse_manager:           MouseManager,
    pub pointer_target_registry: PointerTargetRegistry,
    pub last_position:           Rc<Cell<Vector2>>,
    pub position:                Uniform<Vector2<i32>>,
    pub click_count:             Uniform<i32>,
    /// The encoded value of pointer target ID. The offscreen canvas containing the encoded IDs is
    /// sampled and the most recent sample is stored here. Please note, that this sample may be
    /// from a few frames back, as it is not guaranteed when we will receive the data from GPU.
    pub pointer_target_encoded:  Uniform<Vector4<u32>>,
    /// The display objects which are currently hovered by mouse, where "hovered" means
    /// that it's a shape directly hovered, or one of the descendant is hovered. It's updated in
    /// [`switch_target`] method.
    ///
    /// This field may be updated with a few frames delay, as we need to read mouse target from
    /// the texture.
    pub hovered_objects:         Rc<RefCell<Vec<display::object::WeakInstance>>>,
    pub target:                  Rc<Cell<PointerTargetId>>,
    pub handles:                 Rc<[callback::Handle; 6]>,
    pub scene_frp:               Frp,
    /// Stored in order to be converted to [`mouse::Over`], [`mouse::Out`], [`mouse::Enter`], and
    /// [`mouse::Leave`] when the mouse enters or leaves an element.
    pub last_move_event:         Rc<RefCell<Option<mouse::Move>>>,
    /// # Deprecated
    /// This API is deprecated. Instead, use the display object's event API. For example, to get an
    /// FRP endpoint for mouse event, you can use the [`crate::display::Object::on_event`]
    /// function.
    pub frp_deprecated:          enso_frp::io::Mouse_DEPRECATED,
    pub background:              PointerTarget_DEPRECATED,
}

impl Mouse {
    pub fn new(
        scene_frp: &Frp,
        scene_object: &display::object::Instance,
        root: &web::dom::WithKnownShape<web::HtmlDivElement>,
        variables: &UniformScope,
        display_mode: &Rc<Cell<glsl::codes::DisplayModes>>,
    ) -> Self {
        let background = PointerTarget_DEPRECATED::new();
        let pointer_target_registry = PointerTargetRegistry::new(&background, scene_object);
        let last_pressed_elem: Rc<RefCell<HashMap<mouse::Button, PointerTargetId>>> = default();

        let scene_frp = scene_frp.clone_ref();
        let target = PointerTargetId::default();
        let last_position = Rc::new(Cell::new(Vector2::default()));
        let position = variables.add_or_panic("mouse_position", Vector2::default());
        let click_count = variables.add_or_panic("mouse_click_count", 0);
        let pointer_target_encoded = variables.add_or_panic("mouse_hover_ids", Vector4::default());
        let target = Rc::new(Cell::new(target));
        let shaped_dom = root.clone_ref().into();
        let mouse_manager = MouseManager::new(&shaped_dom, root, &web::window);
        let frp_deprecated = frp::io::Mouse_DEPRECATED::new();
        let last_move_event = Rc::new(RefCell::new(None));
        let on_move = mouse_manager.on_move.add(
            f!([pointer_target_registry, target, frp_deprecated, scene_frp, position, last_position, last_move_event]
            (event: &mouse::Move) {
                last_move_event.borrow_mut().replace(event.clone());
                let shape = scene_frp.shape.value();
                let pixel_ratio = shape.pixel_ratio;
                let new_pos = event.client();
                let pos_changed = new_pos != last_position.get();
                if pos_changed {
                    last_position.set(new_pos);
                    let new_canvas_position = new_pos.map(|v| (v * pixel_ratio) as i32);
                    position.set(new_canvas_position);
                    let position_bottom_left = new_pos;
                    let position_top_left = Vector2(new_pos.x, shape.height - new_pos.y);
                    let position = position_bottom_left - shape.center();
                    frp_deprecated.position_bottom_left.emit(position_bottom_left);
                    frp_deprecated.position_top_left.emit(position_top_left);
                    frp_deprecated.position.emit(position);

                    pointer_target_registry.with_mouse_target(target.get(), |_, d| {
                        d.emit_event(event.clone());
                    });
                }
            }),
        );
        let on_down = mouse_manager.on_down.add(
            f!([pointer_target_registry, target, last_pressed_elem, frp_deprecated, click_count, display_mode]
            (event:&mouse::Down) {
                click_count.modify(|v| *v += 1);
                if display_mode.get().allow_mouse_events() {
                    let button = event.button();
                    frp_deprecated.down.emit(button);

                    let current_target = target.get();
                    last_pressed_elem.borrow_mut().insert(button, current_target);
                    pointer_target_registry.with_mouse_target(current_target, |t, d| {
                        t.emit_mouse_down(button);
                        d.emit_event(event.clone());
                    });
                }
            }),
        );
        let on_up = mouse_manager.on_up.add(
            f!([pointer_target_registry, target, last_pressed_elem, frp_deprecated, display_mode]
            (event: &mouse::Up) {
                if display_mode.get().allow_mouse_events() {
                    let button = event.button();

                    let current_target = target.get();
                    if let Some(last_target) = last_pressed_elem.borrow_mut().remove(&button) {
                        pointer_target_registry.with_mouse_target(last_target, |t, d| {
                            d.emit_event(event.clone().unchecked_convert_to::<mouse::Release>());
                            t.emit_mouse_release(button);
                        });
                    }
                    pointer_target_registry.with_mouse_target(current_target, |t, d| {
                        d.emit_event(event.clone());
                        t.emit_mouse_up(button);
                    });
                    frp_deprecated.up.emit(button);
                }
            }),
        );
        let on_wheel = mouse_manager.on_wheel.add(
            f!([pointer_target_registry, target, frp_deprecated, display_mode] (event: &mouse::Wheel) {
                if display_mode.get().allow_mouse_events() {
                    frp_deprecated.wheel.emit(());
                    pointer_target_registry.with_mouse_target(target.get(), |_, d| {
                        d.emit_event(event.clone());
                    });
                }
            }),
        );

        let on_leave = mouse_manager.on_leave.add(f!((_event: &mouse::Leave)
            scene_frp.focused_source.emit(false);
        ));
        let on_enter = mouse_manager.on_enter.add(f!((_event: &mouse::Enter)
            scene_frp.focused_source.emit(true);
        ));

        let handles = Rc::new([on_move, on_down, on_up, on_wheel, on_leave, on_enter]);
        let hovered_objects = default();
        Self {
            pointer_target_registry,
            mouse_manager,
            last_position,
            position,
            click_count,
            pointer_target_encoded,
            target,
            handles,
            frp_deprecated,
            scene_frp,
            last_move_event,
            background,
            hovered_objects,
        }
    }

    /// Discover what object the mouse pointer is on.
    fn handle_over_and_out_events(&self) {
        let opt_new_target = PointerTargetId::decode_from_rgba(self.pointer_target_encoded.get());
        let new_target = opt_new_target.unwrap_or_else(|err| {
            error!("{err}");
            default()
        });
        self.switch_target(new_target);
    }

    /// Set mouse target and emit hover events if necessary.
    fn switch_target(&self, new_target: PointerTargetId) {
        let current_target = self.target.get();
        if new_target != current_target {
            self.target.set(new_target);
            if let Some(event) = (*self.last_move_event.borrow()).clone() {
                let mut new_hovered = self
                    .pointer_target_registry
                    .with_mouse_target(new_target, |_, d| d.rev_parent_chain())
                    .unwrap_or_default();
                new_hovered.sort();
                let currently_hovered_weak = self.hovered_objects.take().into_iter();
                let currently_hovered =
                    currently_hovered_weak.filter_map(|w| w.upgrade()).sorted().collect_vec();
                let left =
                    currently_hovered.iter().filter(|t| new_hovered.binary_search(t).is_err());
                for d in left {
                    let out_event = event.clone().unchecked_convert_to::<mouse::Out>();
                    let leave_event = event.clone().unchecked_convert_to::<mouse::Leave>();
                    d.emit_event(out_event);
                    d.emit_event_without_bubbling(leave_event);
                }
                let entered =
                    new_hovered.iter().filter(|t| currently_hovered.binary_search(t).is_err());
                for d in entered {
                    let over_event = event.clone().unchecked_convert_to::<mouse::Over>();
                    let enter_event = event.clone().unchecked_convert_to::<mouse::Enter>();
                    d.emit_event(over_event);
                    d.emit_event_without_bubbling(enter_event);
                }
                self.hovered_objects
                    .replace(new_hovered.into_iter().map(|t| t.downgrade()).collect_vec());

                self.pointer_target_registry.with_mouse_target(current_target, |t, _| {
                    t.mouse_out.emit(());
                });
                self.pointer_target_registry.with_mouse_target(new_target, |t, _| {
                    t.mouse_over.emit(());
                });

                // Re-emitting position event. See the docs of [`re_emit_position_event`] to learn
                // why.
                self.pointer_target_registry.with_mouse_target(new_target, |_, d| {
                    d.emit_event(event.clone());
                });
                self.re_emit_position_event();
            }
        }
    }

    /// Re-emits FRP mouse changed position event with the last mouse position value.
    ///
    /// The immediate question that appears is why it is even needed. The reason is tightly coupled
    /// with how the rendering engine works, and it is important to understand it properly. When
    /// moving a mouse the following events happen:
    /// - `MouseManager` gets notification and fires callbacks.
    /// - Callback above is run. The value of `screen_position` uniform changes and FRP events are
    ///   emitted.
    /// - FRP events propagate through the whole system.
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
        let position = new_pos - shape.center();
        self.frp_deprecated.position.emit(position);
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
    pub fn new(target: &web::EventTarget) -> Self {
        let frp = enso_frp::io::keyboard::Keyboard::default();
        let bindings = Rc::new(enso_frp::io::keyboard::DomBindings::new(target, &frp));
        Self { frp, bindings }
    }
}



// ===========
// === Dom ===
// ===========

/// DOM element manager. Creates root div element containing [`DomLayers`] upon construction and
/// removes them once dropped.
#[derive(Clone, Debug)]
pub struct Dom {
    /// Root DOM element of the scene.
    pub root:   web::dom::WithKnownShape<web::HtmlDivElement>,
    /// DomLayers of the scene.
    pub layers: DomLayers,
}

impl Dom {
    /// Constructor.
    pub fn new() -> Self {
        let root = web::document.create_div_or_panic();
        let layers = DomLayers::new(&root);
        root.set_class_name("scene");
        root.set_style_or_warn("height", "100vh");
        root.set_style_or_warn("width", "100vw");
        root.set_style_or_warn("display", "block");
        root.set_style_or_warn("outline", "none");
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

impl Default for Dom {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Dom {
    fn drop(&mut self) {
        self.root.remove();
    }
}



// =================
// === DomLayers ===
// =================

/// DOM DomLayers of the scene. It contains several CSS 3D layers and a canvas layer in the middle.
/// The CSS layers are used to manage DOM elements and to simulate depth-sorting of DOM and canvas
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
    /// DOM scene layer for Node Searcher DOM elements. This layer should probably be removed once
    /// all parts of Node Searcher will use ensogl primitives instead of DOM objects for rendering.
    pub node_searcher:  DomScene,
    /// The WebGL scene layer.
    pub canvas:         web::HtmlCanvasElement,
}

impl DomLayers {
    /// Constructor.
    pub fn new(dom: &web::HtmlDivElement) -> Self {
        let welcome_screen = DomScene::new();
        welcome_screen.dom.set_class_name("welcome_screen");
        welcome_screen.dom.set_style_or_warn("z-index", "0");
        dom.append_or_warn(&welcome_screen.dom);

        let back = DomScene::new();
        back.dom.set_class_name("back");
        back.dom.set_style_or_warn("z-index", "1");
        dom.append_or_warn(&back.dom);

        let fullscreen_vis = DomScene::new();
        fullscreen_vis.dom.set_class_name("fullscreen_vis");
        fullscreen_vis.dom.set_style_or_warn("z-index", "2");
        dom.append_or_warn(&fullscreen_vis.dom);

        let canvas = web::document.create_canvas_or_panic();
        canvas.set_style_or_warn("display", "block");
        canvas.set_style_or_warn("z-index", "3");
        // These properties are set by `DomScene::new` constructor for other layers.
        // See its documentation for more info.
        canvas.set_style_or_warn("position", "absolute");
        canvas.set_style_or_warn("height", "100vh");
        canvas.set_style_or_warn("width", "100vw");
        canvas.set_style_or_warn("pointer-events", "none");
        dom.append_or_warn(&canvas);

        let node_searcher = DomScene::new();
        node_searcher.dom.set_class_name("node-searcher");
        node_searcher.dom.set_style_or_warn("z-index", "4");
        dom.append_or_warn(&node_searcher.dom);

        let front = DomScene::new();
        front.dom.set_class_name("front");
        front.dom.set_style_or_warn("z-index", "5");
        dom.append_or_warn(&front.dom);

        Self { back, welcome_screen, fullscreen_vis, front, node_searcher, canvas }
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
    shape: ShapeDirty,
}

impl Dirty {
    pub fn new<OnMut: Fn() + Clone + 'static>(on_mut: OnMut) -> Self {
        Self { shape: ShapeDirty::new(Box::new(on_mut)) }
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
    dom:          Rc<Dom>,
    variables:    UniformScope,
    pub pipeline: Rc<CloneCell<render::Pipeline>>,
    pub composer: Rc<RefCell<Option<render::Composer>>>,
}

impl Renderer {
    fn new(dom: &Rc<Dom>, variables: &UniformScope) -> Self {
        let dom = dom.clone_ref();
        let variables = variables.clone_ref();
        let pipeline = default();
        let composer = default();
        Self { dom, variables, pipeline, composer }
    }

    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    fn set_context(&self, context: Option<&Context>) {
        let composer = context.map(|context| {
            // To learn more about the blending equations used here, please see the following
            // articles:
            // - http://www.realtimerendering.com/blog/gpus-prefer-premultiplication
            // - https://www.khronos.org/opengl/wiki/Blending#Colors
            context.enable(*Context::BLEND);
            context.blend_equation_separate(*Context::FUNC_ADD, *Context::FUNC_ADD);
            context.blend_func_separate(
                *Context::ONE,
                *Context::ONE_MINUS_SRC_ALPHA,
                *Context::ONE,
                *Context::ONE_MINUS_SRC_ALPHA,
            );

            let (width, height, pixel_ratio) = self.view_size();
            let pipeline = self.pipeline.get();
            render::Composer::new(&pipeline, context, &self.variables, width, height, pixel_ratio)
        });
        *self.composer.borrow_mut() = composer;
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
            let (width, height, pixel_ratio) = self.view_size();
            composer.resize(width, height, pixel_ratio);
        }
    }

    // The width and height in device pixels should be integers. If they are not then this is due to
    // rounding errors. We round to the nearest integer to compensate for those errors.
    fn view_size(&self) -> (i32, i32, f32) {
        let shape = self.dom.shape().device_pixels();
        let width = shape.width.ceil() as i32;
        let height = shape.height.ceil() as i32;
        let pixel_ratio = shape.pixel_ratio;
        (width, height, pixel_ratio)
    }

    /// Run the renderer.
    pub fn run(&self, update_status: UpdateStatus) {
        if let Some(composer) = &mut *self.composer.borrow_mut() {
            debug_span!("Running.").in_scope(|| {
                composer.run(update_status);
            })
        }
    }
}



// ==============
// === Layers ===
// ==============

type RectLayerPartition = LayerSymbolPartition<rectangle::Shape>;

/// Create a new layer partition with the given name.
fn partition_layer<S: display::shape::primitive::system::Shape>(
    base_layer: &Layer,
    name: &str,
) -> LayerSymbolPartition<S> {
    base_layer.create_symbol_partition::<S>(name)
}

/// Please note that currently the `Layers` structure is implemented in a hacky way. It assumes the
/// existence of several layers, which are needed for the GUI to display shapes properly. This
/// should be abstracted away in the future.
///
/// Scene layers hierarchy:
///
/// ```plaintext
/// - root
///   ├── viz
///   │   ├── viz_selection
///   │   ├── viz_resize_grip
///   │   ├── viz_overlay
///   ├── below_main
///   ├── main
///   │   ├── edges
///   │   ├── nodes
///   │   ├── above_inactive_nodes
///   │   ├── active_nodes
///   │   └── above_all_nodes
///   ├── widget
///   ├── port
///   ├── port_selection (Camera: port_selection_cam)
///   ├── label
///   ├── port_hover
///   ├── above_nodes
///   ├── above_nodes_text
///   ├── panel_background (Camera: panel_cam)
///   │   ├── bottom
///   │   └── top
///   ├── panel (Camera: panel_cam)
///   ├── panel_text (Camera: panel_cam)
///   ├── node_searcher (Camera: node_searcher_cam)
///   ├── node_searcher_text (Camera: node_searcher_cam)
///   ├── edited_node (Camera: edited_node_cam)
///   ├── edited_node_text (Camera: edited_node_cam)
///   ├── tooltip
///   ├── tooltip_text
///   └── cursor (Camera: cursor_cam)
/// - DETACHED
/// ```
#[derive(Clone, CloneRef, Debug)]
#[allow(non_snake_case)]
pub struct HardcodedLayers {
    /// A special layer used to store shapes not attached to any layer. This layer will not be
    /// rendered. You should not need to use it directly.
    pub DETACHED: Layer,
    pub root: Layer,
    pub viz: Layer,
    pub viz_selection: RectLayerPartition,
    pub viz_resize_grip: RectLayerPartition,
    pub viz_overlay: RectLayerPartition,
    pub below_main: Layer,
    pub main: Layer,
    pub main_edges_level: RectLayerPartition,
    pub main_nodes_level: RectLayerPartition,
    pub main_above_inactive_nodes_level: RectLayerPartition,
    pub main_active_nodes_level: RectLayerPartition,
    pub main_above_all_nodes_level: RectLayerPartition,
    pub widget: Layer,
    pub port: Layer,
    pub port_selection: Layer,
    pub label: Layer,
    pub port_hover: Layer,
    pub above_nodes: Layer,
    pub above_nodes_text: Layer,
    // `panel_*` layers contains UI elements with fixed position (not moving with the panned scene)
    // like status bar, breadcrumbs or similar.
    pub panel_background_rect_level_0: RectLayerPartition,
    pub panel_background_rect_level_1: RectLayerPartition,
    pub panel_background: Layer,
    pub panel: Layer,
    pub panel_text: Layer,
    pub panel_overlay: RectLayerPartition,
    pub node_searcher: Layer,
    pub node_searcher_text: Layer,
    pub edited_node: Layer,
    pub edited_node_text: Layer,
    pub tooltip: Layer,
    pub tooltip_text: Layer,
    pub cursor: Layer,
}

impl Deref for HardcodedLayers {
    type Target = Layer;
    fn deref(&self) -> &Self::Target {
        &self.root
    }
}

impl HardcodedLayers {
    pub fn new() -> Self {
        let main_cam = Camera2d::new();
        let node_searcher_cam = Camera2d::new();
        let panel_cam = Camera2d::new();
        let edited_node_cam = Camera2d::new();
        let port_selection_cam = Camera2d::new();
        let cursor_cam = Camera2d::new();

        #[allow(non_snake_case)]
        let DETACHED = Layer::new("DETACHED");
        let root = Layer::new_with_camera("root", &main_cam);

        let viz = root.create_sublayer("viz");
        let viz_selection = partition_layer(&viz, "viz_selection");
        let viz_resize_grip = partition_layer(&viz, "viz_resize_grip");
        let viz_overlay = partition_layer(&viz, "viz_overlay");
        let below_main = root.create_sublayer("below_main");
        let main = root.create_sublayer("main");
        let main_edges_level = partition_layer(&main, "edges");
        let main_nodes_level = partition_layer(&main, "nodes");
        let main_above_inactive_nodes_level = partition_layer(&main, "above_inactive_nodes");
        let main_active_nodes_level = partition_layer(&main, "active_nodes");
        let main_above_all_nodes_level = partition_layer(&main, "above_all_nodes");
        let widget = root.create_sublayer("widget");
        let port = root.create_sublayer("port");
        let port_selection =
            root.create_sublayer_with_camera("port_selection", &port_selection_cam);
        let label = root.create_sublayer("label");
        let port_hover = root.create_sublayer("port_hover");
        let above_nodes = root.create_sublayer("above_nodes");
        let above_nodes_text = root.create_sublayer("above_nodes_text");


        let panel_background = root.create_sublayer_with_camera("panel_background", &panel_cam);
        let panel_background_rect_level_0 = partition_layer(&panel_background, "bottom");
        let panel_background_rect_level_1 = partition_layer(&panel_background, "top");
        let panel = root.create_sublayer_with_camera("panel", &panel_cam);
        let panel_text = root.create_sublayer_with_camera("panel_text", &panel_cam);
        let panel_overlay = partition_layer(&panel_text, "overlay");
        let node_searcher = root.create_sublayer_with_camera("node_searcher", &node_searcher_cam);
        let node_searcher_text =
            root.create_sublayer_with_camera("node_searcher_text", &node_searcher_cam);
        let edited_node = root.create_sublayer_with_camera("edited_node", &edited_node_cam);
        let edited_node_text =
            root.create_sublayer_with_camera("edited_node_text", &edited_node_cam);
        let tooltip = root.create_sublayer("tooltip");
        let tooltip_text = root.create_sublayer("tooltip_text");
        let cursor = root.create_sublayer_with_camera("cursor", &cursor_cam);

        Self {
            DETACHED,
            root,
            viz,
            viz_selection,
            viz_resize_grip,
            viz_overlay,
            below_main,
            main,
            main_edges_level,
            main_nodes_level,
            main_above_inactive_nodes_level,
            main_active_nodes_level,
            main_above_all_nodes_level,
            widget,
            port,
            port_selection,
            label,
            port_hover,
            above_nodes,
            above_nodes_text,
            panel_background,
            panel,
            panel_text,
            panel_overlay,
            node_searcher,
            node_searcher_text,
            edited_node,
            edited_node_text,
            tooltip,
            tooltip_text,
            cursor,
            panel_background_rect_level_0,
            panel_background_rect_level_1,
        }
    }
}

impl Default for HardcodedLayers {
    fn default() -> Self {
        Self::new()
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
    pub focused:           frp::Stream<bool>,
    camera_changed_source: frp::Source,
    frame_time_source:     frp::Source<f32>,
    focused_source:        frp::Source<bool>,
    post_update:           frp::Source,
}

impl Frp {
    /// Constructor
    pub fn new(shape: &frp::Sampler<Shape>) -> Self {
        frp::new_network! { network
            camera_changed_source <- source();
            frame_time_source <- source();
            focused_source <- source();
            post_update <- source();
        }
        let shape = shape.clone_ref();
        let camera_changed = camera_changed_source.clone_ref().into();
        let frame_time = frame_time_source.clone_ref().into();
        let focused = focused_source.clone_ref().into();
        Self {
            network,
            shape,
            camera_changed,
            frame_time,
            focused,
            camera_changed_source,
            frame_time_source,
            focused_source,
            post_update,
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



// ====================
// === UpdateStatus ===
// ====================

/// Scene update status. Used to tell the renderer what is the minimal amount of per-frame
/// processing. For example, if scene was not dirty (no animations), but pointer position changed,
/// the scene should not be re-rendered, but the id-texture pixel under the mouse should be read.
#[derive(Clone, Copy, Debug, Default)]
pub struct UpdateStatus {
    pub scene_was_dirty:          bool,
    pub pointer_position_changed: bool,
}



// =================
// === SceneData ===
// =================

#[derive(Debug)]
pub struct SceneData {
    pub display_object: display::object::Root,
    pub dom: Rc<Dom>,
    pub context: Rc<RefCell<Option<Context>>>,
    pub variables: UniformScope,
    pub mouse: Mouse,
    pub keyboard: Keyboard,
    pub uniforms: Uniforms,
    pub stats: Stats,
    pub dirty: Dirty,
    pub renderer: Renderer,
    pub layers: HardcodedLayers,
    pub style_sheet: style::Sheet,
    pub bg_color_var: style::Var,
    pub bg_color_change: callback::Handle,
    pub frp: Frp,
    pub pointer_position_changed: Rc<Cell<bool>>,
    pub shader_compiler: shader::compiler::Controller,
    initial_shader_compilation: Rc<Cell<TaskState>>,
    display_mode: Rc<Cell<glsl::codes::DisplayModes>>,
    extensions: Extensions,
    disable_context_menu: Rc<EventListenerHandle>,
}

impl SceneData {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<OnMut: Fn() + Clone + 'static>(
        stats: &Stats,
        on_mut: OnMut,
        display_mode: &Rc<Cell<glsl::codes::DisplayModes>>,
    ) -> Self {
        debug!("Initializing.");
        let display_mode = display_mode.clone_ref();
        let dom = default();
        let display_object = display::object::Root::new_named("Scene");
        let variables = world::with_context(|t| t.variables.clone_ref());
        let dirty = Dirty::new(on_mut);
        let layers = world::with_context(|t| t.layers.clone_ref());
        let stats = stats.clone();
        let uniforms = Uniforms::new(&variables);
        let renderer = Renderer::new(&dom, &variables);
        let style_sheet = world::with_context(|t| t.style_sheet.clone_ref());
        let frp = Frp::new(&dom.root.shape);
        let mouse = Mouse::new(&frp, &display_object, &dom.root, &variables, &display_mode);
        let disable_context_menu = Rc::new(web::ignore_context_menu(&dom.root));
        let keyboard = Keyboard::new(&web::window);
        let network = &frp.network;
        let extensions = Extensions::default();
        let bg_color_var = style_sheet.var("application.background");
        let bg_color_change = bg_color_var.on_change(f!([dom](change){
            change.color().for_each(|color| {
                let color = color.to_javascript_string();
                dom.root.set_style_or_warn("background-color",color);
            })
        }));

        layers.main.add(&display_object);
        frp::extend! { network
            eval_ frp.shape (dirty.shape.set());
        }

        uniforms.pixel_ratio.set(dom.shape().pixel_ratio);
        let context = default();
        let pointer_position_changed = default();
        let shader_compiler = default();
        let initial_shader_compilation = default();
        Self {
            display_object,
            display_mode,
            dom,
            context,
            variables,
            mouse,
            keyboard,
            uniforms,
            stats,
            dirty,
            renderer,
            layers,
            style_sheet,
            bg_color_var,
            bg_color_change,
            frp,
            pointer_position_changed,
            shader_compiler,
            initial_shader_compilation,
            extensions,
            disable_context_menu,
        }
        .init()
    }

    fn init(self) -> Self {
        self.init_pointer_position_changed_check();
        self
    }

    pub fn shape(&self) -> &frp::Sampler<Shape> {
        &self.dom.root.shape
    }

    pub fn camera(&self) -> Camera2d {
        self.layers.main.camera()
    }

    pub fn new_symbol(&self, label: &'static str) -> Symbol {
        world::with_context(|t| t.new(label))
    }

    /// If enabled, the scene will be rendered with 1.0 device pixel ratio, even on high-dpi
    /// monitors.
    pub fn low_resolution_mode(&self, enabled: bool) {
        if enabled {
            self.dom.root.override_device_pixel_ratio(Some(1.0));
        } else {
            self.dom.root.override_device_pixel_ratio(None);
        }
    }

    fn update_shape(&self) -> bool {
        if self.dirty.shape.check_all() {
            let screen = self.dom.shape();
            self.resize_canvas(screen);
            self.layers.iter_sublayers_and_masks_nested(|layer| {
                layer.camera().set_screen(screen.width, screen.height)
            });
            self.renderer.resize_composer();
            self.dirty.shape.unset_all();
            true
        } else {
            false
        }
    }

    fn update_symbols(&self) -> bool {
        world::with_context(|context| context.update())
    }

    fn update_camera(&self, scene: &Scene) -> bool {
        let mut was_dirty = false;
        // Updating camera for DOM layers. Please note that DOM layers cannot use multi-camera
        // setups now, so we are using here the main camera only.
        let camera = self.camera();
        let fullscreen_vis_camera = self.layers.panel.camera();
        // We are using unnavigable camera to disable panning behavior.
        let welcome_screen_camera = self.layers.panel.camera();
        let changed = camera.update(scene);
        if changed {
            was_dirty = true;
            self.frp.camera_changed_source.emit(());
            world::with_context(|t| t.set_camera(&camera));
            self.dom.layers.front.update_view_projection(&camera);
            self.dom.layers.back.update_view_projection(&camera);
        }
        let fs_vis_camera_changed = fullscreen_vis_camera.update(scene);
        if fs_vis_camera_changed {
            was_dirty = true;
            self.dom.layers.fullscreen_vis.update_view_projection(&fullscreen_vis_camera);
            self.dom.layers.welcome_screen.update_view_projection(&welcome_screen_camera);
        }
        let node_searcher_camera = self.layers.node_searcher.camera();
        let node_searcher_camera_changed = node_searcher_camera.update(scene);
        if node_searcher_camera_changed {
            was_dirty = true;
            self.dom.layers.node_searcher.update_view_projection(&node_searcher_camera);
        }

        // Updating all other cameras (the main camera was already updated, so it will be skipped).
        self.layers.iter_sublayers_and_masks_nested(|layer| {
            let dirty = layer.camera().update(scene);
            layer.update_debug_view();
            was_dirty = was_dirty || dirty;
        });

        was_dirty
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
        debug_span!("Resized to {}px x {}px.", screen.width, screen.height).in_scope(|| {
            self.dom.layers.canvas.set_attribute_or_warn("width", width.to_string());
            self.dom.layers.canvas.set_attribute_or_warn("height", height.to_string());
            if let Some(context) = &*self.context.borrow() {
                context.viewport(0, 0, width, height);
            }
        });
    }

    pub fn render(&self, update_status: UpdateStatus) {
        if let Some(context) = &*self.context.borrow() {
            context.profiler.measure_drawing(|| {
                self.renderer.run(update_status);
                // WebGL `flush` should be called when expecting results such as queries, or at
                // completion of a rendering frame. Flush tells the implementation
                // to push all pending commands out for execution, flushing them out
                // of the queue, instead of waiting for more commands to
                // enqueue before sending for execution.
                //
                // Not flushing commands can sometimes cause context loss. To learn more, see:
                // [https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices#flush_when_expecting_results].

                context.flush()
            });
        }
    }

    pub fn screen_to_scene_coordinates(&self, position: Vector3<f32>) -> Vector3<f32> {
        let zoom = self.camera().zoom();
        if zoom == 0.0 {
            return Vector3::zero();
        }
        let position = position / zoom;
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
        let layer = object.display_layer();
        let camera = layer.map_or(self.camera(), |l| l.camera());
        let origin_clip_space = camera.view_projection_matrix() * origin_world_space;
        if let Some(inv_object_matrix) = object.transformation_matrix().try_inverse() {
            let shape = camera.screen();
            let clip_space_z = origin_clip_space.z;
            let clip_space_x = origin_clip_space.w * 2.0 * screen_pos.x / shape.width;
            let clip_space_y = origin_clip_space.w * 2.0 * screen_pos.y / shape.height;
            let clip_space = Vector4(clip_space_x, clip_space_y, clip_space_z, origin_clip_space.w);
            let world_space = camera.inversed_view_projection_matrix() * clip_space;
            (inv_object_matrix * world_space).xy()
        } else {
            warn!(
                "The object transformation matrix is not invertible, \
                this can cause visual artifacts."
            );
            default()
        }
    }
}


// === Mouse ===

impl SceneData {
    fn init_pointer_position_changed_check(&self) {
        let network = &self.frp.network;
        let pointer_position_changed = &self.pointer_position_changed;
        frp::extend! { network
            eval_ self.mouse.frp_deprecated.position (pointer_position_changed.set(true));
        }
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
    no_mut_access:        Rc<SceneData>,
    // Context handlers keep reference to SceneData, thus cannot be put inside it.
    context_lost_handler: Rc<RefCell<Option<ContextLostHandler>>>,
}

impl Scene {
    pub fn new<OnMut: Fn() + Clone + 'static>(
        stats: &Stats,
        on_mut: OnMut,
        display_mode: &Rc<Cell<glsl::codes::DisplayModes>>,
    ) -> Self {
        let no_mut_access = SceneData::new(stats, on_mut, display_mode);
        let context_lost_handler = default();
        let this = Self { no_mut_access: Rc::new(no_mut_access), context_lost_handler };
        this
    }

    pub fn display_in(&self, parent_dom: impl DomPath) {
        match parent_dom.try_into_dom_element() {
            None => error!("The scene host element could not be found."),
            Some(parent_dom) => {
                parent_dom.append_or_warn(&self.dom.root);
                self.dom.recompute_shape_with_reflow();
                self.uniforms.pixel_ratio.set(self.dom.shape().pixel_ratio);
                self.init();
            }
        }
    }

    fn init(&self) {
        let context_loss_handler =
            crate::system::gpu::context::init_webgl_2_context(&self.no_mut_access);
        match context_loss_handler {
            Err(err) => error!("{err}"),
            Ok(handler) => self.context_lost_handler.set(handler),
        }
    }

    /// Run the GPU profiler. If the result is [`None`], either the GPU context is not initialized
    /// to the profiler is not available at the current platform. In case the resulting vector
    /// is empty, the previous frame measurements are not available yet and they will be
    /// provided in the future.
    pub fn on_frame_start(&self) -> Option<Vec<Results>> {
        if let Some(context) = &*self.context.borrow() {
            context.profiler.start_frame()
        } else {
            None
        }
    }

    pub fn extension<T: Extension>(&self) -> T {
        self.extensions.get(self)
    }

    /// Begin any preparation necessary to render, e.g. compilation of shaders. Returns when the
    /// scene is ready to start being displayed.
    #[profile(Task)]
    pub async fn prepare_to_render(&self) {
        match self.initial_shader_compilation.get() {
            TaskState::Unstarted => {
                self.begin_shader_initialization();
                self.next_shader_compiler_idle().await;
                self.initial_shader_compilation.set(TaskState::Completed);
            }
            TaskState::Running => self.next_shader_compiler_idle().await,
            TaskState::Completed => (),
        }
    }

    /// Begin compiling shaders.
    #[profile(Task)]
    pub fn begin_shader_initialization(&self) {
        if self.initial_shader_compilation.get() != TaskState::Unstarted {
            return;
        }
        world::SHAPES_DEFINITIONS.with_borrow(|shapes| {
            for shape in shapes.iter().filter(|shape| shape.is_main_application_shape()) {
                // Instantiate shape so that its shader program will be submitted to the
                // shader compiler. The runtime compiles the shaders in background threads,
                // and starting early ensures they will be ready when we want to render
                // them.
                let _shape = (shape.cons)();
            }
        });
        self.initial_shader_compilation.set(TaskState::Running);
    }

    /// Wait until the next time the compiler goes from busy to idle. If the compiler is already
    /// idle, this will complete during the next shader-compiler run.
    pub async fn next_shader_compiler_idle(&self) {
        if let Some(context) = &*self.context.borrow() {
            // Ensure the callback will be run if the queue is already idle.
            context.shader_compiler.submit_probe_job();
        } else {
            return;
        };
        // Register a callback that triggers a future, and await it.
        let (sender, receiver) = futures::channel::oneshot::channel();
        let sender = Box::new(RefCell::new(Some(sender)));
        let _handle = self.shader_compiler.on_idle(move || {
            if let Some(sender) = sender.take() {
                sender.send(()).unwrap();
            }
        });
        receiver.await.unwrap();
    }
}

impl system::gpu::context::Display for Scene {
    fn device_context_handler(&self) -> &system::gpu::context::DeviceContextHandler {
        self.no_mut_access.device_context_handler()
    }

    fn set_context(&self, context: Option<&Context>) {
        self.no_mut_access.set_context(context)
    }
}

impl system::gpu::context::Display for Rc<SceneData> {
    fn device_context_handler(&self) -> &system::gpu::context::DeviceContextHandler {
        &self.dom.layers.canvas
    }

    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    fn set_context(&self, context: Option<&Context>) {
        let _profiler = profiler::start_objective!(profiler::APP_LIFETIME, "@set_context");
        world::with_context(|t| t.set_context(context));
        *self.context.borrow_mut() = context.cloned();
        self.dirty.shape.set();
        self.renderer.set_context(context);
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
    /// Perform layout phase of scene update. This includes updating camera and the layout of all
    /// display objects. No GPU buffers are updated yet, giving the opportunity to perform
    /// additional updates that affect the layout of display objects after the main scene layout
    /// has been performed.
    ///
    /// During this phase, the layout updates can be observed using `on_transformed` FRP events on
    /// each individual display object. Any further updates to the scene may require the `update`
    /// method to be manually called on affected objects in order to affect rendering
    /// during this frame.
    #[profile(Debug)]
    pub fn update_layout(&self, time: animation::TimeInfo) -> UpdateStatus {
        debug_span!("Early update.").in_scope(|| {
            world::with_context(|t| t.theme_manager.update());

            let mut scene_was_dirty = false;
            self.frp.frame_time_source.emit(time.since_animation_loop_started.unchecked_raw());
            // Please note that `update_camera` is called first as it may trigger FRP events
            // which may change display objects layout.
            scene_was_dirty |= self.update_camera(self);
            self.display_object.update(self);
            UpdateStatus { scene_was_dirty, pointer_position_changed: false }
        })
    }

    /// Perform rendering phase of scene update. At this point, all display object state is being
    /// committed for rendering. This includes updating the layer stack, refreshing GPU buffers and
    /// handling mouse events.
    #[profile(Debug)]
    pub fn update_rendering(
        &self,
        time: animation::TimeInfo,
        early_status: UpdateStatus,
    ) -> UpdateStatus {
        debug_span!("Late update.").in_scope(|| {
            let UpdateStatus { mut scene_was_dirty, mut pointer_position_changed } = early_status;
            scene_was_dirty |= self.layers.update();
            scene_was_dirty |= self.update_shape();
            if let Some(context) = &*self.context.borrow() {
                context.profiler.measure_data_upload(|| {
                    scene_was_dirty |= self.update_symbols();
                });
                self.mouse.handle_over_and_out_events();
                scene_was_dirty |= self.shader_compiler.run(context, time);
            }
            pointer_position_changed |= self.pointer_position_changed.get();
            self.pointer_position_changed.set(false);

            // FIXME: setting it to true for now in order to make cursor blinking work.
            //   Text cursor animation is in GLSL. To be handled properly in this PR:
            //   #183406745
            scene_was_dirty |= true;
            UpdateStatus { scene_was_dirty, pointer_position_changed }
        })
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


// === Initial shader compilation state ===

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq)]
enum TaskState {
    #[default]
    Unstarted,
    Running,
    Completed,
}



// ==================
// === Test Utils ===
// ==================

/// Extended API for tests.
pub mod test_utils {
    use super::*;
    use display::shape::ShapeInstance;
    use enso_callback::traits::*;

    pub trait MouseExt {
        /// Set the pointer target and emit mouse enter/leave and over/out events.
        fn set_hover_target(&self, target: PointerTargetId) -> &Self;
        /// Simulate mouse down event.
        fn emit_down(&self, event: mouse::Down) -> &Self;
        /// Simulate mouse up event.
        fn emit_up(&self, event: mouse::Up) -> &Self;
        /// Simulate mouse move event.
        fn emit_move(&self, event: mouse::Move) -> &Self;
        /// Simulate mouse wheel event.
        fn emit_wheel(&self, event: mouse::Wheel) -> &Self;
        /// Get current screen shape.
        fn screen_shape(&self) -> Shape;

        /// Convert main camera scene position to needed event position in test environment.
        fn scene_to_event_position(&self, scene_pos: Vector2) -> Vector2 {
            scene_pos + self.screen_shape().center()
        }

        /// Simulate mouse move and hover with manually specified event data and target.
        fn hover_raw(&self, data: mouse::MouseEventData, target: PointerTargetId) -> &Self {
            self.emit_move(mouse::Move::simulated(data, self.screen_shape()))
                .set_hover_target(target)
        }

        /// Simulate mouse hover and click with manually specified event data and target.
        fn click_on_raw(&self, data: mouse::MouseEventData, target: PointerTargetId) -> &Self {
            self.hover_raw(data, target)
                .emit_down(mouse::Down::simulated(data, self.screen_shape()))
                .emit_up(mouse::Up::simulated(data, self.screen_shape()))
        }

        /// Simulate mouse hover on a on given shape.
        fn hover<S>(&self, instance: &ShapeInstance<S>, scene_pos: Vector2) -> &Self {
            let pos = self.scene_to_event_position(scene_pos);
            self.hover_raw(mouse::MouseEventData::primary_at(pos), PointerTargetId::Symbol {
                id: instance.sprite.borrow().global_instance_id,
            })
        }

        /// Simulate mouse hover on background.
        fn hover_background(&self, scene_pos: Vector2) -> &Self {
            let pos = self.scene_to_event_position(scene_pos);
            self.hover_raw(mouse::MouseEventData::primary_at(pos), PointerTargetId::Background)
        }

        /// Simulate mouse hover and click on given shape.
        fn click_on<S>(&self, instance: &ShapeInstance<S>, scene_pos: Vector2) {
            let pos = self.scene_to_event_position(scene_pos);
            self.click_on_raw(mouse::MouseEventData::primary_at(pos), PointerTargetId::Symbol {
                id: instance.sprite.borrow().global_instance_id,
            });
        }
        /// Simulate mouse click on background.
        fn click_on_background(&self, scene_pos: Vector2) {
            let pos = self.scene_to_event_position(scene_pos);
            self.click_on_raw(mouse::MouseEventData::primary_at(pos), PointerTargetId::Background);
        }
    }

    impl MouseExt for Mouse {
        fn set_hover_target(&self, target: PointerTargetId) -> &Self {
            self.switch_target(target);
            self
        }

        fn emit_down(&self, event: mouse::Down) -> &Self {
            self.mouse_manager.on_down.run_all(&event);
            self
        }

        fn emit_up(&self, event: mouse::Up) -> &Self {
            self.mouse_manager.on_up.run_all(&event);
            self
        }

        fn emit_move(&self, event: mouse::Move) -> &Self {
            self.mouse_manager.on_move.run_all(&event);
            self
        }

        fn emit_wheel(&self, event: mouse::Wheel) -> &Self {
            self.mouse_manager.on_wheel.run_all(&event);
            self
        }

        fn screen_shape(&self) -> Shape {
            self.scene_frp.shape.value()
        }
    }
}
