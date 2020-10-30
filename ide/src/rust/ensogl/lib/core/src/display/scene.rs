#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod dom;

pub use crate::display::symbol::registry::SymbolId;
pub use crate::system::web::dom::Shape;

use crate::prelude::*;

use crate::animation;
use crate::control::callback;
use crate::control::io::mouse::MouseManager;
use crate::control::io::mouse;
use crate::data::color;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2d;
use crate::display::render::RenderComposer;
use crate::display::render::RenderPipeline;
use crate::display::scene::dom::DomScene;
use crate::display::shape::ShapeSystemInstance;
use crate::display::shape::system::ShapeSystemOf;
use crate::display::style::data::DataMatch;
use crate::display::style;
use crate::display::symbol::Symbol;
use crate::display::symbol::registry::SymbolRegistry;
use crate::display;
use crate::gui::component;
use crate::system::gpu::data::uniform::Uniform;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::gpu::shader::Context;
use crate::system::web::IgnoreContextMenuHandle;
use crate::system::web::NodeInserter;
use crate::system::web::StyleSetter;
use crate::system::web;


use enso_frp as frp;
use enso_frp::io::js::CurrentJsEvent;
use std::any::TypeId;
use web_sys::HtmlElement;


pub trait MouseTarget : Debug + 'static {
    fn mouse_down (&self) -> &frp::Source;
    fn mouse_over (&self) -> &frp::Source;
    fn mouse_out  (&self) -> &frp::Source;
}



// =========================
// === ComponentRegistry ===
// =========================

shared! { ShapeRegistry
#[derive(Debug,Default)]
pub struct ShapeRegistryData {
    scene            : Option<Scene>,
    shape_system_map : HashMap<TypeId,Box<dyn Any>>,
    mouse_target_map : HashMap<(i32,usize),Rc<dyn MouseTarget>>,
}

impl {
    fn get<T:ShapeSystemInstance>(&self) -> Option<T> {
        let id = TypeId::of::<T>();
        self.shape_system_map.get(&id).and_then(|any| any.downcast_ref::<T>()).map(|t| t.clone_ref())
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

    pub fn shape_system<T:display::shape::system::Shape>(&mut self, _phantom:PhantomData<T>) -> ShapeSystemOf<T> {
        self.get_or_register::<ShapeSystemOf<T>>()
    }

    pub fn new_instance<T:display::shape::system::Shape>(&mut self) -> T {
        let system = self.get_or_register::<ShapeSystemOf<T>>();
        system.new_instance()
    }

    pub fn insert_mouse_target<T:MouseTarget>(&mut self, symbol_id:i32, instance_id:usize, target:T) {
        let target = Rc::new(target);
        self.mouse_target_map.insert((symbol_id,instance_id),target);
    }

    pub fn remove_mouse_target(&mut self, symbol_id:i32, instance_id:usize) {
        self.mouse_target_map.remove(&(symbol_id,instance_id));
    }

    pub fn get_mouse_target(&mut self, target:Target) -> Option<Rc<dyn MouseTarget>> {
        match target {
            Target::Background => None,
            Target::Symbol {symbol_id,instance_id} => {
                let symbol_id   = symbol_id   as i32;
                let instance_id = instance_id as usize;
                self.mouse_target_map.get(&(symbol_id,instance_id)).map(|t| t.clone_ref())
            }
        }
    }
}}



// ==============
// === Target ===
// ==============

/// Result of a Decoding operation in the Target.
#[derive(Debug,Clone,Copy,Eq,PartialEq)]
enum DecodingResult{
    /// Values had to be truncated.
    Truncated(u8,u8,u8),
    /// Values have been encoded successfully.
    Ok(u8,u8,u8)
}

/// Mouse target. Contains a path to an object pointed by mouse.
#[derive(Debug,Clone,Copy,Eq,PartialEq)]
pub enum Target {
    Background,
    Symbol {
        symbol_id   : u32,
        instance_id : u32,
    }
}

impl Target {

    /// Encode two u32 values into three u8 values.
    ///
    /// This is the same encoding that is used in the `fragment_runner`. This encoding is lossy and
    /// can only encode values up to 12^2=4096 each
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
    /// 32    24    16     8      0         32    24    16     8      0   <- Bit index
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
    fn encode(value1:u32, value2:u32) -> DecodingResult {
        let chunk1 = (value1 >> 4u32) & 0x00FFu32;
        let chunk2 = (value1 & 0x000Fu32) << 4u32;
        let chunk2 = chunk2 | ((value2 & 0x0F00u32) >> 8u32);
        let chunk3 = value2 & 0x00FFu32;

        if value1 > 2u32.pow(12) ||value2 > 2u32.pow(12) {
            DecodingResult::Truncated(chunk1 as u8, chunk2 as u8, chunk3 as u8)
        }else{
            DecodingResult::Ok(chunk1 as u8, chunk2 as u8, chunk3 as u8)
        }
    }

    /// Decode the symbol_id and instance_id that was encoded in the `fragment_runner`.
    ///
    /// See the `encode` method for more information on the encoding.
    fn decode(chunk1:u32, chunk2:u32, chunk3:u32) -> (u32, u32) {
        let value1 = (chunk1 << 4) + (chunk2 >> 4);
        let value2 = chunk3 + ((chunk2 & 0x000F) << 8);
        (value1, value2)
    }

    fn to_internal(&self, logger:&Logger) -> Vector4<u32> {
        match self {
            Self::Background                     => Vector4::new(0,0,0,0),
            Self::Symbol {symbol_id,instance_id} => {
                match Self::encode(*symbol_id,*instance_id) {
                    DecodingResult::Truncated(pack0,pack1,pack2) => {
                        warning!(logger,"Target values too big to encode: \
                                         ({symbol_id},{instance_id}).");
                        Vector4::new(pack0.into(),pack1.into(),pack2.into(),1)
                    },
                    DecodingResult::Ok(pack0,pack1,pack2) => {
                        Vector4::new(pack0.into(),pack1.into(),pack2.into(),1)
                    },
                }
            },
        }
    }

    fn from_internal(v:Vector4<u32>) -> Self {
        if v.w == 0 {
            Self::Background
        }
        else if v.w == 255 {
            let decoded     = Self::decode(v.x,v.y,v.z);
            let symbol_id   = decoded.0;
            let instance_id = decoded.1;
            Self::Symbol {symbol_id,instance_id}
        } else {
            panic!("Wrong internal format alpha for mouse target.")
        }
    }
}

impl Default for Target {
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
    fn assert_valid_roundtrip(value1:u32, value2:u32) {
        let pack   = Target::encode(value1,value2);
        match pack {
            DecodingResult::Truncated {..} => {
               panic!("Values got truncated. This is an invalid test case: {}, {}", value1, value1)
            },
            DecodingResult::Ok(pack0,pack1,pack2) => {
                let unpack = Target::decode(pack0.into(),pack1.into(),pack2.into());
                assert_eq!(unpack.0,value1);
                assert_eq!(unpack.1,value2);
            },
        }
    }

    #[test]
    fn test_roundtrip_coding() {
        assert_valid_roundtrip(   0,   0);
        assert_valid_roundtrip(   0,   5);
        assert_valid_roundtrip( 512,   0);
        assert_valid_roundtrip(1024,  64);
        assert_valid_roundtrip(1024, 999);
    }

    #[test]
    fn test_encoding() {
        let pack = Target::encode(0,0);
        assert_eq!(pack,DecodingResult::Ok(0,0,0));

        let pack = Target::encode(3,7);
        assert_eq!(pack,DecodingResult::Ok(0,48,7));

        let pack = Target::encode(3,256);
        assert_eq!(pack,DecodingResult::Ok(0,49,0));

        let pack = Target::encode(255,356);
        assert_eq!(pack,DecodingResult::Ok(15,241,100));

        let pack = Target::encode(256,356);
        assert_eq!(pack,DecodingResult::Ok(16,1,100));

        let pack = Target::encode(31256,0);
        assert_eq!(pack,DecodingResult::Truncated(161,128,0));
    }
}



// =============
// === Mouse ===
// =============

#[derive(Clone,CloneRef,Debug)]
pub struct Mouse {
    pub mouse_manager : MouseManager,
    pub last_position : Rc<Cell<Vector2<i32>>>,
    pub position      : Uniform<Vector2<i32>>,
    pub hover_ids     : Uniform<Vector4<u32>>,
    pub target        : Rc<Cell<Target>>,
    pub handles       : Rc<[callback::Handle;3]>,
    pub frp           : enso_frp::io::Mouse,
    pub scene_frp     : Frp,
    pub logger        : Logger
}

impl Mouse {
    pub fn new
    (scene_frp:&Frp, variables:&UniformScope, current_js_event:&CurrentJsEvent, logger:Logger)
    -> Self {
        let scene_frp       = scene_frp.clone_ref();
        let target          = Target::default();
        let last_position   = Rc::new(Cell::new(Vector2::new(0,0)));
        let position        = variables.add_or_panic("mouse_position",Vector2::new(0,0));
        let hover_ids       = variables.add_or_panic("mouse_hover_ids",target.to_internal(&logger));
        let target          = Rc::new(Cell::new(target));
        let body            = web::dom::WithKnownShape::new(&web::document().body().unwrap());
        let mouse_manager   = MouseManager::new_separated(&body.into(),&web::window());
        let frp             = frp::io::Mouse::new();
        let on_move         = mouse_manager.on_move.add(current_js_event.make_event_handler(
            f!([frp,scene_frp,position,last_position] (event:&mouse::OnMove) {
                let shape       = scene_frp.shape.value();
                let pixel_ratio = shape.pixel_ratio as i32;
                let screen_x    = event.client_x();
                let screen_y    = event.client_y();

                let new_pos     = Vector2::new(screen_x,screen_y);
                let pos_changed = new_pos != last_position.get();
                if pos_changed {
                    last_position.set(new_pos);
                    let new_canvas_position = new_pos * pixel_ratio;
                    position.set(new_canvas_position);
                    let position = Vector2(new_pos.x as f32,new_pos.y as f32) - shape.center();
                    frp.position.emit(position);
                }
            }
        )));
        let on_down = mouse_manager.on_down.add(current_js_event.make_event_handler(
            f!((event:&mouse::OnDown) frp.down.emit(event.button())))
        );
        let on_up = mouse_manager.on_up.add(current_js_event.make_event_handler(
            f!((event:&mouse::OnUp) frp.up.emit(event.button())))
        );
        let handles = Rc::new([on_move,on_down,on_up]);
        Self {mouse_manager,last_position,position,hover_ids,target,handles,frp,scene_frp,logger}
    }

    /// Reemits FRP mouse changed position event with the last mouse position value.
    ///
    /// The immediate question that appears is why it is even needed. The reason is tightly coupled
    /// with how the rendering engine works and it is important to understand it properly. When
    /// moving a mouse the following events happen:
    /// - `MouseManager` gets notification and fires callbacks.
    /// - Callback above is run. The value of `screen_position` uniform changes and FRP events are
    ///   emitted.
    /// - FRP events propagate trough the whole system.
    /// - The rendering engine renders a frame and waits for the pixel read pass to report symbol
    ///   ID under the cursor. This is normally done the next frame but sometimes could take even
    ///   few frames.
    /// - When the new ID are received, we emit `over` and `out` FRP events for appropriate
    ///   elements.
    /// - After emitting `over` and `out `events, the `position` event is reemitted.
    ///
    /// The idea is that if your FRP network listens on both `position` and `over` or `out` events,
    /// then you do not need to think about the whole asynchronous mechanisms going under the hood,
    /// and you can assume that it is synchronous. Whenever mouse moves, it is discovered what
    /// element it hovers, and its position change event is emitted as well.
    pub fn reemit_position_event(&self) {
        let shape    = self.scene_frp.shape.value();
        let new_pos  = self.last_position.get();
        let position = Vector2(new_pos.x as f32,new_pos.y as f32) - shape.center();
        self.frp.position.emit(position);
    }
}



// ================
// === Keyboard ===
// ================

#[derive(Clone,CloneRef,Debug)]
pub struct Keyboard {
    pub frp : enso_frp::io::keyboard::Keyboard,
    bindings : Rc<enso_frp::io::keyboard::DomBindings>,
}

impl Keyboard {
    pub fn new(current_event:&CurrentJsEvent) -> Self {
        let logger   = Logger::new("keyboard");
        let frp      = enso_frp::io::keyboard::Keyboard::default();
        let bindings = Rc::new(enso_frp::io::keyboard::DomBindings::new(&logger,&frp,current_event));
        Self {frp,bindings}
    }
}



// ===========
// === Dom ===
// ===========

/// DOM element manager
#[derive(Clone,CloneRef,Debug)]
pub struct Dom {
    /// Root DOM element of the scene.
    pub root : web::dom::WithKnownShape<web::HtmlDivElement>,
    /// Layers of the scene.
    pub layers : Layers,
}

impl Dom {
    /// Constructor.
    pub fn new(logger:&Logger) -> Self {
        let root   = web::create_div();
        let layers = Layers::new(&logger,&root);
        root.set_class_name("scene");
        root.set_style_or_panic("height"  , "100vh");
        root.set_style_or_panic("width"   , "100vw");
        root.set_style_or_panic("display" , "block");
        let root = web::dom::WithKnownShape::new(&root);
        Self {root,layers}
    }

    pub fn shape(&self) -> Shape {
        self.root.shape()
    }

    pub fn recompute_shape_with_reflow(&self) {
        self.root.recompute_shape_with_reflow();
    }
}



// ==============
// === Layers ===
// ==============

/// DOM Layers of the scene. It contains a 2 CSS 3D layers and a canvas layer in the middle. The
/// CSS layers are used to manage DOM elements and to simulate depth-sorting of DOM and canvas
/// elements.
#[derive(Clone,CloneRef,Debug)]
pub struct Layers {
    /// Back DOM scene layer.
    pub back: DomScene,
    /// Front DOM scene layer.
    pub front: DomScene,
    /// The WebGL scene layer.
    pub canvas : web_sys::HtmlCanvasElement,

}

impl Layers {
    /// Constructor.
    pub fn new(logger:&Logger, dom:&web_sys::HtmlDivElement) -> Self {
        let canvas = web::create_canvas();
        let front  = DomScene::new(logger);
        let back   = DomScene::new(logger);
        canvas.set_style_or_warn("height"        , "100vh"   , &logger);
        canvas.set_style_or_warn("width"         , "100vw"   , &logger);
        canvas.set_style_or_warn("display"       , "block"   , &logger);
        // Position must not be "static" to have z-index working.
        canvas.set_style_or_warn("position"      , "absolute", &logger);
        canvas.set_style_or_warn("z-index"       , "1"       , &logger);
        canvas.set_style_or_warn("pointer-events", "none"    , &logger);
        front.dom.set_class_name("front");
        front.dom.set_style_or_warn("z-index", "1", &logger);
        back.dom.set_class_name("back");
        back.dom.set_style_or_warn("pointer-events", "auto", &logger);
        back.dom.set_style_or_warn("z-index"       , "0"   , &logger);
        dom.append_or_panic(&canvas);
        dom.append_or_panic(&front.dom);
        dom.append_or_panic(&back.dom);
        Self {front,canvas,back}
    }
}



// ================
// === Uniforms ===
// ================

/// Uniforms owned by the scene.
#[derive(Clone,CloneRef,Debug)]
pub struct Uniforms {
    /// Pixel ratio of the screen used to display the scene.
    pub pixel_ratio : Uniform<f32>,
}

impl Uniforms {
    /// Constructor.
    pub fn new(scope:&UniformScope) -> Self {
        let pixel_ratio = scope.add_or_panic("pixel_ratio" , 1.0);
        Self {pixel_ratio}
    }
}



// =============
// === Dirty ===
// =============

pub type ShapeDirty          = dirty::SharedBool<Box<dyn Fn()>>;
pub type SymbolRegistryDirty = dirty::SharedBool<Box<dyn Fn()>>;

#[derive(Clone,CloneRef,Debug)]
pub struct Dirty {
    symbols : SymbolRegistryDirty,
    shape   : ShapeDirty,
}



// ================
// === Renderer ===
// ================

#[derive(Clone,CloneRef,Debug)]
pub struct Renderer {
    logger    : Logger,
    dom       : Dom,
    context   : Context,
    variables : UniformScope,

    pub pipeline : Rc<CloneCell<RenderPipeline>>,
    pub composer : Rc<CloneCell<RenderComposer>>,
}

impl Renderer {
    fn new(logger:impl AnyLogger, dom:&Dom, context:&Context, variables:&UniformScope) -> Self {
        let logger    = Logger::sub(logger,"renderer");
        let dom       = dom.clone_ref();
        let context   = context.clone_ref();
        let variables = variables.clone_ref();
        let pipeline  = default();
        let shape     = dom.shape().device_pixels();
        let width     = shape.width  as i32;
        let height    = shape.height as i32;
        let composer  = RenderComposer::new(&pipeline,&context,&variables,width,height);
        let pipeline  = Rc::new(CloneCell::new(pipeline));
        let composer  = Rc::new(CloneCell::new(composer));

        context.enable(Context::BLEND);
        // To learn more about the blending equations used here, please see the following articles:
        // - http://www.realtimerendering.com/blog/gpus-prefer-premultiplication
        // - https://www.khronos.org/opengl/wiki/Blending#Colors
        context.blend_equation_separate ( Context::FUNC_ADD, Context::FUNC_ADD );
        context.blend_func_separate     ( Context::ONE , Context::ONE_MINUS_SRC_ALPHA
                                        , Context::ONE , Context::ONE_MINUS_SRC_ALPHA );

        Self {logger,dom,context,variables,pipeline,composer}
    }

    pub fn set_pipeline<P:Into<RenderPipeline>>(&self, pipeline:P) {
        self.pipeline.set(pipeline.into());
        self.reload_composer();
    }

    pub fn reload_composer(&self) {
        let shape    = self.dom.shape().device_pixels();
        let width    = shape.width  as i32;
        let height   = shape.height as i32;
        let pipeline = self.pipeline.get();
        let composer = RenderComposer::new(&pipeline,&self.context,&self.variables,width,height);
        self.composer.set(composer);
    }

    /// Run the renderer.
    pub fn run(&self) {
        group!(self.logger, "Running.", {
            self.composer.get().run();
        })
    }
}



// ============
// === View ===
// ============

// === Definition ===

#[derive(Debug,Clone,CloneRef)]
pub struct View {
    data : Rc<ViewData>
}

#[derive(Debug,Clone,CloneRef)]
pub struct WeakView {
    data : Weak<ViewData>
}

#[derive(Debug,Clone)]
pub struct ViewData {
    logger     : Logger,
    pub camera : Camera2d,
    symbols    : RefCell<Vec<SymbolId>>,
}

impl AsRef<ViewData> for View {
    fn as_ref(&self) -> &ViewData {
        &self.data
    }
}

impl std::borrow::Borrow<ViewData> for View {
    fn borrow(&self) -> &ViewData {
        &self.data
    }
}

impl Deref for View {
    type Target = ViewData;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}


// === API ===

impl View {
    pub fn new(logger:&Logger, width:f32, height:f32) -> Self {
        let data = ViewData::new(logger,width,height);
        let data = Rc::new(data);
        Self {data}
    }

    pub fn new_with_camera(logger:&Logger, camera:&Camera2d) -> Self {
        let data = ViewData::new_with_camera(logger,camera);
        let data = Rc::new(data);
        Self {data}
    }

    pub fn downgrade(&self) -> WeakView {
        let data = Rc::downgrade(&self.data);
        WeakView {data}
    }

    pub fn add(&self, symbol:&Symbol) {
        let mut symbols = self.symbols.borrow_mut();
        //FIXME:We check if the symbol was already added to this view to avoid a glitch that makes
        //the symbol to be rendered more than once. The Breadcrumb object, for instance, is added
        //to the view object every time a new breadcrumb is created. It gets really visible when
        //the breadcrumb is semi-transparent because, every time it's rendered, the shape is added
        //on top of the already rendered ones.
        //TODO:Symbols insertion can run faster if refactored as HashSet.
        if symbols.iter().find(|id| **id == symbol.id as usize).is_none() {
            symbols.push(symbol.id as usize); // TODO strange conversion
        }
    }

    pub fn remove(&self, symbol:&Symbol) {
        self.symbols.borrow_mut().remove_item(&(symbol.id as usize)); // TODO strange conversion
    }

    /// Add all `Symbol`s associated with the given ShapeView.
    pub fn add_shape_view<T: display::shape::primitive::system::Shape>
    (&self, shape_view:&component::ShapeView<T>) {
        shape_view.shape.sprites().iter().for_each(|sprite| self.add(&sprite.symbol) );
    }

    /// Remove all `Symbol`s associated with the given ShapeView.
    pub fn remove_shape_view<T: display::shape::primitive::system::Shape>
    (&self, shape_view:&component::ShapeView<T>) {
        shape_view.shape.sprites().iter().for_each(|sprite| self.remove(&sprite.symbol) );
    }
}

impl WeakView {
    pub fn upgrade(&self) -> Option<View> {
        self.data.upgrade().map(|data| View{data})
    }
}

impl ViewData {
    pub fn new(logger:impl AnyLogger, width:f32, height:f32) -> Self {
        let logger  = Logger::sub(logger,"view");
        let camera  = Camera2d::new(&logger,width,height);
        let symbols = default();
        Self {logger,camera,symbols}
    }

    pub fn new_with_camera(logger:impl AnyLogger, camera:&Camera2d) -> Self {
        let logger  = Logger::sub(logger,"view");
        let camera  = camera.clone_ref();
        let symbols = default();
        Self {logger,camera,symbols}
    }

    pub fn symbols(&self) -> Vec<SymbolId> {
        self.symbols.borrow().clone()
    }
}



// =============
// === Views ===
// =============

/// Please note that currently the `Views` structure is implemented in a hacky way. It assumes the
/// existence of `main`, `overlay`, `cursor`, and `label` views, which are needed for the GUI to
/// display shapes properly. This should be abstracted away in the future.
#[derive(Clone,CloneRef,Debug)]
pub struct Views {
    logger             : Logger,
    pub viz            : View,
    pub main           : View,
    pub cursor         : View,
    pub label          : View,
    pub viz_fullscreen : View,
    pub breadcrumbs    : View,
    all                : Rc<RefCell<Vec<WeakView>>>,
    width              : f32,
    height             : f32,
}

impl Views {
    pub fn mk(logger:impl AnyLogger, width:f32, height:f32) -> Self {
        let logger         = Logger::sub(logger,"views");
        let main           = View::new(&logger,width,height);
        let viz            = View::new_with_camera(&logger,&main.camera);
        let cursor         = View::new(&logger,width,height);
        let label          = View::new_with_camera(&logger,&main.camera);
        let viz_fullscreen = View::new(&logger,width,height);
        let breadcrumbs    = View::new(&logger,width,height);
        let all            = vec![
            viz.downgrade(),
            main.downgrade(),
            cursor.downgrade(),
            label.downgrade(),
            viz_fullscreen.downgrade(),
            breadcrumbs.downgrade()
        ];
        let all = Rc::new(RefCell::new(all));
        Self {logger,viz,main,cursor,label,viz_fullscreen,all,width,height,breadcrumbs}
    }

    /// Creates a new view for this scene.
    #[allow(clippy::new_ret_no_self)]
    pub fn new(&self) -> View {
        let view = View::new(&self.logger,self.width,self.height);
        self.all.borrow_mut().push(view.downgrade());
        view
    }

    pub fn all(&self) -> Ref<Vec<WeakView>> {
        self.all.borrow()
    }
}



// ===========
// === FRP ===
// ===========

/// FRP Scene interface.
#[derive(Clone,CloneRef,Debug)]
pub struct Frp {
    pub network                      : frp::Network,
    pub shape                        : frp::Sampler<Shape>,
    pub camera_changed               : frp::Stream,
    pub frame_time                   : frp::Stream<f32>,
    camera_changed_source            : frp::Source,
    frame_time_source                : frp::Source<f32>,
}

impl Frp {
    /// Constructor
    pub fn new(shape:&frp::Sampler<Shape>) -> Self {
        frp::new_network! { network
            camera_changed_source        <- source();
            frame_time_source            <- source();
        }
        let shape            = shape.clone_ref();
        let camera_changed   = camera_changed_source.clone_ref().into();
        let frame_time       = frame_time_source.clone_ref().into();
        Self {network,shape,camera_changed,frame_time,camera_changed_source,frame_time_source}
    }
}



// =================
// === Extension ===
// =================

pub trait Extension : 'static + CloneRef {
    fn init(scene:&Scene) -> Self;
}

#[derive(Clone,CloneRef,Debug,Default)]
pub struct Extensions {
    map : Rc<RefCell<HashMap<TypeId,Box<dyn Any>>>>,
}

impl Extensions {
    pub fn get<T:Extension>(&self, scene:&Scene) -> T {
        let type_id = TypeId::of::<T>();
        let map_mut = &mut self.map.borrow_mut();
        let entry   = map_mut.entry(type_id).or_insert_with(||Box::new(T::init(scene)));
        entry.downcast_ref::<T>().unwrap().clone_ref()
    }
}



// =================
// === SceneData ===
// =================

#[derive(Clone,CloneRef,Debug)]
pub struct SceneData {
    pub display_object   : display::object::Instance,
    pub dom              : Dom,
    pub context          : Context,
    pub symbols          : SymbolRegistry,
    pub variables        : UniformScope,
    pub current_js_event : CurrentJsEvent,
    pub mouse            : Mouse,
    pub keyboard         : Keyboard,
    pub uniforms         : Uniforms,
    pub shapes           : ShapeRegistry,
    pub stats            : Stats,
    pub dirty            : Dirty,
    pub logger           : Logger,
    pub renderer         : Renderer,
    pub views            : Views,
    pub style_sheet      : style::Sheet,
    pub bg_color_var     : style::Var,
    pub bg_color_change  : callback::Handle,
    pub frp              : Frp,
    extensions           : Extensions,
    disable_context_menu : Rc<IgnoreContextMenuHandle>,
}

impl SceneData {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<OnMut:Fn()+Clone+'static>
    (parent_dom:&HtmlElement, logger:Logger, stats:&Stats, on_mut:OnMut) -> Self {
        logger.trace("Initializing.");

        let dom = Dom::new(&logger);
        parent_dom.append_child(&dom.root).unwrap();
        dom.recompute_shape_with_reflow();

        let display_object       = display::object::Instance::new(&logger);
        let context              = web::get_webgl2_context(&dom.layers.canvas);
        let sub_logger           = Logger::sub(&logger,"shape_dirty");
        let shape_dirty          = ShapeDirty::new(sub_logger,Box::new(on_mut.clone()));
        let sub_logger           = Logger::sub(&logger,"symbols_dirty");
        let dirty_flag           = SymbolRegistryDirty::new(sub_logger,Box::new(on_mut));
        let on_change            = enclose!((dirty_flag) move || dirty_flag.set());
        let var_logger           = Logger::sub(&logger,"global_variables");
        let variables            = UniformScope::new(var_logger,&context);
        let symbols              = SymbolRegistry::mk(&variables,&stats,&context,&logger,on_change);
        let screen_shape         = dom.shape();
        let width                = screen_shape.width;
        let height               = screen_shape.height;
        let symbols_dirty        = dirty_flag;
        let views                = Views::mk(&logger,width,height);
        let stats                = stats.clone();
        let shapes               = ShapeRegistry::default();
        let uniforms             = Uniforms::new(&variables);
        let dirty                = Dirty {symbols:symbols_dirty,shape:shape_dirty};
        let renderer             = Renderer::new(&logger,&dom,&context,&variables);
        let style_sheet          = style::Sheet::new();
        let current_js_event     = CurrentJsEvent::new();
        let frp                  = Frp::new(&dom.root.shape);
        let mouse_logger         = Logger::sub(&logger,"mouse");
        let mouse                = Mouse::new(&frp,&variables,&current_js_event,mouse_logger);
        let disable_context_menu = Rc::new(web::ignore_context_menu(&dom.root).unwrap());
        let keyboard             = Keyboard::new(&current_js_event);
        let network              = &frp.network;
        let extensions           = Extensions::default();
        let bg_color_var         = style_sheet.var("application . background . color");
        let bg_color_change      = bg_color_var.on_change(f!([dom](change){
            change.color().for_each(|color| {
                let color = color::Rgba::from(color);
                let color = format!("rgba({},{},{},{})",255.0*color.red,255.0*color.green,255.0*color.blue,255.0*color.alpha);
                dom.root.set_style_or_panic("background-color",color);
            })
        }));

        frp::extend! { network
            eval_ frp.shape (dirty.shape.set());
        }

        uniforms.pixel_ratio.set(dom.shape().pixel_ratio);
        Self {renderer,display_object,dom,context,symbols,views,dirty,logger,variables,stats
             ,uniforms,mouse,keyboard,shapes,style_sheet,bg_color_var,bg_color_change,frp
             ,extensions,disable_context_menu,current_js_event}
    }

    pub fn shape(&self) -> &frp::Sampler<Shape> {
        &self.dom.root.shape
    }

    pub fn camera(&self) -> &Camera2d {
        &self.views.main.camera
    }

    pub fn new_symbol(&self) -> Symbol {
        let symbol = self.symbols.new();
        self.views.main.add(&symbol);
        symbol
    }

    pub fn symbols(&self) -> &SymbolRegistry {
        &self.symbols
    }

    fn handle_mouse_events(&self) {
        let new_target     = Target::from_internal(self.mouse.hover_ids.get());
        let current_target = self.mouse.target.get();
        if new_target != current_target {
            self.mouse.target.set(new_target);
            self.shapes.get_mouse_target(current_target) . for_each(|t| t.mouse_out().emit(()));
            self.shapes.get_mouse_target(new_target)     . for_each(|t| t.mouse_over().emit(()));
            self.mouse.reemit_position_event(); // See docs to learn why.
        }
    }

    fn update_shape(&self) {
        if self.dirty.shape.check_all() {
            let screen = self.dom.shape();
            self.resize_canvas(screen);
            for view in &*self.views.all.borrow() {
                view.upgrade().for_each(|v| v.camera.set_screen(screen.width,screen.height))
            }
            self.renderer.reload_composer();
            self.dirty.shape.unset_all();
        }
    }

    fn update_symbols(&self) {
        if self.dirty.symbols.check_all() {
            self.symbols.update();
            self.dirty.symbols.unset_all();
        }
    }

    fn update_camera(&self, scene:&Scene) {
        // Updating camera for DOM layers. Please note that DOM layers cannot use multi-camera
        // setups now, so we are using here the main camera only.
        let camera  = self.camera();
        let changed = camera.update(scene);
        if changed {
            self.frp.camera_changed_source.emit(());
            self.symbols.set_camera(camera);
            self.dom.layers.front.update_view_projection(camera);
            self.dom.layers.back.update_view_projection(camera);
        }

        // Updating all other cameras (the main camera was already updated, so it will be skipped).
        for view in &*self.views.all() {
            view.upgrade().for_each(|v| v.camera.update(scene));
        }
    }

    /// Resize the underlying canvas. This function should rather not be called
    /// directly. If you want to change the canvas size, modify the `shape` and
    /// set the dirty flag.
    fn resize_canvas(&self, screen:Shape) {
        let canvas = screen.device_pixels();
        group!(self.logger,"Resized to {screen.width}px x {screen.height}px.", {
            self.dom.layers.canvas.set_attribute("width",  &canvas.width.to_string()).unwrap();
            self.dom.layers.canvas.set_attribute("height", &canvas.height.to_string()).unwrap();
            self.context.viewport(0,0,canvas.width as i32, canvas.height as i32);
        });
    }

    pub fn screen_to_scene_coordinates(&self, position:Vector3<f32>) -> Vector3<f32> {
        let position = position / self.camera().zoom();
        let position = Vector4::new(position.x, position.y, position.z, 1.0);
        (self.camera().inversed_view_matrix() * position).xyz()
    }

    /// Transforms screen position to the object (display object) coordinate system.
    pub fn screen_to_object_space(&self, object:&impl display::Object, screen_pos:Vector2) -> Vector2 {
        let origin_world_space = Vector4(0.0,0.0,0.0,1.0);
        let origin_clip_space  = self.camera().view_projection_matrix() * origin_world_space;
        let inv_object_matrix  = object.transform_matrix().try_inverse().unwrap();

        let shape        = self.frp.shape.value();
        let clip_space_z = origin_clip_space.z;
        let clip_space_x = origin_clip_space.w * 2.0 * screen_pos.x / shape.width;
        let clip_space_y = origin_clip_space.w * 2.0 * screen_pos.y / shape.height;
        let clip_space   = Vector4(clip_space_x,clip_space_y,clip_space_z,origin_clip_space.w);
        let world_space  = self.camera().inversed_view_projection_matrix() * clip_space;
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

#[derive(Clone,CloneRef,Debug)]
pub struct Scene {
    no_mut_access : SceneData
}

impl Scene {
    pub fn new<OnMut:Fn()+Clone+'static>
    (parent_dom:&HtmlElement, logger:impl AnyLogger, stats:&Stats, on_mut:OnMut) -> Self {
        let logger        = Logger::sub(logger,"scene");
        let no_mut_access = SceneData::new(parent_dom,logger,stats,on_mut);
        let this = Self {no_mut_access};
        this.no_mut_access.shapes.rc.borrow_mut().scene = Some(this.clone_ref()); // FIXME ugly
        this
    }

    pub fn extension<T:Extension>(&self) -> T {
        self.extensions.get(self)
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
    pub fn update(&self, t:animation::TimeInfo) {
        group!(self.logger, "Updating.", {
            self.frp.frame_time_source.emit(t.local);
            // Please note that `update_camera` is called first as it may trigger FRP events which
            // may change display objects layout.
            self.update_camera(self);
            self.display_object.update(self);
            self.update_shape();
            self.update_symbols();
            self.handle_mouse_events();
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
