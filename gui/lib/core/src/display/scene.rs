#![allow(missing_docs)]

use crate::prelude::*;

pub use crate::display::symbol::registry::SymbolId;
use crate::closure;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::camera::Camera2d;
use crate::display::object::DisplayObjectData;
use crate::display::shape::text::font::Fonts;
use crate::display::shape::text;
use crate::display::symbol::registry::SymbolRegistry;
use crate::display::symbol::Symbol;
use crate::display::render::RenderComposer;
use crate::display::render::RenderPipeline;
use crate::system::gpu::data::uniform::UniformScope;
use crate::system::gpu::shader::Context;
use crate::system::web::resize_observer::ResizeObserver;
use crate::system::web;

use wasm_bindgen::prelude::Closure;



// =============
// === Error ===
// =============

#[derive(Debug, Fail, From)]
pub enum Error {
    #[fail(display = "Web Platform error: {}", error)]
    WebError { error: web::Error },
}



// =============
// === Shape ===
// =============

// === Shape ===

#[derive(Clone,Debug)]
pub struct Shape {
    rc: Rc<RefCell<ShapeData>>
}

impl Default for Shape {
    fn default() -> Self {
        let rc = Rc::new(RefCell::new(default()));
        Self {rc}
    }
}

impl Shape {
    pub fn screen_shape(&self) -> ShapeData {
        self.rc.borrow().clone()
    }

    pub fn canvas_shape(&self) -> ShapeData {
        let mut shape = self.screen_shape();
        shape.width  *= shape.pixel_ratio;
        shape.height *= shape.pixel_ratio;
        shape
    }

    pub fn set_screen_dimension(&self, width:f32, height:f32) {
        self.rc.borrow_mut().set_screen_dimension(width,height);
    }

    pub fn pixel_ratio(&self) -> f32 {
        self.rc.borrow().pixel_ratio
    }
}


// === ShapeData ===

#[derive(Clone,Debug)]
pub struct ShapeData {
    pub width       : f32,
    pub height      : f32,
    pub pixel_ratio : f32
}

impl Default for ShapeData {
    fn default() -> Self {
        let width       = 100.0;
        let height      = 100.0;
        let pixel_ratio = web::device_pixel_ratio().unwrap_or(1.0) as f32;
        Self {width,height,pixel_ratio}
    }
}

impl ShapeData {
    pub fn set_screen_dimension(&mut self, width:f32, height:f32) {
        self.width  = width;
        self.height = height;
    }
}



// =============
// === Scene ===
// =============

shared! { Scene

#[derive(Derivative)]
#[derivative(Debug)]
pub struct SceneData {
    root : DisplayObjectData,
    canvas          : web_sys::HtmlCanvasElement,
    context         : Context,
    symbols         : SymbolRegistry,
    symbols_dirty   : SymbolRegistryDirty,
    camera          : Camera2d,
    shape           : Shape,
    shape_dirty     : ShapeDirty,
    logger          : Logger,
    listeners       : Listeners,
    variables       : UniformScope,
    #[derivative(Debug="ignore")]
    on_resize       : Option<Box<dyn Fn(&Shape)>>,
    // TODO[AO] this is a very temporary solution. Need to develop some general component handling.
    text_components : Vec<text::TextComponent>,
    pipeline        : RenderPipeline,
    composer        : RenderComposer,
    stats           : Stats,

}

impl {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<Dom:Str, OnMut:Fn()+Clone+'static>
    (dom:Dom, logger:Logger, stats:&Stats, on_mut:OnMut) -> Self {
        logger.trace("Initializing.");
        let root  = DisplayObjectData::new(logger.clone());
        let dom             = dom.as_ref();
        let canvas          = web::get_canvas(dom).unwrap();
        let context         = web::get_webgl2_context(&canvas).unwrap();
        let sub_logger      = logger.sub("shape_dirty");
        let shape_dirty     = ShapeDirty::new(sub_logger,Box::new(on_mut.clone()));
        let sub_logger      = logger.sub("symbols_dirty");
        let dirty_flag      = SymbolRegistryDirty::new(sub_logger,Box::new(on_mut));
        let on_change       = symbols_on_change(dirty_flag.clone_ref());
        let sub_logger      = logger.sub("symbols");
        let variables       = UniformScope::new(logger.sub("global_variables"),&context);
        let symbols         = SymbolRegistry::new(&variables,&stats,&context,sub_logger,on_change);
        let shape           = Shape::default();
        let listeners       = Self::init_listeners(&logger,&canvas,&shape,&shape_dirty);
        let symbols_dirty   = dirty_flag;
        let camera          = Camera2d::new(logger.sub("camera"),&variables);
        let text_components = default();
        let on_resize       = default();
        let stats           = stats.clone();

        variables.add("pixel_ratio", shape.pixel_ratio());

        context.enable(Context::BLEND);

        // To learn more about the blending equations used here, please see the following articles:
        // - http://www.realtimerendering.com/blog/gpus-prefer-premultiplication
        // - https://www.khronos.org/opengl/wiki/Blending#Colors
        context.blend_equation_separate ( Context::FUNC_ADD, Context::FUNC_ADD );
        context.blend_func_separate     ( Context::ONE , Context::ONE_MINUS_SRC_ALPHA
                                        , Context::ONE , Context::ONE_MINUS_SRC_ALPHA );


        let pipeline = default();
        let width    = shape.canvas_shape().width  as i32;
        let height   = shape.canvas_shape().height as i32;
        let composer = RenderComposer::new(&pipeline,&context,&variables,width,height);

        Self {pipeline,composer,root,canvas,context,symbols,camera,symbols_dirty,shape,shape_dirty,logger
             ,listeners,variables,on_resize,text_components,stats}
    }

    pub fn context(&self) -> Context {
        self.context.clone()
    }

    pub fn variables(&self) -> UniformScope {
        self.variables.clone_ref()
    }

    pub fn set_render_pipeline<P:Into<RenderPipeline>>(&mut self, pipeline:P) {
        self.pipeline = pipeline.into();
        self.init_composer();
    }

    pub fn init_composer(&mut self) {
        let width    = self.shape.canvas_shape().width  as i32;
        let height   = self.shape.canvas_shape().height as i32;
        self.composer = RenderComposer::new(&self.pipeline,&self.context,&self.variables,width,height);
    }

    pub fn render(&mut self) {
        group!(self.logger, "Updating.", {
            if self.shape_dirty.check_all() {
                let screen = self.shape.screen_shape();
                self.resize_canvas(&self.shape);
                self.camera.set_screen(screen.width, screen.height);
                self.init_composer();
                self.shape_dirty.unset_all();
            }
            if self.symbols_dirty.check_all() {
                self.symbols.update();
                self.symbols_dirty.unset_all();
            }
            self.logger.info("Clearing the scene.");
            self.context.clear_color(0.0, 0.0, 0.0, 1.0);
            self.context.clear(Context::COLOR_BUFFER_BIT);
            self.logger.info("Rendering meshes.");
            self.symbols.render(&self.camera);

            self.composer.run();
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self, fonts:&mut Fonts) {
        self.render();
        if !self.text_components.is_empty() {
            self.logger.info("Rendering text components");
            for text_component in &mut self.text_components {
                text_component.display(fonts);
            }
        }
    }

    pub fn stats(&self) -> Stats {
        self.stats.clone_ref()
    }

    pub fn index(&self, ix:usize) -> Symbol {
        self.symbols.index(ix)
    }

    /// Create a new `Symbol` instance.
    pub fn new_symbol(&self) -> Symbol {
        self.symbols.new_symbol()
    }
}}

impl Into<DisplayObjectData> for &SceneData {
    fn into(self) -> DisplayObjectData {
        self.root.clone()
    }
}

impl Into<DisplayObjectData> for &Scene {
    fn into(self) -> DisplayObjectData {
        let data:&SceneData = &self.rc.borrow();
        data.into()
    }
}


// === Types ===

pub type ShapeDirty          = dirty::SharedBool<Box<dyn Fn()>>;
pub type SymbolRegistryDirty = dirty::SharedBool<Box<dyn Fn()>>;


// === Callbacks ===

closure! {
fn symbols_on_change(dirty:SymbolRegistryDirty) -> OnSymbolRegistryChange {
    || dirty.set()
}}


// === Implementation ===

#[derive(Debug)]
pub struct Listeners {
    resize: ResizeObserver,
}

impl Scene {
    pub fn tmp_borrow_mut(&self) -> RefMut<'_,SceneData> {
        self.rc.borrow_mut()
    }
}

impl SceneData {

    pub fn tmp_text_components(&mut self) -> &mut Vec<text::TextComponent> {
        &mut self.text_components
    }


    /// Initialize all listeners and attach them to DOM elements.
    fn init_listeners
    (logger:&Logger, canvas:&web_sys::HtmlCanvasElement, shape:&Shape, dirty:&ShapeDirty)
    -> Listeners {
        let logger = logger.clone();
        let shape  = shape.clone();
        let dirty  = dirty.clone();
        let on_resize = Closure::new(move |width, height| {
            group!(logger, "Resize observer event.", {
                shape.set_screen_dimension(width as f32,height as f32);
                dirty.set();
            })
        });
        let resize = ResizeObserver::new(canvas,on_resize);
        Listeners {resize}
    }



    /// Resize the underlying canvas. This function should rather not be called
    /// directly. If you want to change the canvas size, modify the `shape` and
    /// set the dirty flag.
    fn resize_canvas(&self, shape:&Shape) {
        let screen = shape.screen_shape();
        let canvas = shape.canvas_shape();
        self.logger.group(fmt!("Resized to {}px x {}px.", screen.width, screen.height), || {
            self.canvas.set_attribute("width",  &canvas.width.to_string()).unwrap();
            self.canvas.set_attribute("height", &canvas.height.to_string()).unwrap();
            self.context.viewport(0,0,canvas.width as i32, canvas.height as i32);
            self.on_resize.iter().for_each(|f| f(shape));
        });
    }

    pub fn text_components_mut(&mut self) -> &mut Vec<text::TextComponent> {
        &mut self.text_components
    }
}
