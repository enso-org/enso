#![allow(missing_docs)]

use crate::prelude::*;

pub use crate::display::symbol::registry::SymbolId;

use crate::closure;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::data::function::callback::*;
use crate::debug::stats::Stats;
use crate::display::render::webgl;
use crate::display::shape::text::font::Fonts;
use crate::display::shape::text;
use crate::display::world::scene::Scene;
use crate::display::symbol::registry;
use crate::promote;
use crate::promote_all;
use crate::promote_symbol_registry_types;
use crate::system::web::fmt;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::system::web::resize_observer::ResizeObserver;
use crate::system::web;
use crate::system::gpu::data::uniform::UniformScope;


use eval_tt::*;
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



// =================
// === Workspace ===
// =================

#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Workspace<OnMut> {
    pub canvas        : web_sys::HtmlCanvasElement,
    pub context       : webgl::Context,
    pub symbols       : SymbolRegistry<OnMut>,
    pub symbols_dirty : SymbolRegistryDirty<OnMut>,
    pub scene         : Scene,
    pub shape         : Shape,
    pub shape_dirty   : ShapeDirty<OnMut>,
    pub logger        : Logger,
    pub listeners     : Listeners,
    // TODO[AO] this is a very temporary solution. Need to develop some general component handling.
    pub text_components : Vec<text::TextComponent>,
}


// === Types ===

pub type ShapeDirty          <Callback> = dirty::SharedBool<Callback>;
pub type SymbolRegistryDirty <Callback> = dirty::SharedBool<Callback>;
promote_symbol_registry_types!{ [OnSymbolRegistryChange] registry }

#[macro_export]
/// Promote relevant types to parent scope. See `promote!` macro for more information.
macro_rules! promote_workspace_types { ($($args:tt)*) => {
    crate::promote_symbol_registry_types! { $($args)* }
    promote! { $($args)* [Workspace] }
};}


// === Callbacks ===

closure! {
fn symbols_on_change<C:Callback0> (dirty:SymbolRegistryDirty<C>) -> OnSymbolRegistryChange {
    || dirty.set()
}}


// === Implementation ===

#[derive(Debug)]
pub struct Listeners {
    resize: ResizeObserver,
}

impl<OnMut: Clone + Callback0 + 'static> Workspace<OnMut> {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<Dom:Str>
    (dom:Dom, variables:&UniformScope, logger:Logger, stats:&Stats, on_dirty:OnMut) -> Result<Self, Error> {
        logger.trace("Initializing.");
        let dom             = dom.as_ref();
        let canvas          = web::get_canvas(dom)?;
        let context         = web::get_webgl2_context(&canvas)?;
        let sub_logger      = logger.sub("shape_dirty");
        let shape_dirty     = ShapeDirty::new(sub_logger,on_dirty.clone());
        let sub_logger      = logger.sub("symbols_dirty");
        let dirty_flag      = SymbolRegistryDirty::new(sub_logger, on_dirty);
        let on_change       = symbols_on_change(dirty_flag.clone_ref());
        let sub_logger      = logger.sub("symbols");
        let symbols         = SymbolRegistry::new(variables,&stats,&context,sub_logger,on_change);
        let shape           = Shape::default();
        let listeners       = Self::init_listeners(&logger,&canvas,&shape,&shape_dirty);
        let symbols_dirty   = dirty_flag;
        let scene           = Scene::new(logger.sub("scene"));
        let text_components = default();

        variables.add("pixel_ratio", shape.pixel_ratio());

        // FIXME: use correct blending function and rething premultiplying the alpha.
        context.enable(webgl::Context::BLEND);
        // context.blend_func(webgl::Context::ONE, webgl::Context::ONE_MINUS_SRC_ALPHA);
        context.blend_func(webgl::Context::SRC_ALPHA, webgl::Context::ONE);

        let this = Self {canvas,context,symbols,scene,symbols_dirty
            ,shape,shape_dirty,logger,listeners,text_components};


        Ok(this)
    }

    /// Initialize all listeners and attach them to DOM elements.
    fn init_listeners
    (logger:&Logger, canvas:&web_sys::HtmlCanvasElement, shape:&Shape, dirty:&ShapeDirty<OnMut>)
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

    /// Create a new `Symbol` instance.
    pub fn new_symbol(&mut self) -> SymbolId {
        self.symbols.new_symbol()
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
            self.context.viewport(0, 0, canvas.width as i32, canvas.height as i32);
        });
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self, fonts:&mut Fonts) {
        group!(self.logger, "Updating.", {
            if self.shape_dirty.check_all() {
                let screen = self.shape.screen_shape();
                self.resize_canvas(&self.shape);
                self.scene.camera.set_screen(screen.width, screen.height);
                self.shape_dirty.unset_all();
            }
            if self.symbols_dirty.check_all() {
                self.symbols.update();
                self.symbols_dirty.unset_all();
            }

            self.logger.info("Clearing the scene.");
            self.context.clear_color(0.0, 0.0, 0.0, 1.0);
            self.context.clear(webgl::Context::COLOR_BUFFER_BIT);
            self.logger.info("Rendering meshes.");
            self.symbols.render(&self.scene.camera);
            if !self.text_components.is_empty() {
                self.logger.info("Rendering text components");
                for text_component in &mut self.text_components {
                    text_component.display(fonts);
                }
            }
        })
    }
}

impl<OnMut> Index<usize> for Workspace<OnMut> {
    type Output = Symbol<OnMut>;
    fn index(&self, ix: usize) -> &Self::Output {
        self.symbols.index(ix)
    }
}

impl<OnMut> IndexMut<usize> for Workspace<OnMut> {
    fn index_mut(&mut self, ix: usize) -> &mut Self::Output {
        self.symbols.index_mut(ix)
    }
}
