use crate::prelude::*;

pub use crate::display::world::scene::symbol_registry::SymbolId;

use crate::display::render::webgl;
use crate::closure;
use crate::data::function::callback::*;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::display::world::scene::symbol_registry;
use crate::display::world::scene::Scene;
use crate::promote_all;
use crate::promote_symbol_registry_types;
use crate::promote;
use crate::system::web;
use crate::system::web::fmt;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::system::web::resize_observer::ResizeObserver;
use crate::display::shape::text;
use crate::display::shape::text::font::Fonts;
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

    pub fn set_screen_dimension(&self, width:f64, height:f64) {
        self.rc.borrow_mut().set_screen_dimension(width,height);
    }
}


// === ShapeData ===

#[derive(Clone,Debug)]
pub struct ShapeData {
    pub width       : f64,
    pub height      : f64,
    pub pixel_ratio : f64
}

impl Default for ShapeData {
    fn default() -> Self {
        let width       = 100.0;
        let height      = 100.0;
        let pixel_ratio = web::device_pixel_ratio().unwrap_or(1.0);
        Self {width,height,pixel_ratio}
    }
}

impl ShapeData {
    pub fn set_screen_dimension(&mut self, width:f64, height:f64) {
        self.width  = width;
        self.height = height;
    }
}



// =================
// === Workspace ===
// =================

#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Workspace<OnDirty> {
    pub canvas                : web_sys::HtmlCanvasElement,
    pub context               : webgl::Context,
    pub symbol_registry       : SymbolRegistry<OnDirty>,
    pub symbol_registry_dirty : SymbolRegistryDirty<OnDirty>,
    pub scene                 : Scene, // FIXME We support only 1 scene now;
    pub shape                 : Shape,
    pub shape_dirty           : ShapeDirty<OnDirty>,
    pub logger                : Logger,
    pub listeners             : Listeners,
    // TODO[AO] this is a very temporary solution. Need to develop some general
    // component handling
    pub text_components     : Vec<text::TextComponent>,
}


// === Types ===

pub type ShapeDirty          <Callback> = dirty::SharedBool<Callback>;
pub type SymbolRegistryDirty <Callback> = dirty::SharedBool<Callback>;
promote_symbol_registry_types!{ [OnSymbolRegistryChange] symbol_registry }

#[macro_export]
macro_rules! promote_workspace_types { ($($args:tt)*) => {
    crate::promote_symbol_registry_types! { $($args)* }
    promote! { $($args)* [Workspace] }
};}


// === Callbacks ===

closure! {
fn symbol_registry_on_change<C:Callback0> (dirty:SymbolRegistryDirty<C>) -> OnSymbolRegistryChange {
    || dirty.set()
}}


// === Implementation ===

#[derive(Debug)]
pub struct Listeners {
    resize: ResizeObserver,
}

impl<OnDirty: Clone + Callback0 + 'static> Workspace<OnDirty> {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<Dom:Str>
    (dom:Dom, logger:Logger, on_dirty:OnDirty) -> Result<Self, Error> {
        logger.trace("Initializing.");
        let dom                   = dom.as_ref();
        let canvas                = web::get_canvas(dom)?;
        let context               = web::get_webgl2_context(&canvas)?;
        let sub_logger            = logger.sub("shape_dirty");
        let shape_dirty           = ShapeDirty::new(sub_logger,on_dirty.clone());
        let sub_logger            = logger.sub("symbol_registry_dirty");
        let dirty_flag            = SymbolRegistryDirty::new(sub_logger, on_dirty);
        let on_change             = symbol_registry_on_change(dirty_flag.clone_ref());
        let sub_logger            = logger.sub("symbol_registry");
        let symbol_registry       = SymbolRegistry::new(&context,sub_logger, on_change);
        let shape                 = default();
        let listeners             = Self::init_listeners(&logger,&canvas,&shape,&shape_dirty);
        let symbol_registry_dirty = dirty_flag;
        let scene                 = Scene::new(logger.sub("scene1"));
        let text_components       = default();
        let this = Self {canvas,context,symbol_registry,scene,symbol_registry_dirty
            ,shape,shape_dirty,logger,listeners,text_components};
        Ok(this)
    }

    /// Initialize all listeners and attach them to DOM elements.
    fn init_listeners
    (logger:&Logger, canvas:&web_sys::HtmlCanvasElement, shape:&Shape, dirty:&ShapeDirty<OnDirty>)
    -> Listeners {
        let logger = logger.clone();
        let shape  = shape.clone();
        let dirty  = dirty.clone();
        let on_resize = Closure::new(move |width, height| {
            group!(logger, "Resize observer event.", {
                shape.set_screen_dimension(width,height);
                dirty.set();
            })
        });
        let resize = ResizeObserver::new(canvas,on_resize);
        Listeners {resize}
    }

    /// Build new instance with the provided builder object.
    pub fn build<Name:Into<String>> (name:Name) -> WorkspaceBuilder {
        let name = name.into();
        WorkspaceBuilder {name}
    }

    /// Create a new `Symbol` instance.
    pub fn new_symbol(&mut self) -> SymbolId {
        self.symbol_registry.new_symbol()
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
                self.scene.camera.set_screen(screen.width as f32, screen.height as f32);
                self.shape_dirty.unset_all();
            }
            if self.symbol_registry_dirty.check_all() {
                self.symbol_registry.update();
                self.symbol_registry_dirty.unset_all();
            }

            self.logger.info("Clearing the scene.");
            self.context.clear_color(0.0, 0.0, 0.0, 1.0);
            self.context.clear(webgl::Context::COLOR_BUFFER_BIT);
            self.logger.info("Rendering meshes.");
            self.symbol_registry.render(&self.scene.camera);
            if !self.text_components.is_empty() {
                self.logger.info("Rendering text components");
                for text_component in &mut self.text_components {
                    text_component.display(fonts);
                }
            }
        })
    }
}

impl<OnDirty> Index<usize> for Workspace<OnDirty> {
    type Output = Symbol<OnDirty>;
    fn index(&self, ix: usize) -> &Self::Output {
        self.symbol_registry.index(ix)
    }
}

impl<OnDirty> IndexMut<usize> for Workspace<OnDirty> {
    fn index_mut(&mut self, ix: usize) -> &mut Self::Output {
        self.symbol_registry.index_mut(ix)
    }
}



// ========================
// === WorkspaceBuilder ===
// ========================

pub struct WorkspaceBuilder {
    pub name: String
}
