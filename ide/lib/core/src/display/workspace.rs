use crate::prelude::*;

pub use crate::display::mesh_registry::MeshID;

use crate::backend::webgl;
use crate::closure;
use crate::data::function::callback::*;
use crate::dirty;
use crate::dirty::traits::*;
use crate::display::mesh_registry;
use crate::promote_all;
use crate::promote_mesh_registry_types;
use crate::promote; 
use crate::system::web;
use crate::system::web::fmt;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::system::web::resize_observer::ResizeObserver;
use eval_tt::*;
use wasm_bindgen::prelude::Closure;
use web_sys::WebGlRenderingContext;
use crate::text;


// =============
// === Error ===
// =============

#[derive(Debug, Fail, From)]
pub enum Error {
    #[fail(display = "Web Platform error: {}", error)]
    WebError { error: web::Error },
}

// =================
// === Workspace ===
// =================

#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Workspace<OnDirty> {
    pub canvas              : web_sys::HtmlCanvasElement,
    pub context             : WebGlRenderingContext,
    pub pixel_ratio         : f64,
    pub mesh_registry       : MeshRegistry<OnDirty>,
    pub mesh_registry_dirty : MeshRegistryDirty<OnDirty>,
    pub shape               : Rc<RefCell<WorkspaceShape>>,
    pub shape_dirty         : ShapeDirty<OnDirty>,
    pub logger              : Logger,
    pub listeners           : Listeners,
    // TODO[AO] this is a very temporary solution. Need to develop some general
    // component handling
    pub text_components     : Vec<text::TextComponent>,
}

#[derive(Default)]
#[derive(Debug)]
pub struct WorkspaceShape {
    pub width  : i32,
    pub height : i32,
}

pub type WorkspaceShapeDirtyState = WorkspaceShape;

// === Types ===

pub type ShapeDirty        <Callback> = dirty::SharedBool<Callback>;
pub type MeshRegistryDirty <Callback> = dirty::SharedBool<Callback>;
promote_mesh_registry_types!{ [OnMeshRegistryChange] mesh_registry }

#[macro_export]
macro_rules! promote_workspace_types { ($($args:tt)*) => {
    crate::promote_mesh_registry_types! { $($args)* }
    promote! { $($args)* [Workspace] }
};}

// === Callbacks ===

closure! {
fn mesh_registry_on_change<C:Callback0> (dirty:MeshRegistryDirty<C>) -> 
    OnMeshRegistryChange { || dirty.set() }
}

// === Implementation ===

#[derive(Debug)]
pub struct Listeners {
    resize: ResizeObserver,
}

impl<OnDirty: Clone + Callback0 + 'static> Workspace<OnDirty> {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<Dom: Str>
    (dom:Dom, logger:Logger, on_dirty:OnDirty) -> Result<Self, Error> {
        logger.trace("Initializing.");
        let dom           = dom.as_ref();
        let canvas        = web::get_canvas(dom)?;
        let context       = web::get_webgl_context(&canvas,1)?;
        let pixel_ratio   = web::device_pixel_ratio()?;
        let sub_logger    = logger.sub("shape_dirty");
        let shape_dirty   = ShapeDirty::new(sub_logger,on_dirty.clone());
        let sub_logger    = logger.sub("mesh_registry_dirty");
        let dirty_flag    = MeshRegistryDirty::new(sub_logger, on_dirty);
        let on_change     = mesh_registry_on_change(dirty_flag.clone_rc());
        let sub_logger    = logger.sub("mesh_registry");
        let mesh_registry = MeshRegistry::new(sub_logger, on_change);
        let shape         = default();
        let listeners     = Self::init_listeners(&canvas,&shape,&shape_dirty);
        let mesh_registry_dirty = dirty_flag;
        let text_components     = Vec::new();
        let this = Self
            {canvas,context,pixel_ratio,mesh_registry,mesh_registry_dirty
            ,shape,shape_dirty,logger,listeners, text_components};
        Ok(this)
    }
    /// Initialize all listeners and attach them to DOM elements.
    fn init_listeners
    ( canvas : &web_sys::HtmlCanvasElement
    , shape  : &Rc<RefCell<WorkspaceShape>>
    , dirty  : &ShapeDirty<OnDirty>
    ) -> Listeners {
        let shape = shape.clone();
        let dirty = dirty.clone();
        let on_resize = Closure::new(move |width, height| {
            *shape.borrow_mut() = WorkspaceShape {width,height};
            dirty.set();
        });
        let resize = ResizeObserver::new(canvas,on_resize);
        Listeners {resize}
    }
    /// Build new instance with the provided builder object.
    pub fn build<Name:Into<String>> (name:Name) -> WorkspaceBuilder {
        let name = name.into();
        WorkspaceBuilder {name}
    }
    /// Create a new mesh instance.
    pub fn new_mesh(&mut self) -> MeshID {
        self.mesh_registry.new_mesh()
    }
    /// Resize the underlying canvas. This function should rather not be called
    /// directly. If you want to change the canvas size, modify the `shape` and
    /// set the dirty flag.
    fn resize_canvas(&self, shape:&WorkspaceShape) {
        let ratio  = self.pixel_ratio.floor() as i32;
        let width  = ratio * shape.width;
        let height = ratio * shape.height;
        self.logger.group(fmt!("Resized to {}px x {}px.", width, height), || {
            self.canvas.set_attribute("width",  &width.to_string()).unwrap();
            self.canvas.set_attribute("height", &height.to_string()).unwrap();
            self.context.viewport(0, 0, width, height);
        });
    }

    // TODO TODO  TODO  TODO  TODO  TODO  TODO  TODO  TODO  TODO  TODO  TODO 
    // THIS FUNCTION WILL BE REFACTORED IN THE NEAR FUTURE. IT IS A ROUGH
    // MOCK NOW. DO NOT REVIEW IT.
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.shape_dirty.check() {
                self.resize_canvas(&self.shape.borrow());
                self.shape_dirty.unset();
            }
            if self.mesh_registry_dirty.check() {
                self.mesh_registry.update();
                self.mesh_registry_dirty.unset();
            }

            // Clear
            self.context.clear_color(0.0, 0.0, 0.0, 1.0);
            self.context.clear(webgl::Context::COLOR_BUFFER_BIT);

            for text_component in &self.text_components {
                text_component.display()
            }

            if self.text_components.is_empty() {
            // Note [broken indentation]
            let vert_shader = webgl::compile_shader(
            &self.context,
            webgl::Context::VERTEX_SHADER,
            r#"
attribute vec4 position;
void main() {
    gl_Position = position;
}
"#,
            )
            .unwrap();
            let frag_shader = webgl::compile_shader(
                &self.context,
                webgl::Context::FRAGMENT_SHADER,
                r#"
    void main() {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
"#,
            )
            .unwrap();
            let program =
                webgl::link_program(&self.context, &vert_shader, &frag_shader).unwrap();

            let pos_loc = self.context.get_attrib_location(&program, "position");
            let pos_loc = pos_loc as u32;

            println!("pos_loc: {}", pos_loc);

            let vertices: [f32; 9] =
                 [ -1.0, -1.0, 0.0
                 ,  1.0, -1.0, 0.0
                 ,  0.0,  1.0, 0.0
                 ];

            let buffer = self.context.create_buffer().ok_or("failed to create buffer").unwrap();
            self.context.bind_buffer(webgl::Context::ARRAY_BUFFER, Some(&buffer));

            // Note that `Float32Array::view` is somewhat dangerous (hence the
            // `unsafe`!). This is creating a raw view into our module's
            // `WebAssembly.Memory` buffer, but if we allocate more pages for ourself
            // (aka do a memory allocation in Rust) it'll cause the buffer to change,
            // causing the `Float32Array` to be invalid.
            //
            // As a result, after `Float32Array::view` we have to be very careful not to
            // do any memory allocations before it's dropped.
            unsafe {
                let vert_array = js_sys::Float32Array::view(&vertices);

                self.context.buffer_data_with_array_buffer_view(
                    webgl::Context::ARRAY_BUFFER,
                    &vert_array,
                    webgl::Context::STATIC_DRAW,
                );
            }

            // =================
            // === Rendering ===
            // =================

            self.context.use_program(Some(&program));

            self.context.enable_vertex_attrib_array(pos_loc);
            self.context.bind_buffer(webgl::Context::ARRAY_BUFFER, Some(&buffer));

            // hidden part: binds ARRAY_BUFFER to the attribute
            self.context.vertex_attrib_pointer_with_i32(
                pos_loc,
                3,                     // size - 3 components per iteration
                webgl::Context::FLOAT, // type
                false,                 // normalize
                0,                     // stride
                0,                     // offset
            );



            self.context.draw_arrays(webgl::Context::TRIANGLES, 0, (vertices.len() / 3) as i32);
            }
})
    }
}

/* Note [broken indentation]
 *
 * This code is refactored on another branch, so is left as it is to avoid
 * heavy merge/rebase conflicts
 */

impl<OnDirty> Index<usize> for Workspace<OnDirty> {
    type Output = Mesh<OnDirty>;
    fn index(&self, ix: usize) -> &Self::Output {
        self.mesh_registry.index(ix)
    }
}

impl<OnDirty> IndexMut<usize> for Workspace<OnDirty> {
    fn index_mut(&mut self, ix: usize) -> &mut Self::Output {
        self.mesh_registry.index_mut(ix)
    }
}


// ========================
// === WorkspaceBuilder ===
// ========================

pub struct WorkspaceBuilder {
    pub name: String 
}

