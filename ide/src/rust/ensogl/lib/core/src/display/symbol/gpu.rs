#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod geometry;
#[warn(missing_docs)]
pub mod material;
#[warn(missing_docs)]
pub mod registry;
#[warn(missing_docs)]
pub mod shader;

use crate::prelude::*;

use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::system::gpu::data::buffer::IsBuffer;
use crate::system::gpu::data::uniform::AnyUniform;
use crate::system::gpu::data::uniform::AnyTextureUniform;
use crate::system::gpu::data::uniform::AnyPrimUniform;
use crate::system::gpu::data::uniform::AnyPrimUniformOps;
use crate::display::symbol::geometry::primitive::mesh;
use crate::display;

use shader::Shader;
use wasm_bindgen::JsValue;
use web_sys::WebGlProgram;
use web_sys::WebGlUniformLocation;
use web_sys::WebGlVertexArrayObject;



// ===============
// === Exports ===
// ===============

pub mod types {
    use super::*;
    pub use geometry::types::*;
}
pub use types::*;



// ======================
// === UniformBinding ===
// ======================

/// Binds input variable definition in shader to both its location and an uniform declaration.
#[derive(Clone,Debug)]
pub struct UniformBinding {
    name     : String,
    location : WebGlUniformLocation,
    uniform  : AnyPrimUniform,
}

impl UniformBinding {
    /// Create new uniform binding.
    pub fn new<Name:Str>(name:Name, location:WebGlUniformLocation, uniform:AnyPrimUniform) -> Self {
        let name = name.into();
        Self {name,location,uniform}
    }

    /// Upload uniform value.
    pub fn upload(&self, context:&Context) {
        self.uniform.upload(context,&self.location);
    }
}

type TextureUnit = u32;

/// Binds input sampler definition in shader to its location, uniform declaration and texture unit.
#[derive(Clone,Debug)]
pub struct TextureBinding {
    name         : String,
    location     : WebGlUniformLocation,
    uniform      : AnyTextureUniform,
    texture_unit : TextureUnit,
}

impl TextureBinding {
    /// Create new texture binding.
    pub fn new<Name:Str>
    ( name         : Name
    , location     : WebGlUniformLocation
    , uniform      : AnyTextureUniform
    , texture_unit : TextureUnit
    ) -> Self {
        let name = name.into();
        Self {name,location,uniform,texture_unit}
    }

    /// Bind texture to proper texture unit.
    pub fn bind_texture_unit(&self, context:&Context) -> TextureBindGuard {
        self.uniform.raw.bind_texture_unit(context,self.texture_unit.into())
    }

    /// Upload uniform value.
    pub fn upload_uniform(&self, context:&Context) {
        context.uniform1i(Some(&self.location),self.texture_unit as i32);
    }
}



// =========================
// === VertexArrayObject ===
// =========================

/// A safe wrapper for WebGL VertexArrayObject. It releases the VAO from GPU memory as soon as all
/// references to this object are dropped.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct VertexArrayObject {
    rc : Rc<VertexArrayObjectData>
}

impl VertexArrayObject {
    /// Constructor
    pub fn new(context:&Context) -> Self {
        let rc = Rc::new(VertexArrayObjectData::new(context));
        Self {rc}
    }
}


// === Data ===

/// Internal representation for `VertexArrayObject`.
#[derive(Debug)]
pub struct VertexArrayObjectData {
    context : Context,
    vao     : WebGlVertexArrayObject,
}


// === Public API ===

impl VertexArrayObjectData {
    /// Creates a new VAO instance.
    pub fn new(context:&Context) -> Self {
        let context = context.clone();
        let vao     = context.create_vertex_array().unwrap();
        Self {context,vao}
    }

    /// Binds the VAO, evaluates the provided function, and unbinds the VAO.
    pub fn with<F:FnOnce() -> T,T>(&self, f:F) -> T {
        self.bind();
        let out = f();
        self.unbind();
        out
    }
}


// === Private API ===

impl VertexArrayObjectData {
    fn bind(&self) {
        self.context.bind_vertex_array(Some(&self.vao));
    }

    fn unbind(&self) {
        self.context.bind_vertex_array(None);
    }
}


// === Instances ===

impl Drop for VertexArrayObjectData {
    fn drop(&mut self) {
        self.context.delete_vertex_array(Some(&self.vao));
    }
}



// ===================
// === SymbolStats ===
// ===================

/// Wrapper for `Stats` which counts the number of symbols.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
pub struct SymbolStats {
    rc : Rc<SymbolStatsData>
}

/// Internal representation for `SymbolStats`.
#[derive(Debug,Shrinkwrap)]
pub struct SymbolStatsData {
    stats : Stats
}

impl SymbolStats {
    /// Constructor.
    pub fn new(stats:&Stats) -> Self {
        let rc = Rc::new(SymbolStatsData::new(stats));
        Self {rc}
    }
}

impl SymbolStatsData {
    /// Constructor.
    pub fn new(stats:&Stats) -> Self {
        stats.inc_symbol_count();
        let stats = stats.clone_ref();
        Self {stats}
    }
}

impl Drop for SymbolStatsData {
    fn drop(&mut self) {
        self.stats.dec_symbol_count();
    }
}



// ==============
// === Symbol ===
// ==============

// === Types ===

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum ScopeType {
    Mesh(mesh::ScopeType), Symbol, Global
}

pub type GeometryDirty = dirty::SharedBool<Box<dyn Fn()>>;
pub type ShaderDirty   = dirty::SharedBool<Box<dyn Fn()>>;


// === Bindings ====

#[derive(Clone,Debug,Default)]
pub struct Bindings {
    vao      : Option<VertexArrayObject>,
    uniforms : Vec<UniformBinding>,
    textures : Vec<TextureBinding>,
}


// === Definition ===

/// Symbol is a surface with attached `Shader`.
///
/// # Depth-sorting, memory cleaning, and indexes re-using.
/// There are several possible implementation architectures for attribute management. The currently
/// used architecture may not be the best one, but the choice is not obvious and would require
/// complex benchmarking. However, lets compare the available architectures and lets list their
/// good and bad sides:
///
///
/// ### A. Drawing instanced geometry (the current architecture).
///
/// 1. Rendering.
///    Very fast. May not be as fast as some of other methods, but that may not be the case with
///    modern hardware, see: https://stackoverflow.com/a/65376034/889902, and also
///    https://stackoverflow.com/questions/62537968/using-opengl-instancing-for-rendering-2d-scene-with-object-depths-and-alpha-blen#answer-62538277
///
/// 1. Changing attribute & GPU memory consumption.
///    Very fast and with low memory consumption. Requires only 1 WebGL call (attribute per
///    instance).
///
/// 2. Visual sorting of instances (depth management).
///    Complex. Requires sorting of all attribute buffers connected with a particular instance. For
///    big buffers (many instances) it may require significant CPU -> GPU data upload. For example,
///    taking the last element to the front, would require shifting all attributes in all buffers,
///    which basically would mean uploading all data to the GPU from scratch for that particular
///    geometry. Also, this would require keeping instance IDs in some kind of `Rc<Cell<usize>>`,
///    as during sorting, the instance IDs will change, so all sprites would need to be updated.
///
///
/// ### B. Drawing non-instanced, indexed geometry.
///
/// 1. Rendering.
///    Very fast. May be faster than architecture (A). See it's description to learn more.
///
/// 1. Changing attribute & GPU memory consumption.
///    4 times slower and 4 times more memory hungry than architecture (A). Requires setting each
///    attribute for each vertex (4 WebGL calls). During drawing, vertexes are re-used by using
///    indexed geometry rendering.
///
/// 2. Visual sorting of instances (depth management).
///    The same issues as in architecture (A). Even more CPU -> GPU heavy, as the attribute count
///    is bigger.
///
///
/// ### C. Drawing non-instanced, non-indexed geometry. Using indexing for sorting.
///
/// 1. Rendering.
///    Very fast. May be faster than architecture (A). See it's description to learn more.
///
/// 1. Changing attribute & GPU memory consumption.
///    6 times slower and 6 times more memory hungry than architecture (A). Requires setting each
///    attribute for each vertex (6 WebGL calls). During drawing, vertexes are not re-used, and thus
///    we need to set attributes for each vertex of each triangle.
///
/// 2. Visual sorting of instances (depth management).
///    Simple. We can re-use index buffer to sort the geometry by telling GPU in what order it
///    should render each of the vertexes. Unlike previous architectures, this would not require to
///    create any more internally mutable state regarding attribute index management (the indexes
///    will not change during sorting).
///
///    However, sorting for the needs of memory compression (removing unused memory for sparse
///    attrib arrays) would still require re-uploading sorted data to GPU, just as in architecture
///    (A).
///
///
/// ### D. Keeping all attribute values in a texture and passing index buffer to the shader.
///
/// This is a very different architecture to what is currently implemented and might require very
/// complex refactoring in order to be even tested and benchmarked properly. To learn more about the
/// idea, follow the link: https://stackoverflow.com/a/65376034/889902.
///
/// 1. Rendering.
///    Fast. May be slower than architecture (A). Needs real benchmarks.
///
/// 1. Changing attribute & GPU memory consumption.
///    Changing attribute would require 2 WebGL calls: the `bindTexture`, and `texParameterf` (or
///    similar). Performance of this solution is questionable, but in real life, it may be as fast
///    as architecture (A). The memory consumption should be fine as well, as WebGL textures behave
///    like C++ Vectors, so even if we allocate the texture of max size, it will occupy only the
///    needed space. This will also limit the number of instances on the stage, but the limit will
///    be big enough (assuming max texture od 2048px x 2048px and 20 float attributes per shader,
///    this will allow us to render over 200 000 shapes). Also, this architecture would allow us to
///    pass more attributes to shaders than it is currently possible, which on the other hand,
///    would probably negatively affect the fragment shader performance.
///
/// 2. Visual sorting of instances (depth management).
///    Simple. Next to the attribute texture, we can pass index buffer to the shader, which will
///    dictate what initial offset in the texture should be used. This would allow for the fastest
///    sorting mechanism of all of the above architectures.
///
///    However, sorting for the needs of memory compression (removing unused memory for sparse
///    attrib arrays) would still require re-uploading sorted data to GPU, just as in architecture
///    (A).
///
///
/// ### E. Using the depth-buffer for sorting.
///
/// As with architecture (C), this is a very different architecture to what is currently
/// implemented and might require very complex refactoring in order to be even tested and
/// benchmarked properly. This architecture, however, is the most common architecture among all
/// WebGL / OpenGL applications, but it is not really well suitable for SDF-based shapes rendering,
/// as it requires anti-aliasing to be done by multisampling, which is not needed with SDF-based
/// rasterization. It lowers the quality and drastically increases the rendering time (in the case
/// of 4x4 multisampling, the rendering time is 16x bigger than the time of architecture (A)).
///
/// There is one additional thread to consider here, namely, with some browsers, systems, and GPU
/// combinations, the super-sampling anti-aliasing is not accessible in WebGL. In such situations we
/// could use a post-processing anti-aliasing techniques, such as [FXAA][1] or [SMAA][2], however,
/// the resulting image quality will be even worse. We could also use custom multi-sampled render
/// buffers for implementing [multi-sampled depth buffers][3].
/// [1] https://github.com/mitsuhiko/webgl-meincraft/blob/master/assets/shaders/fxaa.glsl
/// [2] http://www.iryoku.com/papers/SMAA-Enhanced-Subpixel-Morphological-Antialiasing.pdf
/// [3] https://stackoverflow.com/questions/50613696/whats-the-purpose-of-multisample-renderbuffers
///
/// 1. Rendering.
///    May be 9x - 16x slower than architecture (A), depending on multi-sampling level. Also, the
///    final image quality and edge sharpness will be lower. There is, however, an open question,
///    whether there is an SDF-suitable depth-buffer sorting technique which would not cause such
///    downsides (maybe involving SDF-based depth buffer). Currently, we don't know of any such
///    technique.
///
/// 1. Changing attribute & GPU memory consumption.
///    Fast with low memory consumption. The same as with architecture (A), (B), or (C).
///
/// 2. Visual sorting of instances (depth management).
///    Simple and fast. Much faster than any other architecture listed before, as it does not
///    require upfront CPU-side buffer sorting.
///
///
/// ### F. Using depth-peeling / dual depth-peeling algorithms.
///
/// As with architecture (C), this is a very different architecture to what is currently
/// implemented and might require very complex refactoring in order to be even tested and
/// benchmarked properly. The idea is to render the scene multiple times, as long as some objects
/// do overlap, by "peeling" the top-most (and bottom-most) layers every time. See the
/// [Interactive Order-Independent Transparency][1], the
/// [Order Independent Transparency with Dual Depth Peeling][2], and the
/// [sample WebGL implementation][3] to learn more.
///
/// [1] https://my.eng.utah.edu/~cs5610/handouts/order_independent_transparency.pdf
/// [2] http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.193.3485&rep=rep1&type=pdf
/// [3] https://medium.com/@shrekshao_71662/dual-depth-peeling-implementation-in-webgl-11baa061ba4b
///
/// 1. Rendering.
///    May be several times slower than architecture (A) due to the need to render the scene by
///    peeling components. However, in contrast to the architecture (D), the final image quality
///    should be as good as with architecture (A), (B), or (C).
///
/// 1. Changing attribute & GPU memory consumption.
///    Fast with low memory consumption. The same as with architecture (A), (B), or (C).
///
/// 2. Visual sorting of instances (depth management).
///    Simple and fast. As fast as architecture (E), as it does not require upfront CPU-side buffer
///    sorting.
#[derive(Debug,Clone,CloneRef)]
pub struct Symbol {
    display_object    : display::object::Instance,
    pub id            : i32,
    surface           : Mesh,
    shader            : Shader,
    surface_dirty     : GeometryDirty,
    shader_dirty      : ShaderDirty,
    variables         : UniformScope,
    global_variables  : UniformScope,
    symbol_id_uniform : Uniform<i32>,
    context           : Context,
    logger            : Logger,
    bindings          : Rc<RefCell<Bindings>>,
    stats             : SymbolStats,
    is_hidden         : Rc<Cell<bool>>,
}

impl Symbol {
    /// Create new instance with the provided on-dirty callback.
    pub fn new <OnMut:Fn()+Clone+'static>
    ( logger           : Logger
    , context          : &Context
    , stats            : &Stats
    , id               : i32
    , global_variables : &UniformScope
    , on_mut           : OnMut
    ) -> Self {
        let init_logger = logger.clone();
        debug!(init_logger, "Initializing.", || {
            let on_mut2           = on_mut.clone();
            let surface_logger    = Logger::sub(&logger,"surface");
            let shader_logger     = Logger::sub(&logger,"shader");
            let geo_dirt_logger   = Logger::sub(&logger,"surface_dirty");
            let mat_dirt_logger   = Logger::sub(&logger,"shader_dirty");
            let surface_dirty     = GeometryDirty::new(geo_dirt_logger,Box::new(on_mut2));
            let shader_dirty      = ShaderDirty::new(mat_dirt_logger,Box::new(on_mut));
            let surface_on_mut    = Box::new(f!(surface_dirty.set()));
            let shader_on_mut     = Box::new(f!(shader_dirty.set()));
            let shader            = Shader::new(shader_logger,&stats,context,shader_on_mut);
            let surface           = Mesh::new(surface_logger,&stats,context,surface_on_mut);
            let variables         = UniformScope::new(Logger::sub(&logger,"uniform_scope"),context);
            let global_variables  = global_variables.clone_ref();
            let bindings          = default();
            let stats             = SymbolStats::new(stats);
            let context           = context.clone_ref();
            let symbol_id_uniform = variables.add_or_panic("symbol_id",id);
            let display_object    = display::object::Instance::new(logger.clone());
            let is_hidden         = Rc::new(Cell::new(false));
            display_object.set_on_hide(f_!(is_hidden.set(true)));
            display_object.set_on_show(f_!(is_hidden.set(false)));
            Self{id,surface,shader,surface_dirty,shader_dirty,variables,global_variables,logger
                ,context,bindings,stats,symbol_id_uniform,display_object,is_hidden}
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&self) {
        debug!(self.logger, "Updating.", || {
            if self.surface_dirty.check() {
                self.surface.update();
                self.surface_dirty.unset();
            }
            if self.shader_dirty.check() {
                let var_bindings = self.discover_variable_bindings();
                self.shader.update(&var_bindings);
                self.init_variable_bindings(&var_bindings);
                self.shader_dirty.unset();
            }
        })
    }

    pub fn lookup_variable<S:Str>(&self, name:S) -> Option<ScopeType> {
        let name = name.as_ref();
        self.surface.lookup_variable(name).map(ScopeType::Mesh).or_else(|| {
            if      self.variables.contains(name)        { Some(ScopeType::Symbol) }
            else if self.global_variables.contains(name) { Some(ScopeType::Global) }
            else                                         { None }
        })
    }

    pub fn render(&self) {
        debug!(self.logger, "Rendering.", || {
            if self.is_hidden() {
                return;
            }
            self.with_program(|_|{
                for binding in &self.bindings.borrow().uniforms {
                    binding.upload(&self.context);
                }

                let context              = &self.context;
                let textures             = &self.bindings.borrow().textures;
                let bound_textures_iter  = textures.iter().map(|t| {t.bind_texture_unit(context)});
                let _textures_keep_alive = bound_textures_iter.collect_vec();

                let mode           = Context::TRIANGLE_STRIP;
                let first          = 0;
                let count          = self.surface.point_scope().size()    as i32;
                let instance_count = self.surface.instance_scope().size() as i32;

                // FIXME: we should uncomment the following code in some pedantic debug mode. It
                //        introduces severe performance overhead (0.8ms -> 3ms per frame) because
                //        it requires GPU to sync. However, we should maintain a "pedantic mode" in
                //        case something goes horribly wrong and we would like to discover what.

                // // Check if we are ready to render. If we don't assert here we wil only get a warning
                // // that won't tell us where things went wrong.
                // {
                //     let framebuffer_status = context.check_framebuffer_status(Context::FRAMEBUFFER);
                //     debug_assert_eq!(
                //         framebuffer_status,
                //         Context::FRAMEBUFFER_COMPLETE,
                //         "Framebuffer incomplete (status: {}).",
                //         framebuffer_status
                //         )
                // }

                self.stats.inc_draw_call_count();
                if instance_count > 0 {
                    self.context.draw_arrays_instanced(mode,first,count,instance_count);
                } else {
                    self.context.draw_arrays(mode,first,count);
                }
            });
        })
    }
}


// === Visibility ===

impl Symbol {
    pub fn set_hidden(&self, b:bool) {
        self.is_hidden.set(b)
    }

    pub fn hide(&self) {
        self.set_hidden(true)
    }

    pub fn show(&self) {
        self.set_hidden(false)
    }

    pub fn is_hidden(&self) -> bool {
        self.is_hidden.get()
    }
}


// === Getters ===

impl Symbol {
    pub fn surface(&self) -> &Mesh {
        &self.surface
    }

    pub fn shader(&self) -> &Shader {
        &self.shader
    }

    pub fn variables(&self) -> &UniformScope {
        &self.variables
    }
}


// === Private API ===

impl Symbol {
    /// Creates a new VertexArrayObject, discovers all variable bindings from shader to geometry,
    /// and initializes the VAO with the bindings.
    fn init_variable_bindings(&self, var_bindings:&[shader::VarBinding]) {
        let max_texture_units = self.context.get_parameter(Context::MAX_TEXTURE_IMAGE_UNITS);
        let max_texture_units = max_texture_units.unwrap_or_else(|num| {
            error!(self.logger,"Cannot retrieve max texture units: {num:?}. Assuming minimal \
                texture units possible");
            JsValue::from_f64(2.0)
        });
        let max_texture_units     = max_texture_units.as_f64().unwrap() as u32;
        let mut texture_unit_iter = 0..max_texture_units;
        self.bindings.borrow_mut().vao      = Some(VertexArrayObject::new(&self.context));
        self.bindings.borrow_mut().uniforms = default();
        self.bindings.borrow_mut().textures = default();
        self.with_program_mut(|this,program|{
            for binding in var_bindings {
                match binding.scope.as_ref() {
                    Some(ScopeType::Mesh(s)) => this.init_attribute_binding(program,binding,*s),
                    Some(_) => this.init_uniform_binding(program,binding,&mut texture_unit_iter),
                    None    => {}
                }
            }
        });
    }

    fn init_attribute_binding
    (&self, program:&WebGlProgram, binding:&shader::VarBinding, mesh_scope_type:mesh::ScopeType) {
        let vtx_name = shader::builder::mk_vertex_name(&binding.name);
        let scope    = self.surface.scope_by_type(mesh_scope_type);
        let location = self.context.get_attrib_location(program, &vtx_name);
        if location < 0 {
            error!(self.logger,"Attribute '{vtx_name}' not found.");
        } else {
            let location     = location as u32;
            let buffer       = &scope.buffer(&binding.name).unwrap();
            let is_instanced = mesh_scope_type == mesh::ScopeType::Instance;
            buffer.bind(Context::ARRAY_BUFFER);
            buffer.vertex_attrib_pointer(location, is_instanced);
        }
    }

    /// Init uniform binding. This function should be run in context of `program` (used inside
    /// closure passed as argument to `with_program`).
    fn init_uniform_binding
    ( &self
    , program:&WebGlProgram
    , binding:&shader::VarBinding
    , texture_unit_iter : &mut dyn Iterator<Item=TextureUnit>
    ) {
        let name         = &binding.name;
        let uni_name     = shader::builder::mk_uniform_name(name);
        let opt_location = self.context.get_uniform_location(program,&uni_name);

        opt_location.map(|location|{
            let uniform = match &binding.scope {
                Some(ScopeType::Symbol) => self.variables.get(name),
                Some(ScopeType::Global) => self.global_variables.get(name),
                _ => todo!()
            };
            let uniform = uniform.unwrap_or_else(||{
                panic!("Internal error. Variable {} not found in program.",name)
            });
            match uniform {
                AnyUniform::Prim(uniform) =>
                    self.bindings.borrow_mut().uniforms.push(UniformBinding::new(name,location,uniform)),
                AnyUniform::Texture(uniform) => {
                    match texture_unit_iter.next() {
                        Some(texture_unit) => {
                            let binding = TextureBinding::new(name,location,uniform,texture_unit);
                            binding.upload_uniform(&self.context);
                            self.bindings.borrow_mut().textures.push(binding);
                        }
                        None => {
                            error!(self.logger,"Texture unit limit exceeded.");
                        }
                    }
                }
            }
        });
    }

    /// For each variable from the shader definition, looks up its position in geometry scopes.
    fn discover_variable_bindings(&self) -> Vec<shader::VarBinding> {
        let var_decls = self.shader.collect_variables();
        var_decls.into_iter().map(|(var_name,var_decl)| {
            let target = self.lookup_variable(&var_name);
            if target.is_none() {
                warning!(self.logger,"Unable to bind variable '{var_name}' to geometry buffer.");
            }
            shader::VarBinding::new(var_name,var_decl,target)
        }).collect()
    }

    /// Runs the provided function in a context of active program and active VAO. After the function
    /// is executed, both program and VAO are bound to None.
    fn with_program<F:FnOnce(&WebGlProgram) -> T,T>(&self, f:F) -> T {
        let program = self.shader.program().unwrap(); // FIXME
        self.context.use_program(Some(&program));
        let bindings = self.bindings.borrow();
        let vao      = bindings.vao.as_ref().unwrap(); // FIXME
        let out      = vao.with(||{ f(&program) });
        self.context.use_program(None);
        out
    }

    /// Runs the provided function in a context of active program and active VAO. After the function
    /// is executed, both program and VAO are bound to None.
    fn with_program_mut<F:FnOnce(&Self, &WebGlProgram) -> T,T>(&self, f:F) -> T {
        let this:&Self = self;
        let program = this.shader.program().as_ref().unwrap().clone(); // FIXME
        this.context.use_program(Some(&program));
        let out = this.with_vao_mut(|this|{ f(this,&program) });
        self.context.use_program(None);
        out
    }

    fn with_vao_mut<F:FnOnce(&Self) -> T,T>(&self, f:F) -> T {
        self.bindings.borrow().vao.as_ref().unwrap().bind();
        let out = f(self);
        self.bindings.borrow().vao.as_ref().unwrap().unbind();
        out
    }
}


// === Conversions ===

impl display::Object for Symbol {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
