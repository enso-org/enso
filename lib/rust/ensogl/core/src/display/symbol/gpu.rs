// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display;
use crate::display::symbol::geometry::primitive::mesh;
use crate::system::gpu::data::buffer::IsBuffer;
use crate::system::gpu::data::texture::class::TextureOps;
use crate::system::gpu::data::uniform::AnyPrimUniform;
use crate::system::gpu::data::uniform::AnyPrimUniformOps;
use crate::system::gpu::data::uniform::AnyTextureUniform;
use crate::system::gpu::data::uniform::AnyUniform;

use enso_shapely::newtype_prim;
use shader::Shader;
use wasm_bindgen::JsValue;
use web_sys::WebGlProgram;
use web_sys::WebGlUniformLocation;
use web_sys::WebGlVertexArrayObject;


// ==============
// === Export ===
// ==============

#[warn(missing_docs)]
pub mod geometry;
#[warn(missing_docs)]
pub mod material;
#[warn(missing_docs)]
pub mod registry;
#[warn(missing_docs)]
pub mod shader;



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
#[derive(Clone, Debug)]
pub struct UniformBinding {
    name:     String,
    location: WebGlUniformLocation,
    uniform:  AnyPrimUniform,
}

impl UniformBinding {
    /// Create new uniform binding.
    pub fn new<Name: Str>(
        name: Name,
        location: WebGlUniformLocation,
        uniform: AnyPrimUniform,
    ) -> Self {
        let name = name.into();
        Self { name, location, uniform }
    }

    /// Upload uniform value.
    pub fn upload(&self, context: &Context) {
        self.uniform.upload(context, &self.location);
    }
}

type TextureUnit = u32;

/// Binds input sampler definition in shader to its location, uniform declaration and texture unit.
#[derive(Clone, Debug)]
pub struct TextureBinding {
    name:         String,
    location:     WebGlUniformLocation,
    uniform:      AnyTextureUniform,
    texture_unit: TextureUnit,
}

impl TextureBinding {
    /// Create new texture binding.
    pub fn new<Name: Str>(
        name: Name,
        location: WebGlUniformLocation,
        uniform: AnyTextureUniform,
        texture_unit: TextureUnit,
    ) -> Self {
        let name = name.into();
        Self { name, location, uniform, texture_unit }
    }

    /// Bind texture to proper texture unit.
    pub fn bind_texture_unit(&self, context: &Context) -> TextureBindGuard {
        self.uniform.bind_texture_unit(context, self.texture_unit.into())
    }

    /// Upload uniform value.
    pub fn upload_uniform(&self, context: &Context) {
        context.uniform1i(Some(&self.location), self.texture_unit as i32);
    }
}



// =========================
// === VertexArrayObject ===
// =========================

/// A safe wrapper for WebGL VertexArrayObject. It releases the VAO from GPU memory as soon as all
/// references to this object are dropped.
#[derive(Clone, Debug, Shrinkwrap)]
pub struct VertexArrayObject {
    rc: Rc<VertexArrayObjectData>,
}

impl VertexArrayObject {
    /// Constructor
    pub fn new(context: &Context) -> Self {
        let rc = Rc::new(VertexArrayObjectData::new(context));
        Self { rc }
    }
}


// === Data ===

/// Internal representation for `VertexArrayObject`.
#[derive(Debug)]
pub struct VertexArrayObjectData {
    context: Context,
    vao:     WebGlVertexArrayObject,
}


// === Public API ===

impl VertexArrayObjectData {
    /// Creates a new VAO instance.
    pub fn new(context: &Context) -> Self {
        let context = context.clone();
        let vao = context.create_vertex_array().unwrap();
        Self { context, vao }
    }

    /// Binds the VAO, evaluates the provided function, and unbinds the VAO.
    pub fn with<F: FnOnce() -> T, T>(&self, f: F) -> T {
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
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct SymbolStats {
    rc: Rc<SymbolStatsData>,
}

/// Internal representation for `SymbolStats`.
#[derive(Debug, Shrinkwrap)]
pub struct SymbolStatsData {
    stats: Stats,
}

impl SymbolStats {
    /// Constructor.
    pub fn new(stats: &Stats) -> Self {
        let rc = Rc::new(SymbolStatsData::new(stats));
        Self { rc }
    }
}

impl SymbolStatsData {
    /// Constructor.
    pub fn new(stats: &Stats) -> Self {
        stats.inc_symbol_count();
        let stats = stats.clone_ref();
        Self { stats }
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ScopeType {
    Mesh(mesh::ScopeType),
    Symbol,
    Global,
}

pub type GeometryDirty = dirty::SharedBool<Box<dyn Fn()>>;
pub type ShaderDirty = dirty::SharedBool<Box<dyn Fn()>>;


// === Bindings ====

#[derive(Clone, Debug, Default)]
pub struct Bindings {
    vao:      Option<VertexArrayObject>,
    uniforms: Vec<UniformBinding>,
    textures: Vec<TextureBinding>,
}


// === Definition ===

newtype_prim! {
    /// The ID of a [`Symbol`] instance. The ID is also the index of the symbol inside of symbol
    /// registry. In case the symbol was not yet registered, the ID will be `0`.
    SymbolId(u32);
}

/// Symbol is a surface with attached `Shader`.
#[derive(Debug, Clone, CloneRef)]
pub struct Symbol {
    pub id:            SymbolId,
    display_object:    display::object::Instance,
    surface:           Mesh,
    shader:            Shader,
    surface_dirty:     GeometryDirty,
    shader_dirty:      ShaderDirty,
    variables:         UniformScope,
    /// Please note that changing the uniform type to `u32` breaks node ID encoding in GLSL, as the
    /// functions are declared to work on `int`s, not `uint`s. This might be improved one day.
    symbol_id_uniform: Uniform<i32>,
    context:           Rc<RefCell<Option<Context>>>,
    logger:            Logger,
    bindings:          Rc<RefCell<Bindings>>,
    stats:             SymbolStats,
    is_hidden:         Rc<Cell<bool>>,
}

impl Symbol {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<OnMut: Fn() + Clone + 'static>(stats: &Stats, id: SymbolId, on_mut: OnMut) -> Self {
        let logger = Logger::new(format!("symbol_{}", id));
        let init_logger = logger.clone();
        debug!(init_logger, "Initializing.", || {
            let on_mut2 = on_mut.clone();
            let surface_logger = Logger::new_sub(&logger, "surface");
            let shader_logger = Logger::new_sub(&logger, "shader");
            let geo_dirt_logger = Logger::new_sub(&logger, "surface_dirty");
            let mat_dirt_logger = Logger::new_sub(&logger, "shader_dirty");
            let surface_dirty = GeometryDirty::new(geo_dirt_logger, Box::new(on_mut2));
            let shader_dirty = ShaderDirty::new(mat_dirt_logger, Box::new(on_mut));
            let surface_on_mut = Box::new(f!(surface_dirty.set()));
            let shader_on_mut = Box::new(f!(shader_dirty.set()));
            let shader = Shader::new(shader_logger, stats, shader_on_mut);
            let surface = Mesh::new(surface_logger, stats, surface_on_mut);
            let variables = UniformScope::new(Logger::new_sub(&logger, "uniform_scope"));
            let bindings = default();
            let stats = SymbolStats::new(stats);
            let context = default();
            let symbol_id_uniform = variables.add_or_panic("symbol_id", (*id) as i32);
            let display_object = display::object::Instance::new(logger.clone());
            let is_hidden = Rc::new(Cell::new(false));
            Self {
                id,
                display_object,
                surface,
                shader,
                surface_dirty,
                shader_dirty,
                variables,
                symbol_id_uniform,
                context,
                logger,
                bindings,
                stats,
                is_hidden,
            }
            .init()
        })
    }

    fn init(self) -> Self {
        let is_hidden = &self.is_hidden;
        let id = self.id;
        self.display_object.set_on_hide(f_!(is_hidden.set(true)));
        self.display_object.set_on_show(f__!(is_hidden.set(false)));
        self.display_object.set_on_scene_layer_changed(move |_, old_layers, new_layers| {
            for layer in old_layers.iter().filter_map(|t| t.upgrade()) {
                layer.remove_symbol(id)
            }
            for layer in new_layers.iter().filter_map(|t| t.upgrade()) {
                layer.add_symbol(id)
            }
        });
        self
    }

    pub(crate) fn set_context(&self, context: Option<&Context>) {
        *self.context.borrow_mut() = context.cloned();
        self.surface.set_context(context);
        self.shader.set_context(context);
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&self, global_variables: &UniformScope) {
        if self.context.borrow().is_some() {
            debug!(self.logger, "Updating.", || {
                if self.surface_dirty.check() {
                    self.surface.update();
                    self.surface_dirty.unset();
                }
                if self.shader_dirty.check() {
                    let var_bindings = self.discover_variable_bindings(global_variables);
                    self.shader.update(&var_bindings);
                    self.init_variable_bindings(&var_bindings, global_variables);
                    self.shader_dirty.unset();
                }
            })
        }
    }

    pub fn lookup_variable<S: Str>(
        &self,
        name: S,
        global_variables: &UniformScope,
    ) -> Option<ScopeType> {
        let name = name.as_ref();
        self.surface.lookup_variable(name).map(ScopeType::Mesh).or_else(|| {
            if self.variables.contains(name) {
                Some(ScopeType::Symbol)
            } else if global_variables.contains(name) {
                Some(ScopeType::Global)
            } else {
                None
            }
        })
    }

    pub fn render(&self) {
        debug!(self.logger, "Rendering.", || {
            if self.is_hidden() {
                return;
            }
            if let Some(context) = &*self.context.borrow() {
                self.with_program(context, |_| {
                    for binding in &self.bindings.borrow().uniforms {
                        binding.upload(context);
                    }

                    let textures = &self.bindings.borrow().textures;
                    let bound_textures_iter = textures.iter().map(|t| t.bind_texture_unit(context));
                    let _textures_keep_alive = bound_textures_iter.collect_vec();

                    let mode = Context::TRIANGLE_STRIP;
                    let first = 0;
                    let count = self.surface.point_scope().size() as i32;
                    let instance_count = self.surface.instance_scope().size() as i32;

                    // println!("rendering symbol {:?}. count {}, instance count
                    // {}",self.id,count,instance_count);

                    // FIXME: we should uncomment the following code in some pedantic debug mode. It
                    //        introduces severe performance overhead (0.8ms -> 3ms per frame)
                    // because        it requires GPU to sync. However, we
                    // should maintain a "pedantic mode" in        case
                    // something goes horribly wrong and we would like to discover what.

                    // // Check if we are ready to render. If we don't assert here we wil only get a
                    // warning // that won't tell us where things went wrong.
                    // {
                    //     let framebuffer_status =
                    // context.check_framebuffer_status(Context::FRAMEBUFFER);
                    //     debug_assert_eq!(
                    //         framebuffer_status,
                    //         Context::FRAMEBUFFER_COMPLETE,
                    //         "Framebuffer incomplete (status: {}).",
                    //         framebuffer_status
                    //         )
                    // }

                    self.stats.inc_draw_call_count();
                    if instance_count > 0 {
                        context.draw_arrays_instanced(mode, first, count, instance_count);
                    } else {
                        context.draw_arrays(mode, first, count);
                    }
                });
            }
        })
    }
}

impl From<&Symbol> for SymbolId {
    fn from(t: &Symbol) -> Self {
        t.id
    }
}


// === Visibility ===

impl Symbol {
    pub fn set_hidden(&self, b: bool) {
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
    fn init_variable_bindings(
        &self,
        var_bindings: &[shader::VarBinding],
        global_variables: &UniformScope,
    ) {
        if let Some(context) = &*self.context.borrow() {
            let max_texture_units = context.get_parameter(Context::MAX_TEXTURE_IMAGE_UNITS);
            let max_texture_units = max_texture_units.unwrap_or_else(|num| {
                let min_texture_units = 2;
                error!(
                    self.logger,
                    "Cannot retrieve max texture units: {num:?}. \
                    Assuming minimal texture units possible ({min_texture_units})."
                );
                JsValue::from_f64(min_texture_units as f64)
            });
            let max_texture_units = max_texture_units.as_f64().unwrap() as u32;
            let mut texture_unit_iter = 0..max_texture_units;
            self.bindings.borrow_mut().vao = Some(VertexArrayObject::new(context));
            self.bindings.borrow_mut().uniforms = default();
            self.bindings.borrow_mut().textures = default();
            self.with_program_mut(context, |this, program| {
                for binding in var_bindings {
                    match binding.scope.as_ref() {
                        Some(ScopeType::Mesh(s)) =>
                            this.init_attribute_binding(context, program, binding, *s),
                        Some(_) => this.init_uniform_binding(
                            context,
                            program,
                            binding,
                            &mut texture_unit_iter,
                            global_variables,
                        ),
                        None => {}
                    }
                }
            });
        }
    }

    fn init_attribute_binding(
        &self,
        context: &Context,
        program: &WebGlProgram,
        binding: &shader::VarBinding,
        mesh_scope_type: mesh::ScopeType,
    ) {
        let vtx_name = shader::builder::mk_vertex_name(&binding.name);
        let scope = self.surface.scope_by_type(mesh_scope_type);
        let location = context.get_attrib_location(program, &vtx_name);
        if location < 0 {
            error!(self.logger, "Attribute '{vtx_name}' not found.");
        } else {
            let location = location as u32;
            let buffer = &scope.buffer(&binding.name).unwrap();
            let is_instanced = mesh_scope_type == mesh::ScopeType::Instance;
            buffer.bind(Context::ARRAY_BUFFER);
            buffer.vertex_attrib_pointer(location, is_instanced);
        }
    }

    /// Init uniform binding. This function should be run in context of `program` (used inside
    /// closure passed as argument to `with_program`).
    fn init_uniform_binding(
        &self,
        context: &Context,
        program: &WebGlProgram,
        binding: &shader::VarBinding,
        texture_unit_iter: &mut dyn Iterator<Item = TextureUnit>,
        global_variables: &UniformScope,
    ) {
        let name = &binding.name;
        let uni_name = shader::builder::mk_uniform_name(name);
        let opt_location = context.get_uniform_location(program, &uni_name);

        opt_location.map(|location| {
            let uniform = match &binding.scope {
                Some(ScopeType::Symbol) => self.variables.get(name),
                Some(ScopeType::Global) => global_variables.get(name),
                _ => todo!(),
            };
            let uniform = uniform.unwrap_or_else(|| {
                panic!("Internal error. Variable {} not found in program.", name)
            });
            match uniform {
                AnyUniform::Prim(uniform) => self
                    .bindings
                    .borrow_mut()
                    .uniforms
                    .push(UniformBinding::new(name, location, uniform)),
                AnyUniform::Texture(uniform) => {
                    match texture_unit_iter.next() {
                        Some(texture_unit) => {
                            let binding =
                                TextureBinding::new(name, location, uniform, texture_unit);
                            // The following line binds the uniform to the right texture unit.
                            // Without it symbols will multiple textures would not work.
                            binding.upload_uniform(context);
                            self.bindings.borrow_mut().textures.push(binding);
                        }
                        None => {
                            error!(self.logger, "Texture unit limit exceeded.");
                        }
                    }
                }
            }
        });
    }

    /// For each variable from the shader definition, looks up its position in geometry scopes.
    fn discover_variable_bindings(
        &self,
        global_variables: &UniformScope,
    ) -> Vec<shader::VarBinding> {
        let var_decls = self.shader.collect_variables();
        var_decls
            .into_iter()
            .map(|(var_name, var_decl)| {
                let target = self.lookup_variable(&var_name, global_variables);
                if target.is_none() {
                    warning!(
                        self.logger,
                        "Unable to bind variable '{var_name}' to geometry buffer."
                    );
                }
                shader::VarBinding::new(var_name, var_decl, target)
            })
            .collect()
    }

    /// Runs the provided function in a context of active program and active VAO. After the function
    /// is executed, both program and VAO are bound to None.
    fn with_program<F: FnOnce(&WebGlProgram)>(&self, context: &Context, f: F) {
        if let Some(program) = self.shader.program().as_ref() {
            context.use_program(Some(program));
            let bindings = self.bindings.borrow();
            if let Some(vao) = bindings.vao.as_ref() {
                vao.with(|| f(program));
            }
            context.use_program(None);
        }
    }

    /// Runs the provided function in a context of active program and active VAO. After the function
    /// is executed, both program and VAO are bound to None.
    fn with_program_mut<F: FnOnce(&Self, &WebGlProgram)>(&self, context: &Context, f: F) {
        if let Some(program) = self.shader.program().as_ref() {
            context.use_program(Some(program));
            self.with_vao_mut(|this| f(this, program));
            context.use_program(None);
        }
    }

    fn with_vao_mut<F: FnOnce(&Self) -> T, T>(&self, f: F) -> T {
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
