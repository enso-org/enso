//! GPU representation of symbols, meshes with shaders.

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display;
use crate::display::symbol::geometry::primitive::mesh;
use crate::system::gpu;
use crate::system::gpu::context::native::ContextOps;
use crate::system::gpu::data::buffer::IsBuffer;
use crate::system::gpu::data::texture::class::TextureOps;
use crate::system::gpu::data::uniform::AnyPrimUniform;
use crate::system::gpu::data::uniform::AnyPrimUniformOps;
use crate::system::gpu::data::uniform::AnyTextureUniform;
use crate::system::gpu::data::uniform::AnyUniform;

use enso_shapely::newtype_prim;
use enso_shapely::shared2;
use shader::Shader;
use shader::WeakShader;
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

/// Popular types.
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



// ================================
// === GlobalInstanceIdProvider ===
// ================================

newtype_prim! {
    /// Global [`Symbol`] instance id. Allows encoding symbol IDs in a texture and then decode on
    /// mouse interaction.
    ///
    /// Please see the [`fragment_runner.glsl`] file to see the encoding implementation and learn
    /// more about the possible overflow behavior.
    GlobalInstanceId(u32);
}

shared2! { GlobalInstanceIdProvider
    /// [`GlobalInstanceId`] provider.
    #[derive(Debug,Default)]
    pub struct GlobalInstanceIdProviderData {
        next: GlobalInstanceId,
        free: Vec<GlobalInstanceId>,
    }

    impl {
        /// Get a new [`GlobalInstanceId`] either by reusing previously disposed one or reserving a
        /// new one.
        pub fn reserve(&mut self) -> GlobalInstanceId {
            self.free.pop().unwrap_or_else(|| {
                let out = self.next;
                self.next = GlobalInstanceId::new((*out) + 1);
                out
            })
        }

        /// Dispose previously used [`GlobalInstanceId`]. It will be reused for new instances.
        pub fn dispose(&mut self, id: GlobalInstanceId) {
            self.free.push(id);
        }
    }
}



// ==============
// === Symbol ===
// ==============

// === Types ===

/// Attribute scope type. Attributes can be defined in one of the supported scopes and will be
/// automatically bound to the material definition during shader compilation.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum ScopeType {
    Mesh(mesh::ScopeType),
    Symbol,
    Global,
}

/// A dirty flag for the symbols' geometry.
pub type GeometryDirty = dirty::SharedBool<Box<dyn Fn()>>;

/// A dirty flag for the symbols' shader.
pub type ShaderDirty = dirty::SharedBool<Box<dyn Fn()>>;
/// A weak-referencing version of `ShaderDirty`.
pub type WeakShaderDirty = dirty::WeakSharedBool<Box<dyn Fn()>>;


// === Bindings ====

/// All attributes and uniforms bindings of symbol with the associated Vertex Array Object.
#[derive(Clone, Debug, Default)]
pub struct Bindings {
    vao:      Option<VertexArrayObject>,
    uniforms: Vec<UniformBinding>,
    textures: Vec<TextureBinding>,
}


// === Definition ===

newtype_prim! {
    /// The ID of a [`Symbol`] instance. The ID is also the index of the symbol inside of symbol
    /// registry.
    SymbolId(u32);
}

/// Symbol is a [`Mesh`] with attached [`Shader`].
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct Symbol {
    #[deref]
    data:         Rc<SymbolData>,
    shader_dirty: ShaderDirty,
    shader:       Shader,
}

impl Symbol {
    /// Constructor.
    pub fn new<OnMut: Fn() + Clone + 'static>(
        stats: &Stats,
        id: SymbolId,
        global_id_provider: &GlobalInstanceIdProvider,
        on_mut: OnMut,
    ) -> Self {
        let logger = Logger::new(format!("symbol_{}", id));
        debug_span!("Initializing.").in_scope(|| {
            let on_mut2 = on_mut.clone();
            let shader_dirty = ShaderDirty::new(Box::new(on_mut));
            let shader_logger = Logger::new_sub(&logger, "shader");
            let shader_on_mut = Box::new(f!(shader_dirty.set()));
            let shader = Shader::new(shader_logger, stats, shader_on_mut);
            let data = Rc::new(SymbolData::new(logger, stats, id, global_id_provider, on_mut2));
            Self { data, shader_dirty, shader }
        })
    }

    /// Create a new instance of this symbol.
    pub fn new_instance(&self) -> SymbolInstance {
        SymbolInstance::new(self)
    }

    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    pub(crate) fn set_context(&self, context: Option<&Context>) {
        *self.context.borrow_mut() = context.cloned();
        self.surface.set_context(context);
        self.shader.set_context(context);
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&self, global_variables: &UniformScope) {
        if self.context.borrow().is_some() {
            debug_span!("Updating.").in_scope(|| {
                if self.surface_dirty.check() {
                    self.surface.update();
                    self.surface_dirty.unset();
                }
                if self.shader_dirty.check() {
                    let var_bindings = self.discover_variable_bindings(global_variables);
                    let data = self.data.clone_ref();
                    let global_variables = global_variables.clone_ref();
                    self.shader.update(var_bindings, move |var_bindings, program| {
                        data.init_variable_bindings(var_bindings, &global_variables, program)
                    });
                    self.shader_dirty.unset();
                }
            })
        }
    }

    /// Render the symbol. You should never need to call this function directly. Use the rendering
    /// pipeline instead.
    pub fn render(&self) {
        debug_span!("Rendering.").in_scope(|| {
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

                    self.stats.inc_draw_call_count();
                    if instance_count > 0 {
                        context.draw_arrays_instanced(*mode, first, count, instance_count);
                    } else {
                        context.draw_arrays(*mode, first, count);
                    }
                });
            }
        })
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
        if let Some(program) = self.shader.native_program().as_ref() {
            context.with_program(program, || self.with_vao(|_| f(program)));
        }
    }
}


// === Visibility ===

impl Symbol {
    /// Show or hide the symbol.
    pub fn set_hidden(&self, b: bool) {
        self.is_hidden.set(b)
    }

    /// Hide the symbol.
    pub fn hide(&self) {
        self.set_hidden(true)
    }

    /// Show the previously hidden symbol.
    pub fn show(&self) {
        self.set_hidden(false)
    }

    /// Check whether the symbol was hidden.
    pub fn is_hidden(&self) -> bool {
        self.is_hidden.get()
    }
}


// === Getters ===

#[allow(missing_docs)]
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


// === Conversions ===

impl display::Object for Symbol {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl From<&Symbol> for SymbolId {
    fn from(t: &Symbol) -> Self {
        t.id
    }
}


// === Weak References ===

/// Weak reference to a [`Symbol`].
#[derive(Debug, Clone)]
pub struct WeakSymbol {
    data:         Weak<SymbolData>,
    shader_dirty: WeakShaderDirty,
    shader:       WeakShader,
}

impl WeakElement for WeakSymbol {
    type Strong = Symbol;

    fn new(view: &Self::Strong) -> Self {
        let data = view.data.downgrade();
        let shader_dirty = view.shader_dirty.downgrade();
        let shader = view.shader.downgrade();
        Self { data, shader_dirty, shader }
    }

    fn view(&self) -> Option<Self::Strong> {
        let data = self.data.upgrade()?;
        let shader_dirty = self.shader_dirty.upgrade()?;
        let shader = self.shader.upgrade()?;
        Some(Symbol { data, shader_dirty, shader })
    }

    fn is_expired(&self) -> bool {
        self.data.is_expired()
    }

    fn clone(view: &Self::Strong) -> Self::Strong
    where Self: Sized {
        view.clone()
    }
}



// ==================
// === SymbolData ===
// ==================

/// Internal representation of [`Symbol`]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct SymbolData {
    pub id:             SymbolId,
    global_id_provider: GlobalInstanceIdProvider,
    display_object:     display::object::Instance,
    surface:            Mesh,
    surface_dirty:      GeometryDirty,
    variables:          UniformScope,
    context:            RefCell<Option<Context>>,
    logger:             Logger,
    bindings:           RefCell<Bindings>,
    stats:              SymbolStats,
    is_hidden:          Rc<Cell<bool>>,
    global_instance_id: Buffer<i32>,
}



impl SymbolData {
    /// Create new instance with the provided on-dirty callback.
    pub fn new<OnMut: Fn() + Clone + 'static>(
        logger: Logger,
        stats: &Stats,
        id: SymbolId,
        global_id_provider: &GlobalInstanceIdProvider,
        on_mut: OnMut,
    ) -> Self {
        let global_id_provider = global_id_provider.clone_ref();
        let surface_logger = Logger::new_sub(&logger, "surface");
        let surface_dirty = GeometryDirty::new(Box::new(on_mut));
        let surface_on_mut = Box::new(f!(surface_dirty.set()));
        let surface = Mesh::new(surface_logger, stats, surface_on_mut);
        let variables = UniformScope::new(Logger::new_sub(&logger, "uniform_scope"));
        let bindings = default();
        let stats = SymbolStats::new(stats);
        let context = default();
        let display_object = display::object::Instance::new();
        let is_hidden = Rc::new(Cell::new(false));

        let instance_scope = surface.instance_scope();
        let global_instance_id = instance_scope.add_buffer("global_instance_id");

        Self {
            id,
            global_id_provider,
            display_object,
            surface,
            surface_dirty,
            variables,
            context,
            bindings,
            stats,
            is_hidden,
            global_instance_id,
            logger,
        }
        .init()
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


    /// Lookup variable by name. All mesh scopes, symbol uniform scope, and the global scope will be
    /// searched.
    pub fn lookup_variable<S: Str>(
        &self,
        name: S,
        global_variables: &UniformScope,
    ) -> Option<ScopeType> {
        let name = name.as_ref();
        match self.surface.lookup_variable(name) {
            Some(mesh_scope) => Some(ScopeType::Mesh(mesh_scope)),
            _ if self.variables.contains(name) => Some(ScopeType::Symbol),
            _ if global_variables.contains(name) => Some(ScopeType::Global),
            _ => None,
        }
    }
}


// === Private API ===

impl SymbolData {
    /// Creates a new VertexArrayObject, discovers all variable bindings from shader to geometry,
    /// and initializes the VAO with the bindings.
    fn init_variable_bindings(
        &self,
        var_bindings: &[shader::VarBinding],
        global_variables: &UniformScope,
        program: &gpu::shader::Program,
    ) {
        if let Some(context) = &*self.context.borrow() {
            let max_texture_units = context.get_parameter(*Context::MAX_TEXTURE_IMAGE_UNITS);
            let max_texture_units = max_texture_units.unwrap_or_else(|num| {
                let min_texture_units = 2;
                error!(
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
            context.with_program(&program.native, || {
                self.with_vao(|this| {
                    for binding in var_bindings {
                        match binding.scope.as_ref() {
                            Some(ScopeType::Mesh(scope)) =>
                                this.init_attribute_binding(context, program, binding, *scope),
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
            error!("Attribute '{vtx_name}' not found.");
        } else {
            let location = location as u32;
            let buffer = &scope.buffer(&binding.name).unwrap();
            let is_instanced = mesh_scope_type == mesh::ScopeType::Instance;
            buffer.bind(*Context::ARRAY_BUFFER);
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
                            error!("Texture unit limit exceeded.");
                        }
                    }
                }
            }
        });
    }

    fn with_vao<F: FnOnce(&Self) -> T, T>(&self, f: F) -> T {
        self.with_borrowed_vao_or_warn(|vao| vao.bind());
        let out = f(self);
        self.with_borrowed_vao_or_warn(|vao| vao.unbind());
        out
    }

    fn with_borrowed_vao_or_warn<T>(&self, f: impl FnOnce(&VertexArrayObject) -> T) -> Option<T> {
        let out = self.bindings.borrow().vao.as_ref().map(f);
        if out.is_none() {
            error!("Vertex Array Object not found during rendering.");
        }
        out
    }
}



// ===================
// === RenderGroup ===
// ===================

/// Collection of [`Symbol`]s to be rendered, in render-order.
#[derive(Debug, Default, Derivative)]
pub struct RenderGroup {
    /// Symbols, identified by ID.
    ids:     Vec<SymbolId>,
    /// If present, this refers to the same symbols as `ids`, ready to render.
    #[derivative(Debug = "ignore")]
    symbols: RefCell<Option<Vec<WeakSymbol>>>,
}

impl RenderGroup {
    /// Set the [`Symbol`]s to be rendered, identified by IDs.
    pub fn set(&mut self, symbols: Vec<SymbolId>) {
        self.ids = symbols;
        self.symbols.borrow_mut().take();
    }
}



// ======================
// === SymbolInstance ===
// ======================

/// Instance of a [`Symbol`]. It does not define any custom parameters, however, it manages the
/// [`InstanceIndex`] and [`GlobalInstanceId`] ones.
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct SymbolInstance {
    rc: Rc<SymbolInstanceData>,
}

/// Internal representation of [`SymbolInstance`].
#[derive(Debug, NoCloneBecauseOfCustomDrop)]
#[allow(missing_docs)]
pub struct SymbolInstanceData {
    pub symbol:             Symbol,
    pub instance_id:        attribute::InstanceIndex,
    pub global_instance_id: GlobalInstanceId,
}

impl SymbolInstance {
    fn new(symbol: &Symbol) -> Self {
        let symbol = symbol.clone_ref();
        let global_instance_id = symbol.global_id_provider.reserve();
        let instance_id = symbol.surface().instance_scope().add_instance();

        let global_instance_id_attr = symbol.global_instance_id.at(instance_id);
        global_instance_id_attr.set(*global_instance_id as i32);

        let data = SymbolInstanceData { symbol, instance_id, global_instance_id };
        let rc = Rc::new(data);
        Self { rc }
    }
}

impl Drop for SymbolInstanceData {
    fn drop(&mut self) {
        self.symbol.surface().instance_scope().dispose(self.instance_id);
        self.symbol.global_id_provider.dispose(self.global_instance_id);
    }
}
