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

use crate::closure;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::data::function::callback::*;
use crate::debug::stats::Stats;
use crate::display::render::webgl::Context;
use crate::display::render::webgl;
use crate::system::gpu::buffer::IsBuffer;
use crate::system::gpu::data::uniform::AnyUniform;
use crate::system::gpu::data::uniform::AnyUniformOps;
use crate::system::gpu::data::uniform::UniformScope;
use crate::display::symbol::geometry::primitive::mesh;
use crate::promote;
use crate::promote_all;
use crate::promote_mesh_types;
use crate::promote_shader_types;
use crate::system::web::group;
use crate::system::web::Logger;

use eval_tt::*;
use web_sys::WebGlVertexArrayObject;
use web_sys::WebGlProgram;
use web_sys::WebGlUniformLocation;




/// Binds input variable definition in shader to both its location and an uniform declaration.
#[derive(Clone,Debug)]
pub struct UniformBinding {
    name     : String,
    location : WebGlUniformLocation,
    uniform  : AnyUniform,
}

impl UniformBinding {
    pub fn new<Name:Str>(name:Name, location:WebGlUniformLocation, uniform:AnyUniform) -> Self {
        let name = name.into();
        Self {name,location,uniform}
    }
}



// =========================
// === VertexArrayObject ===
// =========================

/// A safe wrapper for WebGL VertexArrayObject. It implements `drop` which deletes the VAO from
/// GPU memory as soon as this object is dropped.
#[derive(Debug)]
pub struct VertexArrayObject {
    context : Context,
    vao     : WebGlVertexArrayObject,
}

// === Public API ===

impl VertexArrayObject {
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

impl VertexArrayObject {
    fn bind(&self) {
        self.context.bind_vertex_array(Some(&self.vao));
    }

    fn unbind(&self) {
        self.context.bind_vertex_array(None);
    }
}


// === Instances ===

impl Drop for VertexArrayObject {
    fn drop(&mut self) {
        self.context.delete_vertex_array(Some(&self.vao));
    }
}



// ==============
// === Symbol ===
// ==============

// === Definition ===

/// Symbol is a surface with attached `Shader`.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Symbol<OnMut> {
    pub surface        : Mesh          <OnMut>,
    pub shader         : Shader        <OnMut>,
    pub surface_dirty  : GeometryDirty <OnMut>,
    pub shader_dirty   : ShaderDirty   <OnMut>,
    pub logger         : Logger,
    context            : Context,
    vao                : Option<VertexArrayObject>,
    uniforms           : Vec<UniformBinding>,
    stats              : Stats,
}

// === Types ===

pub type GeometryDirty<Callback> = dirty::SharedBool<Callback>;
pub type ShaderDirty<Callback> = dirty::SharedBool<Callback>;
promote_mesh_types!   { [OnSurfaceMut] mesh }
promote_shader_types! { [OnSurfaceMut] shader }

#[macro_export]
/// Promote relevant types to parent scope. See `promote!` macro for more information.
macro_rules! promote_symbol_types { ($($args:tt)*) => {
    crate::promote_mesh_types!   {$($args)*}
    crate::promote_shader_types! {$($args)*}
    promote! {$($args)* [Symbol]}
};}


// === Callbacks ===

closure! {
fn surface_on_mut<C:Callback0>(dirty:GeometryDirty<C>) -> OnSurfaceMut {
    || dirty.set()
}}

closure! {
fn shader_on_mut<C:Callback0>(dirty:ShaderDirty<C>) -> OnShaderMut {
    || dirty.set()
}}


// === Implementation ===

impl<OnMut:Callback0+Clone> Symbol<OnMut> {

    /// Create new instance with the provided on-dirty callback.
    pub fn new
    (global:&UniformScope, logger:Logger, stats:&Stats, ctx:&Context, on_dirty:OnMut) -> Self {
        stats.inc_symbol_count();
        let init_logger = logger.clone();
        group!(init_logger, "Initializing.", {
            let context         = ctx.clone();
            let on_dirty2       = on_dirty.clone();
            let surface_logger  = logger.sub("surface");
            let shader_logger   = logger.sub("shader");
            let geo_dirt_logger = logger.sub("surface_dirty");
            let mat_dirt_logger = logger.sub("shader_dirty");
            let surface_dirty   = GeometryDirty::new(geo_dirt_logger,on_dirty2);
            let shader_dirty    = ShaderDirty::new(mat_dirt_logger,on_dirty);
            let geo_on_change   = surface_on_mut(surface_dirty.clone_ref());
            let mat_on_change   = shader_on_mut(shader_dirty.clone_ref());
            let shader          = Shader::new(shader_logger,&stats,ctx,mat_on_change);
            let surface         = Mesh::new(global,surface_logger,&stats,ctx,geo_on_change);
            let vao             = default();
            let uniforms        = default();
            let stats           = stats.clone_ref();
            Self{surface,shader,surface_dirty,shader_dirty,logger,context,vao,uniforms,stats}
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.surface_dirty.check() {
                self.surface.update();
                self.surface_dirty.unset();
            }
            if self.shader_dirty.check() {
                let var_bindings = self.discover_variable_bindings();
                self.shader.update(&var_bindings);
                self.init_vao(&var_bindings);
                self.shader_dirty.unset();
            }
        })
    }

    /// Creates a new VertexArrayObject, discovers all variable bindings from shader to geometry,
    /// and initializes the VAO with the bindings.
    fn init_vao(&mut self, var_bindings:&[shader::VarBinding]) {
        self.vao = Some(VertexArrayObject::new(&self.context));
        let mut uniforms: Vec<UniformBinding> = default();
        self.with_program(|program|{
            for binding in var_bindings {
                if let Some(scope_type) = binding.scope.as_ref() {
                    let opt_scope = self.surface.var_scope(*scope_type);
                    match opt_scope {
                        None => {
                            let name     = &binding.name;
                            let uni_name = shader::builder::mk_uniform_name(name);
                            let location = self.context.get_uniform_location(program,&uni_name);
                            match location {
                                None => self.logger.warning(|| format!("The uniform '{}' is not used in this shader. It is recommended to remove it from the material definition.", name)),
                                Some(location) => {
                                    let uniform = self.surface.scopes.global.get(name).unwrap();
                                    let binding = UniformBinding::new(name,location,uniform);
                                    uniforms.push(binding);
                                }
                            }

                        },
                        Some(scope) => {
                            let vtx_name = shader::builder::mk_vertex_name(&binding.name);
                            let location = self.context.get_attrib_location(program, &vtx_name);
                            if location < 0 {
                                self.logger.error(|| format!("Attribute '{}' not found.",vtx_name));
                            } else {
                                let location     = location as u32;
                                let buffer       = &scope.buffer(&binding.name).unwrap();
                                let is_instanced = scope_type == &mesh::ScopeType::Instance;
                                buffer.bind(webgl::Context::ARRAY_BUFFER);
                                buffer.vertex_attrib_pointer(location, is_instanced);
                            }
                        }
                    }
                }
            }
        });
        self.uniforms = uniforms;
    }

    /// For each variable from the shader definition, looks up its position in geometry scopes.
    pub fn discover_variable_bindings(&self) -> Vec<shader::VarBinding> {
        let var_decls = self.shader.collect_variables();
        var_decls.into_iter().map(|(var_name,var_decl)| {
            let target = self.surface.lookup_variable(&var_name);
            if target.is_none() {
                let msg = || format!("Unable to bind variable '{}' to geometry buffer.", var_name);
                self.logger.warning(msg);
            }
            shader::VarBinding::new(var_name,var_decl,target)
        }).collect()
    }

    /// Runs the provided function in a context of active program and active VAO. After the function
    /// is executed, both program and VAO are bound to None.
    pub fn with_program<F:FnOnce(&WebGlProgram) -> T,T>(&self, f:F) -> T {
        let program = self.shader.program().as_ref().unwrap(); // FIXME
        self.context.use_program(Some(&program));
        let vao = self.vao.as_ref().unwrap(); // FIXME
        let out = vao.with(|| {
            f(program)
        });
        self.context.use_program(None);
        out
    }

    pub fn render(&self) {
        group!(self.logger, "Rendering.", {
            self.with_program(|_|{
                for binding in &self.uniforms {
                    binding.uniform.upload(&self.context,&binding.location);
                }

                let mode           = webgl::Context::TRIANGLE_STRIP;
                let first          = 0;
                let count          = self.surface.scopes.point.size()    as i32;
                let instance_count = self.surface.scopes.instance.size() as i32;

                self.stats.inc_draw_call_count();
                self.context.draw_arrays_instanced(mode,first,count,instance_count);
            });

        })
    }
}

impl<OnMut> Drop for Symbol<OnMut> {
    fn drop(&mut self) {
        self.stats.dec_symbol_count();
    }
}
