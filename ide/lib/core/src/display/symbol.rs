#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod geometry;
#[warn(missing_docs)]
pub mod material;
#[warn(missing_docs)]
pub mod registry;
#[warn(missing_docs)]
pub mod shader;

pub mod types {
    use super::*;
    pub use geometry::types::*;
}
pub use types::*;


use crate::prelude::*;

use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::system::gpu::shader::Context;
use crate::system::gpu::data::buffer::IsBuffer;
use crate::system::gpu::data::uniform::AnyUniform;
use crate::system::gpu::data::uniform::AnyTextureUniform;
use crate::system::gpu::data::uniform::AnyPrimUniform;
use crate::system::gpu::data::uniform::AnyPrimUniformOps;
use crate::display::symbol::geometry::primitive::mesh;

use shader::Shader;

use web_sys::WebGlVertexArrayObject;
use web_sys::WebGlProgram;
use web_sys::WebGlUniformLocation;





/// Binds input variable definition in shader to both its location and an uniform declaration.
#[derive(Clone,Debug)]
pub struct UniformBinding {
    name     : String,
    location : WebGlUniformLocation,
    uniform  : AnyPrimUniform,
}

impl UniformBinding {
    pub fn new<Name:Str>(name:Name, location:WebGlUniformLocation, uniform:AnyPrimUniform) -> Self {
        let name = name.into();
        Self {name,location,uniform}
    }
}



#[derive(Clone,Debug)]
pub struct TextureBinding {
    name     : String,
    location : WebGlUniformLocation,
    uniform  : AnyTextureUniform,
}

impl TextureBinding {
    pub fn new<Name:Str>(name:Name, location:WebGlUniformLocation, uniform:AnyTextureUniform) -> Self {
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
pub struct Symbol {
    pub surface        : Mesh,
    pub shader         : Shader,
    pub surface_dirty  : GeometryDirty,
    pub shader_dirty   : ShaderDirty,
    symbol_scope       : UniformScope,
    global_scope       : UniformScope,
    context            : Context,
    logger             : Logger,
    vao                : Option<VertexArrayObject>,
    uniforms           : Vec<UniformBinding>,
    textures           : Vec<TextureBinding>,
    stats              : Stats,
}

// === Types ===

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum ScopeType {
    Mesh(mesh::ScopeType), Symbol, Global
}

pub type GeometryDirty = dirty::SharedBool<Box<dyn Fn()>>;
pub type ShaderDirty   = dirty::SharedBool<Box<dyn Fn()>>;



// === Implementation ===

impl Symbol {

    /// Create new instance with the provided on-dirty callback.
    pub fn new <OnMut:Fn()+Clone+'static>
    (global_scope:&UniformScope, logger:Logger, stats:&Stats, context:&Context, on_mut:OnMut) -> Self {
        stats.inc_symbol_count();
        let init_logger = logger.clone();
        group!(init_logger, "Initializing.", {
            let on_mut2         = on_mut.clone();
            let surface_logger  = logger.sub("surface");
            let shader_logger   = logger.sub("shader");
            let geo_dirt_logger = logger.sub("surface_dirty");
            let mat_dirt_logger = logger.sub("shader_dirty");
            let surface_dirty   = GeometryDirty::new(geo_dirt_logger,Box::new(on_mut2));
            let shader_dirty    = ShaderDirty::new(mat_dirt_logger,Box::new(on_mut));
            let surface_dirty2  = surface_dirty.clone_ref();
            let shader_dirty2   = shader_dirty.clone_ref();
            let surface_on_mut  = Box::new(move || { surface_dirty2.set() });
            let shader_on_mut   = Box::new(move || { shader_dirty2.set() });
            let shader          = Shader::new(shader_logger,&stats,context,shader_on_mut);
            let surface         = Mesh::new(surface_logger,&stats,context,surface_on_mut);
            let symbol_scope    = UniformScope::new(logger.sub("uniform_scope"),context);
            let global_scope    = global_scope.clone();
            let vao             = default();
            let uniforms        = default();
            let textures        = default();
            let stats           = stats.clone_ref();
            let context         = context.clone();
            Self{surface,shader,surface_dirty,shader_dirty,symbol_scope,global_scope,logger,context,vao,uniforms,textures,stats}
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
                self.init_variable_bindings(&var_bindings);
                self.shader_dirty.unset();
            }
        })
    }

    /// Creates a new VertexArrayObject, discovers all variable bindings from shader to geometry,
    /// and initializes the VAO with the bindings.
    fn init_variable_bindings(&mut self, var_bindings:&[shader::VarBinding]) {
        self.vao      = Some(VertexArrayObject::new(&self.context));
        self.uniforms = default();
        self.textures = default();
        self.with_program_mut(|this,program|{
            for binding in var_bindings {
                if let Some(scope_type) = binding.scope.as_ref() {
                    match scope_type {
                        ScopeType::Mesh(s) => this.init_attribute_binding(program,binding,*s),
                        _                  => this.init_uniform_binding(program,binding),
                    }
                }
            }
        });
    }


    fn init_attribute_binding(&mut self, program:&WebGlProgram, binding:&shader::VarBinding, mesh_scope_type:mesh::ScopeType) {
        let vtx_name = shader::builder::mk_vertex_name(&binding.name);
        let scope    = self.surface.scope_by_type(mesh_scope_type);
        let location = self.context.get_attrib_location(program, &vtx_name);
        if location < 0 {
            self.logger.error(|| format!("Attribute '{}' not found.",vtx_name));
        } else {
            let location     = location as u32;
            let buffer       = &scope.buffer(&binding.name).unwrap();
            let is_instanced = mesh_scope_type == mesh::ScopeType::Instance;
            buffer.bind(Context::ARRAY_BUFFER);
            buffer.vertex_attrib_pointer(location, is_instanced);
        }
    }

    fn init_uniform_binding(&mut self, program:&WebGlProgram, binding:&shader::VarBinding) {
        let name         = &binding.name;
        let uni_name     = shader::builder::mk_uniform_name(name);
        let opt_location = self.context.get_uniform_location(program,&uni_name);
        opt_location.map(|location|{
            let uniform = self.global_scope.get(name).unwrap_or_else(||{
                panic!("Internal error. Variable {} not found in program.",name)
            });
            match uniform {
                AnyUniform::Prim(uniform) =>
                    self.uniforms.push(UniformBinding::new(name,location,uniform)),
                AnyUniform::Texture(uniform) =>
                    self.textures.push(TextureBinding::new(name,location,uniform)),
            }
        });
    }

    pub fn lookup_variable<S:Str>(&self, name:S) -> Option<ScopeType> {
        let name = name.as_ref();
        self.surface.lookup_variable(name).map(ScopeType::Mesh).or_else(|| {
            if      self.symbol_scope.contains(name) { Some(ScopeType::Symbol) }
            else if self.global_scope.contains(name) { Some(ScopeType::Global) }
            else                                     { None }
        })
    }

    /// For each variable from the shader definition, looks up its position in geometry scopes.
    pub fn discover_variable_bindings(&self) -> Vec<shader::VarBinding> {
        let var_decls = self.shader.collect_variables();
        var_decls.into_iter().map(|(var_name,var_decl)| {
            let target = self.lookup_variable(&var_name);
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
        let out = vao.with(||{ f(program) });
        self.context.use_program(None);
        out
    }

    /// Runs the provided function in a context of active program and active VAO. After the function
    /// is executed, both program and VAO are bound to None.
    pub fn with_program_mut<F:FnOnce(&mut Self, &WebGlProgram) -> T,T>(&mut self, f:F) -> T {
        let this:&mut Self = self;
        let program = this.shader.program().as_ref().unwrap().clone(); // FIXME
        this.context.use_program(Some(&program));
        let out = this.with_vao_mut(|this|{ f(this,&program) });
        self.context.use_program(None);
        out
    }

    pub fn with_vao_mut<F:FnOnce(&mut Self) -> T,T>(&mut self, f:F) -> T {
        self.vao.as_ref().unwrap().bind();
        let out = f(self);
        self.vao.as_ref().unwrap().unbind();
        out
    }

    pub fn render(&self) {
        group!(self.logger, "Rendering.", {
            self.with_program(|_|{
                for binding in &self.uniforms {
                    binding.uniform.upload(&self.context,&binding.location);
                }

                let mode           = Context::TRIANGLE_STRIP;
                let first          = 0;
                let count          = self.surface.point.size()    as i32;
                let instance_count = self.surface.instance.size() as i32;

                self.stats.inc_draw_call_count();
                self.context.draw_arrays_instanced(mode,first,count,instance_count);
            });

        })
    }
}

impl Drop for Symbol {
    fn drop(&mut self) {
        self.stats.dec_symbol_count();
    }
}
