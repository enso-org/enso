// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::control::callback;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::symbol::material::Material;
use crate::display::symbol::material::VarDecl;
use crate::display::symbol::ScopeType;
use crate::system::gpu::shader;
use crate::system::gpu::shader::compiler as shader_compiler;
use crate::system::gpu::shader::glsl;
use crate::system::gpu::Context;

use enso_shapely::shared;
use web_sys::WebGlProgram;


// ==============
// === Export ===
// ==============

#[warn(missing_docs)]
pub mod builder;



// ==================
// === VarBinding ===
// ==================

#[derive(Clone, Debug)]
pub struct VarBinding {
    pub name:  String,
    pub decl:  VarDecl,
    pub scope: Option<ScopeType>,
}

impl VarBinding {
    pub fn new<Name: Str>(name: Name, decl: VarDecl, scope: Option<ScopeType>) -> Self {
        let name = name.into();
        Self { name, decl, scope }
    }
}



// ==============
// === Shader ===
// ==============

pub type Dirty = dirty::SharedBool<Box<dyn FnMut()>>;

shared! { Shader
/// Shader keeps track of a shader and related WebGL Program.
#[derive(Debug)]
pub struct ShaderData {
    context             : Option<Context>,
    geometry_material   : Material,
    surface_material    : Material,
    program             : Rc<RefCell<Option<shader::Program>>>,
    shader_compiler_job : Option<shader_compiler::JobHandler>,
    dirty               : Dirty,
    stats               : Stats,
    profiler            : Option<profiler::Debug>,
}

impl {
    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    #[profile(Debug)]
    pub fn set_context(&mut self, context:Option<&Context>) {
        if context.is_some() {
            self.dirty.set();
            self.profiler.get_or_insert_with(new_profiler);
        }
        self.context = context.cloned();
    }

    pub fn program(&self) -> Option<shader::Program> {
        self.program.borrow().clone()
    }

    pub fn native_program(&self) -> Option<WebGlProgram> {
        self.program.borrow().as_ref().map(|t| t.native.clone())
    }

    #[profile(Detail)]
    pub fn set_geometry_material<M:Into<Material>>(&mut self, material:M) {
        self.geometry_material = material.into();
        self.dirty.set();
        self.profiler.get_or_insert_with(new_profiler);
    }

    #[profile(Detail)]
    pub fn set_material<M:Into<Material>>(&mut self, material:M) {
        self.surface_material = material.into();
        self.dirty.set();
        self.profiler.get_or_insert_with(new_profiler);
    }

    /// Creates new shader with attached callback.
    pub fn new<OnMut:callback::NoArgs>(stats:&Stats, on_mut:OnMut) -> Self {
        stats.inc_shader_count();
        let context = default();
        let geometry_material = default();
        let surface_material = default();
        let program = default();
        let shader_compiler_job = default();
        let dirty = Dirty::new(Box::new(on_mut));
        let stats = stats.clone_ref();
        let profiler = None;
        Self {
            context, geometry_material, surface_material, program, shader_compiler_job, dirty,
            stats, profiler
        }
    }

    /// Get the shader code in GLSL 310 format. The shader parameters will not be bound to any
    /// particular mesh and thus this code can be used for optimization purposes only.
    pub fn abstract_shader_code_in_glsl_310(&self) -> crate::system::gpu::shader::Code {
        let bindings = self.collect_variables().into_iter().map(|mut binding| {
            binding.scope = Some(ScopeType::Mesh(crate::display::symbol::geometry::primitive::mesh::ScopeType::Instance));
            binding
        }).collect_vec();
        self.gen_gpu_code(glsl::Version::V310, &bindings)
    }

    /// Generate the final GLSL code based on the provided bindings.
    pub fn gen_gpu_code(
            &self,
            glsl_version: glsl::Version,
            bindings:&[VarBinding]
        ) -> crate::system::gpu::shader::Code {
        debug_span!("Generating GPU code.").in_scope(|| {
            let mut shader_cfg = builder::ShaderConfig::new(glsl_version);
            let mut shader_builder = builder::ShaderBuilder::new(glsl_version);

            for binding in bindings {
                let name = &binding.name;
                let tp   = &binding.decl.tp;
                match binding.scope {
                    None => {
                        warn!("Default shader values are not implemented yet. \
                               This will cause visual glitches.");
                        shader_cfg.add_uniform(name,tp);
                    },
                    Some(scope_type) => match scope_type {
                        ScopeType::Symbol => shader_cfg.add_uniform   (name,tp),
                        ScopeType::Global => shader_cfg.add_uniform   (name,tp),
                        _                 => shader_cfg.add_attribute (name,tp),
                    }
                }
            }

            self.geometry_material.outputs().iter().for_each(|(name,decl)|{
                shader_cfg.add_shared_attribute(name,&decl.tp);
            });

            shader_cfg.add_output("color", glsl::PrimType::Vec4);
            self.surface_material.outputs().iter().for_each(|(name,decl)|{
                shader_cfg.add_output(name,&decl.tp);
            });

            let vertex_code = self.geometry_material.code().clone();
            let fragment_code = self.surface_material.code().clone();
            shader_builder.compute(&shader_cfg, vertex_code, fragment_code);
            shader_builder.build()
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update<F: 'static + Fn(&[VarBinding], &shader::Program)>
    (&mut self, bindings:Vec<VarBinding>, on_ready:F) {
        debug_span!("Updating.").in_scope(|| {
            if let Some(context) = &self.context {
                if self.dirty.check_all() {
                    self.stats.inc_shader_compile_count();
                    let code = self.gen_gpu_code(glsl::Version::V300, &bindings);

                    *self.program.borrow_mut() = None;
                    let program = self.program.clone_ref();
                    let profiler = self.profiler.take().unwrap_or_else(new_profiler);
                    let handler = context.shader_compiler.submit(code, profiler, move |prog| {
                        on_ready(&bindings, &prog);
                        *program.borrow_mut() = Some(prog);
                    });
                    self.cancel_previous_shader_compiler_job_and_use_new_one(handler);
                    self.dirty.unset_all();
                }
            }
        })
    }

    fn cancel_previous_shader_compiler_job_and_use_new_one
    (&mut self, handler: shader_compiler::JobHandler) {
        // Dropping the previous handler.
        self.shader_compiler_job = Some(handler);
    }

    /// Traverses the shader definition and collects all attribute names.
    pub fn collect_variables(&self) -> Vec<VarBinding> {
        let mut out = vec![];
        let vars = self.geometry_material.inputs().iter().chain(self.surface_material.inputs());
        for (name,decl) in vars {
            out.push(VarBinding::new(name.clone(), decl.clone(), None));
        }
        out
    }
}}

impl Drop for ShaderData {
    fn drop(&mut self) {
        self.stats.dec_shader_count();
    }
}

/// Create a profiler to track shader recompilation.
fn new_profiler() -> profiler::Debug {
    profiler::create_debug!("compile_shader")
}
