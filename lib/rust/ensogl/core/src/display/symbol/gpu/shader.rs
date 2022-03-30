// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use crate::data::dirty::traits::*;
use crate::prelude::*;
use crate::system::gpu::shader::*;

use crate::control::callback;
use crate::data::dirty;
use crate::data::Fresh;
use crate::debug::stats::Stats;
use crate::display::symbol::material::Material;
use crate::display::symbol::material::VarDecl;
use crate::display::symbol::shader;
use crate::display::symbol::ScopeType;
use crate::system::Context;
use crate::system::gpu::shader::ShaderCode;
use crate::system::gpu::shader::Compiler as ShaderCompiler;

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

pub type Dirty = dirty::SharedBool<Box<dyn Fn()>>;

shared! { Shader
/// Shader keeps track of a shader and related WebGL Program.
#[derive(Debug)]
pub struct ShaderData {
    context           : Option<Context>,
    geometry_material : Material,
    surface_material  : Material,
    program           : Rc<Fresh<CloneCell<Option<WebGlProgram>>>>,
    shader            : Option<ShaderCode>,
    dirty             : Dirty,
    logger            : Logger,
    stats             : Stats,
    compiler          : Rc<RefCell<ShaderCompiler>>,
}

impl {
    /// Set the WebGL context. See the main architecture docs of this library to learn more.
    pub fn set_context(&mut self, context:Option<&Context>) {
        if context.is_some() {
            self.dirty.set();
        }
        self.context = context.cloned();
    }

    pub fn program(&self) -> Option<WebGlProgram> {
        self.program.get()
    }

    pub fn is_ready(&self) -> bool {
        self.program.is_fresh()
    }

    pub fn set_geometry_material<M:Into<Material>>(&mut self, material:M) {
        self.geometry_material = material.into();
        self.dirty.set();
    }

    pub fn set_material<M:Into<Material>>(&mut self, material:M) {
        self.surface_material = material.into();
        self.dirty.set();
    }

    /// Creates new shader with attached callback.
    pub fn new<OnMut: callback::NoArgs>(
        logger: Logger,
        stats: &Stats,
        on_mut: OnMut,
        compiler: Rc<RefCell<ShaderCompiler>>
    ) -> Self {
        stats.inc_shader_count();
        let context           = default();
        let geometry_material = default();
        let surface_material  = default();
        let program           = default();
        let shader            = default();
        let dirty_logger      = Logger::new_sub(&logger,"dirty");
        let dirty             = Dirty::new(dirty_logger,Box::new(on_mut));
        let stats             = stats.clone_ref();
        Self {context,geometry_material,surface_material,program,shader,dirty,logger,stats,compiler}
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update<F: FnOnce() + 'static>(&mut self, bindings:&[VarBinding], on_ready: F) {
        debug!(self.logger, "Updating.", || {
            if let Some(_context) = &self.context {
                if self.dirty.check_all() {
                    self.stats.inc_shader_compile_count();
                    let shader_cfg = self.make_cfg(bindings);
                    let mut shader_builder = shader::builder::ShaderBuilder::new();
                    let vertex_code = self.geometry_material.code().clone();
                    let fragment_code = self.surface_material.code().clone();
                    shader_builder.compute(&shader_cfg,vertex_code,fragment_code);
                    let shader = shader_builder.build();
                    self.shader = Some(shader.clone());
                    let logger = self.logger.clone_ref();
                    let program_cell = self.program.setter();
                    self.compiler.borrow_mut().compile(shader, Box::new(move |program| {
                        match program {
                            Ok(program) => {
                                if program_cell.set(Some(program)).is_ok() {
                                    on_ready();
                                }
                            }
                            Err(err) => error!(logger, "{err}"),
                        }
                    }));
                    self.dirty.unset_all();
                }
            }
        })
    }

    fn make_cfg(&self, bindings: &[VarBinding]) -> shader::builder::ShaderConfig {
        let mut shader_cfg     = shader::builder::ShaderConfig::new();
        for binding in bindings {
            let name = &binding.name;
            let tp   = &binding.decl.tp;
            match binding.scope {
                None => {
                    warning!(self.logger,"[TODO] Default shader values are not \
                        implemented. This will cause visual glitches.");
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
        shader_cfg
    }

    /// Traverses the shader definition and collects all attribute names.
    pub fn collect_variables(&self) -> BTreeMap<String,VarDecl> {
        let geometry_material_inputs = self.geometry_material.inputs().clone();
        let surface_material_inputs  = self.surface_material.inputs().clone();
        geometry_material_inputs.into_iter().chain(surface_material_inputs).collect()
    }

    /// Get the generated shader, if it was already generated.
    pub fn shader(&self) -> Option<ShaderCode> {
        self.shader.clone()
    }
}}

impl Drop for ShaderData {
    fn drop(&mut self) {
        self.stats.dec_shader_count();
    }
}
