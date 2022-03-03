#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod builder;

use crate::prelude::*;

use crate::control::callback;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::debug::stats::Stats;
use crate::display::symbol::material::Material;
use crate::display::symbol::material::VarDecl;
use crate::display::symbol::shader;
use crate::display::symbol::shader::ContextLossOrError;
use crate::display::symbol::ScopeType;
use crate::system::gpu::shader::*;
use crate::system::Context;

use web_sys::WebGlProgram;

use enso_shapely::shared;



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
    program           : Option<WebGlProgram>,
    dirty             : Dirty,
    logger            : Logger,
    stats             : Stats,
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
        self.program.clone()
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
    pub fn new<OnMut:callback::NoArgs>(logger:Logger, stats:&Stats, on_mut:OnMut) -> Self {
        stats.inc_shader_count();
        let context           = default();
        let geometry_material = default();
        let surface_material  = default();
        let program           = default();
        let dirty_logger      = Logger::new_sub(&logger,"dirty");
        let dirty             = Dirty::new(dirty_logger,Box::new(on_mut));
        let stats             = stats.clone_ref();
        Self {context,geometry_material,surface_material,program,dirty,logger,stats}
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self, bindings:&[VarBinding]) {
        debug!(self.logger, "Updating.", || {
            if let Some(context) = &self.context {
                if self.dirty.check_all() {

                    self.stats.inc_shader_compile_count();

                    let mut shader_cfg     = shader::builder::ShaderConfig::new();
                    let mut shader_builder = shader::builder::ShaderBuilder::new();

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

                    let vertex_code = self.geometry_material.code().clone();
                    let fragment_code = self.surface_material.code().clone();
                    shader_builder.compute(&shader_cfg,vertex_code,fragment_code);
                    let shader = shader_builder.build();
                    let program = compile_program(context,&shader.vertex,&shader.fragment);
                    match program {
                        Ok(program) => { self.program = Some(program);}
                        Err(ContextLossOrError::Error(err)) => error!(self.logger, "{err}"),
                        Err(ContextLossOrError::ContextLoss) => {}
                    }

                    self.dirty.unset_all();
                }
            }
        })
    }

    /// Traverses the shader definition and collects all attribute names.
    pub fn collect_variables(&self) -> BTreeMap<String,VarDecl> {
        let geometry_material_inputs = self.geometry_material.inputs().clone();
        let surface_material_inputs  = self.surface_material.inputs().clone();
        geometry_material_inputs.into_iter().chain(surface_material_inputs).collect()
    }
}}

impl Drop for ShaderData {
    fn drop(&mut self) {
        self.stats.dec_shader_count();
    }
}
