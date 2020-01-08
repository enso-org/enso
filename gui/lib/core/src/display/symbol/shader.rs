#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod builder;

use crate::prelude::*;

use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::data::function::callback::*;
use crate::debug::stats::Stats;
use crate::display::render::webgl::Context;
use crate::display::render::webgl::glsl;
use crate::display::render::webgl;
use crate::display::symbol::material::Material;
use crate::display::symbol::material::VarDecl;
use crate::display::symbol::shader;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::display::symbol::geometry::primitive::mesh::ScopeType;

use web_sys::WebGlProgram;



// ==================
// === VarBinding ===
// ==================

#[derive(Clone,Debug)]
pub struct VarBinding {
    pub name  : String,
    pub decl  : VarDecl,
    pub scope : Option<ScopeType>,
}

impl VarBinding {
    pub fn new<Name:Str>(name:Name, decl:VarDecl, scope:Option<ScopeType>) -> Self {
        let name = name.into();
        Self {name,decl,scope}
    }
}



// ==============
// === Shader ===
// ==============

// === Definition ===

/// Shader keeps track of a shader and related WebGL Program.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Shader<OnMut> {
    geometry_material : Material,
    material          : Material,
    program           : Option<WebGlProgram>,
    pub dirty         : Dirty <OnMut>,
    pub logger        : Logger,
    context           : Context,
    stats             : Stats,
}

// === Types ===

pub type Dirty <F> = dirty::SharedBool<F>;

#[macro_export]
/// Promote relevant types to parent scope. See `promote!` macro for more information.
macro_rules! promote_shader_types { ($($args:tt)*) => {
    promote! {$($args)* [Shader]}
};}


// === Implementation ===

impl<OnMut:Callback0> Shader<OnMut> {

    /// Creates new shader with attached callback.
    pub fn new(logger:Logger, stats:&Stats, context:&Context, on_mut:OnMut) -> Self {
        stats.inc_shader_count();
        let geometry_material = default();
        let material          = default();
        let program           = default();
        let dirty_logger      = logger.sub("dirty");
        let dirty             = Dirty::new(dirty_logger,on_mut);
        let context           = context.clone();
        let stats             = stats.clone_ref();
        dirty.set();
        Self {geometry_material,material,program,dirty,logger,context,stats}
    }

    // TODO: this is very work-in-progress function. It should be refactored in the next PR.
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self, bindings:&[VarBinding]) {
        group!(self.logger, "Updating.", {
            if self.dirty.check_all() {

                self.stats.inc_shader_compile_count();

                let mut shader_cfg     = shader::builder::ShaderConfig::new();
                let mut shader_builder = shader::builder::ShaderBuilder::new();

                for binding in bindings {
                    let name = &binding.name;
                    let tp   = &binding.decl.tp;
                    match binding.scope {
                        None => todo!(),
                        Some(scope_type) => match scope_type {
                            ScopeType::Instance => shader_cfg.add_attribute (name,tp),
                            ScopeType::Point    => shader_cfg.add_attribute (name,tp),
                            ScopeType::Global   => shader_cfg.add_uniform   (name,tp),
                            _ => todo!()
                        }
                    }
                }

                self.geometry_material.outputs().iter().for_each(|(name,decl)|{
                    shader_cfg.add_shared_attribute(name,&decl.tp);
                });

                shader_cfg.add_output("color", glsl::PrimType::Vec4);

                let vertex_code   = self.geometry_material.code().clone();
                let fragment_code = self.material.code().clone();
                shader_builder.compute(&shader_cfg,vertex_code,fragment_code);
                let shader      = shader_builder.build();
                let vert_shader = webgl::compile_vertex_shader  (&self.context,&shader.vertex);
                let frag_shader = webgl::compile_fragment_shader(&self.context,&shader.fragment);
                if let Err(ref err) = frag_shader {
                    self.logger.error(|| format!("{}", err))
                }

                let vert_shader = vert_shader.unwrap();
                let frag_shader = frag_shader.unwrap();
                let program     = webgl::link_program(&self.context,&vert_shader,&frag_shader);

                let program     = program.unwrap();
                self.program    = Some(program);
                self.dirty.unset_all();
            }
        })
    }

    /// Traverses the shader definition and collects all attribute names.
    pub fn collect_variables(&self) -> BTreeMap<String,VarDecl> {
        let geometry_material_inputs = self.geometry_material.inputs().clone();
        let surface_material_inputs  = self.material.inputs().clone();
        geometry_material_inputs.into_iter().chain(surface_material_inputs).collect()
    }
}

impl<OnMut> Drop for Shader<OnMut> {
    fn drop(&mut self) {
        self.stats.dec_shader_count();
    }
}


// === Getters ===

impl<OnMut> Shader<OnMut> {
    pub fn program(&self) -> &Option<WebGlProgram> {
        &self.program
    }
}


// === Setters ===

impl<OnMut:Callback0> Shader<OnMut> {
    pub fn set_geometry_material<M:Into<Material>>(&mut self, material:M) {
        self.geometry_material = material.into();
        self.dirty.set();
    }

    pub fn set_material<M:Into<Material>>(&mut self, material:M) {
        self.material = material.into();
        self.dirty.set();
    }
}
