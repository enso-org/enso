pub mod shader;

use crate::prelude::*;

use crate::backend::webgl::Context;
use crate::data::function::callback::*;
use crate::dirty;
use crate::dirty::traits::*;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::backend::webgl;
use web_sys::WebGlProgram;


// ================
// === Geometry ===
// ================

// === Definition ===

/// Material keeps track of a shader and related WebGL Program.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Material<OnDirty> {
    program    : Option<WebGlProgram>,
    pub dirty  : Dirty <OnDirty>,
    pub logger : Logger,
    context    : Context
}

// === Types ===

pub type Dirty <F> = dirty::SharedBool<F>;

#[macro_export]
macro_rules! promote_material_types { ($($args:tt)*) => {
    promote! {$($args)* [Material]}
};}

// === Implementation ===

impl<OnDirty: Callback0> Material<OnDirty> {

    /// Creates new material with attached callback.
    pub fn new(context:&Context, logger:Logger, on_dirty:OnDirty) -> Self {
        let program      = default();
        let dirty_logger = logger.sub("dirty");
        let dirty        = Dirty::new(dirty_logger,on_dirty);
        let context      = context.clone();
        dirty.set();
        Self {program,dirty,logger,context}
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.dirty.check_all() {

                // FIXME: Hardcoded variables until we get proper shaders EDSL.

                let mut shader_cfg     = shader::builder::ShaderConfig::new();
                let mut shader_builder = shader::builder::ShaderBuilder::new();
                shader_cfg.insert_attribute        ("bbox"            , shader::glsl::PrimType::Vec2);
                shader_cfg.insert_attribute        ("uv"              , shader::glsl::PrimType::Vec2);
                shader_cfg.insert_attribute        ("transform"       , shader::glsl::PrimType::Mat4);
                shader_cfg.insert_shared_attribute ("local"           , shader::glsl::PrimType::Vec3);
                shader_cfg.insert_uniform          ("view_projection" , shader::glsl::PrimType::Mat4);
                shader_cfg.insert_output           ("color"           , shader::glsl::PrimType::Vec4);

                let vtx_template = shader::builder::CodeTemplete::from_main("
                mat4 model_view_projection = view_projection * transform;
                local                      = vec3((uv - 0.5) * bbox, 0.0);
                gl_Position                = model_view_projection * vec4(local,1.0);
                ");
                let frag_template = shader::builder::CodeTemplete::from_main("
                out_color = vec4(1.0,1.0,1.0,1.0);
                ");
                shader_builder.compute(&shader_cfg,vtx_template,frag_template);
                let shader      = shader_builder.build();
                let vert_shader = webgl::compile_vertex_shader  (&self.context,&shader.vertex);
                let frag_shader = webgl::compile_fragment_shader(&self.context,&shader.fragment);
                let vert_shader = vert_shader.unwrap();
                let frag_shader = frag_shader.unwrap();
                let program     = webgl::link_program(&self.context,&vert_shader,&frag_shader);
                let program     = program.unwrap();
                self.program    = Some(program);
                self.dirty.unset_all();
            }
        })
    }

    /// Traverses the material definition and collects all attribute names.
    pub fn collect_variables(&self) -> Vec<String> {
        // FIXME: Hardcoded.
        vec!["bbox".into(),"uv".into(),"transform".into()]
    }
}


// === Getters ===

impl<OnDirty> Material<OnDirty> {
    pub fn program(&self) -> &Option<WebGlProgram> {
        &self.program
    }
}
