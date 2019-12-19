use crate::prelude::*;

use crate::backend::webgl;
use crate::backend::webgl::Context;
use crate::closure;
use crate::data::function::callback::*;
use crate::dirty;
use crate::dirty::traits::*;
use crate::display::symbol::geometry;
use crate::display::symbol::material;
use crate::promote;
use crate::promote_all;
use crate::promote_geometry_types;
use crate::promote_material_types;
use crate::system::web::Logger;
use crate::system::web::group;
use crate::display::symbol::buffer::item::ContextUniformOps;
use eval_tt::*;

use crate::display::symbol::buffer::IsBuffer;
use crate::display::symbol::display_object::Camera2D;

// ============
// === Mesh ===
// ============

// === Definition ===

/// Mesh is a `Geometry` with attached `Material`.
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Mesh<OnDirty> {
    #[shrinkwrap(main_field)]
    pub geometry       : Geometry      <OnDirty>,
    pub material       : Material      <OnDirty>,
    pub geometry_dirty : GeometryDirty <OnDirty>,
    pub material_dirty : MaterialDirty <OnDirty>,
    pub logger         : Logger,
    context            : Context
}

// === Types ===

pub type GeometryDirty<Callback> = dirty::SharedBool<Callback>;
pub type MaterialDirty<Callback> = dirty::SharedBool<Callback>;
promote_geometry_types!{ [OnGeometryChange] geometry }
promote_material_types!{ [OnGeometryChange] material }

#[macro_export]
macro_rules! promote_mesh_types { ($($args:tt)*) => {
    crate::promote_geometry_types! {$($args)*}
    crate::promote_material_types! {$($args)*}
    promote! {$($args)* [Mesh]}
};}

// === Callbacks ===

closure! {
fn geometry_on_change<C:Callback0>(dirty:GeometryDirty<C>) -> OnGeometryChange {
    || dirty.set()
}}

closure! {
fn material_on_change<C:Callback0>(dirty:MaterialDirty<C>) -> OnMaterialChange {
    || dirty.set()
}}

// === Implementation ===

impl<OnDirty:Callback0+Clone> Mesh<OnDirty> {
    /// Create new instance with the provided on-dirty callback.
    pub fn new(ctx:&Context, logger:Logger, on_dirty:OnDirty) -> Self {
        let init_logger = logger.clone();
        group!(init_logger, "Initializing.", {
            let context         = ctx.clone();
            let on_dirty2       = on_dirty.clone();
            let geo_logger      = logger.sub("geometry");
            let mat_logger      = logger.sub("material");
            let geo_dirt_logger = logger.sub("geometry_dirty");
            let mat_dirt_logger = logger.sub("material_dirty");
            let geometry_dirty  = GeometryDirty::new(geo_dirt_logger,on_dirty2);
            let material_dirty  = MaterialDirty::new(mat_dirt_logger,on_dirty);
            let geo_on_change   = geometry_on_change(geometry_dirty.clone_rc());
            let mat_on_change   = material_on_change(material_dirty.clone_rc());
            let material        = Material::new(ctx,mat_logger,mat_on_change);
            let geometry        = Geometry::new(ctx,geo_logger,geo_on_change);
            Self{geometry,material,geometry_dirty,material_dirty,logger,context}
        })
    }
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.geometry_dirty.check_all() {
                self.geometry.update();
                self.geometry_dirty.unset_all();
            }
        })
    }

    pub fn render(&self, camera:&Camera2D) {
        group!(self.logger, "Rendering.", {
            let vert_shader = webgl::compile_shader(
                &self.context,
                webgl::Context::VERTEX_SHADER,
                r#"#version 300 es

    precision highp float;
    precision highp int;

    in vec4 vertex_position;
    out vec4 _position;

    in mat4 vertex_model_matrix;
    out mat4 model_matrix;
    in vec2 vertex_bbox;
    out vec2 bbox;
    in float vertex_symbol_id;
    out float symbol_id;
    in float vertex_symbol_family_id;
    out float symbolFamilyID;
    in vec2 vertex_uv;
    out vec2 uv;

    out vec3 local;

    uniform mediump mat4 view_matrix;
    uniform mediump mat4 projection_matrix;
    uniform mediump mat4 view_projection_matrix;

    uniform mediump float zoom;

    void main() {
        model_matrix   = vertex_model_matrix;
        bbox           = vertex_bbox;
        symbol_id      = vertex_symbol_id;
        symbolFamilyID = vertex_symbol_family_id;
        uv             = vertex_uv;

        vec4 position2 = model_matrix * vec4(0.0,0.0,0.0,1.0);



        mat4 model_view_projection_matrix = view_projection_matrix * model_matrix;


        local       = vec3((uv - 0.5) * bbox, 0.0);
        gl_Position = model_view_projection_matrix * vec4(local,1.0);




        _position = view_projection_matrix * vec4(local,1.0);


//        mat4 model_view_matrix  = view_matrix * model_matrix;
//        vec4 eyeT               = model_view_matrix * vec4(local,1.0);
//        gl_Position             = projection_matrix * eyeT;
//        world                   = gl_Position.xyz;
//        eye                     = eyeT.xyz;
//        eye.z                   = -eye.z;

    }
"#,
            )
                .unwrap();
            let frag_shader = webgl::compile_shader(
                &self.context,
                webgl::Context::FRAGMENT_SHADER,
                r#"#version 300 es

    precision highp float;
    precision highp int;

    out vec4 out_color;
    in mat4 model_matrix;
    in vec4 _position;
    in vec2 uv;

    in vec3 local;

    uniform mediump mat4 view_matrix;
    uniform mediump mat4 projection_matrix;
    uniform mediump mat4 view_projection_matrix;

    void main() {
        out_color = vec4(uv.x, uv.y, 0.0, 1.0);
        out_color = vec4(local, 1.0);
        out_color = _position;
        out_color = vec4(1.0,1.0,1.0,1.0);
    }
"#,
            )
                .unwrap();

//            println!("{:?}", self.context.get_shader_info_log(&vert_shader));
//            println!("{:?}", self.context.get_shader_info_log(&frag_shader));


            let program =
                webgl::link_program(&self.context, &vert_shader, &frag_shader).unwrap();


            // === Rendering ==

            self.context.use_program(Some(&program));


            self.geometry.scopes.point.name_map.keys().for_each(|name| {
                let vtx_name = format!("vertex_{}",name);
                let location = self.context.get_attrib_location(&program, &vtx_name) as u32;
                // TODO handle missing location
                let buffer = &self.geometry.scopes.point.buffer(name).unwrap();
                buffer.bind(webgl::Context::ARRAY_BUFFER);
                buffer.vertex_attrib_pointer(location);
            });


//            println!("!! 3");

            let view_projection_matrix_location = self.context.get_uniform_location(&program, "view_projection_matrix");
//            println!("{:?}",view_projection_matrix_location);
//            println!("{:?}",self.context.get_error());


//            println!("----- {} , {}", Context::INVALID_VALUE, Context::INVALID_OPERATION);



            camera.update();
            self.context.set_uniform(&view_projection_matrix_location.unwrap(), &camera.view_projection_matrix());

//            println!("CAMERA");
//            println!("{:?}", camera.view_projection_matrix());


            let pts = self.geometry.scopes.point.size();
            self.context.draw_arrays(webgl::Context::TRIANGLE_STRIP, 0, pts as i32);

//            println!("{:?}",&self.geometry.scopes.point.buffers[0]);
        })
    }
}

// ==================
// === SharedMesh ===
// ==================

/// A shared version of `Mesh`.
#[derive(Shrinkwrap)]
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct SharedMesh<OnDirty> {
    pub raw: RefCell<Mesh<OnDirty>>
}

impl<OnDirty:Callback0+Clone> SharedMesh<OnDirty> {
    /// Create new instance with the provided on-dirty callback.
    pub fn new(context:&Context, logger:Logger, on_dirty:OnDirty) -> Self {
        let raw = RefCell::new(Mesh::new(context,logger, on_dirty));
        Self { raw }
    }
}
