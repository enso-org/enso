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
use crate::display::symbol::material::shader;

use web_sys::WebGlVertexArrayObject;
use web_sys::WebGlProgram;


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
    context            : Context,
    vao                : Option<VertexArrayObject>,
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
            let vao             = default();
            Self{geometry,material,geometry_dirty,material_dirty,logger,context,vao}
        })
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.geometry_dirty.check() {
                self.geometry.update();
                self.geometry_dirty.unset();
            }
            if self.material_dirty.check() {
                self.material.update();
                self.init_vao();
               self.material_dirty.unset();
            }
        })
    }

    /// Creates a new VertexArrayObject, discovers all variable bindings from material to geometry,
    /// and initializes the VAO with the bindings.
    fn init_vao(&mut self) {
        self.vao = Some(VertexArrayObject::new(&self.context));
        self.with_program(|program|{
            let var_bindings = self.discover_variable_bindings();
            for (variable,opt_scope_type) in &var_bindings {
                if let Some(scope_type) = opt_scope_type {
                    let opt_scope = self.geometry.var_scope(*scope_type);
                    match opt_scope {
                        None => self.logger.error("Internal error. Invalid var scope."),
                        Some(scope) => {
                            let vtx_name = shader::builder::mk_vertex_name(&variable);
                            let location = self.context.get_attrib_location(program, &vtx_name);
                            if location < 0 {
                                self.logger.error(|| format!("Attribute '{}' not found.",vtx_name));
                            } else {
                                let location     = location as u32;
                                let buffer       = &scope.buffer(&variable).unwrap();
                                let is_instanced = scope_type == &geometry::ScopeType::Instance;
                                buffer.bind(webgl::Context::ARRAY_BUFFER);
                                buffer.vertex_attrib_pointer(location, is_instanced);
                            }
                        }
                    }
                }
            }
        });
    }

    /// For each variable from the material definition, looks up its position in geometry scopes.
    pub fn discover_variable_bindings(&self) -> Vec<(String,Option<geometry::ScopeType>)> {
        let variables = self.material.collect_variables();
        variables.into_iter().map(|variable| {
            let target = self.geometry.lookup_variable(&variable);
            if target.is_none() {
                let msg = || format!("Unable to bind variable '{}' to geometry buffer.", variable);
                self.logger.warning(msg);
            }
            (variable,target)
        }).collect()
    }

    /// Runs the provided function in a context of active program and active VAO. After the function
    /// is executed, both program and VAO are bound to None.
    pub fn with_program<F:FnOnce(&WebGlProgram) -> T,T>(&self, f:F) -> T {
        let program = self.material.program().as_ref().unwrap(); // FIXME
        self.context.use_program(Some(&program));
        let vao = self.vao.as_ref().unwrap(); // FIXME
        let out = vao.with(|| {
            f(program)
        });
        self.context.use_program(None);
        out
    }

    pub fn render(&self, camera:&Camera2D) {
        group!(self.logger, "Rendering.", {
            self.with_program(|program|{

                // FIXME: do proper uniform management
                let view_projection_location =
                    self.context.get_uniform_location(program, "view_projection");
                camera.update();
                self.context.set_uniform(&view_projection_location.unwrap(),
                    &camera.view_projection_matrix());

                let mode           = webgl::Context::TRIANGLE_STRIP;
                let first          = 0;
                let count          = self.geometry.scopes.point.size() as i32;
                let instance_count = self.geometry.scopes.instance.size() as i32;
                self.context.draw_arrays_instanced(mode,first,count,instance_count);
            });

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
