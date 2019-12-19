use crate::prelude::*;

use crate::backend::webgl::Context;
use crate::closure;
use crate::data::function::callback::*;
use crate::data::opt_vec::OptVec;
use crate::dirty;
use crate::dirty::traits::*;
use crate::display::symbol::mesh;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::promote;
use crate::promote_all;
use crate::promote_mesh_types;
use eval_tt::*;
use crate::display::symbol::display_object::Camera2D;


// ====================
// === MeshRegistry ===
// ====================

// === Definition ===

/// Registry for all the created meshes.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct MeshRegistry <OnDirty> {
    pub meshes     : OptVec<Mesh<OnDirty>>,
    pub mesh_dirty : MeshDirty<OnDirty>,
    pub logger     : Logger,
    context        : Context
}

// === Types ===

pub type MeshID              = usize;
pub type MeshDirty <OnDirty> = dirty::SharedSet<MeshID, OnDirty>;
promote_mesh_types!{ [OnMeshChange] mesh }

#[macro_export]
macro_rules! promote_mesh_registry_types { ($($args:tt)*) => {
    crate::promote_mesh_types! { $($args)* }
    promote! { $($args)* [MeshRegistry] }
};}

// === Callbacks ===

closure! {
fn mesh_on_change<C:Callback0> (dirty:MeshDirty<C>, ix:MeshID) -> OnMeshChange {
    || dirty.set(ix)
}}

// === Implementation ===

impl<OnDirty:Callback0> MeshRegistry<OnDirty> {
    /// Create new instance with the provided on-dirty callback.
    pub fn new(context:&Context, logger:Logger, on_dirty:OnDirty) -> Self {
        logger.info("Initializing.");
        let mesh_logger = logger.sub("mesh_dirty");
        let mesh_dirty  = MeshDirty::new(mesh_logger,on_dirty);
        let meshes      = default();
        let context     = context.clone();
        Self {meshes,mesh_dirty,logger,context}
    }
    /// Creates a new mesh instance.
    pub fn new_mesh(&mut self) -> MeshID {
        let mesh_dirty = self.mesh_dirty.clone();
        let logger     = &self.logger;
        let context    = &self.context;
        self.meshes.insert_with_ix(|ix| {
            let on_dirty   = mesh_on_change(mesh_dirty, ix);
            let logger     = logger.sub(format!("mesh{}",ix));
            Mesh::new(context,logger,on_dirty)
        })
    }
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            for mesh_id in self.mesh_dirty.iter() {
                self.meshes[*mesh_id].update()
            }
            self.mesh_dirty.unset_all();
        })
    }

    pub fn render(&self, camera:&Camera2D) {
        group!(self.logger, "Rendering.", {
            for mesh in &self.meshes {
                mesh.render(camera);
            }
        })
    }
}

impl<OnDirty> Index<usize> for MeshRegistry<OnDirty> {
    type Output = Mesh<OnDirty>;
    fn index(&self, ix:usize) -> &Self::Output {
        self.meshes.index(ix)
    }
}

impl<OnDirty> IndexMut<usize> for MeshRegistry<OnDirty> {
    fn index_mut(&mut self, ix:usize) -> &mut Self::Output {
        self.meshes.index_mut(ix)
    }
}
