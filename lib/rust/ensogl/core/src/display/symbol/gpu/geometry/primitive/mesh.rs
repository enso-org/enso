//! This module defines a [polygon mesh](https://en.wikipedia.org/wiki/Polygon_mesh).

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::control::callback;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::system::gpu::Context;

use enso_shapely::shared2;
use num_enum::IntoPrimitive;



// ===============
// === Exports ===
// ===============

/// Common data types.
pub mod types {
    pub use super::Mesh;
    pub use crate::system::gpu::types::*;
}
pub use types::*;


// --------------------------------------------------

/// Container for all scopes owned by a mesh.
#[derive(Debug)]
pub struct Scopes {
    /// Point Scope. A point is simply a point in space. Points are often assigned with such
    /// variables as 'position' or 'color'.
    pub point: AttributeScope,

    /// Vertex Scope. A vertex is a reference to a point. Primitives use vertices to reference
    /// points. For example, the corners of a polygon, the center of a sphere, or a control vertex
    /// of a spline curve. Primitives can share points, while vertices are unique to a primitive.
    pub vertex: AttributeScope,

    /// Primitive Scope. Primitives refer to a unit of geometry, lower-level than an object but
    /// above points. There are several different types of primitives, including polygon faces or
    /// Bezier/NURBS surfaces.
    pub primitive: AttributeScope,

    /// Instance Scope. Instances are virtual copies of the same geometry. They share point,
    /// vertex, and primitive variables.
    pub instance: AttributeScope,
}

/// A singleton for each of scope types.
#[derive(Copy, Clone, Debug, Display, IntoPrimitive, PartialEq, Eq)]
#[allow(missing_docs)]
#[repr(u8)]
pub enum ScopeType {
    Point,
    Vertex,
    Primitive,
    Instance,
}

impl From<ScopeType> for usize {
    fn from(t: ScopeType) -> Self {
        Into::<u8>::into(t).into()
    }
}


// === Types ===

/// Dirty flag remembering which scopes were mutated.
pub type ScopesDirty = dirty::SharedEnum<u8, ScopeType, Box<dyn FnMut()>>;


// === Implementation ===

macro_rules! update_scopes {
    ($self:ident . {$($name:ident),*} {$($uname:ident),*}) => {$(
        if $self.scopes_dirty.check(&ScopeType::$uname) {
            $self.scopes.$name.update()
        }
    )*}
}


// ============
// === Mesh ===
// ============

// === Definition ===

shared2! { Mesh
/// A polygon mesh is a collection of vertices, edges and faces that defines the shape of a
/// polyhedral object. Mesh describes the shape of the display element. It consists of several
/// scopes containing sets of variables. See the documentation of `Scopes` to learn more.
///
/// Please note, that there are other, higher-level scopes defined by other structures, including:
///
///   - Symbol Scope
///     Object refers to the whole geometry with all of its instances.
///
///   - Global Scope
///     Global scope is shared by all objects, and it contains some universal global variables, like
///     the current 'time' counter.
///
/// Each scope can contain named attributes which can be accessed from within materials. If the same
/// name was defined in various scopes, it gets resolved to the var defined in the most specific
/// scope. For example, if var 'color' was defined in both 'instance' and 'point' scope, the 'point'
/// definition overlaps the other one.
#[derive(Debug)]
pub struct MeshData {
    scopes       : Scopes,
    scopes_dirty : ScopesDirty,
    stats        : Stats,
}

impl {
    /// Creates new mesh with attached dirty callback.
    pub fn new<OnMut:callback::NoArgs>
    (stats:&Stats, on_mut:OnMut) -> Self {
        stats.inc_mesh_count();
        let stats         = stats.clone();
        let scopes_dirty  = ScopesDirty::new(Box::new(on_mut));
        let scopes        = debug_span!("Initializing.").in_scope(|| {
            macro_rules! new_scope { ({ $($name:ident),* } { $($uname:ident),* } ) => {$(
                let status_mod = ScopeType::$uname;
                let scs_dirty  = scopes_dirty.clone_ref();
                let callback   = move || {scs_dirty.set(status_mod)};
                let $name      = AttributeScope::new(&stats, callback);
            )*}}
            new_scope! ({point,vertex,primitive,instance}{Point,Vertex,Primitive,Instance});
            Scopes {point,vertex,primitive,instance}
        });
        Self {scopes, scopes_dirty, stats}
    }

    /// Point scope accessor.
    pub fn point_scope(&self) -> AttributeScope {
        self.scopes.point.clone_ref()
    }

    /// Vertex scope accessor.
    pub fn vertex_scope(&self) -> AttributeScope {
        self.scopes.vertex.clone_ref()
    }

    /// Primitive scope accessor.
    pub fn primitive_scope(&self) -> AttributeScope {
        self.scopes.primitive.clone_ref()
    }

    /// Instance scope accessor.
    pub fn instance_scope(&self) -> AttributeScope {
        self.scopes.instance.clone_ref()
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        debug_span!("Updating.").in_scope(|| {
            if self.scopes_dirty.check_all() {
                update_scopes!{
                    self.{point,vertex,primitive,instance}{Point,Vertex,Primitive,Instance}
                }
                self.scopes_dirty.unset_all()
            }
        })
    }

    /// Browses all scopes and finds where a variable was defined. Scopes are browsed in a
    /// hierarchical order. To learn more about the ordering see the documentation of `Mesh`.
    pub fn lookup_variable<S:Str>(&self, name:S) -> Option<ScopeType> {
        let name = name.as_ref();
        if      self.scopes.point     . contains(name) { Some(ScopeType::Point)     }
        else if self.scopes.vertex    . contains(name) { Some(ScopeType::Vertex)    }
        else if self.scopes.primitive . contains(name) { Some(ScopeType::Primitive) }
        else if self.scopes.instance  . contains(name) { Some(ScopeType::Instance)  }
        else {None}
    }

    /// Gets reference to scope based on the scope type.
    pub fn scope_by_type(&self, scope_type:ScopeType) -> AttributeScope {
        match scope_type {
            ScopeType::Point     => &self.scopes.point,
            ScopeType::Vertex    => &self.scopes.vertex,
            ScopeType::Primitive => &self.scopes.primitive,
            ScopeType::Instance  => &self.scopes.instance,
        }.clone_ref()
    }

    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    pub(crate) fn set_context(&self, context:Option<&Context>) {
        macro_rules! set_scope_context { ($($name:ident),*) => {
            $( self.scopes.$name.set_context(context); )*
        }}
        set_scope_context!(point,vertex,primitive,instance);
    }
}}

impl Drop for MeshData {
    fn drop(&mut self) {
        self.stats.dec_mesh_count();
    }
}
