pub mod buffer;
pub mod scope;

use crate::prelude::*;

use crate::display::render::webgl::Context;
use crate::closure;
use crate::data::function::callback::*;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::promote_all;
use crate::promote_scope_types;
use crate::promote;
use crate::system::web::group;
use crate::system::web::Logger;
use eval_tt::*;
use num_enum::IntoPrimitive;


// ============
// === Mesh ===
// ============

// === Definition ===

/// A polygon mesh is a collection of vertices, edges and faces that defines the shape of a
/// polyhedral object. Mesh describes the shape of the display element. It consist of several
/// scopes containing sets of variables.
///
///   - Point Scope
///     A point is simply a point in space. Points are often assigned with such variables as
///     'position' or 'color'.
///
///   - Vertex Scope
///     A vertex is a reference to a point. Primitives use vertices to reference points. For
///     example, the corners of a polygon, the center of a sphere, or a control vertex of a spline
///     curve. Primitives can share points, while vertices are unique to a primitive.
///
///   - Primitive Scope
///     Primitives refer to a unit of geometry, lower-level than an object but above points. There
///     are several different types of primitives, including polygon faces or Bezier/NURBS surfaces.
///
///   - Instance Scope
///     Instances are virtual copies of the same geometry. They share point, vertex, and primitive
///     variables.
///
///   - Object Scope
///     Object refers to the whole geometry with all of its instances.
///
///   - Global Scope
///     Global scope is shared by all objects and it contains some universal global variables, like
///     the current 'time' counter.
///
/// Each scope can contain named attributes which can be accessed from within materials. If the same
/// name was defined in various scopes, it gets resolved to the var defined in the most specific
/// scope. For example, if var 'color' was defined in both 'instance' and 'point' scope, the 'point'
/// definition overlapps the other one.
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Mesh<OnMut> {
    #[shrinkwrap(main_field)]
    pub scopes       : Scopes      <OnMut>,
    pub scopes_dirty : ScopesDirty <OnMut>,
    pub logger       : Logger,
    context          : Context
}

#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Scopes<OnMut> {
    pub point     : VarScope     <OnMut>,
    pub vertex    : VarScope     <OnMut>,
    pub primitive : VarScope     <OnMut>,
    pub instance  : VarScope     <OnMut>,
    pub object    : UniformScope <OnMut>,
    pub global    : GlobalScope  <OnMut>,
}

#[derive(Copy,Clone,Debug,IntoPrimitive,PartialEq)]
#[repr(u8)]
pub enum ScopeType {
    Point, Vertex, Primitive, Instance, Object, Global
}

impl From<ScopeType> for usize {
    fn from(t: ScopeType) -> Self {
        Into::<u8>::into(t).into()
    }
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{:?}",self)
    }
}


// === Types ===

pub type ScopesDirty  <F> = dirty::SharedEnum<u8,ScopeType,F>;
pub type VarScope     <F> = scope::Scope<ScopeOnChange<F>>;
pub type UniformScope <F> = scope::Scope<ScopeOnChange<F>>; // FIXME mock
pub type GlobalScope  <F> = scope::Scope<ScopeOnChange<F>>; // FIXME mock
promote_scope_types!{ [ScopeOnChange] scope }

#[macro_export]
macro_rules! promote_mesh_types { ($($args:tt)*) => {
    crate::promote_scope_types! { $($args)* }
    promote! {$($args)* [Mesh,Scopes,VarScope,UniformScope,GlobalScope]}
};}


// === Callbacks ===

closure! {
fn scope_on_change<C:Callback0>(dirty:ScopesDirty<C>, item:ScopeType) -> ScopeOnChange {
    || dirty.set(item)
}}


// === Implementation ===

macro_rules! update_scopes { ($self:ident . {$($name:ident),*} {$($uname:ident),*}) => {$(
    if $self.scopes_dirty.check(&ScopeType::$uname) {
        $self.scopes.$name.update()
    }
)*}}

impl<OnMut: Callback0> Mesh<OnMut> {

    /// Creates new mesh with attached dirty callback.
    pub fn new(context:&Context, logger:Logger, on_mut:OnMut) -> Self {
        let scopes_logger = logger.sub("scopes_dirty");
        let scopes_dirty  = ScopesDirty::new(scopes_logger,on_mut);
        let context       = context.clone();
        let scopes        = group!(logger, "Initializing.", {
            macro_rules! new_scope { ($cls:ident { $($name:ident),* } { $($uname:ident),* } ) => {$(
                let sub_logger = logger.sub(stringify!($name));
                let status_mod = ScopeType::$uname;
                let scs_dirty  = scopes_dirty.clone_ref();
                let callback   = scope_on_change(scs_dirty, status_mod);
                let $name      = $cls::new(&context,sub_logger,callback);
            )*}}
            new_scope!(VarScope {point,vertex,primitive,instance}{Point,Vertex,Primitive,Instance});
            new_scope!(VarScope {object}{Object});
            new_scope!(VarScope {global}{Global});
            Scopes {point,vertex,primitive,instance,object,global}
        });
        Self {context,scopes,scopes_dirty,logger}
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.scopes_dirty.check_all() {
                update_scopes!(self.{point,vertex,primitive,instance}
                                    {Point,Vertex,Primitive,Instance});
                update_scopes!(self.{object,global}{Object,Global});
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
        else if self.scopes.object    . contains(name) { Some(ScopeType::Object)    }
        else if self.scopes.global    . contains(name) { Some(ScopeType::Global)    }
        else {None}
    }

    /// Gets reference to scope based on the scope type.
    pub fn var_scope(&self, scope_type:ScopeType) -> Option<&VarScope<OnMut>> {
        match scope_type {
            ScopeType::Point     => Some(&self.scopes.point),
            ScopeType::Vertex    => Some(&self.scopes.vertex),
            ScopeType::Primitive => Some(&self.scopes.primitive),
            ScopeType::Instance  => Some(&self.scopes.instance),
            _                    => None
        }
    }
}
