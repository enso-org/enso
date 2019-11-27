use crate::prelude::*;

use crate::closure;
use crate::data::function::callback::*;
use crate::dirty;
use crate::display::symbol::scope;
use crate::system::web::group;
use crate::system::web::Logger;
use crate::promote;
use crate::promote_all;
use crate::promote_scope_types;
use num_enum::IntoPrimitive;
use eval_tt::*;


// ================
// === Geometry ===
// ================

// === Definition ===

/// Geometry describes the shape of the display element. It consist of several
/// scopes containing sets of variables.
/// 
///   - Point Scope
///     A point is simply a point in space. Points are often assigned with such 
///     variables as 'position' or 'color'.
/// 
///   - Vertex Scope
///     A vertex is a reference to a point. Primitives use vertices to reference 
///     points. For example, the corners of a polygon, the center of a sphere, 
///     or a control vertex of a spline curve. Primitives can share points, 
///     while vertices are unique to a primitive.
/// 
///   - Primitive Scope
///     Primitives refer to a unit of geometry, lower-level than an object but
///     above points. There are several different types of primitives, including
///     polygon faces or Bezier/NURBS surfaces.
/// 
///   - Instance Scope
///     Instances are virtual copies of the same geometry. They share point,
///     vertex, and primitive variables.
/// 
///   - Object Scope
///     Object refers to the whole geometry with all of its instances.
/// 
///   - Global Scope
///     Global scope is shared by all objects and it contains some universal
///     global variables, like the current 'time' counter.
/// 
/// Each scope can contain named attributes which can be accessed from within 
/// materials. If the same name was defined in various scopes, it gets resolved
/// to the var defined in the most specific scope. For example, if var 'color'
/// was defined in both 'instance' and 'point' scope, the 'point' definition 
/// overlapps the other one. 
#[derive(Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Geometry<OnDirty> {
    #[shrinkwrap(main_field)]
    pub scopes       : Scopes      <OnDirty>,
    pub scopes_dirty : ScopesDirty <OnDirty>,
    pub logger       : Logger,
}

#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct Scopes<OnDirty> {
    pub point     : VarScope     <OnDirty>,
    pub vertex    : VarScope     <OnDirty>,
    pub primitive : VarScope     <OnDirty>,
    pub instance  : VarScope     <OnDirty>,
    pub object    : UniformScope <OnDirty>,
    pub global    : GlobalScope  <OnDirty>,
}

#[allow(non_camel_case_types)]
#[derive(Copy,Clone,Debug,IntoPrimitive)]
#[repr(u8)]
pub enum ScopesDirtyStatus {
    point, vertex, primitive, instance, object, global
}

impl From<ScopesDirtyStatus> for usize {
    fn from(t: ScopesDirtyStatus) -> Self {
        Into::<u8>::into(t).into()
    }
}

// === Types ===

pub type ScopesDirty  <F> = dirty::SharedEnum<u8,ScopesDirtyStatus, F>;
pub type VarScope     <F> = scope::Scope<ScopeOnChange<F>>;
pub type UniformScope <F> = scope::Scope<ScopeOnChange<F>>; // FIXME mock
pub type GlobalScope  <F> = scope::Scope<ScopeOnChange<F>>; // FIXME mock
promote_scope_types!{ [ScopeOnChange] scope }

#[macro_export]
macro_rules! promote_geometry_types { ($($args:tt)*) => {
    crate::promote_scope_types! { $($args)* }
    promote! {$($args)* [Geometry,Scopes,VarScope,UniformScope,GlobalScope]}
};}

// === Callbacks ===

closure! {
fn scope_on_change<C:Callback0>(dirty:ScopesDirty<C>, item:ScopesDirtyStatus) ->
    ScopeOnChange { || dirty.set_with((item,)) }
}

// === Implementation ===

macro_rules! update_scopes { ($self:ident . {$($name:ident),*}) => {$(
    if $self.scopes_dirty.check_for(&(ScopesDirtyStatus::$name,)) {
        $self.scopes.$name.update()
    }
)*}}

impl<OnDirty: Callback0> Geometry<OnDirty> {
    /// Creates new geometry with attached dirty callback.
    pub fn new(logger:Logger, on_dirty:OnDirty) -> Self {
        let scopes_logger = logger.sub("scopes_dirty");
        let scopes_dirty  = ScopesDirty::new(scopes_logger,on_dirty);
        let scopes        = group!(logger, "Initializing.", {
            macro_rules! new_scope { ($cls:ident { $($name:ident),* } ) => {$(
                let sub_logger = logger.sub(stringify!($name));
                let status_mod = ScopesDirtyStatus::$name;
                let scs_dirty  = scopes_dirty.clone_rc();
                let callback   = scope_on_change(scs_dirty, status_mod);
                let $name      = $cls::new(sub_logger, callback);
            )*}}
            new_scope!(VarScope {point,vertex,primitive,instance});
            new_scope!(VarScope {object});
            new_scope!(VarScope {global});
            Scopes {point,vertex,primitive,instance,object,global}
        });
        Self {scopes,scopes_dirty,logger}
    }
    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        group!(self.logger, "Updating.", {
            if self.scopes_dirty.check() {
                update_scopes!(self.{point,vertex,primitive,instance});
                update_scopes!(self.{object,global});
                self.scopes_dirty.unset()
            }
        })
    }
}