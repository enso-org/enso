#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod transform;

use crate::prelude::*;

use crate::closure;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use data::opt_vec::OptVec;

use nalgebra::Vector3;
use nalgebra::Matrix4;
use transform::CachedTransform;



// ==================
// === ParentBind ===
// ==================

/// Description of parent-child relation. It contains reference to parent node and information
/// about the child index there. It is used when a child is reconnected to different parent to
/// update the old parent with the information that the child was removed.
#[derive(Clone,Debug)]
pub struct ParentBind {
    pub parent : DisplayObjectData,
    pub index  : usize
}

impl ParentBind {
    pub fn dispose(&self) {
        self.parent.remove_child_by_index(self.index);
    }
}


use crate::control::callback::DynEventDispatcher;
use crate::control::callback::DynEvent;



// =====================================
// === HierarchicalObjectDescription ===
// =====================================

/// Hierarchical description of objects. Each object contains binding to its parent and to its
/// children. This is the most underlying structure in the display object hierarchy.
#[derive(Debug)]
pub struct HierarchicalObjectData {
    pub parent_bind : Option<ParentBind>,
    pub children    : OptVec<DisplayObjectData>,
    pub dispatcher  : DynEventDispatcher,
    pub logger      : Logger,
}


// === Public API ===

impl HierarchicalObjectData {
    pub fn new(logger:Logger) -> Self {
        let parent_bind = default();
        let children    = default();
        let dispatcher  = default();
        Self {parent_bind,children,dispatcher,logger}
    }

    pub fn dispatch(&mut self, event:&DynEvent) {
        self.dispatcher.dispatch(event);
        self.parent_bind.iter().for_each(|bind| bind.parent.dispatch(event));
    }

    pub fn child_count(&self) -> usize {
        self.children.len()
    }
}


// === Private API ===

impl HierarchicalObjectData {
    fn take_parent_bind(&mut self) -> Option<ParentBind> {
        self.parent_bind.take()
    }

    fn set_parent_bind(&mut self, bind:ParentBind) {
        self.parent_bind = Some(bind);
    }

    fn register_child<T:DisplayObject>(&mut self, child:T) -> usize {
        let child = child.display_object();
        self.children.insert(child)
    }

    pub fn remove_child_by_index(&mut self, index:usize) {
        let opt_child = self.children.remove(index);
        opt_child.for_each(|t| t.raw_unset_parent());
    }
}


// === Getters ===

impl DisplayObjectDataMut {
    pub fn parent(&self) -> Option<&DisplayObjectData> {
        self.parent_bind.as_ref().map(|ref t| &t.parent)
    }
}



// ======================================
// === LazyTransformObjectDescription ===
// ======================================

/// Internal representation of `ObjectData`. See it's documentation for more information.
#[derive(Derivative,Shrinkwrap)]
#[shrinkwrap(mutable)]
#[derivative(Debug)]
pub struct DisplayObjectDataMut {
    #[shrinkwrap(main_field)]
    pub wrapped          : HierarchicalObjectData,
    pub transform        : CachedTransform<Option<OnChange>>,
    pub child_dirty      : ChildDirty,
    pub new_parent_dirty : NewParentDirty,
    #[derivative(Debug="ignore")]
    pub on_updated: Option<Box<dyn Fn(&DisplayObjectDataMut)>>,
    #[derivative(Debug="ignore")]
    pub on_render: Option<Box<dyn Fn()>>,
}


// === Types ===

pub type ChildDirty     = dirty::SharedSet<usize,Option<OnChange>>;
pub type NewParentDirty = dirty::SharedBool<()>;
pub type TransformDirty = dirty::SharedBool<Option<OnChange>>;


// === Callbacks ===

closure! {
fn fn_on_change(dirty:ChildDirty, ix:usize) -> OnChange { || dirty.set(ix) }
}


// === API ===

impl DisplayObjectDataMut {
    pub fn new(logger:Logger) -> Self {
        let transform        = CachedTransform :: new(logger.sub("transform")       ,None);
        let child_dirty      = ChildDirty      :: new(logger.sub("child_dirty")     ,None);
        let new_parent_dirty = NewParentDirty  :: new(logger.sub("new_parent_dirty"),());
        let wrapped          = HierarchicalObjectData::new(logger);
        let on_updated       = None;
        let on_render        = None;
        Self {wrapped,transform,child_dirty,new_parent_dirty,on_updated,on_render}
    }

    pub fn render(&self) {
        if let Some(f) = &self.on_render { f() }
        self.children.iter().for_each(|child| {
            child.render();
        });
    }

    pub fn update(&mut self) {
        let origin0 = Matrix4::identity();
        self.update_with(&origin0,false)
    }

    pub fn update_with(&mut self, parent_origin:&Matrix4<f32>, force:bool) {
        let use_origin = force || self.new_parent_dirty.check();
        let new_origin = use_origin.as_some(parent_origin);
        let msg        = match new_origin {
            Some(_) => "Update with new parent origin.",
            None    => "Update with old parent origin."
        };
        group!(self.logger, "{msg}", {
            let origin_changed = self.transform.update(new_origin);
            let origin         = &self.transform.matrix;
            if origin_changed {
                self.logger.info("Self origin changed.");
                if !self.children.is_empty() {
                    group!(self.logger, "Updating all children.", {
                        self.children.iter().for_each(|child| {
                            child.update_with(origin,true);
                        });
                    })
                }
            } else {
                self.logger.info("Self origin did not change.");
                if self.child_dirty.check_all() {
                    group!(self.logger, "Updating dirty children.", {
                        self.child_dirty.take().iter().for_each(|ix| {
                            self.children[*ix].update_with(origin,false)
                        });
                    })
                }
            }
            self.child_dirty.unset_all();
        });
        self.new_parent_dirty.unset();
        if let Some(f) = &self.on_updated { f(self) }
    }
}


// === Private API ===

impl DisplayObjectDataMut {
    fn register_child<T:DisplayObject>(&mut self, child:T) -> usize {
        let index = self.wrapped.register_child(child);
        self.child_dirty.set(index);
        index
    }

    fn remove_child_by_index(&mut self, index:usize) {
        self.wrapped.remove_child_by_index(index);
        self.child_dirty.unset(&index);
    }

    fn raw_unset_parent(&mut self) {
        self.logger.info("Removing parent bind.");
        self.transform.dirty.set_callback(None);
        self.child_dirty.set_callback(None);
        self.new_parent_dirty.set();
        self.wrapped.take_parent_bind();
    }

    fn set_parent_bind(&mut self, bind:ParentBind) {
        self.logger.info("Adding new parent bind.");
        let dirty  = bind.parent.rc.borrow().child_dirty.clone_ref();
        let index  = bind.index;
        let on_mut = move || {dirty.set(index)};
        self.transform.dirty.set_callback(Some(Box::new(on_mut.clone())));
        self.child_dirty.set_callback(Some(Box::new(on_mut)));
        self.new_parent_dirty.set();
        self.wrapped.set_parent_bind(bind);
    }
}


// === Getters ===

impl DisplayObjectDataMut {
    pub fn global_position(&self) -> Vector3<f32> {
        self.transform.global_position()
    }

    pub fn position(&self) -> Vector3<f32> {
        self.transform.position()
    }

    pub fn scale(&self) -> Vector3<f32> {
        self.transform.scale()
    }

    pub fn rotation(&self) -> Vector3<f32> {
        self.transform.rotation()
    }

    pub fn matrix(&self) -> Matrix4<f32> {
        self.transform.matrix()
    }
}


// === Setters ===

impl DisplayObjectDataMut {
    pub fn position_mut(&mut self) -> &mut Vector3<f32> {
        self.transform.position_mut()
    }

    pub fn scale_mut(&mut self) -> &mut Vector3<f32> {
        self.transform.scale_mut()
    }

    pub fn rotation_mut(&mut self) -> &mut Vector3<f32> {
        self.transform.rotation_mut()
    }

    pub fn set_position(&mut self, t:Vector3<f32>) {
        self.transform.set_position(t);
    }

    pub fn set_scale(&mut self, t:Vector3<f32>) {
        self.transform.set_scale(t);
    }

    pub fn set_rotation(&mut self, t:Vector3<f32>) {
        self.transform.set_rotation(t);
    }

    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_position(f)
    }

    pub fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_rotation(f)
    }

    pub fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&mut self, f:F) {
        self.transform.mod_scale(f)
    }

    pub fn set_on_updated<F:Fn(&DisplayObjectDataMut)+'static>(&mut self, f:F) {
        self.on_updated = Some(Box::new(f))
    }

    pub fn set_on_render<F:Fn()+'static>(&mut self, f:F) {
        self.on_render = Some(Box::new(f))
    }
}



// ================================
// === DisplayObjectDescription ===
// ================================

/// A hierarchical representation of object containing a position, a scale and a rotation.
///
/// # Safety
/// Please note that you will get runtime crash when running the `update` function if your object
/// hierarchy forms a loop, for example, `obj2` is child of `obj1`, while `obj1` is child of `obj2`.
/// It is not easy to discover such situations, but maybe it will be worth to add some additional
/// safety on top of that in the future.
#[derive(Clone,Debug)]
pub struct DisplayObjectData {
    rc: Rc<RefCell<DisplayObjectDataMut>>,
}


// === Public API ==

impl DisplayObjectData {
    /// Creates a new object instance.
    pub fn new<L:Into<Logger>>(logger:L) -> Self {
        let data = DisplayObjectDataMut::new(logger.into());
        let rc   = Rc::new(RefCell::new(data));
        Self {rc}
    }

    pub fn dispatch(&self, event:&DynEvent) {
        self.rc.borrow_mut().dispatch(event)
    }

    /// Recompute the transformation matrix of this object and update all of its dirty children.
    pub fn update(&self) {
        self.rc.borrow_mut().update();
    }

    /// Adds a new `DisplayObject` as a child to the current one.
    pub fn add_child<T:DisplayObject>(&self, child:T) {
        self.clone_ref().add_child_take(child);
    }

    /// Adds a new `DisplayObject` as a child to the current one.
    pub fn add_child_take<T:DisplayObject>(self, child:T) {
        self.rc.borrow().logger.info("Adding new child.");
        let child = child.display_object();
        child.unset_parent();
        let index = self.rc.borrow_mut().register_child(&child);
        self.rc.borrow().logger.info(|| format!("Child index is {}.", index));
        let parent_bind = ParentBind {parent:self,index};
        child.set_parent_bind(parent_bind);
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    pub fn remove_child<T:DisplayObject>(&self, child:T) {
        let child = child.display_object();
        if self.has_child(&child) {
            child.unset_parent()
        }
    }

    /// Replaces the parent binding with a new parent.
    pub fn set_parent<T:DisplayObject>(&self, parent:T) {
        parent.display_object().add_child_take(self);
    }

    /// Removes the current parent binding.
    pub fn unset_parent(&self) {
        self.take_parent_bind().for_each(|t| t.dispose());
    }

    /// Checks if the provided object is child of the current one.
    pub fn has_child<T:DisplayObject>(&self, child:T) -> bool {
        self.child_index(child).is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T:DisplayObject>(&self, child:T) -> Option<usize> {
        let child = child.display_object();
        child.parent_bind().and_then(|bind| {
            if self == &bind.parent { Some(bind.index) } else { None }
        })
    }

    /// Returns the number of children of this node.
    pub fn child_count(&self) -> usize {
        self.rc.borrow().child_count()
    }

    /// Renders the object to the screen.
    pub fn render(&self) {
        self.rc.borrow().render()
    }
}


// === Private API ===

impl DisplayObjectData {

    /// Updates object transformations by providing a new origin location. See docs of `update` to
    /// learn more.
    fn update_with(&self, new_origin:&Matrix4<f32>, force:bool) {
        self.rc.borrow_mut().update_with(new_origin,force);
    }

    /// Removes and returns the parent bind. Please note that the parent is not updated.
    fn take_parent_bind(&self) -> Option<ParentBind> {
        self.rc.borrow_mut().take_parent_bind()
    }

    /// Gets a reference to a parent bind description, if exists.
    fn parent_bind(&self) -> Option<ParentBind> {
        self.rc.borrow().parent_bind.clone()
    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    fn set_parent_bind(&self, parent:ParentBind) {
        self.rc.borrow_mut().set_parent_bind(parent);
    }

    /// Removes the binding to the parent object.
    fn raw_unset_parent(&self) {
        self.rc.borrow_mut().raw_unset_parent();
    }

    /// Removes child by a given index. Does nothing if the index was incorrect. Please use the
    /// `remove_child` method unless you are 100% sure that the index is correct.
    fn remove_child_by_index(&self, index:usize) {
        self.rc.borrow_mut().remove_child_by_index(index);
    }
}


// === Getters ===

impl DisplayObjectData {
    pub fn parent(&self) -> Option<DisplayObjectData> {
        self.rc.borrow().parent().map(|t| t.clone_ref())
    }

    pub fn index(&self) -> Option<usize> {
        self.parent_bind().map(|t| t.index)
    }

    pub fn global_position(&self) -> Vector3<f32> {
        self.rc.borrow().global_position()
    }

    pub fn position(&self) -> Vector3<f32> {
        self.rc.borrow().position()
    }

    pub fn scale(&self) -> Vector3<f32> {
        self.rc.borrow().scale()
    }

    pub fn rotation(&self) -> Vector3<f32> {
        self.rc.borrow().rotation()
    }

    pub fn matrix(&self) -> Matrix4<f32> {
        self.rc.borrow().matrix()
    }
}


// === Setters ===

impl DisplayObjectData {
    pub fn set_position(&self, t:Vector3<f32>) {
        self.rc.borrow_mut().set_position(t);
    }

    pub fn set_scale(&self, t:Vector3<f32>) {
        self.rc.borrow_mut().set_scale(t);
    }

    pub fn set_rotation(&self, t:Vector3<f32>) {
        self.rc.borrow_mut().set_rotation(t);
    }

    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_position(f)
    }

    pub fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_rotation(f)
    }

    pub fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.rc.borrow_mut().mod_scale(f)
    }

    pub fn set_on_updated<F:Fn(&DisplayObjectDataMut)+'static>(&self, f:F) {
        self.rc.borrow_mut().set_on_updated(f)
    }

    pub fn set_on_render<F:Fn()+'static>(&self, f:F) {
        self.rc.borrow_mut().set_on_render(f)
    }
}


// === Instances ===

impl From<&DisplayObjectData> for DisplayObjectData {
    fn from(t:&DisplayObjectData) -> Self { t.clone_ref() }
}

impl From<Rc<RefCell<DisplayObjectDataMut>>> for DisplayObjectData {
    fn from(rc: Rc<RefCell<DisplayObjectDataMut>>) -> Self {
        Self {rc}
    }
}

impl PartialEq for DisplayObjectData {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc,&other.rc)
    }
}

impl CloneRef for DisplayObjectData {}



// =====================
// === DisplayObject ===
// =====================

pub trait DisplayObject: Into<DisplayObjectData> {
    fn display_object(self) -> DisplayObjectData {
        self.into()
    }
}

impl<T:Into<DisplayObjectData>> DisplayObject for T {}


pub trait DisplayObjectOps<'t>
where &'t Self:DisplayObject, Self:'t {
    fn add_child<T:DisplayObject>(&'t self, child:T) {
        self.display_object().add_child_take(child);
    }

    fn dispatch(&'t self, event:&DynEvent) {
        self.display_object().dispatch(event)
    }
}

impl<'t,T> DisplayObjectOps<'t> for T
where T:'t, &'t T:DisplayObject {}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::PI;

    #[test]
    fn hierarchy_test() {
        let obj1 = DisplayObjectData::new(Logger::new("obj1"));
        let obj2 = DisplayObjectData::new(Logger::new("obj2"));
        let obj3 = DisplayObjectData::new(Logger::new("obj3"));
        obj1.add_child(&obj2);
        assert_eq!(obj2.index(),Some(0));

        obj1.add_child(&obj2);
        assert_eq!(obj2.index(),Some(0));

        obj1.add_child(&obj3);
        assert_eq!(obj3.index(),Some(1));

        obj1.remove_child(&obj3);
        assert_eq!(obj3.index(),None);
    }

    #[test]
    fn transformation_test() {
        let obj1 = DisplayObjectData::new(Logger::new("obj1"));
        let obj2 = DisplayObjectData::new(Logger::new("obj2"));
        let obj3 = DisplayObjectData::new(Logger::new("obj3"));
        assert_eq!(obj1.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj1.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(0.0,0.0,0.0));

        obj1.mod_position(|t| t.x += 7.0);
        obj1.add_child(&obj2);
        obj2.add_child(&obj3);
        assert_eq!(obj1.position()        , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj1.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(0.0,0.0,0.0));

        obj1.update();
        assert_eq!(obj1.position()        , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(7.0,0.0,0.0));

        obj2.mod_position(|t| t.y += 5.0);
        obj1.update();
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(7.0,5.0,0.0));

        obj3.mod_position(|t| t.x += 1.0);
        obj1.update();
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(8.0,5.0,0.0));

        obj2.mod_rotation(|t| t.z += PI/2.0);
        obj1.update();
        assert_eq!(obj1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(obj2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(obj3.global_position() , Vector3::new(7.0,6.0,0.0));

        obj1.add_child(&obj3);
        obj1.update();
        assert_eq!(obj3.global_position() , Vector3::new(8.0,0.0,0.0));

        obj1.remove_child(&obj3);
        obj3.update();
        assert_eq!(obj3.global_position() , Vector3::new(1.0,0.0,0.0));

        obj2.add_child(&obj3);
        obj1.update();
        assert_eq!(obj3.global_position() , Vector3::new(7.0,6.0,0.0));

        obj1.remove_child(&obj3);
        obj1.update();
        obj2.update();
        obj3.update();
        assert_eq!(obj3.global_position() , Vector3::new(7.0,6.0,0.0));
    }

    #[test]
    fn parent_test() {
        let obj1 = DisplayObjectData::new(Logger::new("obj1"));
        let obj2 = DisplayObjectData::new(Logger::new("obj2"));
        let obj3 = DisplayObjectData::new(Logger::new("obj3"));
        obj1.add_child(&obj2);
        obj1.add_child(&obj3);
        obj2.unset_parent();
        obj3.unset_parent();
        assert_eq!(obj1.child_count(),0);
    }
}
