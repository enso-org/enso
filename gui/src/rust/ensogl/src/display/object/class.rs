#![allow(missing_docs)]

use crate::prelude::*;

use super::transform;

use crate::control::callback::DynEvent;
use crate::control::callback::DynEventDispatcher;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::display::scene::Scene;
use data::opt_vec::OptVec;
use nalgebra::Matrix4;
use nalgebra::Vector3;
use transform::CachedTransform;



// ==================
// === ParentBind ===
// ==================

/// Description of parent-child relation. It contains reference to parent node and information
/// about the child index there. It is used when a child is reconnected to different parent to
/// update the old parent with the information that the child was removed.
#[derive(Clone,Debug)]
pub struct ParentBind {
    pub parent : WeakNode,
    pub index  : usize
}

impl ParentBind {
    pub fn dispose(&self) {
        self.parent.upgrade().for_each(|p| p.remove_child_by_index(self.index));
    }

    pub fn parent(&self) -> Option<Instance> {
        self.parent.upgrade()
    }
}



// =================
// === Callbacks ===
// =================

/// Callbacks manager for display objects. Callbacks can be set only once. Panics if you try set
/// another callback to field with an already assigned callback. This design was chosen because it
/// is very lightweight and is not confusing (setting a callback unregisters previous one). We may
/// want to switch to a real callback registry in the future if there will be suitable use cases for
/// it.
#[derive(Default)]
#[allow(clippy::type_complexity)]
pub struct Callbacks {
    pub on_updated   : RefCell<Option<Box<dyn Fn(&NodeData)>>>,
    pub on_show      : RefCell<Option<Box<dyn Fn()>>>,
    pub on_hide      : RefCell<Option<Box<dyn Fn()>>>,
    pub on_show_with : RefCell<Option<Box<dyn Fn(&Scene)>>>,
    pub on_hide_with : RefCell<Option<Box<dyn Fn(&Scene)>>>,
}

impl Callbacks {
    pub fn on_updated(&self, data:&NodeData) {
        if let Some(f) = &*self.on_updated.borrow() { f(data) }
    }

    pub fn on_show(&self) {
        if let Some(f) = &*self.on_show.borrow() { f() }
    }

    pub fn on_hide(&self) {
        if let Some(f) = &*self.on_hide.borrow() { f() }
    }

    pub fn on_show_with(&self, scene:&Scene) {
        if let Some(f) = &*self.on_show_with.borrow() { f(scene) }
    }

    pub fn on_hide_with(&self, scene:&Scene) {
        if let Some(f) = &*self.on_hide_with.borrow() { f(scene) }
    }
}

impl Debug for Callbacks {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Callbacks")
    }
}



// ==================
// === DirtyFlags ===
// ==================

// === Types ===

pub type ChildrenDirty   = dirty::SharedSet<usize,Option<Box<dyn Fn()>>>;
pub type RemovedChildren = dirty::SharedVector<Instance,Option<Box<dyn Fn()>>>;
pub type NewParentDirty  = dirty::SharedBool<()>;
pub type TransformDirty  = dirty::SharedBool<Option<Box<dyn Fn()>>>;


// === Definition ===

#[derive(Derivative)]
#[derivative(Debug)]
pub struct DirtyFlags {
    parent           : NewParentDirty,
    children         : ChildrenDirty,
    removed_children : RemovedChildren,
    transform        : TransformDirty,
    #[derivative(Debug="ignore")]
    callback         : Rc<RefCell<Box<dyn Fn()>>>,
}

impl DirtyFlags {
    #![allow(trivial_casts)]
    pub fn new<L:Into<Logger>>(logger:L) -> Self {
        let logger           = logger.into();
        let parent           = NewParentDirty  :: new(logger.sub("dirty.parent"),());
        let children         = ChildrenDirty   :: new(logger.sub("dirty.children"),None);
        let removed_children = RemovedChildren :: new(logger.sub("dirty.removed_children"),None);
        let transform        = TransformDirty  :: new(logger.sub("dirty.transform"),None);
        let callback         = Rc::new(RefCell::new(Box::new(||{}) as Box<dyn Fn()>));

        let on_mut = enclose!((callback) move || (callback.borrow())() );
        transform        . set_callback(Some(Box::new(on_mut.clone())));
        children         . set_callback(Some(Box::new(on_mut.clone())));
        removed_children . set_callback(Some(Box::new(on_mut)));

        Self {parent,children,removed_children,transform,callback}
    }

    pub fn set_callback<F:'static+Fn()>(&self,f:F) {
        *self.callback.borrow_mut() = Box::new(f);
    }

    pub fn unset_callback(&self) {
        *self.callback.borrow_mut() = Box::new(||{});
    }
}



// ================
// === NodeData ===
// ================

/// A hierarchical representation of object containing a position, a scale and a rotation.
#[derive(Debug)]
pub struct NodeData {
    parent_bind      : CloneCell<Option<ParentBind>>,
    children         : RefCell<OptVec<WeakNode>>,
    transform        : Cell<CachedTransform>,
    event_dispatcher : RefCell<DynEventDispatcher>,
    dirty            : DirtyFlags,
    visible          : Cell<bool>,
    callbacks        : Callbacks,
    logger           : Logger,
}

impl NodeData {
    pub fn new<L:Into<Logger>>(logger:L) -> Self {
        let logger           = logger.into();
        let parent_bind      = default();
        let children         = default();
        let event_dispatcher = default();
        let transform        = default();
        let dirty            = DirtyFlags::new(&logger);
        let visible          = Cell::new(true);
        let callbacks        = default();
        Self {logger,parent_bind,children,event_dispatcher,transform,visible,callbacks,dirty}
    }

    pub fn is_visible(&self) -> bool {
        self.visible.get()
    }

    pub fn parent(&self) -> Option<Instance> {
        self.parent_bind.get().and_then(|t| t.parent())
    }

    pub fn is_orphan(&self) -> bool {
        self.parent_bind.get().is_none()
    }

    pub fn dispatch_event(&self, event:&DynEvent) {
        self.event_dispatcher.borrow_mut().dispatch(event);
        self.parent_bind.get().and_then(|b| b.parent()).map_ref(|p| p.dispatch_event(event));
    }

    pub fn child_count(&self) -> usize {
        self.children.borrow().len()
    }

    /// Removes child by a given index. Does nothing if the index was incorrect. In general, it is a
    /// better idea to use `remove_child` instead. Storing and using index explicitly is error
    /// prone.
    pub fn remove_child_by_index(&self, index:usize) {
        self.children.borrow_mut().remove(index).for_each(|child| {
            self.dirty.children.unset(&index);
            child.upgrade().for_each(|child| {
                child.raw_unset_parent();
                self.dirty.removed_children.set(child);
            });
        });
    }

    /// Recompute the transformation matrix of this object and update all of its dirty children.
    pub fn update(&self) {
        let origin0 = Matrix4::identity();
        self.update_origin(None,origin0,false)
    }

    /// Recompute the transformation matrix of this object and update all of its dirty children.
    pub fn update_with(&self, scene:&Scene) {
        let origin0 = Matrix4::identity();
        self.update_origin(Some(scene),origin0,false)
    }

    /// Updates object transformations by providing a new origin location. See docs of `update` to
    /// learn more.
    fn update_origin(&self, scene:Option<&Scene>, parent_origin:Matrix4<f32>, force:bool) {
        self.update_visibility(scene);
        let parent_changed = self.dirty.parent.check();
        let use_origin     = force || parent_changed;
        let new_origin     = use_origin.as_some(parent_origin);
        let msg            = match new_origin {
            Some(_) => "Update with new parent origin.",
            None    => "Update with old parent origin."
        };
        group!(self.logger, "{msg}", {
            let mut transform  = self.transform.get();
            let origin_changed = transform.update(new_origin);
            let origin         = transform.matrix;
            self.transform.set(transform);
            if origin_changed {
                self.logger.info("Self origin changed.");
                self.callbacks.on_updated(self);
                if !self.children.borrow().is_empty() {
                    group!(self.logger, "Updating all children.", {
                        self.children.borrow().iter().for_each(|child| {
                            child.upgrade().for_each(|t| t.update_origin(scene,origin,true));
                        });
                    })
                }
            } else {
                self.logger.info("Self origin did not change.");
                if self.dirty.children.check_all() {
                    group!(self.logger, "Updating dirty children.", {
                        self.dirty.children.take().iter().for_each(|ix| {
                            self.children.borrow()[*ix].upgrade().for_each(|t| t.update_origin(scene,origin,false))
                        });
                    })
                }
            }
            self.dirty.children.unset_all();
        });
        self.dirty.transform.unset();
        self.dirty.parent.unset();
    }

    /// Internal
    fn update_visibility(&self, scene:Option<&Scene>) {
        if self.dirty.removed_children.check_all() {
            group!(self.logger, "Updating removed children", {
                self.dirty.removed_children.take().into_iter().for_each(|child| {
                    if child.is_orphan() {
                        child.hide();
                        if let Some(s) = scene {
                            child.hide_with(s)
                        }
                    }
                });
            })
        }

        let parent_changed = self.dirty.parent.check();
        if parent_changed && !self.is_orphan() {
            self.show();
            if let Some(s) = scene {
                self.show_with(s)
            }
        }
    }

    /// Hide this node and all of its children. This function is called automatically when updating
    /// a node with a disconnected parent.
    pub fn hide(&self) {
        if self.visible.get() {
            self.logger.info("Hiding.");
            self.visible.set(false);
            self.callbacks.on_hide();
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.hide());
            });
        }
    }

    pub fn hide_with(&self, scene:&Scene) {
            self.logger.info("Hiding.");
            self.callbacks.on_hide_with(scene);
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.hide_with(scene));
            });
    }

    /// Show this node and all of its children. This function is called automatically when updating
    /// a node with a newly attached parent.
    pub fn show(&self) {
        if !self.visible.get() {
            self.logger.info("Showing.");
            self.visible.set(true);
            self.callbacks.on_show();
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.show());
            });
        }
    }

    pub fn show_with(&self, scene:&Scene) {
//        if !self.visible {
            self.logger.info("Showing.");
            self.callbacks.on_show_with(scene);
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.show_with(scene));
            });
//        }
    }

    /// Unset all node's callbacks. Because the Instance structure may live longer than one's could
    /// expect (usually to the next scene refresh), it is wise to unset all callbacks when disposing
    /// object.
    // TODO[ao] Instead if this, the Instance should keep weak references to its children (at least in
    // "removed" list) and do not extend their lifetime.
    pub fn clear_callbacks(&self) {
        self.callbacks.on_updated.clear();
        self.callbacks.on_show.clear();
        self.callbacks.on_hide.clear();
        self.callbacks.on_show_with.clear();
        self.callbacks.on_hide_with.clear();
    }
}


// === Private API ===

impl NodeData {
    fn register_child<T:Object>(&self, child:&T) -> usize {
        let child = child.display_object().clone();
        let index = self.children.borrow_mut().insert(child.downgrade());
        self.dirty.children.set(index);
        index
    }

    /// Removes and returns the parent bind. Please note that the parent is not updated.
    fn take_parent_bind(&self) -> Option<ParentBind> {
        self.parent_bind.take()
    }

    /// Removes the binding to the parent object. This is internal operation. Parent is not updated.
    fn raw_unset_parent(&self) {
        self.logger.info("Removing parent bind.");
        self.dirty.unset_callback();
        self.dirty.parent.set();
    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    fn set_parent_bind(&self, bind:ParentBind) {
        self.logger.info("Adding new parent bind.");
        if let Some(parent) = bind.parent() {
            let index = bind.index;
            let dirty = parent.dirty.children.clone_ref();
            self.dirty.set_callback(move || dirty.set(index));
            self.dirty.parent.set();
            self.parent_bind.set(Some(bind));
        }
    }
}

// === Getters ===

impl NodeData {
    /// Gets a clone of parent bind.
    pub fn parent_bind(&self) -> Option<ParentBind> {
        self.parent_bind.get()
    }

    pub fn global_position(&self) -> Vector3<f32> {
        self.transform.get().global_position()
    }

    pub fn position(&self) -> Vector3<f32> {
        self.transform.get().position()
    }

    pub fn scale(&self) -> Vector3<f32> {
        self.transform.get().scale()
    }

    pub fn rotation(&self) -> Vector3<f32> {
        self.transform.get().rotation()
    }

    pub fn matrix(&self) -> Matrix4<f32> {
        self.transform.get().matrix()
    }
}

// === Setters ===

impl NodeData {
    fn with_transform<F,T>(&self, f:F) -> T
    where F : FnOnce(&mut CachedTransform) -> T {
//        if let Some(bind) = self.parent_bind.get() {
//            bind.parent.dirty.children.set(bind.index);
//        }
        self.dirty.transform.set();

        let mut transform = self.transform.get();
        let out = f(&mut transform);
        self.transform.set(transform);
        out
    }

    pub fn set_position(&self, v:Vector3<f32>) {
        self.with_transform(|t| t.set_position(v));
    }

    pub fn set_scale(&self, v:Vector3<f32>) {
        self.with_transform(|t| t.set_scale(v));
    }

    pub fn set_rotation(&self, v:Vector3<f32>) {
        self.with_transform(|t| t.set_rotation(v));
    }

    pub fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.with_transform(|t| t.mod_position(f));
    }

    pub fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.with_transform(|t| t.mod_rotation(f));
    }

    pub fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.with_transform(|t| t.mod_scale(f));
    }

    pub fn set_on_updated<F:Fn(&NodeData)+'static>(&self, f:F) {
        self.callbacks.on_updated.set(Box::new(f))
    }

    pub fn set_on_show<F:Fn()+'static>(&self, f:F) {
        self.callbacks.on_show.set(Box::new(f))
    }

    pub fn set_on_hide<F:Fn()+'static>(&self, f:F) {
        self.callbacks.on_hide.set(Box::new(f))
    }

    /// Sets a callback which will be called with a reference to scene when the object will be
    /// shown (attached to visible display object graph).
    pub fn set_on_show_with<F:Fn(&Scene)+'static>(&self, f:F) {
        self.callbacks.on_show_with.set(Box::new(f))
    }

    /// Sets a callback which will be called with a reference to scene when the object will be
    /// hidden (detached from display object graph).
    pub fn set_on_hide_with<F:Fn(&Scene)+'static>(&self, f:F) {
        self.callbacks.on_hide_with.set(Box::new(f))
    }
}

impl Display for Instance {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Instance")
    }
}

impl Debug for Instance {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Instance")
    }
}



// ==========
// === Id ===
// ==========

#[derive(Clone,CloneRef,Copy,Debug,Default,Display,Eq,From,Hash,Into,PartialEq)]
pub struct Id(usize);



// ============
// === Instance ===
// ============

#[derive(Clone,CloneRef,Shrinkwrap)]
pub struct Instance {
    pub rc : Rc<NodeData>
}

impl Instance {
    pub fn clone2(&self) -> Self {
        Self {rc:self.rc.clone()}
    }

    /// Constructor.
    pub fn new<L:Into<Logger>>(logger:L) -> Self {
        let rc = Rc::new(NodeData::new(logger));
        Self {rc}
    }

    pub fn downgrade(&self) -> WeakNode {
        let weak = Rc::downgrade(&self.rc);
        WeakNode{weak}
    }
}


// === Public API ==

impl Instance {
    pub fn with_logger<F:FnOnce(&Logger)>(&self, f:F) {
        f(&self.rc.logger)
    }

    pub fn _id(&self) -> Id {
        Id(Rc::downgrade(&self.rc).as_raw() as *const() as usize)
    }

    /// Adds a new `Object` as a child to the current one.
    pub fn _add_child<T:Object>(&self, child:&T) {
        self.clone2().add_child_take(child);
    }

    /// Adds a new `Object` as a child to the current one. This is the same as `add_child` but takes
    /// the ownership of `self`.
    pub fn add_child_take<T:Object>(self, child:&T) {
        self.rc.logger.info("Adding new child.");
        let child = child.display_object();
        child.unset_parent();
        let index = self.register_child(child);
        self.rc.logger.info(|| format!("Child index is {}.", index));
        let parent_bind = ParentBind {parent:self.downgrade(),index};
        child.set_parent_bind(parent_bind);
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    pub fn _remove_child<T:Object>(&self, child:&T) {
        let child = child.display_object();
        if self.has_child(child) {
            child.unset_parent()
        }
    }

    /// Replaces the parent binding with a new parent.
    pub fn set_parent<T:Object>(&self, parent:&T) {
        parent.display_object().add_child(self);
    }

    /// Removes the current parent binding.
    pub fn _unset_parent(&self) {
        self.take_parent_bind().for_each(|t| t.dispose());
    }

    /// Checks if the provided object is child of the current one.
    pub fn has_child<T:Object>(&self, child:&T) -> bool {
        self.child_index(child).is_some()
    }

    /// Checks if the object has a parent.
    pub fn _has_parent(&self) -> bool {
        self.rc.parent_bind().is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T:Object>(&self, child:&T) -> Option<usize> {
        let child = child.display_object();
        child.parent_bind().and_then(|bind| {
            if bind.parent().as_ref() == Some(self) { Some(bind.index) } else { None }
        })
    }
}


// === Getters ===

impl Instance {
    pub fn index(&self) -> Option<usize> {
        self.parent_bind().map(|t| t.index)
    }
}


// === Instances ===

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc,&other.rc)
    }
}



// ================
// === WeakNode ===
// ================

#[derive(Clone,Debug)]
pub struct WeakNode {
    pub weak : Weak<NodeData>
}

impl WeakNode {
    pub fn upgrade(&self) -> Option<Instance> {
        self.weak.upgrade().map(|rc| Instance {rc})
    }
}



// ==============
// === Object ===
// ==============

pub trait Object {
    fn display_object(&self) -> &Instance;
}

impl Object for Instance {
    fn display_object(&self) -> &Instance {
        self
    }
}

impl<T:Object> Object for &T {
    fn display_object(&self) -> &Instance {
        let t : &T = *self;
        t.display_object()
    }
}



// =================
// === ObjectOps ===
// =================

// FIXME
// We are using names with underscores in order to fix this bug
// https://github.com/rust-lang/rust/issues/70727
impl<T:Object> ObjectOps for T {}
pub trait ObjectOps : Object {
    fn add_child<T:Object>(&self, child:&T) {
        self.display_object()._add_child(child.display_object());
    }

    fn remove_child<T:Object>(&self, child:&T) {
        self.display_object()._remove_child(child.display_object());
    }

    fn id(&self) -> Id {
        self.display_object()._id()
    }

    fn unset_parent(&self) {
        self.display_object()._unset_parent();
    }

    fn has_parent(&self) -> bool {
        self.display_object()._has_parent()
    }

    fn dispatch_event(&self, event:&DynEvent) {
        self.display_object().rc.dispatch_event(event)
    }

    fn transform_matrix(&self) -> Matrix4<f32> {
        self.display_object().rc.matrix()
    }

    fn global_position(&self) -> Vector3<f32> {
        self.display_object().rc.global_position()
    }

    fn position(&self) -> Vector3<f32> {
        self.display_object().rc.position()
    }

    fn scale(&self) -> Vector3<f32> {
        self.display_object().rc.scale()
    }

    fn rotation(&self) -> Vector3<f32> {
        self.display_object().rc.rotation()
    }

    fn set_position(&self, t:Vector3<f32>) {
        self.display_object().rc.set_position(t);
    }

    fn set_scale(&self, t:Vector3<f32>) {
        self.display_object().rc.set_scale(t);
    }

    fn set_rotation(&self, t:Vector3<f32>) {
        self.display_object().rc.set_rotation(t);
    }

    fn mod_position<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.display_object().rc.mod_position(f)
    }

    fn mod_rotation<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.display_object().rc.mod_rotation(f)
    }

    fn mod_scale<F:FnOnce(&mut Vector3<f32>)>(&self, f:F) {
        self.display_object().rc.mod_scale(f)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::PI;

    #[test]
    fn hierarchy_test() {
        let node1 = Instance::new(Logger::new("node1"));
        let node2 = Instance::new(Logger::new("node2"));
        let node3 = Instance::new(Logger::new("node3"));
        node1.add_child(&node2);
        assert_eq!(node2.index(),Some(0));

        node1.add_child(&node2);
        assert_eq!(node2.index(),Some(0));

        node1.add_child(&node3);
        assert_eq!(node3.index(),Some(1));

        node1.remove_child(&node3);
        assert_eq!(node3.index(),None);
    }

    #[test]
    fn transformation_test() {
        let node1 = Instance::new(Logger::new("node1"));
        let node2 = Instance::new(Logger::new("node2"));
        let node3 = Instance::new(Logger::new("node3"));
        assert_eq!(node1.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node1.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node2.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node3.global_position() , Vector3::new(0.0,0.0,0.0));

        node1.mod_position(|t| t.x += 7.0);
        node1.add_child(&node2);
        node2.add_child(&node3);
        assert_eq!(node1.position()        , Vector3::new(7.0,0.0,0.0));
        assert_eq!(node2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node1.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node2.global_position() , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node3.global_position() , Vector3::new(0.0,0.0,0.0));

        node1.update();
        assert_eq!(node1.position()        , Vector3::new(7.0,0.0,0.0));
        assert_eq!(node2.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node3.position()        , Vector3::new(0.0,0.0,0.0));
        assert_eq!(node1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(node2.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(node3.global_position() , Vector3::new(7.0,0.0,0.0));

        node2.mod_position(|t| t.y += 5.0);
        node1.update();
        assert_eq!(node1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(node2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(node3.global_position() , Vector3::new(7.0,5.0,0.0));

        node3.mod_position(|t| t.x += 1.0);
        node1.update();
        assert_eq!(node1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(node2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(node3.global_position() , Vector3::new(8.0,5.0,0.0));

        node2.mod_rotation(|t| t.z += PI/2.0);
        node1.update();
        assert_eq!(node1.global_position() , Vector3::new(7.0,0.0,0.0));
        assert_eq!(node2.global_position() , Vector3::new(7.0,5.0,0.0));
        assert_eq!(node3.global_position() , Vector3::new(7.0,6.0,0.0));

        node1.add_child(&node3);
        node1.update();
        assert_eq!(node3.global_position() , Vector3::new(8.0,0.0,0.0));

        node1.remove_child(&node3);
        node3.update();
        assert_eq!(node3.global_position() , Vector3::new(1.0,0.0,0.0));

        node2.add_child(&node3);
        node1.update();
        assert_eq!(node3.global_position() , Vector3::new(7.0,6.0,0.0));

        node1.remove_child(&node3);
        node1.update();
        node2.update();
        node3.update();
        assert_eq!(node3.global_position() , Vector3::new(7.0,6.0,0.0));
    }

    #[test]
    fn parent_test() {
        let node1 = Instance::new(Logger::new("node1"));
        let node2 = Instance::new(Logger::new("node2"));
        let node3 = Instance::new(Logger::new("node3"));
        node1.add_child(&node2);
        node1.add_child(&node3);
        node2.unset_parent();
        node3.unset_parent();
        assert_eq!(node1.child_count(),0);
    }


    #[test]
    fn visibility_test() {
        let node1 = Instance::new(Logger::new("node1"));
        let node2 = Instance::new(Logger::new("node2"));
        let node3 = Instance::new(Logger::new("node3"));
        assert_eq!(node3.is_visible(),true);
        node3.update();
        assert_eq!(node3.is_visible(),true);
        node1.add_child(&node2);
        node2.add_child(&node3);
        node1.update();
        assert_eq!(node3.is_visible(),true);
        node3.unset_parent();
        assert_eq!(node3.is_visible(),true);
        node1.update();
        assert_eq!(node3.is_visible(),false);
        node1.add_child(&node3);
        node1.update();
        assert_eq!(node3.is_visible(),true);
        node2.add_child(&node3);
        node1.update();
        assert_eq!(node3.is_visible(),true);
        node3.unset_parent();
        node1.update();
        assert_eq!(node3.is_visible(),false);
        node2.add_child(&node3);
        node1.update();
        assert_eq!(node3.is_visible(),true);
    }
}
