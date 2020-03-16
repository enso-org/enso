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

use crate::control::callback::DynEventDispatcher;
use crate::control::callback::DynEvent;
use shapely::shared;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    pub use super::Object;
    pub use super::ObjectOps;
}



// ==================
// === ParentBind ===
// ==================

/// Description of parent-child relation. It contains reference to parent node and information
/// about the child index there. It is used when a child is reconnected to different parent to
/// update the old parent with the information that the child was removed.
#[derive(Clone,Debug)]
pub struct ParentBind {
    pub parent : Node,
    pub index  : usize
}

impl ParentBind {
    pub fn dispose(&self) {
        self.parent.remove_child_by_index(self.index);
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
pub struct Callbacks {
    pub on_updated : Option<Box<dyn Fn(&NodeData)>>,
    pub on_show    : Option<Box<dyn Fn()>>,
    pub on_hide    : Option<Box<dyn Fn()>>,
}

impl Callbacks {
    /// Setter. Warning, altering the node structure during execution of the callback may cause
    /// panic.
    pub fn set_on_updated<F:Fn(&NodeData)+'static>(&mut self, f:F) {
        if self.on_updated.is_some() { panic!("The `on_updated` callback was already set.") }
        self.on_updated = Some(Box::new(f))
    }

    /// Setter. Warning, altering the node structure during execution of the callback may cause
    /// panic.
    pub fn set_on_show<F:Fn()+'static>(&mut self, f:F) {
        if self.on_show.is_some() { panic!("The `on_show` callback was already set.") }
        self.on_show = Some(Box::new(f))
    }

    /// Setter. Warning, altering the node structure during execution of the callback may cause
    /// panic.
    pub fn set_on_hide<F:Fn()+'static>(&mut self, f:F) {
        if self.on_hide.is_some() { panic!("The `on_hide` callback was already set.") }
        self.on_hide = Some(Box::new(f))
    }
}

impl Debug for Callbacks {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Callbacks")
    }
}





// ============
// === Node ===
// ============

// === Types ===

pub type ChildDirty      = dirty::SharedSet<usize,Option<OnChange>>;
pub type RemovedChildren = dirty::SharedVector<Node,Option<OnChange>>;
pub type NewParentDirty  = dirty::SharedBool<()>;
pub type TransformDirty  = dirty::SharedBool<Option<OnChange>>;


// === Callbacks ===

closure! {
fn fn_on_change(dirty:ChildDirty, ix:usize) -> OnChange { || dirty.set(ix) }
}


// === Definition ===

shared! { Node
/// A hierarchical representation of object containing a position, a scale and a rotation.
pub struct NodeData {
    parent_bind      : Option<ParentBind>,
    children         : OptVec<Node>,
    removed_children : RemovedChildren,
    child_dirty      : ChildDirty,
    new_parent_dirty : NewParentDirty,
    transform        : CachedTransform<Option<OnChange>>,
    event_dispatcher : DynEventDispatcher,
    visible          : bool,
    callbacks        : Callbacks,
    logger           : Logger,
}

impl {
    pub fn new<L:Into<Logger>>(logger:L) -> Self {
        let logger           = logger.into();
        let parent_bind      = default();
        let children         = default();
        let event_dispatcher = default();
        let transform        = CachedTransform :: new(logger.sub("transform")        , None);
        let child_dirty      = ChildDirty      :: new(logger.sub("child_dirty")      , None);
        let removed_children = RemovedChildren :: new(logger.sub("removed_children") , None);
        let new_parent_dirty = NewParentDirty  :: new(logger.sub("new_parent_dirty") , ());
        let visible          = true;
        let callbacks        = default();
        Self {logger,parent_bind,children,removed_children,event_dispatcher,transform,child_dirty
             ,new_parent_dirty,visible,callbacks}
    }

    pub fn is_visible(&self) -> bool {
        self.visible
    }

    pub fn parent(&self) -> Option<Node> {
        self.parent_bind.as_ref().map(|t| t.parent.clone_ref())
    }

    pub fn is_orphan(&self) -> bool {
        self.parent_bind.is_none()
    }

    pub fn dispatch_event(&mut self, event:&DynEvent) {
        self.event_dispatcher.dispatch(event);
        self.parent_bind.map_ref(|bind| bind.parent.dispatch_event(event));
    }

    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Removes child by a given index. Does nothing if the index was incorrect. In general, it is a
    /// better idea to use `remove_child` instead. Storing and using index explicitly is error
    /// prone.
    pub fn remove_child_by_index(&mut self, index:usize) {
        self.children.remove(index).for_each(|child| {
            child.raw_unset_parent();
            self.child_dirty.unset(&index);
            self.removed_children.set(child);
        });
    }

    /// Recompute the transformation matrix of this object and update all of its dirty children.
    pub fn update(&mut self) {
        let origin0 = Matrix4::identity();
        self.update_with(&origin0,false)
    }

    /// Updates object transformations by providing a new origin location. See docs of `update` to
    /// learn more.
    fn update_with(&mut self, parent_origin:&Matrix4<f32>, force:bool) {
        self.update_visibility();
        let parent_changed = self.new_parent_dirty.check();
        let use_origin     = force || parent_changed;
        let new_origin     = use_origin.as_some(parent_origin);
        let msg            = match new_origin {
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
        if let Some(f) = &self.callbacks.on_updated { f(self) }
    }

    /// Internal
    fn update_visibility(&mut self) {
        if self.removed_children.check_all() {
            group!(self.logger, "Updating removed children", {
                self.removed_children.take().into_iter().for_each(|child| {
                    if child.is_orphan() {
                        child.hide();
                    }
                });
            })
        }

        let parent_changed = self.new_parent_dirty.check();
        if parent_changed && !self.is_orphan() {
            self.show()
        }
    }

    /// Hide this node and all of its children. This function is called automatically when updating
    /// a node with a disconnected parent.
    pub fn hide(&mut self) {
        if self.visible {
            self.logger.info("Hiding.");
            self.visible = false;
            if let Some(f) = &self.callbacks.on_hide { f() }
            self.children.iter().for_each(|child| {
                child.hide();
            });
        }
    }

    /// Show this node and all of its children. This function is called automatically when updating
    /// a node with a newly attached parent.
    pub fn show(&mut self) {
        if !self.visible {
            self.logger.info("Showing.");
            self.visible = true;
            if let Some(f) = &self.callbacks.on_show { f() }
            self.children.iter().for_each(|child| {
                child.show();
            });
        }
    }
}


// === Private API ===

impl {
    pub fn register_child<T:Object>(&mut self, child:T) -> usize {
        let child = child.display_object();
        let index = self.children.insert(child);
        self.child_dirty.set(index);
        index
    }

    /// Removes and returns the parent bind. Please note that the parent is not updated.
    pub fn take_parent_bind(&mut self) -> Option<ParentBind> {
        self.parent_bind.take()
    }

    /// Removes the binding to the parent object. This is internal operation. Parent is not updated.
    pub fn raw_unset_parent(&mut self) {
        self.logger.info("Removing parent bind.");
        self.transform.dirty.set_callback(None);
        self.child_dirty.set_callback(None);
        self.removed_children.set_callback(None);
        self.new_parent_dirty.set();
//        self.take_parent_bind();


    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    pub fn set_parent_bind(&mut self, bind:ParentBind) {
        self.logger.info("Adding new parent bind.");
        let dirty  = bind.parent.rc.borrow().child_dirty.clone_ref();
        let index  = bind.index;
        let on_mut = move || {dirty.set(index)};
        self.transform.dirty.set_callback(Some(Box::new(on_mut.clone())));
        self.child_dirty.set_callback(Some(Box::new(on_mut.clone())));
        self.removed_children.set_callback(Some(Box::new(on_mut)));
        self.new_parent_dirty.set();
        self.parent_bind = Some(bind);
    }
}

// === Getters ===

impl {
    /// Gets a clone of parent bind.
    pub fn parent_bind(&self) -> Option<ParentBind> {
        self.parent_bind.clone()
    }

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

impl {
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

    pub fn set_on_updated<F:Fn(&NodeData)+'static>(&mut self, f:F) {
        self.callbacks.set_on_updated(f)
    }

    pub fn set_on_show<F:Fn()+'static>(&mut self, f:F) {
        self.callbacks.set_on_show(f)
    }

    pub fn set_on_hide<F:Fn()+'static>(&mut self, f:F) {
        self.callbacks.set_on_hide(f)
    }
}}

impl Display for Node {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Node")
    }
}

impl Debug for Node {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"Node")
    }
}



// ============
// === Node ===
// ============

// === Public API ==

impl Node {
    pub fn with_logger<F:FnOnce(&Logger)>(&self, f:F) {
        f(&self.rc.borrow().logger)
    }

    /// Adds a new `Object` as a child to the current one.
    pub fn add_child<T:Object>(&self, child:T) {
        self.clone_ref().add_child_take(child);
    }

    /// Adds a new `Object` as a child to the current one. This is the same as `add_child` but takes
    /// the ownership of `self`.
    pub fn add_child_take<T:Object>(self, child:T) {
        self.rc.borrow().logger.info("Adding new child.");
        let child = child.display_object();
        child.unset_parent();
        let index = self.register_child(&child);
        self.rc.borrow().logger.info(|| format!("Child index is {}.", index));
        let parent_bind = ParentBind {parent:self,index};
        child.set_parent_bind(parent_bind);
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    pub fn remove_child<T:Object>(&self, child:T) {
        let child = child.display_object();
        if self.has_child(&child) {
            child.unset_parent()
        }
    }

    /// Replaces the parent binding with a new parent.
    pub fn set_parent<T:Object>(&self, parent:T) {
        parent.display_object().add_child_take(self);
    }

    /// Removes the current parent binding.
    pub fn unset_parent(&self) {
        self.take_parent_bind().for_each(|t| t.dispose());
    }

    /// Checks if the provided object is child of the current one.
    pub fn has_child<T:Object>(&self, child:T) -> bool {
        self.child_index(child).is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T:Object>(&self, child:T) -> Option<usize> {
        let child = child.display_object();
        child.parent_bind().and_then(|bind| {
            if self == &bind.parent { Some(bind.index) } else { None }
        })
    }
}



// === Getters ===

impl Node {
    pub fn index(&self) -> Option<usize> {
        self.parent_bind().map(|t| t.index)
    }
}

// === Instances ===

impl From<&Node> for Node {
    fn from(t:&Node) -> Self { t.clone_ref() }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc,&other.rc)
    }
}



// ==============
// === Object ===
// ==============

pub trait Object: Into<Node> {
    fn display_object(self) -> Node {
        self.into()
    }
}

impl<T:Into<Node>> Object for T {}

pub trait ObjectOps<'t>
where &'t Self:Object, Self:'t {
    fn add_child<T:Object>(&'t self, child:T) {
        self.display_object().add_child_take(child);
    }

    fn unset_parent(&'t self) {
        self.display_object().unset_parent();
    }

    fn dispatch_event(&'t self, event:&DynEvent) {
        self.display_object().dispatch_event(event)
    }
}

impl<'t,T> ObjectOps<'t> for T
where T:'t, &'t T:Object {}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::PI;

    #[test]
    fn hierarchy_test() {
        let node1 = Node::new(Logger::new("node1"));
        let node2 = Node::new(Logger::new("node2"));
        let node3 = Node::new(Logger::new("node3"));
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
        let node1 = Node::new(Logger::new("node1"));
        let node2 = Node::new(Logger::new("node2"));
        let node3 = Node::new(Logger::new("node3"));
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
        let node1 = Node::new(Logger::new("node1"));
        let node2 = Node::new(Logger::new("node2"));
        let node3 = Node::new(Logger::new("node3"));
        node1.add_child(&node2);
        node1.add_child(&node3);
        node2.unset_parent();
        node3.unset_parent();
        assert_eq!(node1.child_count(),0);
    }


    #[test]
    fn visibility_test() {
        let node1 = Node::new(Logger::new("node1"));
        let node2 = Node::new(Logger::new("node2"));
        let node3 = Node::new(Logger::new("node3"));
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
