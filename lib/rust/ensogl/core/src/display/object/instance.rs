//! Implementation of display objects, elements that have visual representation and can form
//! hierarchical layouts. The implementation is very careful about performance, it tracks the
//! transformation changes and updates only the needed subset of the display object tree on demand.

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::dirty;
use crate::display::scene::layer::Layer;
use crate::display::scene::layer::WeakLayer;
use crate::display::scene::Scene;

use super::event;
use super::transform;
use data::opt_vec::OptVec;
use nalgebra::Matrix4;
use nalgebra::Vector3;
use transform::CachedTransform;


// ==================
// === ParentBind ===
// ==================

/// A parent-child binding. It contains reference to parent node and information about the child
/// index. When dropped, it removes the child from its parent.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct ParentBind {
    parent:      WeakInstance,
    child_index: usize,
}

impl ParentBind {
    fn parent(&self) -> Option<Instance> {
        self.parent.upgrade()
    }
}

impl Drop for ParentBind {
    fn drop(&mut self) {
        if let Some(parent) = self.parent() {
            parent.remove_child_by_index(self.child_index)
        }
    }
}



// ==================
// === DirtyFlags ===
// ==================

// === Types ===

type NewParentDirty = dirty::SharedBool<()>;
type ChildrenDirty = dirty::SharedSet<usize, OnDirtyCallback>;
type RemovedChildren = dirty::SharedVector<WeakInstance, OnDirtyCallback>;
type TransformDirty = dirty::SharedBool<OnDirtyCallback>;
type SceneLayerDirty = dirty::SharedBool<OnDirtyCallback>;


// === Definition ===

/// Set of dirty flags indicating whether some display object properties are not up to date.
///
/// In order to achieve high performance, display object hierarchy is not updated immediately after
/// a change. Instead, dirty flags are set and propagated in the hierarchy and the needed subset of
/// the hierarchy is updated after calling the `update` method.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct DirtyFlags {
    parent:           NewParentDirty,
    children:         ChildrenDirty,
    removed_children: RemovedChildren,
    transform:        TransformDirty,
    scene_layer:      SceneLayerDirty,
    #[derivative(Debug = "ignore")]
    on_dirty:         Rc<RefCell<Box<dyn Fn()>>>,
}

impl Default for DirtyFlags {
    fn default() -> Self {
        Self::new()
    }
}


impl DirtyFlags {
    #![allow(trivial_casts)]
    fn new() -> Self {
        let on_dirty = Rc::new(RefCell::new(Box::new(|| {}) as Box<dyn Fn()>));
        let parent = NewParentDirty::new(());
        let children = ChildrenDirty::new(on_dirty_callback(&on_dirty));
        let removed_children = RemovedChildren::new(on_dirty_callback(&on_dirty));
        let transform = TransformDirty::new(on_dirty_callback(&on_dirty));
        let scene_layer = SceneLayerDirty::new(on_dirty_callback(&on_dirty));
        Self { parent, children, removed_children, transform, scene_layer, on_dirty }
    }

    fn set_on_dirty<F: 'static + Fn()>(&self, f: F) {
        *self.on_dirty.borrow_mut() = Box::new(f);
    }

    fn unset_on_dirty(&self) {
        *self.on_dirty.borrow_mut() = Box::new(|| {});
    }
}


// === Callback ===

type OnDirtyCallback = impl Fn();
fn on_dirty_callback(f: &Rc<RefCell<Box<dyn Fn()>>>) -> OnDirtyCallback {
    let f = f.clone();
    move || (f.borrow())()
}



// =============
// === Model ===
// =============

/// A hierarchical representation of object containing information about transformation in 3D space,
/// list of children, and set of utils for dirty flag propagation.
///
/// See the documentation of [`Instance`] to learn more.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct Model {
    /// This is the [`Instance`]'s FRP network. Feel free to create new FRP nodes here as long as
    /// they are inherently bound with this display object. For example, a sprite, which owns a
    /// display object instance can extend this network to perform computations. However, you
    /// should not extend it from other places, as this will cause memory leak. See docs of FRP
    /// to learn more.
    pub network: frp::Network,
    /// Source for events. See the documentation of [`event::Event`] to learn more about events.
    pub event_source: frp::Source<event::SomeEvent>,
    capturing_event_fan: frp::Fan,
    bubbling_event_fan: frp::Fan,
    pub on_show: frp::Source<(Option<Scene>, Option<WeakLayer>)>,
    pub on_hide: frp::Source<Option<Scene>>,
    pub on_scene_layer_changed: frp::Source<(Option<Scene>, Option<WeakLayer>, Option<WeakLayer>)>,
    pub on_updated: frp::Source<()>,
    focused_descendant: RefCell<Option<WeakInstance>>,
    /// Layer the object was explicitly assigned to by the user, if any.
    assigned_layer: RefCell<Option<WeakLayer>>,
    /// Layer where the object is displayed. It may be set to by user or inherited from the parent.
    layer: RefCell<Option<WeakLayer>>,
    dirty: DirtyFlags,
    parent_bind: Rc<RefCell<Option<ParentBind>>>,
    children: RefCell<OptVec<WeakInstance>>,
    transform: RefCell<CachedTransform>,
    visible: Cell<bool>,
    bounding_box: Cell<Vector2<f32>>,
}

impl Default for Model {
    fn default() -> Self {
        Self::new()
    }
}

impl Model {
    /// Constructor.
    pub fn new() -> Self {
        let network = frp::Network::new("display_object");
        let focused_descendant = default();
        let assigned_layer = default();
        let layer = default();
        let dirty = default();
        let parent_bind = default();
        let children = default();
        let transform = default();
        let visible = default();
        let bounding_box = default();
        let capturing_event_fan = frp::Fan::new(&network);
        let bubbling_event_fan = frp::Fan::new(&network);
        frp::extend! { network
            on_show <- source();
            on_hide <- source();
            on_scene_layer_changed <- source();
            on_updated <- source();
            event_source <- source();
        }
        Self {
            network,
            event_source,
            capturing_event_fan,
            bubbling_event_fan,
            on_show,
            on_hide,
            on_scene_layer_changed,
            on_updated,
            focused_descendant,
            assigned_layer,
            layer,
            dirty,
            parent_bind,
            children,
            transform,
            visible,
            bounding_box,
        }
    }

    pub(crate) fn set_bounding_box(&self, bounding_box: Vector2<f32>) {
        self.bounding_box.set(bounding_box);
    }

    /// Checks whether the object is visible.
    pub fn is_visible(&self) -> bool {
        self.visible.get()
    }

    /// Checks whether the object is orphan (do not have parent object attached).
    pub fn is_orphan(&self) -> bool {
        self.parent_bind.borrow().is_none()
    }

    /// Parent object getter.
    pub fn parent(&self) -> Option<Instance> {
        self.parent_bind.borrow().as_ref().and_then(|t| t.parent())
    }

    /// Count of children objects.
    pub fn children_count(&self) -> usize {
        self.children.borrow().len()
    }

    /// Recompute the transformation matrix of this object and update all of its dirty children.
    pub fn update(&self, scene: &Scene) {
        let origin0 = Matrix4::identity();
        self.update_with_origin(scene, origin0, false, false, None)
    }

    /// The default visibility of a new [`Instance`] is false. You can use this function to override
    /// it. It is mainly used for a special 'root' element if such exists.
    pub fn force_set_visibility(&self, visibility: bool) {
        self.visible.set(visibility);
        // TODO[ao] this function should make the next update call on_show or on_hide
        //     https://github.com/enso-org/ide/issues/1406
    }

    /// Removes child by a given index. Does nothing if the index was incorrect.
    fn remove_child_by_index(&self, index: usize) {
        self.children.borrow_mut().remove(index).for_each(|child| {
            child.upgrade().for_each(|child| child.unsafe_unset_parent_without_update());
            self.dirty.children.unset(&index);
            self.dirty.removed_children.set(child);
        });
    }

    /// Removes all children of this display object and returns them.
    pub fn remove_all_children(&self) -> Vec<Instance> {
        let children: Vec<Instance> =
            self.children.borrow().iter().filter_map(|weak| weak.upgrade()).collect();
        for child in &children {
            child.unset_parent();
        }
        children
    }

    /// Removes the binding to the parent object. Parent is not updated.
    fn unsafe_unset_parent_without_update(&self) {
        trace!("Removing parent bind.");
        self.dirty.unset_on_dirty();
        self.dirty.parent.set();
    }

    /// Get event stream for bubbling events. See docs of [`event::Event`] to learn more.
    pub fn on_event<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.bubbling_event_fan.output::<event::Event<T>>()
    }

    /// Get event stream for capturing events. You should rather not need this function. Use
    /// [`on_event`] instead. See docs of [`event::Event`] to learn more.
    pub fn on_event_capturing<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.capturing_event_fan.output::<event::Event<T>>()
    }
}


// === Update API ===

impl Model {
    /// Updates object transformations by providing a new origin location. See docs of `update` to
    /// learn more.
    fn update_with_origin(
        &self,
        scene: &Scene,
        parent_origin: Matrix4<f32>,
        parent_origin_changed: bool,
        parent_layers_changed: bool,
        parent_layer: Option<&WeakLayer>,
    ) {
        // === Scene Layers Update ===
        let has_new_parent = self.dirty.parent.check();
        let assigned_layer_ref = self.assigned_layer.borrow();
        let assigned_layer = assigned_layer_ref.as_ref();
        let assigned_layers_changed = self.dirty.scene_layer.take().check();
        let has_assigned_layer = assigned_layer.is_some();
        let layer_changed = if assigned_layers_changed {
            // We might as well check here if assigned layers were not removed and accidentally the
            // inherited layers are not the same as previously assigned ones, but this is so rare
            // situation that we are not checking it to optimize the performance of this case.
            true
        } else if has_assigned_layer {
            false
        } else if has_new_parent {
            // Optimization for a common case of switching parent in the same layer.
            self.layer.borrow().as_ref() != parent_layer
        } else {
            parent_layers_changed
        };

        let new_layer_opt = layer_changed.as_some_from(|| {
            if has_assigned_layer {
                assigned_layer
            } else {
                parent_layer
            }
        });
        if let Some(new_layer) = new_layer_opt {
            debug_span!("Scene layer changed.").in_scope(|| {
                let old_layer = mem::replace(&mut *self.layer.borrow_mut(), new_layer.cloned());
                self.on_scene_layer_changed.emit((
                    Some(scene.clone_ref()),
                    old_layer,
                    new_layer.cloned(),
                ));
            });
        }

        let current_layer = self.layer.borrow();
        let new_layer = new_layer_opt.unwrap_or(current_layer.as_ref());


        // === Origin & Visibility Update ===

        self.update_visibility(scene, parent_layer);
        let is_origin_dirty = has_new_parent || parent_origin_changed || layer_changed;
        let new_parent_origin = is_origin_dirty.as_some(parent_origin);
        let parent_origin_label = if new_parent_origin.is_some() { "new" } else { "old" };
        debug_span!("Update with {} parent origin.", parent_origin_label).in_scope(|| {
            let origin_changed = self.transform.borrow_mut().update(new_parent_origin);
            let new_origin = self.transform.borrow().matrix;
            if origin_changed || layer_changed {
                if origin_changed {
                    trace!("Self origin changed.");
                } else {
                    trace!("Self origin did not change, but the layers changed");
                }
                self.on_updated.emit(());
                if !self.children.borrow().is_empty() {
                    debug_span!("Updating all children.").in_scope(|| {
                        let children = self.children.borrow().clone();
                        children.iter().for_each(|weak_child| {
                            weak_child.upgrade().for_each(|child| {
                                child.update_with_origin(
                                    scene,
                                    new_origin,
                                    true,
                                    layer_changed,
                                    new_layer,
                                )
                            });
                        });
                    })
                }
            } else {
                trace!("Self origin and layers did not change.");
                if self.dirty.children.check_all() {
                    debug_span!("Updating dirty children.").in_scope(|| {
                        self.dirty.children.take().iter().for_each(|ix| {
                            self.children
                                .borrow()
                                .safe_index(*ix)
                                .and_then(|t| t.upgrade())
                                .for_each(|child| {
                                    child.update_with_origin(
                                        scene,
                                        new_origin,
                                        false,
                                        layer_changed,
                                        new_layer,
                                    )
                                })
                        });
                    })
                }
            }
            self.dirty.children.unset_all();
        });
        self.dirty.transform.unset();
        self.dirty.parent.unset();
    }

    /// Hide all removed children and show this display object if it was attached to a new parent.
    fn update_visibility(&self, scene: &Scene, parent_layer: Option<&WeakLayer>) {
        self.take_removed_children_and_update_their_visibility(scene);
        let parent_changed = self.dirty.parent.check();
        if parent_changed && !self.is_orphan() {
            self.set_vis_true(scene, parent_layer)
        }
    }

    fn take_removed_children_and_update_their_visibility(&self, scene: &Scene) {
        if self.dirty.removed_children.check_all() {
            debug_span!("Updating removed children.").in_scope(|| {
                for child in self.dirty.removed_children.take().into_iter() {
                    if let Some(child) = child.upgrade() {
                        if !child.has_visible_parent() {
                            child.set_vis_false(scene);
                        }
                        // Even if the child is visible at this point, it does not mean that it
                        // should be visible after the entire update. Therefore, we must ensure that
                        // "removed children" lists in its subtree will be managed.
                        // See also test `visibility_test3`.
                        child.take_removed_children_and_update_their_visibility(scene);
                    }
                }
            })
        }
    }

    fn set_vis_false(&self, scene: &Scene) {
        if self.visible.get() {
            trace!("Hiding.");
            self.visible.set(false);
            self.on_hide.emit(Some(scene.clone_ref()));
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.set_vis_false(scene));
            });
        }
    }

    fn set_vis_true(&self, scene: &Scene, parent_layer: Option<&WeakLayer>) {
        if !self.visible.get() {
            trace!("Showing.");
            let this_scene_layer = self.assigned_layer.borrow();
            let this_scene_layers_ref = this_scene_layer.as_ref();
            let layer =
                if this_scene_layers_ref.is_none() { parent_layer } else { this_scene_layers_ref };
            self.visible.set(true);
            self.on_show.emit((Some(scene.clone_ref()), layer.cloned()));
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.set_vis_true(scene, layer));
            });
        }
    }
}


// === Register / Unregister ===

impl Model {
    /// Removes and returns the parent bind. Please note that the parent is not updated as long as
    /// the parent bind is not dropped.
    fn take_parent_bind(&self) -> Option<ParentBind> {
        let parent_bind = self.parent_bind.borrow_mut().take();
        if let Some(parent) = parent_bind.as_ref().and_then(|t| t.parent.upgrade()) {
            let is_focused = self.focused_descendant.borrow().is_some();
            if is_focused {
                parent.propagate_up_no_focus_instance();
            }
        }
        parent_bind
    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    fn set_parent_bind(&self, bind: ParentBind) {
        trace!("Adding new parent bind.");
        if let Some(focus_instance) = &*self.focused_descendant.borrow() {
            if let Some(parent) = bind.parent.upgrade() {
                parent.blur_tree();
                parent.propagate_up_new_focus_instance(focus_instance);
            }
        }
        if let Some(parent) = bind.parent() {
            let index = bind.child_index;
            let dirty = parent.dirty.children.clone_ref();
            self.dirty.set_on_dirty(move || dirty.set(index));
            self.dirty.parent.set();
            *self.parent_bind.borrow_mut() = Some(bind);
        }
    }
}


// === Getters ===

impl Model {
    /// Position of the object in the global coordinate space.
    pub fn global_position(&self) -> Vector3<f32> {
        self.transform.borrow().global_position()
    }

    /// Position of the object in the parent coordinate space.
    pub fn position(&self) -> Vector3<f32> {
        self.transform.borrow().position()
    }

    /// Scale of the object in the parent coordinate space.
    pub fn scale(&self) -> Vector3<f32> {
        self.transform.borrow().scale()
    }

    /// Rotation of the object in the parent coordinate space.
    pub fn rotation(&self) -> Vector3<f32> {
        self.transform.borrow().rotation()
    }

    /// Transformation matrix of the object in the parent coordinate space.
    pub fn matrix(&self) -> Matrix4<f32> {
        self.transform.borrow().matrix()
    }
}


// === Setters ===

impl Model {
    fn with_mut_borrowed_transform<F, T>(&self, f: F) -> T
    where F: FnOnce(&mut CachedTransform) -> T {
        self.dirty.transform.set();
        f(&mut self.transform.borrow_mut())
    }

    fn set_position(&self, v: Vector3<f32>) {
        self.with_mut_borrowed_transform(|t| t.set_position(v));
    }

    fn set_scale(&self, v: Vector3<f32>) {
        self.with_mut_borrowed_transform(|t| t.set_scale(v));
    }

    fn set_rotation(&self, v: Vector3<f32>) {
        self.with_mut_borrowed_transform(|t| t.set_rotation(v));
    }

    fn mod_position<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.with_mut_borrowed_transform(|t| t.mod_position(f));
    }

    fn mod_rotation<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.with_mut_borrowed_transform(|t| t.mod_rotation(f));
    }

    fn mod_scale<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.with_mut_borrowed_transform(|t| t.mod_scale(f));
    }
}



// ==========
// === Id ===
// ==========

/// Globally unique identifier of a display object.
#[derive(
    Clone, CloneRef, Copy, Debug, Default, Display, Eq, From, Hash, Into, PartialEq, Ord,
    PartialOrd
)]
pub struct Id(usize);



// ================
// === Instance ===
// ================

// TODO: add information how Scene works and that moving objects between scenes is not supported.

/// A hierarchical representation of object containing information about transformation in 3D space,
/// list of children, and set of utils for dirty flag propagation.
///
/// ## Scene Layers
/// Each display object instance contains an optional list of [`scene::LayerId`]. During object
/// update, the list is passed from parent display objects to their children as long as the child
/// does not override it (is assigned with [`None`]). Similar to [`Scene`], the scene layers list
/// plays a very important role in decoupling the architecture. It allows objects and their children
/// to be assigned to a particular [`scene::Layer`], and thus allows for easy to use depth
/// management.
#[derive(Derivative)]
#[derive(CloneRef, Deref, From)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
#[repr(transparent)]
pub struct Instance {
    def: InstanceDef,
}

/// Internal representation of [`Instance`]. It exists only to make the implementation less
/// error-prone. [`Instance`] implements [`Object`]. The [`ObjectOps`] trait defines the public API
/// of display objects, such as the [`add_child`] method. Without this struct, it would need to be
/// implemented as [`self.display_object().add_child(child)`]. However, if then we rename the
/// function in [`Instance`], the [`ObjectOps`] trait would still compile, but its function will
/// loop infinitely. This struct allows the implementation to be written as
/// [`self.display_object().def.add_child(child)`] instead, which will fail to compile after
/// renaming the function in [`InstanceDef`].
#[derive(Derivative)]
#[derive(CloneRef, Deref)]
#[derivative(Clone(bound = ""))]
#[repr(transparent)]
pub struct InstanceDef {
    rc: Rc<Model>,
}

impl Instance {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

impl InstanceDef {
    /// Constructor.
    pub fn new() -> Self {
        Self { rc: Rc::new(Model::new()) }.init()
    }

    fn init(self) -> Self {
        // This implementation is a bit complex because we do not want to clone network to the FRP
        // closure in order to avoid a memory leak.
        let network = &self.network;
        let parent_bind = &self.parent_bind;
        let capturing_event_fan = &self.capturing_event_fan;
        let bubbling_event_fan = &self.bubbling_event_fan;
        frp::extend! { network
            eval self.event_source ([parent_bind, capturing_event_fan, bubbling_event_fan] (event) {
                let parent = parent_bind.borrow().as_ref().and_then(|t| t.parent());
                Self::emit_event_impl(event, parent, &capturing_event_fan, &bubbling_event_fan);
            });
        }
        self
    }

    fn emit_event_impl(
        event: &event::SomeEvent,
        parent: Option<Instance>,
        capturing_event_fan: &frp::Fan,
        bubbling_event_fan: &frp::Fan,
    ) {
        let rev_parent_chain = Self::rev_parent_chain(parent);
        if event.captures.get() {
            for object in &rev_parent_chain {
                if !event.is_cancelled() {
                    object.capturing_event_fan.emit(&event.data);
                } else {
                    break;
                }
            }
        }
        if !event.is_cancelled() {
            capturing_event_fan.emit(&event.data);
        }
        if !event.is_cancelled() {
            bubbling_event_fan.emit(&event.data);
        }
        if event.bubbles.get() {
            for object in rev_parent_chain.iter().rev() {
                if !event.is_cancelled() {
                    object.bubbling_event_fan.emit(&event.data);
                } else {
                    break;
                }
            }
        }
    }

    /// Get reversed parent chain of this display object (`[root, child_of root, ...,
    /// parent]`). The last item is the argument passed to this function.
    fn rev_parent_chain(parent: Option<Instance>) -> Vec<Instance> {
        let mut vec = default();
        Self::build_rev_parent_chain(&mut vec, parent);
        vec
    }

    fn build_rev_parent_chain(vec: &mut Vec<Instance>, parent: Option<Instance>) {
        if let Some(parent) = parent {
            Self::build_rev_parent_chain(vec, parent.parent());
            vec.push(parent);
        }
    }

    fn new_event<T>(&self, payload: T) -> event::SomeEvent
    where T: 'static {
        event::SomeEvent::new(Some(self.downgrade()), payload)
    }

    fn emit_event<T>(&self, payload: T)
    where T: 'static {
        self.event_source.emit(event::SomeEvent::new(Some(self.downgrade()), payload));
    }

    fn focused_descendant(&self) -> Option<Instance> {
        self.focused_descendant.borrow().as_ref().and_then(|t| t.upgrade())
    }

    fn focused_instance(&self) -> Option<Instance> {
        if let Some(child) = self.focused_descendant() {
            Some(child)
        } else {
            self.parent().and_then(|parent| parent.focused_instance())
        }
    }

    fn is_focused(&self) -> bool {
        self.focused_descendant().as_ref().map(|t| &t.def) == Some(self)
    }

    fn focus(&self) {
        self.blur_tree();
        self.propagate_up_new_focus_instance(&self.downgrade());
        let focus_event = self.new_event(event::Focus);
        let focus_in_event = self.new_event(event::FocusIn);
        focus_event.bubbles.set(false);
        self.event_source.emit(focus_event);
        self.event_source.emit(focus_in_event);
    }

    fn blur(&self) {
        if self.is_focused() {
            self.blur_unchecked();
        }
    }

    /// Blur the display object tree this object belongs to. If any tree node (any node directly or
    /// indirectly connected with each other) was focused, it will be blurred.
    fn blur_tree(&self) {
        if let Some(instance) = self.focused_instance() {
            instance.blur_unchecked();
        }
    }

    /// Blur this object and propagate the information to root. Does not check if this object was
    /// focused. Calling this method on a non-focused object may cause inconsistent state, as parent
    /// objects will erase information about the currently focused object.
    fn blur_unchecked(&self) {
        self.propagate_up_no_focus_instance();
        let blur_event = self.new_event(event::Blur);
        let focus_out_event = self.new_event(event::FocusOut);
        blur_event.bubbles.set(false);
        self.event_source.emit(blur_event);
        self.event_source.emit(focus_out_event);
    }

    /// Clears the focus info in this instance and all parent instances. In order to work properly,
    /// this should be called on the focused instance. Otherwise, it may clear the information
    /// only partially.
    fn propagate_up_no_focus_instance(&self) {
        *self.focused_descendant.borrow_mut() = None;
        self.parent().for_each(|parent| parent.propagate_up_no_focus_instance());
    }

    /// Set the focus instance to the provided one here and in all instances on the path to the
    /// root.
    fn propagate_up_new_focus_instance(&self, instance: &WeakInstance) {
        debug_assert!(self.focused_descendant.borrow().is_none());
        *self.focused_descendant.borrow_mut() = Some(instance.clone());
        self.parent().for_each(|parent| parent.propagate_up_new_focus_instance(instance));
    }
}

impl Default for InstanceDef {
    fn default() -> Self {
        Self::new()
    }
}


// === Public API ==

impl InstanceDef {
    /// ID getter of this display object.
    pub fn id(&self) -> Id {
        Id(Rc::downgrade(&self.rc).as_ptr() as *const () as usize)
    }
}


// ============================
// === Hierarchy Management ===
// ============================

impl InstanceDef {
    /// Adds a new `Object` as a child to the current one.
    pub fn add_child(&self, child: &InstanceDef) {
        child.unset_parent();
        let child_index = self.register_child(child);
        trace!("Adding a new child at index {child_index}.");
        let parent_bind = ParentBind { parent: self.downgrade(), child_index };
        child.set_parent_bind(parent_bind);
    }

    fn register_child(&self, child: &InstanceDef) -> usize {
        let index = self.children.borrow_mut().insert(child.downgrade());
        self.dirty.children.set(index);
        index
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    pub fn remove_child<T: Object>(&self, child: &T) {
        let child = child.display_object();
        if self.has_child(child) {
            child.unset_parent()
        }
    }

    /// Replaces the parent binding with a new parent.
    pub fn set_parent(&self, parent: &InstanceDef) {
        parent.add_child(self);
    }

    /// Removes the current parent binding.
    pub fn unset_parent(&self) {
        self.take_parent_bind();
    }
}



// =============================
// === Hierarchy Management ====
// =============================

impl InstanceDef {
    /// Get the layers where this object is displayed. May be equal to layers it was explicitly
    /// assigned, or layers inherited from the parent.
    pub fn display_layers(&self) -> Option<WeakLayer> {
        self.layer.borrow().as_ref().cloned()
    }

    /// Add this object to the provided scene layer.
    /// Do not use this method explicitly. Use layers' methods instead.
    pub(crate) fn add_to_display_layer(&self, layer: &Layer) {
        let layer = layer.downgrade();
        self.dirty.scene_layer.set();
        let mut assigned_layer = self.assigned_layer.borrow_mut();
        if assigned_layer.as_ref() != Some(&layer) {
            *assigned_layer = Some(layer);
        }
    }

    /// Remove this object from the provided scene layer. Do not use this method explicitly. Use
    /// layers' methods instead.
    pub(crate) fn remove_from_scene_layer(&self, layer: &Layer) {
        let layer = layer.downgrade();
        let mut assigned_layer = self.assigned_layer.borrow_mut();
        if assigned_layer.as_ref() == Some(&layer) {
            self.dirty.scene_layer.set();
            *assigned_layer = None;
        }
    }



    /// Checks if the provided object is child of the current one.
    pub fn has_child<T: Object>(&self, child: &T) -> bool {
        self.child_index(child).is_some()
    }

    /// Checks if the object has a parent.
    pub fn has_parent(&self) -> bool {
        self.rc.parent_bind.borrow().is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T: Object>(&self, child: &T) -> Option<usize> {
        let child = child.display_object();
        child.parent_bind.borrow().as_ref().and_then(|bind| {
            if bind.parent().as_ref().map(|t| &t.def) == Some(self) {
                Some(bind.child_index)
            } else {
                None
            }
        })
    }
}


// === Private API ===

impl Instance {
    fn parent_index(&self) -> Option<usize> {
        self.parent_bind.borrow().as_ref().map(|t| t.child_index)
    }

    fn has_visible_parent(&self) -> bool {
        let parent = self.parent_bind.borrow().as_ref().and_then(|b| b.parent.upgrade());
        parent.map_or(false, |parent| parent.is_visible())
    }
}


// === Instances ===

impl PartialEq for InstanceDef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        self.def.eq(&other.def)
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Instance")
    }
}

impl Display for InstanceDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Instance")
    }
}

impl Debug for InstanceDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DisplayObject")
    }
}



// ====================
// === WeakInstance ===
// ====================

/// Weak display object instance. Will be dropped if no all strong instances are dropped.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct WeakInstance {
    weak: Weak<Model>,
}

impl WeakInstance {
    /// Upgrade the weak instance to strong one if it was not yet dropped.
    pub fn upgrade(&self) -> Option<Instance> {
        self.weak.upgrade().map(|rc| InstanceDef { rc }.into())
    }

    /// Checks whether this weak instance still exists (its strong instance was not dropped yet).
    pub fn exists(&self) -> bool {
        self.upgrade().is_some()
    }
}

impl InstanceDef {
    /// Create a new weak pointer to this display object instance.
    pub fn downgrade(&self) -> WeakInstance {
        let weak = Rc::downgrade(&self.rc);
        WeakInstance { weak }
    }
}

impl PartialEq for WeakInstance {
    fn eq(&self, other: &Self) -> bool {
        if self.exists() && other.exists() {
            self.weak.ptr_eq(&other.weak)
        } else {
            false
        }
    }
}



// ==============
// === Object ===
// ==============

/// The abstraction for any display object. In order to make your struct a display object, store
/// the `display::object::Instance` as a field and define impl of this trait. Every struct which
/// implements it, automatically implements the `display::object::ObjectOps`, and thus gets a lot
/// of methods implemented automatically.
#[allow(missing_docs)]
pub trait Object {
    fn display_object(&self) -> &Instance;
    fn weak_display_object(&self) -> WeakInstance {
        self.display_object().downgrade()
    }

    /// See `Any` description.
    fn into_any(self) -> Any
    where Self: Sized + 'static {
        Any { wrapped: Rc::new(self) }
    }
}

impl Object for Instance {
    fn display_object(&self) -> &Instance {
        self
    }
}

impl<T: Object> Object for &T {
    fn display_object(&self) -> &Instance {
        let t: &T = self;
        t.display_object()
    }
}



// ==================
// === Any Object ===
// ==================

/// A structure wrapping any `Object` and hiding the exact type.
///
/// You can convert structure into `Any` using `Object::into_any`. Unfortunately it is not possible
/// to make general `From` implementation, because `Any` itself would use it as well, and it clashes
/// with base implementation `From<T> for T`.
#[derive(CloneRef)]
pub struct Any {
    wrapped: Rc<dyn Object>,
}

impl Clone for Any {
    fn clone(&self) -> Self {
        Self { wrapped: self.wrapped.clone() }
    }
}

impl Debug for Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "display::object::Any")
    }
}

impl Object for Any {
    fn display_object(&self) -> &Instance {
        self.wrapped.display_object()
    }
}



// =========================
// === UnsetParentOnDrop ===
// =========================

/// Wrapper that unsets parent of a display object when dropped. Please note that [`Instance`]
/// implements [`CloneRef`], so it can still be alive even if this struct is dropped.
#[derive(Debug, NoCloneBecauseOfCustomDrop)]
pub struct UnsetParentOnDrop {
    instance: Instance,
}

impl UnsetParentOnDrop {
    /// Constructor.
    pub fn new(instance: impl Into<Instance>) -> Self {
        let instance = instance.into();
        Self { instance }
    }
}

impl Drop for UnsetParentOnDrop {
    fn drop(&mut self) {
        self.instance.unset_parent()
    }
}



// =================
// === ObjectOps ===
// =================

impl<T: Object + ?Sized> ObjectOps for T {}

/// Implementation of operations available for every struct which implements `display::Object`.
/// To learn more about the design, please refer to the documentation of [`Instance`].
//
// HOTFIX[WD]: We are using names with underscores in order to fix this bug:
// https://github.com/rust-lang/rust/issues/70727 . To be removed as soon as the bug is fixed.
#[allow(missing_docs)]
pub trait ObjectOps: Object {
    // === Information ===

    /// Globally unique identifier of this display object.
    fn id(&self) -> Id {
        self.display_object().def.id()
    }


    // === Hierarchy ===

    /// Get the layers where this object is displayed. May be equal to layers it was explicitly
    /// assigned, or layers inherited from the parent.
    fn display_layer(&self) -> Option<WeakLayer> {
        self.display_object().def.display_layers()
    }

    /// Add another display object as a child to this display object. Children will inherit all
    /// transformations of their parents.
    fn add_child<T: Object + ?Sized>(&self, child: &T) {
        self.display_object().def.add_child(child.display_object());
    }

    /// Remove the display object from the children list of this display object. Does nothing if
    /// the child was not registered.
    fn remove_child<T: Object>(&self, child: &T) {
        self.display_object().def.remove_child(child.display_object());
    }

    /// Removes this display object from its parent's children list.
    fn unset_parent(&self) {
        self.display_object().def.unset_parent();
    }

    /// Check whether this display object is attached to a parent.
    fn has_parent(&self) -> bool {
        self.display_object().def.has_parent()
    }

    /// Checks whether the object is visible.
    fn is_visible(&self) -> bool {
        self.display_object().def.rc.is_visible()
    }

    /// Checks whether the object is orphan (do not have parent object attached).
    fn is_orphan(&self) -> bool {
        self.display_object().def.rc.is_orphan()
    }


    // === Events ===

    /// Emit a new event. See docs of [`event::Event`] to learn more.
    fn emit_event<T>(&self, event: T)
    where T: 'static {
        self.display_object().def.emit_event(event)
    }

    /// Get event stream for bubbling events. See docs of [`event::Event`] to learn more.
    fn on_event<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.display_object().def.rc.on_event()
    }

    /// Get event stream for capturing events. You should rather not need this function. Use
    /// [`on_event`] instead. See docs of [`event::Event`] to learn more.
    fn on_event_capturing<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.display_object().def.rc.on_event_capturing()
    }

    /// Creates a new event with this object set to target.
    fn new_event<T: 'static>(&self, payload: T) -> event::SomeEvent {
        self.display_object().def.new_event(payload)
    }


    // === Focus ===

    /// Check whether this object is focused.
    fn is_focused(&self) -> bool {
        self.display_object().def.is_focused()
    }

    /// Focus this object. See docs of [`Event::Focus`] to learn more.
    fn focus(&self) {
        self.display_object().def.focus()
    }

    /// Blur ("unfocus") this object. See docs of [`Event::Blur`] to learn more.
    fn blur(&self) {
        self.display_object().def.blur()
    }

    /// Blur the display object tree this object belongs to. If any tree node (any node directly or
    /// indirectly connected with each other) was focused, it will be blurred.
    fn blur_tree(&self) {
        self.display_object().def.blur_tree()
    }

    /// Get the currently focused object if any. See docs of [`Event::Focus`] to learn more.
    fn focused_instance(&self) -> Option<Instance> {
        InstanceDef::focused_instance(self.display_object())
    }


    // === Transform ===

    fn transform_matrix(&self) -> Matrix4<f32> {
        self.display_object().def.rc.matrix()
    }

    fn global_position(&self) -> Vector3<f32> {
        self.display_object().def.rc.global_position()
    }


    // === Position ===

    fn position(&self) -> Vector3<f32> {
        self.display_object().def.rc.position()
    }

    fn x(&self) -> f32 {
        self.position().x
    }

    fn y(&self) -> f32 {
        self.position().y
    }

    fn z(&self) -> f32 {
        self.position().z
    }

    fn xy(&self) -> Vector2<f32> {
        let position = self.position();
        Vector2(position.x, position.y)
    }

    fn xz(&self) -> Vector2<f32> {
        let position = self.position();
        Vector2(position.x, position.z)
    }

    fn yz(&self) -> Vector2<f32> {
        let position = self.position();
        Vector2(position.y, position.z)
    }

    fn xyz(&self) -> Vector3<f32> {
        self.position()
    }

    fn mod_position<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.display_object().def.rc.mod_position(f)
    }

    fn mod_position_xy<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_position_xy(f(self.position().xy()));
    }

    fn mod_position_xz<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_position_xz(f(self.position().xz()));
    }

    fn mod_position_yz<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_position_yz(f(self.position().yz()));
    }

    fn mod_position_x<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_position_x(f(self.position().x));
    }

    fn mod_position_y<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_position_y(f(self.position().y));
    }

    fn mod_position_z<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_position_z(f(self.position().z));
    }

    fn set_position(&self, t: Vector3<f32>) {
        self.display_object().def.rc.set_position(t);
    }

    fn set_position_xy(&self, t: Vector2<f32>) {
        self.mod_position(|p| {
            p.x = t.x;
            p.y = t.y;
        })
    }

    fn set_position_xz(&self, t: Vector2<f32>) {
        self.mod_position(|p| {
            p.x = t.x;
            p.z = t.y;
        })
    }

    fn set_position_yz(&self, t: Vector2<f32>) {
        self.mod_position(|p| {
            p.y = t.x;
            p.z = t.y;
        })
    }

    fn set_position_x(&self, t: f32) {
        self.mod_position(|p| p.x = t)
    }

    fn set_position_y(&self, t: f32) {
        self.mod_position(|p| p.y = t)
    }

    fn set_position_z(&self, t: f32) {
        self.mod_position(|p| p.z = t)
    }


    // === Scale ===

    fn scale(&self) -> Vector3<f32> {
        self.display_object().def.rc.scale()
    }

    fn mod_scale<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.display_object().def.rc.mod_scale(f)
    }

    fn mod_scale_xy<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_scale_xy(f(self.scale().xy()));
    }

    fn mod_scale_xz<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_scale_xz(f(self.scale().xz()));
    }

    fn mod_scale_yz<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_scale_yz(f(self.scale().yz()));
    }

    fn mod_scale_x<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_scale_x(f(self.scale().x));
    }

    fn mod_scale_y<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_scale_y(f(self.scale().y));
    }

    fn mod_scale_z<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_scale_z(f(self.scale().z));
    }

    fn set_scale(&self, t: Vector3<f32>) {
        self.display_object().def.rc.set_scale(t);
    }

    fn set_scale_xy(&self, t: Vector2<f32>) {
        self.mod_scale(|p| {
            p.x = t.x;
            p.y = t.y;
        })
    }

    fn set_scale_xz(&self, t: Vector2<f32>) {
        self.mod_scale(|p| {
            p.x = t.x;
            p.z = t.y;
        })
    }

    fn set_scale_yz(&self, t: Vector2<f32>) {
        self.mod_scale(|p| {
            p.y = t.x;
            p.z = t.y;
        })
    }

    fn set_scale_x(&self, t: f32) {
        self.mod_scale(|p| p.x = t)
    }

    fn set_scale_y(&self, t: f32) {
        self.mod_scale(|p| p.y = t)
    }

    fn set_scale_z(&self, t: f32) {
        self.mod_scale(|p| p.z = t)
    }


    // === Rotation ===

    fn rotation(&self) -> Vector3<f32> {
        self.display_object().def.rc.rotation()
    }

    fn mod_rotation<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.display_object().def.rc.mod_rotation(f)
    }

    fn mod_rotation_xy<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_rotation_xy(f(self.rotation().xy()));
    }

    fn mod_rotation_xz<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_rotation_xz(f(self.rotation().xz()));
    }

    fn mod_rotation_yz<F: FnOnce(Vector2<f32>) -> Vector2<f32>>(&self, f: F) {
        self.set_rotation_yz(f(self.rotation().yz()));
    }

    fn mod_rotation_x<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_rotation_x(f(self.rotation().x));
    }

    fn mod_rotation_y<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_rotation_y(f(self.rotation().y));
    }

    fn mod_rotation_z<F: FnOnce(f32) -> f32>(&self, f: F) {
        self.set_rotation_z(f(self.rotation().z));
    }

    fn set_rotation(&self, t: Vector3<f32>) {
        self.display_object().def.rc.set_rotation(t);
    }

    fn set_rotation_xy(&self, t: Vector2<f32>) {
        self.mod_rotation(|p| {
            p.x = t.x;
            p.y = t.y;
        })
    }

    fn set_rotation_xz(&self, t: Vector2<f32>) {
        self.mod_rotation(|p| {
            p.x = t.x;
            p.z = t.y;
        })
    }

    fn set_rotation_yz(&self, t: Vector2<f32>) {
        self.mod_rotation(|p| {
            p.y = t.x;
            p.z = t.y;
        })
    }

    fn set_rotation_x(&self, t: f32) {
        self.mod_rotation(|p| p.x = t)
    }

    fn set_rotation_y(&self, t: f32) {
        self.mod_rotation(|p| p.y = t)
    }

    fn set_rotation_z(&self, t: f32) {
        self.mod_rotation(|p| p.z = t)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::world::World;
    use std::f32::consts::PI;

    #[test]
    fn hierarchy_test() {
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        node1.add_child(&node2);
        assert_eq!(node2.parent_index(), Some(0));

        node1.add_child(&node2);
        assert_eq!(node2.parent_index(), Some(0));

        node1.add_child(&node3);
        assert_eq!(node3.parent_index(), Some(1));

        node1.remove_child(&node3);
        assert_eq!(node3.parent_index(), None);
    }

    #[test]
    fn transformation_test() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        assert_eq!(node1.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(0.0, 0.0, 0.0));

        node1.mod_position(|t| t.x += 7.0);
        node1.add_child(&node2);
        node2.add_child(&node3);
        assert_eq!(node1.position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(0.0, 0.0, 0.0));

        node1.update(scene);
        assert_eq!(node1.position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 0.0, 0.0));

        node2.mod_position(|t| t.y += 5.0);
        node1.update(scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 5.0, 0.0));

        node3.mod_position(|t| t.x += 1.0);
        node1.update(scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(8.0, 5.0, 0.0));

        node2.mod_rotation(|t| t.z += PI / 2.0);
        node1.update(scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));

        node1.add_child(&node3);
        node1.update(scene);
        assert_eq!(node3.global_position(), Vector3::new(8.0, 0.0, 0.0));

        node1.remove_child(&node3);
        node3.update(scene);
        assert_eq!(node3.global_position(), Vector3::new(1.0, 0.0, 0.0));

        node2.add_child(&node3);
        node1.update(scene);
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));

        node1.remove_child(&node3);
        node1.update(scene);
        node2.update(scene);
        node3.update(scene);
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));
    }

    #[test]
    fn parent_test() {
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        node1.add_child(&node2);
        node1.add_child(&node3);
        node2.unset_parent();
        node3.unset_parent();
        assert_eq!(node1.children_count(), 0);
    }

    /// A utility to test display object instances' visibility.
    #[derive(Clone, CloneRef, Debug, Deref)]
    struct TestedNode {
        #[deref]
        node:         Instance,
        show_counter: Rc<Cell<usize>>,
        hide_counter: Rc<Cell<usize>>,
    }

    impl Object for TestedNode {
        fn display_object(&self) -> &Instance {
            &self.node
        }
    }

    impl TestedNode {
        fn new() -> Self {
            let node = Instance::new();
            let show_counter = Rc::<Cell<usize>>::default();
            let hide_counter = Rc::<Cell<usize>>::default();
            let network = &node.network;
            frp::extend! { network
                eval_ node.on_show(show_counter.set(show_counter.get() + 1));
                eval_ node.on_hide(hide_counter.set(hide_counter.get() + 1));
            }
            Self { node, show_counter, hide_counter }
        }

        fn reset_counters(&self) {
            self.show_counter.set(0);
            self.hide_counter.set(0);
        }

        fn check_if_was_shown(&self) {
            assert!(self.node.is_visible());
            assert_eq!(self.show_counter.get(), 1);
            assert_eq!(self.hide_counter.get(), 0);
            self.reset_counters();
        }

        fn check_if_was_hidden(&self) {
            assert!(!self.node.is_visible());
            assert_eq!(self.show_counter.get(), 0);
            assert_eq!(self.hide_counter.get(), 1);
            self.reset_counters();
        }

        fn check_if_visibility_did_not_changed(&self, expected_visibility: bool) {
            assert_eq!(self.node.is_visible(), expected_visibility);
            assert_eq!(self.show_counter.get(), 0);
            assert_eq!(self.hide_counter.get(), 0);
        }

        fn check_if_still_shown(&self) {
            self.check_if_visibility_did_not_changed(true)
        }
        fn check_if_still_hidden(&self) {
            self.check_if_visibility_did_not_changed(false)
        }
    }

    #[test]
    fn visibility_test() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        let node3 = TestedNode::new();
        node1.force_set_visibility(true);
        node3.check_if_still_hidden();
        node3.update(scene);
        node3.check_if_still_hidden();

        node1.add_child(&node2);
        node2.add_child(&node3);
        node1.update(scene);
        node3.check_if_was_shown();

        node3.unset_parent();
        node3.check_if_still_shown();

        node1.update(scene);
        node3.check_if_was_hidden();

        node1.add_child(&node3);
        node1.update(scene);
        node3.check_if_was_shown();

        node2.add_child(&node3);
        node1.update(scene);
        node3.check_if_still_shown();

        node3.unset_parent();
        node1.update(scene);
        node3.check_if_was_hidden();

        node2.add_child(&node3);
        node1.update(scene);
        node3.check_if_was_shown();
    }

    #[test]
    fn visibility_test2() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        node1.check_if_still_hidden();
        node1.update(scene);
        node1.check_if_still_hidden();
        node1.force_set_visibility(true);
        node1.update(scene);
        node1.check_if_still_shown();

        node1.add_child(&node2);
        node1.update(scene);
        node1.check_if_still_shown();
        node2.check_if_was_shown();
    }

    #[test]
    fn visibility_test3() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        let node3 = TestedNode::new();
        node1.force_set_visibility(true);
        node1.add_child(&node2);
        node2.add_child(&node3);
        node1.update(scene);
        node2.check_if_was_shown();
        node3.check_if_was_shown();

        node3.unset_parent();
        node3.add_child(&node2);
        node1.update(scene);
        node2.check_if_was_hidden();
        node3.check_if_was_hidden();
    }

    #[test]
    fn visibility_test4() {
        let world = World::new();
        let scene = &world.default_scene;

        let node1 = TestedNode::new();
        let node2 = TestedNode::new();
        let node3 = TestedNode::new();
        let node4 = TestedNode::new();
        node1.force_set_visibility(true);
        node1.add_child(&node2);
        node2.add_child(&node3);
        node1.update(scene);
        node2.check_if_was_shown();
        node3.check_if_was_shown();
        node4.check_if_still_hidden();

        node2.unset_parent();
        node1.add_child(&node2);
        node1.update(scene);
        node2.check_if_still_shown();
        node3.check_if_still_shown();
        node4.check_if_still_hidden();

        node1.add_child(&node4);
        node4.add_child(&node3);
        node1.update(scene);
        node2.check_if_still_shown();
        // TODO[ao]: This assertion fails, see https://github.com/enso-org/ide/issues/1405
        // node3.check_if_still_shown();
        node3.reset_counters();
        node4.check_if_was_shown();

        node4.unset_parent();
        node2.unset_parent();
        node1.update(scene);
        node2.check_if_was_hidden();
        node3.check_if_was_hidden();
        node4.check_if_was_hidden();

        node2.add_child(&node3);
        node1.update(scene);
        node2.check_if_still_hidden();
        node3.check_if_still_hidden();
        node4.check_if_still_hidden();
    }


    #[test]
    fn deep_hierarchy_test() {
        // === Init ===
        let world = World::new();
        let scene = &world.default_scene;

        let root = Instance::new();
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        let node4 = Instance::new();
        let node5 = Instance::new();
        let node6 = Instance::new();

        root.force_set_visibility(true);

        root.add_child(&node1);
        node1.add_child(&node2);
        node2.add_child(&node3);
        node3.add_child(&node4);
        node4.add_child(&node5);
        node5.add_child(&node6);

        assert!(!node3.is_visible());
        assert!(!node4.is_visible());
        assert!(!node5.is_visible());
        assert!(!node6.is_visible());


        // === Init Update ===

        root.update(scene);

        assert!(node3.is_visible());
        assert!(node4.is_visible());
        assert!(node5.is_visible());
        assert!(node6.is_visible());

        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node4.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node5.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node6.global_position(), Vector3::new(0.0, 0.0, 0.0));


        // === Position Modification  ===

        node3.mod_position(|t| t.x += 1.0);
        node4.mod_position(|t| t.x += 3.0);
        node5.mod_position(|t| t.x += 5.0);
        node6.mod_position(|t| t.x += 7.0);

        root.update(scene);

        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(1.0, 0.0, 0.0));
        assert_eq!(node4.global_position(), Vector3::new(4.0, 0.0, 0.0));
        assert_eq!(node5.global_position(), Vector3::new(9.0, 0.0, 0.0));
        assert_eq!(node6.global_position(), Vector3::new(16.0, 0.0, 0.0));


        // === Visibility Modification  ===

        node4.unset_parent();
        node3.unset_parent();
        root.update(scene);

        assert!(!node3.is_visible());
        assert!(!node4.is_visible());
        assert!(!node5.is_visible());
        assert!(!node6.is_visible());
    }

    #[test]
    fn layers_test() {
        let world = World::new();
        let scene = &world.default_scene;

        let layer1 = Layer::new("0");
        let layer2 = Layer::new("1");
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        node1.add_child(&node2);
        node1.add_child(&node3);
        node1.update(scene);
        assert_eq!(node1.display_layer(), None);
        assert_eq!(node2.display_layer(), None);
        assert_eq!(node3.display_layer(), None);

        node1.add_to_display_layer(&layer1);
        node1.update(scene);
        assert_eq!(node1.display_layer(), Some(layer1.downgrade()));
        assert_eq!(node2.display_layer(), Some(layer1.downgrade()));
        assert_eq!(node3.display_layer(), Some(layer1.downgrade()));

        node2.add_to_display_layer(&layer2);
        node1.update(scene);
        assert_eq!(node1.display_layer(), Some(layer1.downgrade()));
        assert_eq!(node2.display_layer(), Some(layer2.downgrade()));
        assert_eq!(node3.display_layer(), Some(layer1.downgrade()));
    }

    #[test]
    fn focus_consistency_test() {
        //         obj_root
        //         /      \
        // obj_left_1     obj_right_1
        //     |               |
        // obj_left_2     obj_right_2
        let obj_root = Instance::new();
        let obj_left_1 = Instance::new();
        let obj_left_2 = Instance::new();
        let obj_right_1 = Instance::new();
        let obj_right_2 = Instance::new();
        obj_root.add_child(&obj_left_1);
        obj_root.add_child(&obj_right_1);
        obj_left_1.add_child(&obj_left_2);
        obj_right_1.add_child(&obj_right_2);

        let check_focus_consistency = |focused: Option<&Instance>| {
            // Check that at most one object is focused and if so, that it is the correct one.
            assert_eq!(obj_root.is_focused(), focused == Some(&obj_root));
            assert_eq!(obj_left_1.is_focused(), focused == Some(&obj_left_1));
            assert_eq!(obj_left_2.is_focused(), focused == Some(&obj_left_2));
            assert_eq!(obj_right_1.is_focused(), focused == Some(&obj_right_1));
            assert_eq!(obj_right_2.is_focused(), focused == Some(&obj_right_2));

            // Check that all nodes contain the valid reference to the focused one.
            assert_eq!(obj_root.focused_instance().as_ref(), focused);
            assert_eq!(obj_left_1.focused_instance().as_ref(), focused);
            assert_eq!(obj_left_2.focused_instance().as_ref(), focused);
            assert_eq!(obj_right_1.focused_instance().as_ref(), focused);
            assert_eq!(obj_right_2.focused_instance().as_ref(), focused);

            // Check that focus information is correctly distributed across the branches.
            if focused == Some(&obj_root) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_left_1) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_left_2) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), focused);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_right_1) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), None);
            } else if focused == Some(&obj_right_2) {
                assert_eq!(obj_root.focused_descendant().as_ref(), focused);
                assert_eq!(obj_left_1.focused_descendant().as_ref(), None);
                assert_eq!(obj_left_2.focused_descendant().as_ref(), None);
                assert_eq!(obj_right_1.focused_descendant().as_ref(), focused);
                assert_eq!(obj_right_2.focused_descendant().as_ref(), focused);
            }
        };

        // === Checking the initial state ===

        check_focus_consistency(None);


        // === Checking if blurring works ===

        obj_left_1.focus();
        check_focus_consistency(Some(&obj_left_1));

        obj_left_2.blur();
        check_focus_consistency(Some(&obj_left_1));

        obj_left_1.blur();
        check_focus_consistency(None);


        // === Checking if focus stealing works ===

        obj_left_1.focus();
        check_focus_consistency(Some(&obj_left_1));

        obj_right_1.focus();
        check_focus_consistency(Some(&obj_right_1));

        obj_left_2.focus();
        check_focus_consistency(Some(&obj_left_2));

        obj_right_2.focus();
        check_focus_consistency(Some(&obj_right_2));

        obj_root.blur_tree();
        check_focus_consistency(None);


        // === Checking if detaching subtree removes focus from parent its parent ===

        obj_left_2.focus();
        check_focus_consistency(Some(&obj_left_2));

        obj_left_1.unset_parent();
        assert!(!obj_root.is_focused());
        assert!(!obj_left_1.is_focused());
        assert!(obj_left_2.is_focused());
        assert!(!obj_right_1.is_focused());
        assert!(!obj_right_2.is_focused());

        assert_eq!(obj_root.focused_instance().as_ref(), None);
        assert_eq!(obj_left_1.focused_instance().as_ref(), Some(&obj_left_2));
        assert_eq!(obj_left_2.focused_instance().as_ref(), Some(&obj_left_2));
        assert_eq!(obj_right_1.focused_instance().as_ref(), None);
        assert_eq!(obj_right_2.focused_instance().as_ref(), None);


        // === Checking if attaching subtree with a focus steals the existing one ===

        obj_right_2.focus();
        obj_root.add_child(&obj_left_1);
        check_focus_consistency(Some(&obj_left_2));
    }

    #[test]
    fn focus_event_propagation_test() {
        let world = World::new();
        let scene = &world.default_scene;

        let obj_1 = Instance::new();
        let obj_2 = Instance::new();
        let obj_3 = Instance::new();
        obj_1.add_child(&obj_2);
        obj_2.add_child(&obj_3);

        let capturing_1 = obj_1.on_event_capturing::<f32>();
        let capturing_2 = obj_2.on_event_capturing::<f32>();
        let capturing_3 = obj_3.on_event_capturing::<f32>();
        let bubbling_1 = obj_1.on_event::<f32>();
        let bubbling_2 = obj_2.on_event::<f32>();
        let bubbling_3 = obj_3.on_event::<f32>();


        // === Event phases test ===

        let network = frp::Network::new("network");
        let out: Rc<RefCell<Vec<&'static str>>> = default();
        frp::extend! { network
            eval_ capturing_1 (out.borrow_mut().push("capturing_1"));
            eval_ capturing_2 (out.borrow_mut().push("capturing_2"));
            eval_ capturing_3 (out.borrow_mut().push("capturing_3"));
            eval_ bubbling_1 (out.borrow_mut().push("bubbling_1"));
            eval_ bubbling_2 (out.borrow_mut().push("bubbling_2"));
            eval_ bubbling_3 (out.borrow_mut().push("bubbling_3"));
        }

        obj_3.emit_event::<f32>(0.0);
        assert_eq!(&*out.borrow(), &[
            "capturing_1",
            "capturing_2",
            "capturing_3",
            "bubbling_3",
            "bubbling_2",
            "bubbling_1"
        ]);
        drop(network);


        // === Cancelling the event ===

        let network = frp::Network::new("network");
        let out: Rc<RefCell<Vec<&'static str>>> = default();
        frp::extend! { network
            eval_ capturing_1 (out.borrow_mut().push("capturing_1"));
            eval capturing_2 ([out] (e) {
                e.stop_propagation();
                out.borrow_mut().push("capturing_2")
            });
            eval_ capturing_3 (out.borrow_mut().push("capturing_3"));
            eval_ bubbling_1 (out.borrow_mut().push("bubbling_1"));
            eval_ bubbling_2 (out.borrow_mut().push("bubbling_2"));
            eval_ bubbling_3 (out.borrow_mut().push("bubbling_3"));
        }

        obj_3.emit_event::<f32>(0.0);
        assert_eq!(&*out.borrow(), &["capturing_1", "capturing_2",]);
        drop(network);


        // === Manual event creation ===

        let network = frp::Network::new("network");
        let out: Rc<RefCell<Vec<&'static str>>> = default();
        frp::extend! { network
            eval_ capturing_1 (out.borrow_mut().push("capturing_1"));
            eval_ capturing_2 (out.borrow_mut().push("capturing_2"));
            eval_ capturing_3 (out.borrow_mut().push("capturing_3"));
            eval_ bubbling_1 (out.borrow_mut().push("bubbling_1"));
            eval bubbling_2 ([out] (e) {
                e.stop_propagation();
                out.borrow_mut().push("bubbling_2")
            });
            eval_ bubbling_3 (out.borrow_mut().push("bubbling_3"));
        }

        let event = obj_3.new_event::<f32>(0.0);
        obj_3.event_source.emit(&event);
        assert_eq!(&*out.borrow(), &[
            "capturing_1",
            "capturing_2",
            "capturing_3",
            "bubbling_3",
            "bubbling_2"
        ]);
        drop(network);
    }
}
