//! Implementation of display objects, elements that have visual representation and can form
//! hierarchical layouts. The implementation is very careful about performance, it tracks the
//! transformation changes and updates only the needed subset of the display object tree on demand.

use crate::prelude::*;

use super::transform;

use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::display::scene::layer::Layer;
use crate::display::scene::layer::WeakLayer;
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
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[allow(missing_docs)]
pub struct ParentBind<Host> {
    pub parent: WeakInstance<Host>,
    pub index:  usize,
}

impl<Host> ParentBind<Host> {
    fn parent(&self) -> Option<Instance<Host>> {
        self.parent.upgrade()
    }
}

impl<Host> Drop for ParentBind<Host> {
    fn drop(&mut self) {
        self.parent().for_each(|p| p.remove_child_by_index(self.index));
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
#[derive(Derivative)]
#[derivative(Default(bound = ""))]
#[allow(clippy::type_complexity)]
pub struct Callbacks<Host> {
    on_updated:              RefCell<Option<Box<dyn Fn(&Model<Host>)>>>,
    on_show:                 RefCell<Option<Box<dyn Fn(&Host, &[WeakLayer])>>>,
    on_hide:                 RefCell<Option<Box<dyn Fn(&Host)>>>,
    on_scene_layers_changed: RefCell<Option<Box<dyn Fn(&Host, &[WeakLayer], &[WeakLayer])>>>,
}

impl<Host> Callbacks<Host> {
    fn on_updated(&self, model: &Model<Host>) {
        if let Some(f) = &*self.on_updated.borrow() {
            f(model)
        }
    }

    fn on_show(&self, host: &Host, layers: &[WeakLayer]) {
        if let Some(f) = &*self.on_show.borrow() {
            f(host, layers)
        }
    }

    fn on_hide(&self, host: &Host) {
        if let Some(f) = &*self.on_hide.borrow() {
            f(host)
        }
    }

    fn on_scene_layers_changed(
        &self,
        host: &Host,
        old_layers: &[WeakLayer],
        new_layers: &[WeakLayer],
    ) {
        if let Some(f) = &*self.on_scene_layers_changed.borrow() {
            f(host, old_layers, new_layers)
        }
    }
}

impl<Host> Debug for Callbacks<Host> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Callbacks")
    }
}



// ==================
// === DirtyFlags ===
// ==================

// === Types ===

type NewParentDirty = dirty::SharedBool<()>;
type ChildrenDirty = dirty::SharedSet<usize, OnDirtyCallback>;
type RemovedChildren<Host> = dirty::SharedVector<WeakInstance<Host>, OnDirtyCallback>;
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
pub struct DirtyFlags<Host> {
    parent:           NewParentDirty,
    children:         ChildrenDirty,
    removed_children: RemovedChildren<Host>,
    transform:        TransformDirty,
    scene_layer:      SceneLayerDirty,
    #[derivative(Debug = "ignore")]
    on_dirty:         Rc<RefCell<Box<dyn Fn()>>>,
}

impl<Host> DirtyFlags<Host> {
    #![allow(trivial_casts)]
    fn new(logger: impl AnyLogger) -> Self {
        let logger: Logger = Logger::new_sub(&logger, "dirty");
        let on_dirty = Rc::new(RefCell::new(Box::new(|| {}) as Box<dyn Fn()>));
        let sub_logger = logger::WarningLogger::new_sub(&logger, "parent");
        let parent = NewParentDirty::new(sub_logger, ());
        let sub_logger = logger::WarningLogger::new_sub(&logger, "children");
        let children = ChildrenDirty::new(sub_logger, on_dirty_callback(&on_dirty));
        let sub_logger = logger::WarningLogger::new_sub(&logger, "removed_children");
        let removed_children = RemovedChildren::new(sub_logger, on_dirty_callback(&on_dirty));
        let sub_logger = logger::WarningLogger::new_sub(&logger, "transform");
        let transform = TransformDirty::new(sub_logger, on_dirty_callback(&on_dirty));
        let sub_logger = logger::WarningLogger::new_sub(&logger, "scene_layer");
        let scene_layer = SceneLayerDirty::new(sub_logger, on_dirty_callback(&on_dirty));
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
/// ## Host
/// The model is parametrized with a `Host`. In real life use cases, host will be instantiated with
/// `Scene`. For the needs of tests, its often instantiated with empty tuple for simplicity. Host
/// has a very important role in decoupling the architecture. You need to provide the `update`
/// method with a reference to the host, which is then passed to `on_show` and `on_hide` callbacks
/// when a particular display objects gets shown or hidden respectively. This can be used for a
/// dynamic management of GPU-side sprites. For example, after adding a display object to a scene,
/// a new sprites can be created to display it visually. After removing the objects, and adding it
/// to a different scene (second GPU context), the sprites in the first context can be removed, and
/// new sprites in the new context can be created. Thus, abstracting over `Host` allows users of
/// this library to define a view model (like few sliders in a box) without the need to contain
/// reference to a particular renderer, and attach the renderer on-demand, when the objects will be
/// placed on the stage.
///
/// Please note, that this functionality is fairly new, and the library do not use it like this yet.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct Model<Host = Scene> {
    host:            PhantomData<Host>,
    /// Layers the object was explicitly assigned to.
    assigned_layers: RefCell<Vec<WeakLayer>>,
    /// Layers where the object is displayed. May be same as assigned layers, or inherited by
    /// parent.
    layers:          RefCell<Vec<WeakLayer>>,
    dirty:           DirtyFlags<Host>,
    callbacks:       Callbacks<Host>,
    parent_bind:     RefCell<Option<ParentBind<Host>>>,
    children:        RefCell<OptVec<WeakInstance<Host>>>,
    transform:       RefCell<CachedTransform>,
    visible:         Cell<bool>,
    logger:          Logger,
}

impl<Host> Model<Host> {
    /// Constructor.
    pub fn new(logger: impl AnyLogger) -> Self {
        let logger = Logger::new_from(logger);
        let parent_bind = default();
        let children = default();
        let transform = default();
        let dirty = DirtyFlags::new(&logger);
        let visible = Cell::new(false);
        let callbacks = default();
        let host = default();
        let assigned_layers = default();
        let layers = default();
        Self {
            host,
            assigned_layers,
            layers,
            dirty,
            callbacks,
            parent_bind,
            children,
            transform,
            visible,
            logger,
        }
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
    pub fn parent(&self) -> Option<Instance<Host>> {
        self.parent_bind.borrow().as_ref().and_then(|t| t.parent())
    }

    /// Count of children objects.
    pub fn children_count(&self) -> usize {
        self.children.borrow().len()
    }

    /// Recompute the transformation matrix of this object and update all of its dirty children.
    pub fn update(&self, host: &Host) {
        let origin0 = Matrix4::identity();
        self.update_with_origin(host, origin0, false, false, &[])
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

    /// Removes the binding to the parent object. Parent is not updated.
    fn unsafe_unset_parent_without_update(&self) {
        info!(self.logger, "Removing parent bind.");
        self.dirty.unset_on_dirty();
        self.dirty.parent.set();
    }
}


// === Update API ===

impl<Host> Model<Host> {
    /// Updates object transformations by providing a new origin location. See docs of `update` to
    /// learn more.
    fn update_with_origin(
        &self,
        host: &Host,
        parent_origin: Matrix4<f32>,
        parent_origin_changed: bool,
        parent_layers_changed: bool,
        parent_layers: &[WeakLayer],
    ) {
        // === Scene Layers Update ===
        let has_new_parent = self.dirty.parent.check();
        let assigned_layers_ref = self.assigned_layers.borrow();
        let assigned_layers = assigned_layers_ref.as_slice();
        let assigned_layers_changed = self.dirty.scene_layer.take().check();
        let has_assigned_layers = !assigned_layers.is_empty();
        let layers_changed = if assigned_layers_changed {
            // We might as well check here if assigned layers were not removed and accidentally the
            // inherited layers are not the same as previously assigned ones, but this is so rare
            // situation that we are not checking it to optimize the performance of this case in
            // most popular cases.
            true
        } else if has_assigned_layers {
            false
        } else if has_new_parent {
            // Optimization for a common case of switching parent in the same layer.
            *self.layers.borrow() != parent_layers
        } else {
            parent_layers_changed
        };

        let new_layers_opt = layers_changed.as_some_from(|| {
            if has_assigned_layers {
                assigned_layers
            } else {
                parent_layers
            }
        });
        if let Some(new_layers) = new_layers_opt {
            debug!(self.logger, "Scene layers changed.", || {
                let old_layers = mem::replace(&mut *self.layers.borrow_mut(), new_layers.to_vec());
                self.callbacks.on_scene_layers_changed(host, &old_layers, new_layers);
            });
        }

        let current_layers = self.layers.borrow();
        let new_layers = new_layers_opt.unwrap_or(&*current_layers);


        // === Origin & Visibility Update ===

        self.update_visibility(host, parent_layers);
        let is_origin_dirty = has_new_parent || parent_origin_changed || layers_changed;
        let new_parent_origin = is_origin_dirty.as_some(parent_origin);
        let parent_origin_label = if new_parent_origin.is_some() { "new" } else { "old" };
        debug!(self.logger, "Update with {parent_origin_label} parent origin.", || {
            let origin_changed = self.transform.borrow_mut().update(new_parent_origin);
            let new_origin = self.transform.borrow().matrix;
            if origin_changed || layers_changed {
                if origin_changed {
                    info!(self.logger, "Self origin changed.");
                } else {
                    info!(self.logger, "Self origin did not change, but the layers changed");
                }
                self.callbacks.on_updated(self);
                if !self.children.borrow().is_empty() {
                    debug!(self.logger, "Updating all children.", || {
                        self.children.borrow().iter().for_each(|weak_child| {
                            weak_child.upgrade().for_each(|child| {
                                child.update_with_origin(
                                    host,
                                    new_origin,
                                    true,
                                    layers_changed,
                                    new_layers,
                                )
                            });
                        });
                    })
                }
            } else {
                info!(self.logger, "Self origin and layers did not change.");
                if self.dirty.children.check_all() {
                    debug!(self.logger, "Updating dirty children.", || {
                        self.dirty.children.take().iter().for_each(|ix| {
                            self.children
                                .borrow()
                                .safe_index(*ix)
                                .and_then(|t| t.upgrade())
                                .for_each(|child| {
                                    child.update_with_origin(
                                        host,
                                        new_origin,
                                        false,
                                        layers_changed,
                                        new_layers,
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
    fn update_visibility(&self, host: &Host, parent_layers: &[WeakLayer]) {
        self.take_removed_children_and_update_their_visibility(host);
        let parent_changed = self.dirty.parent.check();
        if parent_changed && !self.is_orphan() {
            self.set_vis_true(host, parent_layers)
        }
    }

    fn take_removed_children_and_update_their_visibility(&self, host: &Host) {
        if self.dirty.removed_children.check_all() {
            debug!(self.logger, "Updating removed children.", || {
                for child in self.dirty.removed_children.take().into_iter() {
                    if let Some(child) = child.upgrade() {
                        if !child.has_visible_parent() {
                            child.set_vis_false(host);
                        }
                        // Even if the child is visible at this point, it does not mean that it
                        // should be visible after the entire update. Therefore, we must ensure that
                        // "removed children" lists in its subtree will be managed.
                        // See also test `visibility_test3`.
                        child.take_removed_children_and_update_their_visibility(host);
                    }
                }
            })
        }
    }

    fn set_vis_false(&self, host: &Host) {
        if self.visible.get() {
            info!(self.logger, "Hiding.");
            self.visible.set(false);
            self.callbacks.on_hide(host);
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.set_vis_false(host));
            });
        }
    }

    fn set_vis_true(&self, host: &Host, parent_layers: &[WeakLayer]) {
        if !self.visible.get() {
            info!(self.logger, "Showing.");
            let this_scene_layers = self.assigned_layers.borrow();
            let this_scene_layers_slice = this_scene_layers.as_slice();
            let layers = if this_scene_layers_slice.is_empty() {
                parent_layers
            } else {
                this_scene_layers_slice
            };
            self.visible.set(true);
            self.callbacks.on_show(host, layers);
            self.children.borrow().iter().for_each(|child| {
                child.upgrade().for_each(|t| t.set_vis_true(host, layers));
            });
        }
    }
}


// === Register / Unregister ===

impl<Host> Model<Host> {
    fn register_child<T: Object<Host>>(&self, child: &T) -> usize {
        let index = self.children.borrow_mut().insert(child.weak_display_object());
        self.dirty.children.set(index);
        index
    }

    /// Removes and returns the parent bind. Please note that the parent is not updated as long as
    /// the parent bind is not dropped.
    fn take_parent_bind(&self) -> Option<ParentBind<Host>> {
        self.parent_bind.borrow_mut().take()
    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    fn set_parent_bind(&self, bind: ParentBind<Host>) {
        info!(self.logger, "Adding new parent bind.");
        if let Some(parent) = bind.parent() {
            let index = bind.index;
            let dirty = parent.dirty.children.clone_ref();
            self.dirty.set_on_dirty(move || dirty.set(index));
            self.dirty.parent.set();
            *self.parent_bind.borrow_mut() = Some(bind);
        }
    }
}


// === Getters ===

impl<Host> Model<Host> {
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

impl<Host> Model<Host> {
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

    /// Sets a callback which will be called with a reference to the display object when the object
    /// will be updated.
    pub fn set_on_updated<F>(&self, f: F)
    where F: Fn(&Model<Host>) + 'static {
        self.callbacks.on_updated.set(Box::new(f))
    }

    /// Sets a callback which will be called with a reference to scene when the object will be
    /// shown (attached to visible display object graph).
    pub fn set_on_show<F>(&self, f: F)
    where F: Fn(&Host, &[WeakLayer]) + 'static {
        self.callbacks.on_show.set(Box::new(f))
    }

    /// Sets a callback which will be called with a reference to scene when the object will be
    /// hidden (detached from display object graph).
    pub fn set_on_hide<F>(&self, f: F)
    where F: Fn(&Host) + 'static {
        self.callbacks.on_hide.set(Box::new(f))
    }

    /// Sets a callback which will be called on every change to the layers assignment. The callback
    /// will be provided with a reference to scene and two lists of layers this object was
    /// previously and is currently attached to .
    pub fn set_on_scene_layer_changed<F>(&self, f: F)
    where F: Fn(&Host, &[WeakLayer], &[WeakLayer]) + 'static {
        self.callbacks.on_scene_layers_changed.set(Box::new(f))
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

/// A hierarchical representation of object containing information about transformation in 3D space,
/// list of children, and set of utils for dirty flag propagation.
///
/// ## Host
/// The structure is parametrized with a `Host`. In real life use cases, host will be instantiated
/// with [`Scene`]. For simplicity, it is instantiated to empty tuple in tests. Host has a very
/// important role in decoupling the architecture. You need to provide the `update` method with a
/// reference to the host, which is then passed to `on_show` and `on_hide` callbacks when a
/// particular display objects gets shown or hidden respectively. This can be used for a dynamic
/// management of GPU-side sprites. For example, after adding a display object to a scene, a new
/// sprites can be created to display it visually. After removing the objects, and adding it to a
/// different scene (second GPU context), the sprites in the first context can be removed, and new
/// sprites in the new context can be created. Thus, abstracting over `Host` allows users of this
/// library to define a view model (like few sliders in a box) without the need to contain reference
/// to a particular renderer, and attach the renderer on-demand, when the objects will be placed on
/// the stage.
///
/// ## Scene Layers
/// Each display object instance contains an optional list of [`scene::LayerId`]. During object
/// update, the list is passed from parent display objects to their children as long as the child
/// does not override it (is assigned with [`None`]). Similar to [`Host`], the scene layers list
/// plays a very important role in decoupling the architecture. It allows objects and their children
/// to be assigned to a particular [`scene::Layer`], and thus allows for easy to use depth
/// management.
///
/// ## Future Development
/// Please note, that currently, the design is abstract over [`Host`], but it is not abstract over
/// scene layers. This may change in the future, but first, the [`Scene`] implementation has to be
/// refactored to allow the creation of [`Symbol`]s without requirement of a [`Scene`] instance
/// existence. See this ticket to learn more: https://github.com/enso-org/ide/issues/1129 .

#[derive(Derivative)]
#[derive(CloneRef)]
#[derivative(Clone(bound = ""))]
pub struct Instance<Host = Scene> {
    rc: Rc<Model<Host>>,
}

impl<Host> Deref for Instance<Host> {
    type Target = Rc<Model<Host>>;
    fn deref(&self) -> &Self::Target {
        &self.rc
    }
}

impl<Host> Instance<Host> {
    /// Constructor.
    pub fn new(logger: impl AnyLogger) -> Self {
        Self { rc: Rc::new(Model::new(logger)) }
    }

    /// Create a new weak pointer to this display object instance.
    pub fn downgrade(&self) -> WeakInstance<Host> {
        let weak = Rc::downgrade(&self.rc);
        WeakInstance { weak }
    }
}


// === Public API ==

impl<Host> Instance<Host> {
    /// ID getter of this display object.
    pub fn _id(&self) -> Id {
        Id(Rc::downgrade(&self.rc).as_ptr() as *const () as usize)
    }

    /// Get the layers where this object is displayed. May be equal to layers it was explicitly
    /// assigned, or layers inherited from the parent.
    pub fn _display_layers(&self) -> Vec<WeakLayer> {
        self.layers.borrow().clone()
    }

    /// Add this object to the provided scene layer and remove it from all other layers. Do not use
    /// this method explicitly. Use layers' methods instead.
    pub(crate) fn add_to_display_layer(&self, layer: &Layer) {
        let layer = layer.downgrade();
        self.dirty.scene_layer.set();
        let mut layers = self.assigned_layers.borrow_mut();
        if !layers.contains(&layer) {
            layers.push(layer);
        }
    }

    /// Add this object to the provided scene layer and remove it from all other layers. Do not use
    /// this method explicitly. Use layers' methods instead.
    pub(crate) fn add_to_display_layer_exclusive(&self, layer: &Layer) {
        let layer = layer.downgrade();
        self.dirty.scene_layer.set();
        *self.assigned_layers.borrow_mut() = vec![layer];
    }

    /// Remove this object from the provided scene layer. Do not use this method explicitly. Use
    /// layers' methods instead.
    pub(crate) fn remove_from_scene_layer(&self, layer: &Layer) {
        let layer = layer.downgrade();
        self.dirty.scene_layer.set();
        self.assigned_layers.borrow_mut().remove_item(&layer);
    }

    /// Adds a new `Object` as a child to the current one.
    pub fn _add_child<T: Object<Host>>(&self, child: &T) {
        self.clone_ref().add_child_take(child);
    }

    /// Adds a new `Object` as a child to the current one. This is the same as `add_child` but takes
    /// the ownership of `self`.
    pub fn add_child_take<T: Object<Host>>(self, child: &T) {
        info!(self.rc.logger, "Adding new child.");
        let child = child.display_object();
        child.unset_parent();
        let index = self.register_child(child);
        info!(self.rc.logger, "Child index is {index}.");
        let parent_bind = ParentBind { parent: self.downgrade(), index };
        child.set_parent_bind(parent_bind);
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    pub fn _remove_child<T: Object<Host>>(&self, child: &T) {
        let child = child.display_object();
        if self.has_child(child) {
            child.unset_parent()
        }
    }

    /// Replaces the parent binding with a new parent.
    pub fn set_parent<T: Object<Host>>(&self, parent: &T) {
        parent.display_object().add_child(self);
    }

    /// Removes the current parent binding.
    pub fn _unset_parent(&self) {
        self.take_parent_bind();
    }

    /// Checks if the provided object is child of the current one.
    pub fn has_child<T: Object<Host>>(&self, child: &T) -> bool {
        self.child_index(child).is_some()
    }

    /// Checks if the object has a parent.
    pub fn _has_parent(&self) -> bool {
        self.rc.parent_bind.borrow().is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T: Object<Host>>(&self, child: &T) -> Option<usize> {
        let child = child.display_object();
        child.parent_bind.borrow().as_ref().and_then(|bind| {
            if bind.parent().as_ref() == Some(self) {
                Some(bind.index)
            } else {
                None
            }
        })
    }
}


// === Private API ===

impl<Host> Instance<Host> {
    fn parent_index(&self) -> Option<usize> {
        self.parent_bind.borrow().as_ref().map(|t| t.index)
    }

    fn has_visible_parent(&self) -> bool {
        let parent = self.parent_bind.borrow().as_ref().and_then(|b| b.parent.upgrade());
        parent.map_or(false, |parent| parent.is_visible())
    }
}


// === Instances ===

impl<Host> PartialEq for Instance<Host> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }
}

impl<Host> Display for Instance<Host> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Instance")
    }
}

impl<Host> Debug for Instance<Host> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DisplayObject({})", self.logger.path())
    }
}



// ====================
// === WeakInstance ===
// ====================

/// Weak display object instance. Will be dropped if no all strong instances are dropped.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct WeakInstance<Host> {
    weak: Weak<Model<Host>>,
}

impl<Host> WeakInstance<Host> {
    /// Upgrade the weak instance to strong one if it was not yet dropped.
    pub fn upgrade(&self) -> Option<Instance<Host>> {
        self.weak.upgrade().map(|rc| Instance { rc })
    }

    /// Checks whether this weak instance still exists (its strong instance was not dropped yet).
    pub fn exists(&self) -> bool {
        self.upgrade().is_some()
    }
}

impl<Host> PartialEq for WeakInstance<Host> {
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
pub trait Object<Host = Scene> {
    fn display_object(&self) -> &Instance<Host>;
    fn weak_display_object(&self) -> WeakInstance<Host> {
        self.display_object().downgrade()
    }

    /// See `Any` description.
    fn into_any(self) -> Any<Host>
    where Self: Sized + 'static {
        Any { wrapped: Rc::new(self) }
    }
}

impl<Host> Object<Host> for Instance<Host> {
    fn display_object(&self) -> &Instance<Host> {
        self
    }
}

impl<Host, T: Object<Host>> Object<Host> for &T {
    fn display_object(&self) -> &Instance<Host> {
        let t: &T = *self;
        t.display_object()
    }
}



// =================
// === ObjectOps ===
// =================

impl<Host, T: Object<Host>> ObjectOps<Host> for T {}

/// Implementation of operations available for every struct which implements `display::Object`.
/// To learn more about the design, please refer to the documentation of [`Instance`].
//
// HOTFIX[WD]: We are using names with underscores in order to fix this bug:
// https://github.com/rust-lang/rust/issues/70727 . To be removed as soon as the bug is fixed.
#[allow(missing_docs)]
pub trait ObjectOps<Host = Scene>: Object<Host> {
    /// Globally unique identifier of this display object.
    fn id(&self) -> Id {
        self.display_object()._id()
    }

    /// Get the layers where this object is displayed. May be equal to layers it was explicitly
    /// assigned, or layers inherited from the parent.
    fn display_layers(&self) -> Vec<WeakLayer> {
        self.display_object()._display_layers()
    }

    /// Add another display object as a child to this display object. Children will inherit all
    /// transformations of their parents.
    fn add_child<T: Object<Host>>(&self, child: &T) {
        self.display_object()._add_child(child.display_object());
    }

    /// Remove the display object from the children list of this display object. Does nothing if
    /// the child was not registered.
    fn remove_child<T: Object<Host>>(&self, child: &T) {
        self.display_object()._remove_child(child.display_object());
    }

    /// Removes this display object from its parent's children list.
    fn unset_parent(&self) {
        self.display_object()._unset_parent();
    }

    /// Check whether this display object is attached to a parent.
    fn has_parent(&self) -> bool {
        self.display_object()._has_parent()
    }

    /// Checks whether the object is visible.
    fn is_visible(&self) -> bool {
        self.display_object().rc.is_visible()
    }

    /// Checks whether the object is orphan (do not have parent object attached).
    fn is_orphan(&self) -> bool {
        self.display_object().rc.is_orphan()
    }


    // === Transform ===

    fn transform_matrix(&self) -> Matrix4<f32> {
        self.display_object().rc.matrix()
    }

    fn global_position(&self) -> Vector3<f32> {
        self.display_object().rc.global_position()
    }


    // === Position ===

    fn position(&self) -> Vector3<f32> {
        self.display_object().rc.position()
    }

    fn mod_position<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.display_object().rc.mod_position(f)
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
        self.display_object().rc.set_position(t);
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
        self.display_object().rc.scale()
    }

    fn mod_scale<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.display_object().rc.mod_scale(f)
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
        self.display_object().rc.set_scale(t);
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
        self.display_object().rc.rotation()
    }

    fn mod_rotation<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
        self.display_object().rc.mod_rotation(f)
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
        self.display_object().rc.set_rotation(t);
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



// ==================
// === Any Object ===
// ==================

/// A structure wrapping any `Object` and hiding the exact type.
///
/// You can convert structure into `Any` using `Object::into_any`. Unfortunately it is not possible
/// to make general `From` implementation, because `Any` itself would use it as well, and it clashes
/// with base implementation `From<T> for T`.
#[derive(CloneRef)]
pub struct Any<Host = Scene> {
    wrapped: Rc<dyn Object<Host>>,
}

impl<Host> Clone for Any<Host> {
    fn clone(&self) -> Self {
        Self { wrapped: self.wrapped.clone() }
    }
}

impl<Host> Debug for Any<Host> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "display::object::Any")
    }
}

impl<Host> Object<Host> for Any<Host> {
    fn display_object(&self) -> &Instance<Host> {
        self.wrapped.display_object()
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
        let node1 = Instance::<()>::new(Logger::new("node1"));
        let node2 = Instance::<()>::new(Logger::new("node2"));
        let node3 = Instance::<()>::new(Logger::new("node3"));
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
        let node1 = Instance::<()>::new(Logger::new("node1"));
        let node2 = Instance::<()>::new(Logger::new("node2"));
        let node3 = Instance::<()>::new(Logger::new("node3"));
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

        node1.update(&());
        assert_eq!(node1.position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 0.0, 0.0));

        node2.mod_position(|t| t.y += 5.0);
        node1.update(&());
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 5.0, 0.0));

        node3.mod_position(|t| t.x += 1.0);
        node1.update(&());
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(8.0, 5.0, 0.0));

        node2.mod_rotation(|t| t.z += PI / 2.0);
        node1.update(&());
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));

        node1.add_child(&node3);
        node1.update(&());
        assert_eq!(node3.global_position(), Vector3::new(8.0, 0.0, 0.0));

        node1.remove_child(&node3);
        node3.update(&());
        assert_eq!(node3.global_position(), Vector3::new(1.0, 0.0, 0.0));

        node2.add_child(&node3);
        node1.update(&());
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));

        node1.remove_child(&node3);
        node1.update(&());
        node2.update(&());
        node3.update(&());
        assert_eq!(node3.global_position(), Vector3::new(7.0, 6.0, 0.0));
    }

    #[test]
    fn parent_test() {
        let node1 = Instance::<()>::new(Logger::new("node1"));
        let node2 = Instance::<()>::new(Logger::new("node2"));
        let node3 = Instance::<()>::new(Logger::new("node3"));
        node1.add_child(&node2);
        node1.add_child(&node3);
        node2.unset_parent();
        node3.unset_parent();
        assert_eq!(node1.children_count(), 0);
    }

    /// A utility to test display object instances' visibility.
    #[derive(Clone, CloneRef, Debug)]
    struct TestedNode {
        node:         Instance<()>,
        show_counter: Rc<Cell<usize>>,
        hide_counter: Rc<Cell<usize>>,
    }

    impl Deref for TestedNode {
        type Target = Instance<()>;
        fn deref(&self) -> &Self::Target {
            &self.node
        }
    }

    impl Object<()> for TestedNode {
        fn display_object(&self) -> &Instance<()> {
            &self.node
        }
    }

    impl TestedNode {
        fn new(label: impl Into<ImString>) -> Self {
            let node = Instance::<()>::new(Logger::new(label));
            let show_counter = Rc::<Cell<usize>>::default();
            let hide_counter = Rc::<Cell<usize>>::default();
            node.set_on_show(f__!(show_counter.set(show_counter.get() + 1)));
            node.set_on_hide(f_!(hide_counter.set(hide_counter.get() + 1)));
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
        let node1 = TestedNode::new("node1");
        let node2 = TestedNode::new("node2");
        let node3 = TestedNode::new("node3");
        node1.force_set_visibility(true);
        node3.check_if_still_hidden();
        node3.update(&());
        node3.check_if_still_hidden();

        node1.add_child(&node2);
        node2.add_child(&node3);
        node1.update(&());
        node3.check_if_was_shown();

        node3.unset_parent();
        node3.check_if_still_shown();

        node1.update(&());
        node3.check_if_was_hidden();

        node1.add_child(&node3);
        node1.update(&());
        node3.check_if_was_shown();

        node2.add_child(&node3);
        node1.update(&());
        node3.check_if_still_shown();

        node3.unset_parent();
        node1.update(&());
        node3.check_if_was_hidden();

        node2.add_child(&node3);
        node1.update(&());
        node3.check_if_was_shown();
    }

    #[test]
    fn visibility_test2() {
        let node1 = TestedNode::new("node1");
        let node2 = TestedNode::new("node2");
        node1.check_if_still_hidden();
        node1.update(&());
        node1.check_if_still_hidden();
        node1.force_set_visibility(true);
        node1.update(&());
        node1.check_if_still_shown();

        node1.add_child(&node2);
        node1.update(&());
        node1.check_if_still_shown();
        node2.check_if_was_shown();
    }

    #[test]
    fn visibility_test3() {
        let node1 = TestedNode::new("node1");
        let node2 = TestedNode::new("node2");
        let node3 = TestedNode::new("node3");
        node1.force_set_visibility(true);
        node1.add_child(&node2);
        node2.add_child(&node3);
        node1.update(&());
        node2.check_if_was_shown();
        node3.check_if_was_shown();

        node3.unset_parent();
        node3.add_child(&node2);
        node1.update(&());
        node2.check_if_was_hidden();
        node3.check_if_was_hidden();
    }

    #[test]
    fn visibility_test4() {
        let node1 = TestedNode::new("node1");
        let node2 = TestedNode::new("node2");
        let node3 = TestedNode::new("node3");
        let node4 = TestedNode::new("node4");
        node1.force_set_visibility(true);
        node1.add_child(&node2);
        node2.add_child(&node3);
        node1.update(&());
        node2.check_if_was_shown();
        node3.check_if_was_shown();
        node4.check_if_still_hidden();

        node2.unset_parent();
        node1.add_child(&node2);
        node1.update(&());
        node2.check_if_still_shown();
        node3.check_if_still_shown();
        node4.check_if_still_hidden();

        node1.add_child(&node4);
        node4.add_child(&node3);
        node1.update(&());
        node2.check_if_still_shown();
        // TODO[ao]: This assertion fails, see https://github.com/enso-org/ide/issues/1405
        // node3.check_if_still_shown();
        node3.reset_counters();
        node4.check_if_was_shown();

        node4.unset_parent();
        node2.unset_parent();
        node1.update(&());
        node2.check_if_was_hidden();
        node3.check_if_was_hidden();
        node4.check_if_was_hidden();

        node2.add_child(&node3);
        node1.update(&());
        node2.check_if_still_hidden();
        node3.check_if_still_hidden();
        node4.check_if_still_hidden();
    }


    #[test]
    fn deep_hierarchy_test() {
        // === Init ===

        let world = Instance::<()>::new(Logger::new("world"));
        let node1 = Instance::<()>::new(Logger::new("node1"));
        let node2 = Instance::<()>::new(Logger::new("node2"));
        let node3 = Instance::<()>::new(Logger::new("node3"));
        let node4 = Instance::<()>::new(Logger::new("node4"));
        let node5 = Instance::<()>::new(Logger::new("node5"));
        let node6 = Instance::<()>::new(Logger::new("node6"));

        world.force_set_visibility(true);

        world.add_child(&node1);
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

        world.update(&());

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

        world.update(&());

        assert_eq!(node1.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(0.0, 0.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(1.0, 0.0, 0.0));
        assert_eq!(node4.global_position(), Vector3::new(4.0, 0.0, 0.0));
        assert_eq!(node5.global_position(), Vector3::new(9.0, 0.0, 0.0));
        assert_eq!(node6.global_position(), Vector3::new(16.0, 0.0, 0.0));


        // === Visibility Modification  ===

        node4.unset_parent();
        node3.unset_parent();
        world.update(&());

        assert!(!node3.is_visible());
        assert!(!node4.is_visible());
        assert!(!node5.is_visible());
        assert!(!node6.is_visible());
    }

    #[test]
    fn layers_test() {
        let layer1 = Layer::new(Logger::new("0"));
        let layer2 = Layer::new(Logger::new("1"));
        let node1 = Instance::<()>::new(Logger::new("node1"));
        let node2 = Instance::<()>::new(Logger::new("node2"));
        let node3 = Instance::<()>::new(Logger::new("node3"));
        node1.add_child(&node2);
        node1.add_child(&node3);
        node1.update(&());
        assert_eq!(node1.display_layers(), vec![]);
        assert_eq!(node2.display_layers(), vec![]);
        assert_eq!(node3.display_layers(), vec![]);

        node1.add_to_display_layer(&layer1);
        node1.update(&());
        assert_eq!(node1.display_layers(), vec![layer1.downgrade()]);
        assert_eq!(node2.display_layers(), vec![layer1.downgrade()]);
        assert_eq!(node3.display_layers(), vec![layer1.downgrade()]);

        node2.add_to_display_layer_exclusive(&layer2);
        node1.update(&());
        assert_eq!(node1.display_layers(), vec![layer1.downgrade()]);
        assert_eq!(node2.display_layers(), vec![layer2.downgrade()]);
        assert_eq!(node3.display_layers(), vec![layer1.downgrade()]);
    }
}
