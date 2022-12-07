//! Implementation of display objects, elements that have visual representation and can form
//! hierarchical layouts. The implementation is very careful about performance, it tracks the
//! transformation changes and updates only the needed subset of the display object tree on demand.

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::display::object::event;
use crate::display::object::transformation;
use crate::display::scene::layer::Layer;
use crate::display::scene::layer::WeakLayer;
use crate::display::scene::Scene;

use data::opt_vec::OptVec;
use nalgebra::Matrix4;
use nalgebra::Vector3;
use transformation::CachedTransformation;



// ==========
// === Id ===
// ==========

/// Globally unique identifier of a display object.
#[derive(
    Clone, CloneRef, Copy, Debug, Default, Display, Eq, From, Hash, Into, PartialEq, Ord,
    PartialOrd
)]
pub struct Id(usize);

/// The index of a child of a display object.
#[derive(
    Clone, CloneRef, Copy, Debug, Default, Deref, Display, Eq, From, Hash, Into, PartialEq, Ord,
    PartialOrd
)]
pub struct ChildIndex(usize);



// =============
// === Model ===
// =============

/// Display objects are essential structures used to build elements visible on the screen. They are
/// used to build objects hierarchy, computing elements transformations within this hierarchy
/// (position, rotation, and scale), passing events trough that hierarchy, and layouting the
/// elements on the screen (e.g. with horizontal or vertical layout).
///
/// ## Lazy updates of display objects
/// Some operations on display objects are very expensive. For example, after moving the root object
/// of a hierarchy, the matrix transformations of all its children, their children, etc. need to be
/// updated. That's why these operations are performed in a lazy way. After an element is
/// transformed, or when the hierarchy is modified, the change information is propagated up to the
/// root of the hierarchy and is updated once per frame, after the [`update`] function is called
/// (usually, it is called by the [`Scene`]). Emitting events is not done in a lazy fashion, as they
/// do not require passing the event down the hierarchy. Instead, the event is passed up the
/// hierarchy, from the object the event was emitted on all way to the root of the hierarchy.
///
/// ## Scene Layers
/// Every display object can be assigned to a [`scene::Layer`]. During object update, the assignment
/// information is passed down the hierarchy. If an object was not assigned to a layer explicitly,
/// it will inherit the assignment from its parent object, if any. This means that adding an object
/// to a layer will also move all of its children there, until they are assigned with a different
/// layer explicitly.
///
///
///
/// # Size and layout
/// Display objects can position its children automatically and then, recompute their size based on
/// the children bounding box. There are three layouts available: manual, horizontal, and vertical.
///
/// ## Resizing
/// Every display object can be configured with a horizontal and vertical resizing mode. There are
/// three modes available:
/// - `Hug` (default). In this mode, the display object size will be set to the bounding box of its
///   children. In case of no children, the display object will be resized to (0,0). The only
///   exception is setting the hug resizing on a display object with a manual layout. Then, its size
///   will be set to the children bounding box clipped to the positive X- and Y-axis.
/// - `Fill`. In this mode, the display object will fill the free space in its parent. If several
///   children are set to this mode, the free space will be divided evenly between them. If the
///   parent resizing was set to `Hug`, all its children with resizing set to `Fill` will be resized
///   to 0.
/// - `Fixed`. In this mode, the display object size will be fixed. The size of children can be
///   smaller or bigger than the size of the parent object.
///
/// ## Horizontal and vertical auto-layouts
/// The auto-layout can be set to either horizontal or vertical one. Each layout can be configured
/// with the following options:
/// - `alignment`. The alignment of the children. For example, it allows to align the children to
///   the right edge of their parent. The alignment is covered in detail in the following section.
/// - `spacing`. The space between children.
/// - `padding`. The space between the children and the edge of the parent.
/// - `reversed`. A flag indicating whether the children should be placed in a reversed order. By
///   default, children are placed from left to right or from bottom to top.
///
/// ## Alignment
/// The alignment of the children can be set to one of two modes: packed and spaced. In the packed
/// mode, the children will be placed next to each other and will be separated with the `spacing`
/// value. In the spaced mode, the children will be placed as far away from each other as possible
/// in order to fill the whole space of the parent display object. In case the parent display object
/// size is smaller than the cumulative size of its children, the children will overlap.
///
/// ## Layout documentation
/// Documentation of the layout uses drawings with graphical symbols for different resizing and
/// layout modes. For example, the illustration below shows a ROOT display object with a horizontal
/// layout and fixed resizing. The L child has a horiontal layout, fill horizontal resizing, and a
/// fixed vertical resizing. The R child has a vertical layout, hug horizontal resizing, and a fill
/// vertical resizing.
///
/// ```text
/// ╭▷ ROOT ──────────────────────────╮
/// │   ╭▷ L ◀ ▶ ──╮   ╭R─ ▶ ◀ ───╮   │   Auto-layout Legend:         
/// │   │ ╭ ◀ ▶ ╮  │   ▽ ╭────╮   │   │   ┄── ▷ ──┄ : Horizontal auto-layout.
/// │   │ │ L1  │  │   │ │ R2 ▲   │   │   ┄── ▽ ──┄ : Vertical auto-layout.
/// │   │ │     │  │   │ │    ▼   │   │   ┄───────┄ : Manual layout.   
/// │   │ │     │  ▼   │ ╰────╯   ▲   │
/// │   │ │     │  ▲   │ ╭────╮   ▼   │   Resizing Legend:             
/// │   │ │     │  │   │ │ R1 ▲   │   │   ┄── ◀ ▶ ──┄ : Fill resizing.
/// │   │ │     │  │   │ │    ▼   │   │   ┄── ▶ ◀ ──┄ : Hug resizing.  
/// │   │ ╰─────╯  │   │ ╰────╯   │   │   ┄─────────┄ : Fixed resizing.
/// │   ╰──────────╯   ╰──────────╯   │
/// ╰─────────────────────────────────╯
/// ```
#[derive(Derivative)]
#[derive(CloneRef, Deref, From)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
#[repr(transparent)]
pub struct Instance {
    def: InstanceDef,
}

/// Internal representation of [`Instance`]. It exists only to make the implementation less
/// error-prone. The [`ObjectOps`] trait defines the public API of display objects, such as the
/// [`add_child`] method, and it is automatically defined for every struct that implements
/// the [`Object`] trait, including the [`Instance`]. Without this struct, the [`add_child`] method
/// would need to be implemented as [`self.display_object().add_child(child)`]. Such an
/// implementation will be very error-prone. After renaming the function in [`Instance`], the
/// [`ObjectOps`] trait would still compile, but its function will call itself infinitely (this is
/// not caught by rustc yet: https://github.com/rust-lang/rust/issues/57965). This struct allows the
/// implementation to be written as [`self.display_object().def.add_child(child)`] instead, which
/// will fail to compile after renaming the function in [`InstanceDef`].
#[derive(Derivative)]
#[derive(CloneRef, Deref)]
#[derivative(Clone(bound = ""))]
#[repr(transparent)]
pub struct InstanceDef {
    rc: Rc<Model>,
}

/// A display object model. See the documentation of [`Instance`] to learn more.
#[derive(Debug, Deref)]
pub struct Model {
    /// This is the display object's FRP network. Feel free to extend it with new FRP nodes as long
    /// as they are inherently bound with this display object. For example, a sprite, which owns a
    /// display object instance, can extend this network to perform computations. However, you
    /// should not extend it if you don't own the display object, as nodes created in this network
    /// may survive the lifetime of other objects causing memory leaks. See the docs of FRP to
    /// learn more.
    pub network: frp::Network,

    pub name: &'static str,

    #[deref]
    hierarchy: HierarchyModel,
    event:     EventModel,
    layout:    RefCell<LayoutModel>,
}


// === Contructors ===

impl Instance {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Constructor.
    pub fn new_named(name: &'static str) -> Self {
        Self { def: InstanceDef::new_named(name) }
    }
}

impl InstanceDef {
    /// Constructor.
    pub fn new() -> Self {
        Self { rc: Rc::new(Model::new()) }.init_events_handling()
    }

    /// Constructor.
    pub fn new_named(name: &'static str) -> Self {
        Self { rc: Rc::new(Model::new_named(name)) }.init_events_handling()
    }

    /// ID getter of this display object.
    pub fn id(&self) -> Id {
        Id(Rc::downgrade(&self.rc).as_ptr() as *const () as usize)
    }
}

impl Model {
    /// Constructor.
    pub fn new() -> Self {
        Self::new_named("UnnamedDisplayObject")
    }

    /// Constructor.
    pub fn new_named(name: &'static str) -> Self {
        let network = frp::Network::new("display_object");
        let hierarchy = HierarchyModel::new(&network);
        let event = EventModel::new(&network);
        let layout = RefCell::new(LayoutModel::new());
        Self { network, hierarchy, event, layout, name }
    }
}


// === Impls ===

impl Default for InstanceDef {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for Model {
    fn default() -> Self {
        Self::new()
    }
}

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



// ============
// === Root ===
// ============

/// A root element of a display object hierarchy. Unlike [`Instance`], [`Root`] is visible by
/// default and has explicit methods to hide and show it.
#[derive(Clone, CloneRef, Debug, Deref)]
#[repr(transparent)]
pub struct Root {
    def: Instance,
}

impl Root {
    /// Constructor.
    pub fn new() -> Self {
        let def = default();
        Self { def }.init()
    }

    pub fn new_named(name: &'static str) -> Self {
        let def = Instance::new_named(name);
        Self { def }.init()
    }

    fn init(self) -> Self {
        self.show();
        self
    }

    /// Hide the display object.
    pub fn hide(&self) {
        self.def.hide()
    }

    /// Show the display object.
    pub fn show(&self) {
        self.def.show()
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::new()
    }
}

impl Object for Root {
    fn display_object(&self) -> &Instance {
        &self.def
    }
}



// =================================================================================================
// === Hierarchy ===================================================================================
// =================================================================================================

// ==================
// === ParentBind ===
// ==================

/// A parent-child binding. It contains reference to parent node and information about the child
/// index. When dropped, it removes the child from its parent.
#[derive(Debug)]
pub struct ParentBind {
    /// The parent's child index. If this is a binding stored by [`Instance`], this will be the
    /// instance index in the parent's instance vector.
    child_index: ChildIndex,
    parent:      WeakInstance,
}

impl ParentBind {
    fn parent(&self) -> Option<Instance> {
        self.parent.upgrade()
    }
}

impl Drop for ParentBind {
    fn drop(&mut self) {
        if let Some(parent) = self.parent() {
            if let Some(weak_child) = parent.children.borrow_mut().remove(*self.child_index) {
                parent.dirty.modified_children.unset(&self.child_index);
                if let Some(child) = weak_child.upgrade() {
                    child.dirty.new_parent.set();
                    parent.dirty.removed_children.set(weak_child);
                }
            }
        }
    }
}



// ========================
// === SharedParentBind ===
// ========================

/// A shared version of [`Option<ParentBind>`].
#[derive(Clone, CloneRef, Debug, Default)]
pub struct SharedParentBind {
    data: Rc<RefCell<Option<ParentBind>>>,
}

impl SharedParentBind {
    fn is_none(&self) -> bool {
        self.data.borrow().is_none()
    }

    fn is_some(&self) -> bool {
        self.data.borrow().is_some()
    }

    fn set_bind(&self, bind: ParentBind) {
        *self.data.borrow_mut() = Some(bind)
    }

    fn take_bind(&self) -> Option<ParentBind> {
        self.data.borrow_mut().take()
    }

    fn parent(&self) -> Option<Instance> {
        self.data.borrow().as_ref().and_then(|t| t.parent())
    }

    fn parent_and_child_index(&self) -> Option<(Instance, ChildIndex)> {
        self.data.borrow().as_ref().and_then(|t| t.parent().map(|s| (s, t.child_index)))
    }

    fn child_index(&self) -> Option<ChildIndex> {
        self.data.borrow().as_ref().map(|t| t.child_index)
    }
}



// ===================
// === Dirty Flags ===
// ===================

/// Dirty flags.
pub mod dirty {
    pub use super::*;

    // === Types ===

    type NewParent = crate::data::dirty::RefCellBool<()>;
    type ModifiedChildren = crate::data::dirty::RefCellSet<ChildIndex, OnDirtyCallback>;
    type RemovedChildren = crate::data::dirty::RefCellVector<WeakInstance, OnDirtyCallback>;
    type Transformation = crate::data::dirty::RefCellBool<OnDirtyCallback>;
    type SceneLayer = crate::data::dirty::RefCellBool<OnDirtyCallback>;


    // === Definition ===

    /// A set of dirty flags encoding which hierarchy-related properties of a display object have
    /// been changed and not yet updated. See the docs of [`Instance`] to learn more about the lazy
    /// update mechanism.
    ///
    /// # Performance
    /// Let's consider a deep tree of objects. To render an object, we need its position in the
    /// world-space (global-space). Thus, when the tree root object moves, all of its children,
    /// their sub-children, etc., need to be updated. As there might be hundreds or thousands of
    /// such sub-children, this might be very costly. Even worse, if the user of this library moves
    /// the root object, and then moves its child, all the sub-children of that child would be
    /// recomputed twice if not updated lazily.
    #[derive(Debug)]
    #[allow(missing_docs)]
    pub struct Flags {
        pub new_parent:        NewParent,
        /// A set of children that were added, removed, transformed, moved to a different layer, or
        /// whose ancestors were modified in such a way.
        pub modified_children: ModifiedChildren,
        pub removed_children:  RemovedChildren,
        pub transformation:    Transformation,
        pub new_layer:         SceneLayer,
    }

    impl Flags {
        /// Constructor.
        pub fn new(parent_bind: &SharedParentBind) -> Self {
            let new_parent = NewParent::new(());
            let modified_children = ModifiedChildren::new(on_dirty_callback(parent_bind));
            let removed_children = RemovedChildren::new(on_dirty_callback(parent_bind));
            let transformation = Transformation::new(on_dirty_callback(parent_bind));
            let new_layer = SceneLayer::new(on_dirty_callback(parent_bind));
            Self { new_parent, modified_children, removed_children, transformation, new_layer }
        }

        pub fn check_all(&self) -> bool {
            self.new_parent.check()
                || self.modified_children.check_all()
                || self.removed_children.check_all()
                || self.transformation.check()
                || self.new_layer.check()
        }
    }

    type OnDirtyCallback = impl Fn();
    fn on_dirty_callback(parent_bind: &SharedParentBind) -> OnDirtyCallback {
        let parent_bind = parent_bind.clone_ref();
        move || {
            if let Some((parent, index)) = parent_bind.parent_and_child_index() {
                parent.dirty.modified_children.set(index);
            }
        }
    }
}



// =====================
// === Hierarchy FRP ===
// =====================


// FIXME: maybe we should hide these things somehow, they are too easy accessible.

/// FRP endpoints relate to display object hierarchy modification.
///
/// # WARNING!
/// Do not change the hierarchy of display objects in response to these FRP signals. They are
/// emitted during display object hierarchy update and changing the hierarchy during this update may
/// lead to undefined behavior.
#[derive(Debug)]
pub struct HierarchyFrp {
    /// Fires when the display object is shown. It will fire during the first scene refresh if this
    /// object was invisible and was added as a child to a visible parent.
    pub on_show:            frp::Stream<(Option<Scene>, Option<WeakLayer>)>,
    /// Fires when the display object is hidden. This can happen for example after detaching it
    /// from a visible parent. It will fire during the first scene refresh if this object was
    /// removed from a visible parent or added to an invisible one.
    pub on_hide:            frp::Stream<Option<Scene>>,
    /// Fires during the first scene refresh if this object was moved between scene layers.
    pub on_layer_change:    frp::Stream<(Option<Scene>, Option<WeakLayer>, Option<WeakLayer>)>,
    /// Fires during the first scene refresh if this object needed an update and the update was
    /// performed.
    pub on_updated:         frp::Stream<()>,
    on_show_source:         frp::Source<(Option<Scene>, Option<WeakLayer>)>,
    on_hide_source:         frp::Source<Option<Scene>>,
    on_layer_change_source: frp::Source<(Option<Scene>, Option<WeakLayer>, Option<WeakLayer>)>,
    on_updated_source:      frp::Source<()>,
}

impl HierarchyFrp {
    fn new(network: &frp::Network) -> Self {
        frp::extend! { network
            on_show_source <- source();
            on_hide_source <- source();
            on_layer_change_source <- source();
            on_updated_source <- source();
        }
        let on_show = on_show_source.clone_ref().into();
        let on_hide = on_hide_source.clone_ref().into();
        let on_layer_change = on_layer_change_source.clone_ref().into();
        let on_updated = on_updated_source.clone_ref().into();
        Self {
            on_show_source,
            on_hide_source,
            on_layer_change_source,
            on_updated_source,
            on_show,
            on_hide,
            on_layer_change,
            on_updated,
        }
    }
}



// =======================
// === Hierarchy Model ===
// =======================

/// The part of display object model related to its hierarchy.
#[derive(Debug, Deref)]
pub struct HierarchyModel {
    #[deref]
    frp:            HierarchyFrp,
    visible:        Cell<bool>,
    transformation: RefCell<CachedTransformation>,
    parent_bind:    SharedParentBind,
    children:       RefCell<OptVec<WeakInstance>>,
    /// Layer the object was explicitly assigned to by the user, if any.
    assigned_layer: RefCell<Option<WeakLayer>>,
    /// Layer where the object is displayed. It may be set to by user or inherited from the parent.
    layer:          RefCell<Option<WeakLayer>>,
    dirty:          dirty::Flags,
}

impl HierarchyModel {
    fn new(network: &frp::Network) -> Self {
        let frp = HierarchyFrp::new(network);
        let visible = default();
        let transformation = default();
        let parent_bind = default();
        let children = default();
        let assigned_layer = default();
        let layer = default();
        let dirty = dirty::Flags::new(&parent_bind);
        Self { frp, visible, transformation, parent_bind, children, assigned_layer, layer, dirty }
    }
}



// =======================
// === Hierarchy Logic ===
// =======================

// === Updates and Visibility ===

impl Model {
    /// Get the layer this object is displayed in. May be equal to layer explicitly set by the user
    /// or a layer inherited from the parent.
    fn display_layer(&self) -> Option<Layer> {
        self.layer.borrow().as_ref().and_then(|t| t.upgrade())
    }

    /// Add this object to the provided scene layer. Do not use this method explicitly. Use layers'
    /// methods instead.
    pub(crate) fn add_to_display_layer(&self, layer: &Layer) {
        let layer = layer.downgrade();
        let mut assigned_layer = self.assigned_layer.borrow_mut();
        if assigned_layer.as_ref() != Some(&layer) {
            self.dirty.new_layer.set();
            *assigned_layer = Some(layer);
        }
    }

    /// Remove this object from the provided scene layer. Do not use this method explicitly. Use
    /// layers' methods instead.
    pub(crate) fn remove_from_display_layer(&self, layer: &Layer) {
        let layer = layer.downgrade();
        let mut assigned_layer = self.assigned_layer.borrow_mut();
        if assigned_layer.as_ref() == Some(&layer) {
            self.dirty.new_layer.set();
            *assigned_layer = None;
        }
    }
}

impl Model {
    fn children(&self) -> Vec<Instance> {
        self.children.borrow().iter().filter_map(|t| t.upgrade()).collect()
    }

    /// Checks whether the object is visible.
    pub fn is_visible(&self) -> bool {
        self.visible.get()
    }

    /// Hide the object. This is a helper API. Used by tests and the [`Root`] object.
    fn hide(&self) {
        self.set_vis_false(None)
    }

    /// Show the object. This is a helper API. Used by tests and the [`Root`] object.
    fn show(&self) {
        self.set_vis_true(None, None)
    }

    fn set_vis_false(&self, scene: Option<&Scene>) {
        if self.visible.get() {
            trace!("Hiding.");
            self.visible.set(false);
            self.on_hide_source.emit(scene.cloned());
            self.children
                .borrow()
                .iter()
                .filter_map(|t| t.upgrade())
                .for_each(|t| t.set_vis_false(scene));
        }
    }

    fn set_vis_true(&self, scene: Option<&Scene>, parent_layer: Option<&WeakLayer>) {
        if !self.visible.get() {
            trace!("Showing.");
            self.visible.set(true);
            let assigned_layer_borrow = self.assigned_layer.borrow();
            let assigned_layer = assigned_layer_borrow.as_ref();
            let new_layer = assigned_layer.or(parent_layer);
            self.on_show_source.emit((scene.cloned(), new_layer.cloned()));
            self.children
                .borrow()
                .iter()
                .filter_map(|t| t.upgrade())
                .for_each(|t| t.set_vis_true(scene, new_layer));
        }
    }

    /// Checks whether the object is orphan (do not have parent object attached).
    pub fn has_parent(&self) -> bool {
        self.parent_bind.is_some()
    }

    /// Get reference to the parent object if any.
    pub fn parent(&self) -> Option<Instance> {
        self.parent_bind.parent()
    }

    /// The index of this display object in the parent's children list.
    fn my_index(&self) -> Option<ChildIndex> {
        self.parent_bind.child_index()
    }

    fn has_visible_parent(&self) -> bool {
        self.parent_bind.parent().map_or(false, |parent| parent.is_visible())
    }

    /// Number of children of this object.
    pub fn children_count(&self) -> usize {
        self.children.borrow().len()
    }

    /// Removes and returns the parent bind. Please note that the parent is not updated as long as
    /// the parent bind is not dropped.
    fn take_parent_bind(&self) -> Option<ParentBind> {
        let parent_bind = self.parent_bind.take_bind();
        if let Some(parent) = parent_bind.as_ref().and_then(|t| t.parent.upgrade()) {
            let is_focused = self.event.focused_descendant.borrow().is_some();
            if is_focused {
                parent.propagate_up_no_focus_instance();
            }
        }
        parent_bind
    }

    /// Set parent of the object. If the object already has a parent, the parent would be replaced.
    fn set_parent_bind(&self, bind: ParentBind) {
        trace!("Adding new parent bind.");
        if let Some(parent) = bind.parent() {
            self.parent_bind.set_bind(bind);
            self.dirty.new_parent.set();
            if let Some(focus_instance) = &*self.event.focused_descendant.borrow() {
                parent.blur_tree();
                parent.propagate_up_new_focus_instance(focus_instance);
            }
        }
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

    /// Recompute the transformation matrix of the display object tree starting with this object and
    /// traversing all of its dirty children.
    pub fn update(&self, scene: &Scene) {
        self.refresh_layout();
        let origin0 = Matrix4::identity();
        self.update_with_origin(scene, origin0, false, false, None)
    }

    /// Update the display object tree transformations based on the parent object origin. See docs
    /// of [`update`] to learn more.
    ///
    /// # Update Order
    /// Please note that scene layers assignment update is performed before the origin and
    /// visibility one. This is because there are rare cases where it is desirable to modify the
    /// display object hierarchy in the `on_layer_change` callback (when the display object was
    /// moved to another layer). For example, this mechanism is used in the shape system to replace
    /// the sprite instance to a new one on layer change. Please note that updating the display
    /// object hierarchy during its refresh is a very complex operation and an extra care should be
    /// taken when modifying this logic.
    fn update_with_origin(
        &self,
        scene: &Scene,
        parent_origin: Matrix4<f32>,
        parent_origin_changed: bool,
        parent_layers_changed: bool,
        parent_layer: Option<&WeakLayer>,
    ) {
        // === Scene Layers Update ===

        let has_new_parent = self.dirty.new_parent.check();
        let assigned_layer_ref = self.assigned_layer.borrow();
        let assigned_layer = assigned_layer_ref.as_ref();
        let assigned_layers_changed = self.dirty.new_layer.take().check();
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
                self.on_layer_change_source.emit((
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
            let origin_changed = self.transformation.borrow_mut().update(new_parent_origin);
            let new_origin = self.transformation.borrow().matrix;
            if origin_changed || layer_changed {
                self.dirty.modified_children.unset_all();
                if origin_changed {
                    trace!("Self origin changed.");
                } else {
                    trace!("Self origin did not change, but the layers did.");
                }
                self.on_updated_source.emit(());
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
                if self.dirty.modified_children.check_all() {
                    debug_span!("Updating dirty children.").in_scope(|| {
                        self.dirty.modified_children.take().iter().for_each(|ix| {
                            self.children
                                .borrow()
                                .safe_index(**ix)
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
        });
        self.dirty.transformation.unset();
        self.dirty.new_parent.unset();
    }

    /// Hide all removed children and show this display object if it was attached to a new parent.
    fn update_visibility(&self, scene: &Scene, parent_layer: Option<&WeakLayer>) {
        self.take_removed_children_and_update_their_visibility(scene);
        let parent_changed = self.dirty.new_parent.check();
        if parent_changed && self.has_parent() {
            self.set_vis_true(Some(scene), parent_layer)
        }
    }

    fn take_removed_children_and_update_their_visibility(&self, scene: &Scene) {
        if self.dirty.removed_children.check_all() {
            debug_span!("Updating removed children.").in_scope(|| {
                for child in self.dirty.removed_children.take().into_iter() {
                    if let Some(child) = child.upgrade() {
                        if !child.has_visible_parent() {
                            // The child was not attached to another visible parent.
                            child.set_vis_false(Some(scene));
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
}

impl InstanceDef {
    /// Checks if the provided object is child of the current one.
    pub fn has_child<T: Object>(&self, child: &T) -> bool {
        self.child_index(child).is_some()
    }

    /// Returns the index of the provided object if it was a child of the current one.
    pub fn child_index<T: Object>(&self, child: &T) -> Option<ChildIndex> {
        let child = child.display_object();
        child.parent_bind.parent_and_child_index().and_then(|(parent, index)| {
            if &parent.def == self {
                Some(index)
            } else {
                None
            }
        })
    }

    /// Replaces the parent binding with a new parent.
    fn set_parent(&self, parent: &InstanceDef) {
        parent.add_child(self);
    }

    /// Removes the current parent binding.
    fn unset_parent(&self) {
        self.take_parent_bind();
    }

    /// Attaches the provided display object as a child to this one.
    fn add_child(&self, child: &InstanceDef) {
        child.unset_parent();
        let child_index = self.register_child(child);
        trace!("Adding a new child at index {child_index}.");
        let parent_bind = ParentBind { parent: self.downgrade(), child_index };
        child.set_parent_bind(parent_bind);
    }

    fn add_children<T: Object>(&self, children: impl IntoIterator<Item = T>) {
        children.into_iter().for_each(|child| self.add_child(child.display_object()));
    }

    fn replace_children<T: Object>(&self, children: impl IntoIterator<Item = T>) {
        self.remove_all_children();
        self.add_children(children);
    }

    fn register_child(&self, child: &InstanceDef) -> ChildIndex {
        let index = ChildIndex(self.children.borrow_mut().insert(child.downgrade()));
        self.dirty.modified_children.set(index);
        index
    }

    /// Removes the provided object reference from child list of this object. Does nothing if the
    /// reference was not a child of this object.
    fn remove_child<T: Object>(&self, child: &T) {
        let child = child.display_object();
        if self.has_child(child) {
            child.unset_parent()
        }
    }

    /// Get reversed parent chain of this display object (`[root, child_of root, ..., parent,
    /// self]`). The last item is this object.
    fn rev_parent_chain(&self) -> Vec<Instance> {
        let mut vec = default();
        Self::build_rev_parent_chain(&mut vec, Some(self.clone_ref().into()));
        vec
    }

    fn build_rev_parent_chain(vec: &mut Vec<Instance>, parent: Option<Instance>) {
        if let Some(parent) = parent {
            Self::build_rev_parent_chain(vec, parent.parent());
            vec.push(parent);
        }
    }
}



// =======================
// === Transformations ===
// =======================

impl Model {
    /// Position of the object in the global coordinate space.
    fn global_position(&self) -> Vector3<f32> {
        self.transformation.borrow().global_position()
    }

    /// Position of the object in the parent coordinate space.
    fn position(&self) -> Vector3<f32> {
        self.transformation.borrow().position()
    }

    /// Scale of the object in the parent coordinate space.
    fn scale(&self) -> Vector3<f32> {
        self.transformation.borrow().scale()
    }

    /// Rotation of the object in the parent coordinate space.
    fn rotation(&self) -> Vector3<f32> {
        self.transformation.borrow().rotation()
    }

    /// Transformation matrix of the object in the parent coordinate space.
    fn transformation_matrix(&self) -> Matrix4<f32> {
        self.transformation.borrow().matrix()
    }
}


// === Transformation Setters ===

impl Model {
    fn with_mut_borrowed_transformation<F, T>(&self, f: F) -> T
    where F: FnOnce(&mut CachedTransformation) -> T {
        self.dirty.transformation.set();
        f(&mut self.transformation.borrow_mut())
    }
}

macro_rules! generate_transformation_getters_and_setters {
    ($($name:ident),*) => { paste! {
        impl Model {$(
            fn [<set_ $name>](&self, v: Vector3<f32>) {
                self.with_mut_borrowed_transformation(|t| t.[<set_ $name>](v));
            }

            fn [<update_ $name>](&self, f: impl FnOnce(Vector3<f32>) -> Vector3<f32>) {
                self.with_mut_borrowed_transformation(|t| t.[<update_ $name>](f));
            }

            fn [<modify_ $name>](&self, f: impl FnOnce(&mut Vector3<f32>)) {
                self.with_mut_borrowed_transformation(|t| t.[<modify_ $name>](f));
            }

            fn [<set_ $name _dim>]<D>(&self, dim: D, value: f32)
            where Vector3<f32>: DimSetter<D> {
                self.with_mut_borrowed_transformation(|t|
                    t.[<modify_ $name>](|v| v.set_dim(dim, value))
                );
            }

            fn [<update_ $name _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(f32) -> f32)
            where Vector3<f32>: DimSetter<D> {
                self.with_mut_borrowed_transformation(|t|
                    t.[<modify_ $name>](|v| v.update_dim(dim, f))
                );
            }

            fn [<modify_ $name _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(&mut f32))
            where Vector3<f32>: DimSetter<D> {
                self.with_mut_borrowed_transformation(|t|
                    t.[<modify_ $name>](|v| v.modify_dim(dim, f))
                );
            }
        )*}
    }};
}

generate_transformation_getters_and_setters!(position, scale, rotation);



// =================================================================================================
// === Event System ================================================================================
// =================================================================================================

// ======================
// === Events & Focus ===
// ======================
// See the documentation of [`event::Event`] to learn more about events.

/// The part of display object model related to event handling.
#[derive(Debug)]
pub struct EventModel {
    source:             frp::Source<event::SomeEvent>,
    capturing_fan:      frp::Fan,
    bubbling_fan:       frp::Fan,
    focused_descendant: RefCell<Option<WeakInstance>>,
}

impl EventModel {
    fn new(network: &frp::Network) -> Self {
        let capturing_fan = frp::Fan::new(network);
        let bubbling_fan = frp::Fan::new(network);
        let focused_descendant = default();
        frp::extend! { network
            source <- source();
        }
        Self { source, capturing_fan, bubbling_fan, focused_descendant }
    }
}

impl Model {
    /// Get event stream for bubbling events. See docs of [`event::Event`] to learn more.
    fn on_event<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.event.bubbling_fan.output::<event::Event<T>>()
    }

    /// Get event stream for capturing events. You should rather not need this function. Use
    /// [`on_event`] instead. See docs of [`event::Event`] to learn more.
    fn on_event_capturing<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.event.capturing_fan.output::<event::Event<T>>()
    }
}

impl InstanceDef {
    fn init_events_handling(self) -> Self {
        // This implementation is a bit complex because we do not want to clone network to the FRP
        // closure in order to avoid a memory leak.
        let network = &self.network;
        let parent_bind = &self.parent_bind;
        let capturing_event_fan = &self.event.capturing_fan;
        let bubbling_event_fan = &self.event.bubbling_fan;
        frp::extend! { network
            eval self.event.source ([parent_bind, capturing_event_fan, bubbling_event_fan] (event) {
                let parent = parent_bind.parent();
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
        let rev_parent_chain = parent.map(|p| p.rev_parent_chain()).unwrap_or_default();
        if event.captures.get() {
            for object in &rev_parent_chain {
                if !event.is_cancelled() {
                    object.event.capturing_fan.emit(&event.data);
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
                    object.event.bubbling_fan.emit(&event.data);
                } else {
                    break;
                }
            }
        }
    }

    fn new_event<T>(&self, payload: T) -> event::SomeEvent
    where T: 'static {
        event::SomeEvent::new(Some(self.downgrade()), payload)
    }

    fn emit_event<T>(&self, payload: T)
    where T: 'static {
        self.event.source.emit(event::SomeEvent::new(Some(self.downgrade()), payload));
    }

    fn focused_descendant(&self) -> Option<Instance> {
        self.event.focused_descendant.borrow().as_ref().and_then(|t| t.upgrade())
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
        self.event.source.emit(focus_event);
        self.event.source.emit(focus_in_event);
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
        self.event.source.emit(blur_event);
        self.event.source.emit(focus_out_event);
    }

    /// Clears the focus info in this instance and all parent instances. In order to work properly,
    /// this should be called on the focused instance. Otherwise, it may clear the information
    /// only partially.
    fn propagate_up_no_focus_instance(&self) {
        *self.event.focused_descendant.borrow_mut() = None;
        self.parent().for_each(|parent| parent.propagate_up_no_focus_instance());
    }

    /// Set the focus instance to the provided one here and in all instances on the path to the
    /// root.
    fn propagate_up_new_focus_instance(&self, instance: &WeakInstance) {
        debug_assert!(self.event.focused_descendant.borrow().is_none());
        *self.event.focused_descendant.borrow_mut() = Some(instance.clone());
        self.parent().for_each(|parent| parent.propagate_up_new_focus_instance(instance));
    }
}

impl Debug for InstanceDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DisplayObject")
            .field("name", &self.name)
            // .field("is_dirty", &self.dirty.check_all())
            // .field("dirty", &self.dirty)
            .field("position", &self.position().xy().as_slice())
            .field("size", &self.layout.borrow().size.as_slice())
            // .field("layout", &self.layout)
            .finish()
    }
}

impl Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.def, f)
    }
}



// =================================================================================================
// === Layout and Size =============================================================================
// =================================================================================================

// ========================
// === Alignment Macros ===
// ========================

#[macro_export]
macro_rules! with_display_object_alignment_primary {
    ($f:path $([$($args:tt)*])?) => {
        $f! { $([$($args)*])? [start center end space_between space_around space_evenly] }
    };
}

#[macro_export]
macro_rules! with_display_object_alignment_secondary {
    ($f:path $([$($args:tt)*])?) => {
        $f! { $([$($args)*])? [start center end] }
    };
}

#[macro_export]
macro_rules! with_display_object_alignment_primary_and_secondary {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_display_object_alignment_primary! {
            with_display_object_alignment_primary_and_secondary [@ [$f] [$([$($args)*])?]]
        }
    };
    ([@ [$f:path] [$([$($args:tt)*])?]] [$($primary:ident)*]) => {
        $crate::with_display_object_alignment_secondary! {
            with_display_object_alignment_primary_and_secondary
            [@ [$f] [$($primary)*] [$([$($args)*])?]]
        }
    };
    ([@ [$f:path] [$($primary:ident)*] [$([$($args:tt)*])?]] [$($secondary:ident)*]) => {
        $f! {$([$($args)*])? [$($primary)*] [$($secondary)*]}
    };
}

#[macro_export]
macro_rules! with_display_object_alignment_matrix {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_display_object_alignment_primary_and_secondary! {
            enso_shapely::cartesian [$f $([$($args)*])?]
        }
    };
}


/// Runs the provided macro with an alignment anchor matrix annotated with a name for the anchor
/// pair. The name is created as `$x_$y` with the exception for both anchors being `center` or
/// `spaced`. Then, the name is simply `center` and `spaced`, respectively. For example, if run
/// with the arguments `f [args]`, it results in:
///
/// ```text
/// f!{ [args]
///     [left_bottom left bottom]
///     [left_center left center]
///     [left_top left top]
///     [left_spaced left spaced]
///     ...
///     [center_bottom center bottom]
///     [center center center]
///     [center_top center top]
///     ...
///     [spaced spaced spaced]
///     ...
///     [bottom top]
/// }
/// ```
///
/// The `[args]` argument is optional.
#[macro_export]
macro_rules! with_display_object_alignment_named_matrix {
    ($f:path $([$($args:tt)*])?) => {
        $crate::with_display_object_alignment_matrix! {
            $crate::with_display_object_alignment_named_matrix [$f $([$($args)*])?]
        }
    };
    ([$($fs:tt)*] $($ts:tt)*) => {
        $crate::with_display_object_alignment_named_matrix! {@ [$($fs)*] [] $($ts)*}
    };
    (@ $fs:tt [$($out:tt)*] [[start start] $($ts:tt)*]) => {
        $crate::with_display_object_alignment_named_matrix! {
            @ $fs [$($out)* [start start start]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[center center] $($ts:tt)*]) => {
        $crate::with_display_object_alignment_named_matrix! {
            @ $fs [$($out)* [center center center]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[end end] $($ts:tt)*]) => {
        $crate::with_display_object_alignment_named_matrix! {
            @ $fs [$($out)* [end end end]] [$($ts)*]
        }
    };
    (@ $fs:tt [$($out:tt)*] [[$x:ident $y:ident] $($ts:tt)*]) => { paste! {
        $crate::with_display_object_alignment_named_matrix! {
            @ $fs [$($out)* [[<$x _ $y>] $x $y]] [$($ts)*]
        }
    }};
    (@ [$f:path $([$($args:tt)*])?] $out:tt []) => {
        $f! { $([$($args)*])? $out }
    };
}



// =================
// === Alignment ===
// =================

use crate::display::layout::alignment;

pub type Alignment = alignment::Dim2;

// macro_rules! gen_alignment_primary {
//     ([$($name:tt)*]) => { paste! {
//         #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
//         pub enum AlignmentPrimary {
//             #[default]
//             $([<$name:camel>]),*
//         }
//     }};
// }
// with_display_object_alignment_primary!(gen_alignment_primary);



// ================
// === Resizing ===
// ================

/// The resizing mode. Computing of the size is a complex process with many corner cases. Read the
/// docs of [`LayoutModel`] to learn more about it.
#[derive(Clone, Copy, Debug, Default, PartialEq, From)]
pub enum Resizing {
    /// In this mode, the display object size will be set to the size of its content. The only
    /// exception are display objects with no children, which size will not be changed during
    /// refresh.
    #[default]
    Hug,
    /// In this mode, the display object size is provided explicitly.
    Fixed(f32),
}

impl Resizing {
    /// Checks whether the resizing mode is [`Resizing::Hug`].
    pub fn is_hug(self) -> bool {
        self == Resizing::Hug
    }

    /// Checks whether the resizing mode is [`Resizing::Fixed`].
    pub fn is_fixed(self) -> bool {
        match self {
            Resizing::Fixed(_) => true,
            _ => false,
        }
    }
}

/// Just like `Into<Vector2<Resizing>>`. It is needed because of Rust limitations regarding
/// implementing traits for structs not owned by this crate.
#[allow(missing_docs)]
pub trait IntoResizing {
    fn into_resizing(self) -> Vector2<Resizing>;
}

impl IntoResizing for Vector2<f32> {
    fn into_resizing(self) -> Vector2<Resizing> {
        Vector2::new(self.x.into(), self.y.into())
    }
}

impl IntoResizing for Vector2<Resizing> {
    fn into_resizing(self) -> Vector2<Resizing> {
        self
    }
}

macro_rules! impl_tuple_into_resizing {
    ($(($a:tt, $b:tt)),*) => {$(
        impl IntoResizing for ($a, $b) {
            fn into_resizing(self) -> Vector2<Resizing> {
                Vector2::new(self.0.into(), self.1.into())
            }
        }
    )*};
}

impl_tuple_into_resizing!((f32, f32), (f32, Resizing), (Resizing, f32), (Resizing, Resizing));



// ==================
// === AutoLayout ===
// ==================

/// A phantom type indicating the horizontal auto layout mode.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Horizontal;

/// A phantom type indicating the vertical auto layout mode.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Vertical;

/// The auto layout mode. It is used to automatically position the children of a display object.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum AutoLayout {
    Horizontal(LayoutOptions),
    Vertical(LayoutOptions),
}

impl AutoLayout {
    /// Constructor.
    pub fn horizontal() -> AutoLayoutBuilder<Horizontal> {
        default()
    }

    /// Constructor.
    pub fn vertical() -> AutoLayoutBuilder<Vertical> {
        default()
    }

    // pub fn def_column(&mut self) {
    //     match self {
    //         AutoLayout::Horizontal(opts) => opts.def_column(),
    //         AutoLayout::Vertical(opts) => opts.def_column(),
    //     }
    // }

    pub fn set_column_count(&mut self, count: usize) {
        match self {
            AutoLayout::Horizontal(opts) => opts.set_column_count(count),
            AutoLayout::Vertical(opts) => opts.set_column_count(count),
        }
    }
}

/// Defines the struct and also getters and setters for the [`AutoLayout`] struct.
macro_rules! def_layout_options {
    (
        pub struct $name:ident {$(
            $(#$meta:tt)*
            pub $field:ident : $ty:ty
        ),* $(,)?}
    ) => { paste!{
        /// Options for the auto layout mode.
        #[derive(Clone, Debug, Default, PartialEq)]
        pub struct $name {$(
            $(#$meta)*
            pub $field : $ty
        ),*}

        impl AutoLayout {$(
            /// Getter.
            pub fn [<set_ $field>](&mut self, $field: $ty) {
                match self {
                    AutoLayout::Horizontal(options) => options.$field = $field,
                    AutoLayout::Vertical(options) => options.$field = $field,
                }
            }
            //
            // /// Update the value with the provided function.
            // pub fn [<update_ $field>](&mut self, f: impl FnOnce($ty) -> $ty) {
            //     match self {
            //         AutoLayout::Horizontal(options) => options.$field = f(options.$field),
            //         AutoLayout::Vertical(options) => options.$field = f(options.$field),
            //     }
            // }

            /// Modify the value with the provided function.
            pub fn [<modify_ $field>](&mut self, f: impl FnOnce(&mut $ty)) {
                match self {
                    AutoLayout::Horizontal(options) => f(&mut options.$field),
                    AutoLayout::Vertical(options) => f(&mut options.$field),
                }
            }
        )*}
    }};
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct UnresolvedAxis {
    resizing: Resizing,
    grow:     Option<f32>,
    shrink:   Option<f32>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ResolvedAxis {
    resizing: Resizing,
    grow:     f32,
    shrink:   f32,
    size:     f32,
}

def_layout_options!(
    pub struct LayoutOptions {
        pub alignment:            Alignment,
        /// The spacing between children.
        pub spacing:              f32,
        /// The padding between the children and the edge of the parent.
        pub padding:              Vector2<f32>,
        /// Indicates whether the children should be placed in order or in a reversed order.
        pub reversed:             bool,
        pub wrapped:              bool,
        pub first_axes:           Vector2<UnresolvedAxis>,
        pub other_axes:           (Vec<UnresolvedAxis>, Vec<UnresolvedAxis>),
        pub prim_axis_elem_count: Option<usize>,
    }
);

impl LayoutOptions {
    // pub fn def_column(&mut self) {
    //     self.columns.push(UnresolvedAxis {})
    // }

    pub fn axes(&self) -> (Vec<UnresolvedAxis>, Vec<UnresolvedAxis>) {
        let x = vec![self.first_axes.x].extended(self.other_axes.0.iter().cloned());
        let y = vec![self.first_axes.y].extended(self.other_axes.1.iter().cloned());
        (x, y)
    }

    pub fn set_column_count(&mut self, count: usize) {
        self.prim_axis_elem_count = Some(count)
    }
}



// =========================
// === AutoLayoutBuilder ===
// =========================

/// An [`AutoLayout`] builder. Layouts have a lot of options and this builder allows setting them
/// in a convenient way.
#[derive(Clone, Debug, Default, PartialEq)]
#[allow(missing_docs)]
pub struct AutoLayoutBuilder<Layout> {
    pub options: LayoutOptions,
    pub tp:      PhantomData<Layout>,
}

#[allow(missing_docs)]
impl<Layout> AutoLayoutBuilder<Layout> {
    // pub fn alignment_primary(mut self, alignment: AlignmentPrimary) -> Self {
    //     self.options.alignment_primary = alignment;
    //     self
    // }
    //
    // pub fn alignment_secondary(mut self, alignment: Alignment) -> Self {
    //     self.options.alignment_secondary = alignment;
    //     self
    // }

    pub fn spacing(mut self, spacing: f32) -> Self {
        self.options.spacing = spacing;
        self
    }

    pub fn padding(mut self, padding: Vector2<f32>) -> Self {
        self.options.padding = padding;
        self
    }

    pub fn reversed(mut self, reversed: bool) -> Self {
        self.options.reversed = reversed;
        self
    }

    pub fn wrapped(mut self, wrapped: bool) -> Self {
        self.options.wrapped = wrapped;
        self
    }

    pub fn reverse(mut self) -> Self {
        self.options.reversed = !self.options.reversed;
        self
    }

    pub fn wrap(mut self) -> Self {
        self.options.wrapped = true;
        self
    }
}

impl From<AutoLayoutBuilder<Horizontal>> for Option<AutoLayout> {
    fn from(builder: AutoLayoutBuilder<Horizontal>) -> Self {
        Some(AutoLayout::Horizontal(builder.options))
    }
}

impl From<AutoLayoutBuilder<Vertical>> for Option<AutoLayout> {
    fn from(builder: AutoLayoutBuilder<Vertical>) -> Self {
        Some(AutoLayout::Vertical(builder.options))
    }
}



// ===========================
// === LayoutObjectBuilder ===
// ===========================

/// An [`AutoLayout`] builder for any display object instance. Unlike [`AutoLayoutBuilder`], it is
/// exposed by the standard [`Object`] API.
#[derive(Debug)]
pub struct LayoutObjectBuilder<Layout> {
    instance: Instance,
    layout:   PhantomData<Layout>,
}

#[allow(missing_docs)]
impl<Layout> LayoutObjectBuilder<Layout> {
    fn new(instance: &Instance) -> Self {
        let instance = instance.clone_ref();
        let layout = default();
        Self { instance, layout }
    }

    fn set_alignment(self, alignment: Alignment) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.set_alignment(alignment);
            });
        });
        self
    }
    //
    // fn def_column(self) -> Self {
    //     self.instance.def.modify_layout(|opt_layout| {
    //         opt_layout.as_mut().map(|layout| {
    //             layout.def_column();
    //         });
    //     });
    //     self
    // }

    fn set_column_count(self, count: usize) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.set_column_count(count);
            });
        });
        self
    }

    // fn alignment_primary(self, alignment: AlignmentPrimary) -> Self {
    //     self.instance.def.modify_layout(|opt_layout| {
    //         opt_layout.as_mut().map(|layout| {
    //             layout.set_alignment_primary(alignment);
    //         });
    //     });
    //     self
    // }
    //
    // fn alignment_secondary(self, alignment: Alignment) -> Self {
    //     self.instance.def.modify_layout(|opt_layout| {
    //         opt_layout.as_mut().map(|layout| {
    //             layout.set_alignment_secondary(alignment);
    //         });
    //     });
    //     self
    // }

    pub fn spacing(self, spacing: f32) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.set_spacing(spacing);
            });
        });
        self
    }

    pub fn padding(self, padding: Vector2<f32>) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.set_padding(padding);
            });
        });
        self
    }

    pub fn padding_xy(self, x: f32, y: f32) -> Self {
        self.padding(Vector2::new(x, y))
    }

    pub fn reversed(self, reversed: bool) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.set_reversed(reversed);
            });
        });
        self
    }

    pub fn wrapped(self, wrapped: bool) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.set_wrapped(wrapped);
            });
        });
        self
    }

    pub fn reverse(self) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.modify_reversed(|t| *t = !*t);
            });
        });
        self
    }

    pub fn wrap(self) -> Self {
        self.instance.def.modify_layout(|opt_layout| {
            opt_layout.as_mut().map(|layout| {
                layout.modify_wrapped(|t| *t = !*t);
            });
        });
        self
    }
}

macro_rules! gen_layout_object_builder_alignment {
    ([$([$name:ident $x:ident $y:ident])*]) => {
        paste! {
            impl<Layout> LayoutObjectBuilder<Layout> {$(
                /// Constructor.
                pub fn [<set_alignment_ $name>](self) -> Self {
                    self.set_alignment(Alignment::$name())
                }
            )*}
        }
    }
}

crate::with_alignment_dim2_named_matrix!(gen_layout_object_builder_alignment);

// with_display_object_alignment_primary!(gen_layout_object_builder_alignment[primary Horizontal
// Vertical]); with_display_object_alignment_secondary!
// (gen_layout_object_builder_alignment[secondary Vertical Horizontal]);

macro_rules! gen_layout_object_builder_alignment_matrix {
    ([$([$f:ident $primary:ident $secondary:ident])*]) => { paste! {
        impl<Layout> LayoutObjectBuilder<Layout> {$(
            /// Constructor.
            pub fn [<alignment_ $f>](self) -> Self {
                self
                    .alignment_primary(AlignmentPrimary::[<$primary:camel>])
                    .alignment_secondary(Alignment::[<$secondary:camel>])
            }
        )*}
    }}
}

// with_display_object_alignment_named_matrix!(gen_layout_object_builder_alignment_matrix);



// ===================
// === LayoutModel ===
// ===================

#[derive(Clone, Copy, Debug)]
pub struct LayoutObjectConfig {
    min_size: Vector2<f32>,
    max_size: Vector2<f32>,
    resizing: Vector2<Resizing>,
    grow:     Vector2<f32>,
    shrink:   Vector2<f32>,
}

impl LayoutObjectConfig {
    pub fn new() -> Self {
        let min_size = default();
        let max_size = Vector2::new(f32::INFINITY, f32::INFINITY);
        let resizing = default();
        let grow = default();
        let shrink = default();
        Self { min_size, max_size, resizing, grow, shrink }
    }
}

impl Default for LayoutObjectConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// The layout description of a display object.
///
/// The [`size`] field describes the bounding box dimension, while the [`bbox_origin`], its left
/// bottom corner. When auto-layout is used, the origin is placed in (0, 0). In case of manual
/// layout, the origin is the left bottom corner of the bounding box of all children.
///
/// See the docs of [`Instance`] to learn more about the layout system.
#[derive(Debug, Deref, Default)]
pub struct LayoutModel {
    #[deref]
    config:      LayoutObjectConfig,
    auto:        Option<AutoLayout>,
    size:        Vector2<f32>,
    bbox_origin: Vector2<f32>,
    // FIXME
    //  mozliwe ze powinnimsy miec cos w stylu bbox_origin_alignment, ktory ustawia to gdzie jest
    //  origin tego display objecta. Wtedy sprity moga ustawiac to by default w srodku.
    //  Innym sposobem jest dodanie "deprecated mode", ktore bedzie centrowalo sprity
}

impl LayoutModel {
    fn new() -> Self {
        default()
    }
}

impl Model {
    fn resizing(&self) -> Vector2<Resizing> {
        self.layout.borrow().resizing
    }

    fn set_resizing(&self, resizing: impl IntoResizing) {
        self.dirty.transformation.set();
        *self.layout.borrow_mut().resizing = resizing.into_resizing();
    }

    fn modify_resizing(&self, f: impl FnOnce(&mut Vector2<Resizing>)) {
        self.dirty.transformation.set();
        f(&mut *self.layout.borrow_mut().resizing);
    }

    fn bbox_origin(&self) -> Vector2<f32> {
        self.layout.borrow().bbox_origin
    }

    fn size(&self) -> Vector2<f32> {
        self.layout.size.get()
    }

    fn set_size(&self, size: impl IntoVector2<f32>) {
        self.set_resizing(size.into_vector2())
    }

    fn set_size_x(&self, x: f32) {
        self.modify_resizing(|t| t.x = Resizing::Fixed(x))
    }

    fn set_size_x_hug(&self, x: f32) {
        self.set_resizing((x, Resizing::Hug))
    }

    fn set_size_x_to_hug(&self) {
        self.modify_resizing(|t| t.x = Resizing::Hug);
    }

    fn set_size_y_to_hug(&self) {
        self.modify_resizing(|t| t.y = Resizing::Hug);
    }

    fn allow_grow(&self) {
        self.layout.grow.set(Vector2(1.0, 1.0));
    }

    fn allow_grow_x(&self) {
        self.layout.grow.set_x(1.0);
    }

    fn allow_grow_y(&self) {
        self.layout.grow.set_y(1.0);
    }

    fn set_size_hug_y(&self, y: f32) {
        self.set_resizing((Resizing::Hug, y))
    }

    fn set_size_hug(&self) {
        self.set_resizing((Resizing::Hug, Resizing::Hug))
    }
}

impl Model {
    fn set_layout(&self, layout: impl Into<Option<AutoLayout>>) {
        self.dirty.transformation.set();
        *self.layout.auto.borrow_mut() = layout.into();
    }

    fn modify_layout(&self, f: impl FnOnce(&mut Option<AutoLayout>)) {
        f(&mut *self.layout.auto.borrow_mut());
    }

    fn refresh_layout(&self) {
        self.refresh_self_size(true);
        self.refresh_layout_internal(true);
        self.refresh_layout_internal(false);
    }

    fn refresh_self_size(&self, first_pass: bool) {
        if first_pass {
            let resizing = self.layout.resizing.get();
            match resizing.x {
                Resizing::Fixed(v) => {
                    println!("[X] Setting size.{:?} of {} to {}", "x", self.name, v);
                    self.layout.size.set_x(v);
                }
                _ => {
                    println!("[X2] Setting size.{:?} of {} to 0", "x", self.name);
                    self.layout.size.set_x(0.0);
                }
            }
            match resizing.y {
                Resizing::Fixed(v) => {
                    println!("[X] Setting size.{:?} of {} to {}", "y", self.name, v);
                    self.layout.size.set_y(v);
                }
                _ => {
                    println!("[X2] Setting size.{:?} of {} to 0", "y", self.name);
                    self.layout.size.set_y(0.0);
                }
            }
        }
    }

    fn refresh_layout_internal(&self, first_pass: bool) {
        if !self.dirty.transformation.check() && !self.dirty.modified_children.check_all() {
            return;
        }
        if first_pass {
            if self.layout.auto.borrow().is_some() {
                self.layout.bbox_origin.set(default());
            }
        }
        match &*self.layout.auto.borrow() {
            None =>
                if first_pass {
                    self.refresh_layout_manual(X, first_pass);
                } else {
                    self.refresh_layout_manual(Y, first_pass);
                },
            Some(AutoLayout::Horizontal(opts)) =>
                self.refresh_linear_layout(X, Y, first_pass, &opts, first_pass),
            Some(AutoLayout::Vertical(opts)) =>
                self.refresh_linear_layout(Y, X, !first_pass, &opts, first_pass),
        }
    }

    fn refresh_layout_manual<Dim: Copy>(&self, x: Dim, first_pass: bool)
    where
        Vector2<Resizing>: DimSetter<Dim>,
        Vector2<f32>: DimSetter<Dim>,
        Vector3<f32>: DimSetter<Dim>,
        Dim: Debug, {
        let children = self.children();
        if children.is_empty() {
            if self.layout.resizing.get_dim(x).is_hug() {
                println!("[M] Setting size.{:?} of {} to {}", x, self.name, 0.0);
                self.layout.size.set_dim(x, 0.0);
                self.layout.bbox_origin.set_dim(x, 0.0);
            }
        } else {
            let mut min_x: f32 = f32::MAX;
            let mut max_x: f32 = f32::MIN;
            let mut children_to_grow = vec![];
            for child in &children {
                if child.layout.grow.get_dim(x) > 0.0 {
                    children_to_grow.push(child);
                } else {
                    child.refresh_self_size(first_pass);
                    child.refresh_layout_internal(first_pass);
                    let child_pos = child.position().get_dim(x);
                    let child_size = child.size().get_dim(x);
                    let child_bbox_origin = child.bbox_origin().get_dim(x);
                    let child_min_x = child_pos + child_bbox_origin;
                    let child_max_x = child_min_x + child_size;
                    min_x = min_x.min(child_min_x);
                    max_x = max_x.max(child_max_x);
                }
            }
            let new_size = max_x - min_x;
            let new_bbox_origin = min_x;
            let resizing = self.layout.resizing.get();
            if self.layout.resizing.get_dim(x).is_hug() {
                println!("[M] Setting size.{:?} of {} to {}", x, self.name, new_size);
                self.layout.size.set_dim(x, new_size);
                self.layout.bbox_origin.set_dim(x, new_bbox_origin);
            } else {
                self.layout.bbox_origin.set_dim(x, 0.0);
            }

            for child in children_to_grow {
                println!(
                    "[M] Setting size.{:?} of {} to {}",
                    x,
                    child.name,
                    self.layout.size.get_dim(x)
                );
                child.layout.size.set_dim(x, self.layout.size.get_dim(x));
                child.refresh_layout_internal(first_pass);
            }
        }
    }

    // TODO: wytlumaczyc ze kolejnosc wyliczania szerokosci i wysokosci musi byc inna czasami (jak w
    // przykladzie) dlatego dwa passy.

    /// Updates a linear (horizontal or vertical) layout.
    ///
    ///
    /// # The two-pass update algorithm
    /// The layout update is a two pass algorithm. First, the sizes and positions of elements is
    /// updated in the horizontal direction, then in the vertical direction. To better illustrate
    /// the need of such a solution, consider the following example:
    ///
    /// ```text
    /// ╭▷ ROOT ──────────────────────────╮
    /// │   ╭▷ L ◀ ▶ ──╮   ╭R─ ▶ ◀ ───╮   │   Auto-layout Legend:         
    /// │   │ ╭ ◀ ▶ ╮  │   ▽ ╭────╮   │   │   ┄── ▷ ──┄ : Horizontal auto-layout.
    /// │   │ │ L1  │  │   │ │ R1 ▲   │   │   ┄── ▽ ──┄ : Vertical auto-layout.
    /// │   │ │     │  │   │ │    ▼   │   │   ┄───────┄ : Manual layout.   
    /// │   │ │     │  ▼   │ ╰────╯   ▲   ▼
    /// │   │ │     │  ▲   │ ╭────╮   ▼   ▲   Resizing Legend:             
    /// │   │ │     │  │   │ │ R2 ▲   │   │   ┄── ◀ ▶ ──┄ : Fill resizing.
    /// │   │ │     │  │   │ │    ▼   │   │   ┄── ▶ ◀ ──┄ : Hug resizing.  
    /// │   │ ╰─────╯  │   │ ╰────╯   │   │   ┄─────────┄ : Fixed resizing.
    /// │   ╰──────────╯   ╰──────────╯   │
    /// ╰─────────────────────────────────╯
    /// ```
    ///
    /// 1. In the first pass, we are updating the horizontal layout.
    ///    a) First, we are visiting the `L` object. It's X-axis resizing is set to `Fill`, so we
    ///       can't determine its width yet. Neither we can update the X-axis layout of its child,
    ///       as it may depend on the `L` object width.
    ///    b) Then, we are visiting the `R` object. It's X-axis resizing is set to `Hug`, so we need
    ///       to visit its children to find the widest one. It's layout is set to vertical. Unlike
    ///       in the case of the `L` and `R` objects, we are computing the size in the orthogonal
    ///       direction than the layout the children are placed in.
    ///    c) As the `ROOT` object's width resizing is set to `Fixed`, after finding the `R` object
    ///       width, we can compute the `L` object width.
    ///    d) Finally, we can update the `L` object children layout.
    ///
    /// 2. In the second pass, we are updating the vertical layout.
    ///    a) First, we are visiting the `L` object. It's Y-axis resizing is set to `Hug`, so we
    ///       need to visit its children to find the tallest one.
    ///    b) The `L1` object's Y-axis resizing is set to `Fixed`, so we can simply update its
    ///       height.
    ///    c) Next, we are visiting the `R` object. It's Y-axis resizing is set to `Fill`, so we can
    ///       compute it, as the `ROOT` object height is fixed.
    ///    d) Finally, we can update the `R` object children layout. Both children Y-axis resizing
    ///       is set to `Fill`, so they are equally using the available space.
    ///
    /// Please note, that this algorithm could not be realized in a single pass, as we can't compute
    /// `L` object width without first computing the `R` object width, and we can't compute the
    /// `R` object height without first computing the `L` object height.
    ///
    /// The result of the algorithm is presented below. Please note that only the dimensions written
    /// in parentheses were set manually.
    ///
    /// ```text
    /// ╭▷ ROOT ─────────────────────────────────────────╮
    /// │                          ╭R─ ▶ ◀ ──────╮       │
    /// │                          ▽ ╭────╮      │       │
    /// │                          │ │ R2 ▲ 50   │       │
    /// │  ╭▷ L ◀ ▶ ────────╮      │ │    ▼      │       │
    /// │  │ ╭ ◀ ▶ ╮        │      │ ╰────╯      │       │
    /// │  │ │ L1  │        ▼      │  (30)       │       │
    /// │  │ │     │ (50)   ▲ 50   │             ▲ 100   │
    /// │  │ ╰─────╯        │      │ ╭────╮      ▼       │ (100)
    /// │  │   70           │      │ │ R1 ▲ 50   │       │
    /// │  ╰────────────────╯      │ │    ▼      │       │
    /// │         70               │ ╰────╯      │       │
    /// │                          │  (20)       │       │
    /// │                          ╰─────────────╯       │
    /// │                               30               │
    /// ╰────────────────────────────────────────────────╯
    ///                      (100)   
    /// ```
    ///
    /// # Meaning of the function parameters.
    /// In order to make the code easy to understand, all variables were named as if the code was
    /// updating horizontal layout only. In reality, the variables [`x`] and [`y`] can be flipped to
    /// update vertical layout instead.
    ///
    /// The [`update_x`] flag indicates whether we are updating the X- or the Y- local axis. For
    /// example, in the example described above, during the horizontal layout update, the `R` object
    /// children were traversed. During the traversal, the [`x`] variable was set to the Y-axis, and
    /// the [`y`] variable was set to the X-axis, so the `R1` and `R2` objects can be considered as
    /// placed in an horizontal layout. The [`update_x`] flag was set to `false`, as we were
    /// interested in the width of `R1` and `R2`, which in the local coordinate system was the
    /// Y-axis.
    ///
    /// The [`first_pass`] flag indicated whether we are in the first or the second pass.
    #[inline(always)]
    fn refresh_linear_layout<Dim1: Copy, Dim2: Copy>(
        &self,
        x: Dim1,
        y: Dim2,
        update_x: bool,
        opts: &LayoutOptions,
        first_pass: bool,
    ) where
        Vector2<Resizing>: DimSetter<Dim1>,
        Vector2<Resizing>: DimSetter<Dim2>,
        Vector2<alignment::Dim1>: DimSetter<Dim1>,
        Vector2<alignment::Dim1>: DimSetter<Dim2>,
        Vector2<f32>: DimSetter<Dim1>,
        Vector2<f32>: DimSetter<Dim2>,
        Vector3<f32>: DimSetter<Dim1>,
        Vector3<f32>: DimSetter<Dim2>,
        Dim1: Debug,
        Dim2: Debug,
    {
        println!("[{}] refresh_linear_layout. Update x? {}", self.name, update_x);
        let children = if opts.reversed { self.children().reversed() } else { self.children() };
        if children.is_empty() {
            return;
        }
        let resizing = self.layout.resizing.get_dim(x);
        if update_x {
            // === Recomputing X-axis elements size of the X-axis horizontal layout ===

            let defined_axes = opts.axes().0; // FIXME: get_dim
            let column_count = opts.prim_axis_elem_count.unwrap_or_else(|| children.len());
            let column_axes = defined_axes.iter().cycle().enumerate().take(column_count);
            println!("column_count: {}", column_count);
            let unresolved_columns = column_axes
                .map(|(i, t)| {
                    (*t, children.iter().skip(i).step_by(column_count).cloned().collect_vec())
                })
                .collect_vec();
            println!("unresolved_columns: {:?}", unresolved_columns);
            let resolved_columns = unresolved_columns
                .into_iter()
                .map(|(axis, children)| {
                    let mut grow = 0.0;
                    let mut shrink = 999.0; // FIXME
                    let mut size = 0.0;
                    for child in &children {
                        child.refresh_self_size(first_pass);
                        if child.layout.resizing.get_dim(x).is_hug() {
                            child.refresh_layout_internal(first_pass);
                        }
                        let child_size = child.layout.size.get_dim(x);
                        let child_grow = child.layout.grow.get_dim(x);
                        let child_shrink = child.layout.shrink.get_dim(x);
                        grow = if child_grow > grow { child_grow } else { grow };
                        shrink = if child_shrink < shrink { child_shrink } else { shrink };
                        size = if child_size > size { child_size } else { size };
                    }
                    grow = axis.grow.unwrap_or(grow);
                    shrink = axis.shrink.unwrap_or(shrink);
                    let resizing = axis.resizing;
                    let resolved_axis = ResolvedAxis { resizing, grow, shrink, size };
                    (resolved_axis, children)
                })
                .collect_vec();

            let space_left = if resizing.is_fixed() {
                let width = self.layout.size.get_dim(x);
                let total_padding_x = 2.0 * opts.padding.get_dim(x);
                let total_spacing_x = (children.len() - 1) as f32 * opts.spacing;
                let mut space_left = width - total_padding_x - total_spacing_x;
                let mut total_grow_coeff = 0.0;
                for column in &resolved_columns {
                    space_left -= column.0.size;
                    total_grow_coeff += column.0.grow;
                }

                let mut pos_x = opts.padding.get_dim(x);
                for column in &resolved_columns {
                    let column_grow = column.0.grow;
                    let grow_size = if column_grow > 0.0 {
                        column.0.grow / total_grow_coeff * space_left
                    } else {
                        0.0
                    };
                    space_left -= grow_size;
                    let column_size = column.0.size + grow_size;
                    for child in &column.1 {
                        if child.layout.grow.get_dim(x) > 0.0 {
                            child.layout.size.set_dim(x, column_size);
                        }
                        let child_size = child.size();
                        let bbox_origin = child.bbox_origin();
                        child.set_position_dim(x, pos_x - bbox_origin.get_dim(x));
                        pos_x += child_size.get_dim(x) + opts.spacing;
                    }
                }
                space_left
            } else {
                todo!()
            };
        } else {
            // // === Recomputing Y-axis elements size of the X-axis horizontal layout ===
            //
            // let padding_y = opts.padding.get_dim(y);
            // let total_padding_y = 2.0 * opts.padding.get_dim(y);
            // let mut children_to_grow = vec![];
            // let mut height: f32 = 0.0;
            // for child in &children {
            //     if child.layout.grow.get_dim(y) > 0.0 {
            //         children_to_grow.push(child);
            //     } else {
            //         child.refresh_self_size(first_pass);
            //         child.refresh_layout_internal(first_pass);
            //         height = height.max(child.layout.size.get_dim(y));
            //     }
            // }
            //
            // if self.layout.resizing.get_dim(y).is_hug() {
            //     let height = height + 2.0 * opts.padding.get_dim(y);
            //     println!("Setting size.{:?} of {} to {}", y, self.name, height);
            //     self.layout.size.set_dim(y, height);
            // } else {
            //     height = self.layout.size.get_dim(y) - total_padding_y;
            // }
            //
            // for child in children_to_grow {
            //     child.refresh_self_size(first_pass);
            //     println!("Setting size.{:?} of {} to {}", y, child, height);
            //     child.layout.size.set_dim(y, height);
            //     child.refresh_layout_internal(first_pass);
            // }
            //
            //
            // // === Recomputing Y-axis elements position of the X-axis horizontal layout ===
            //
            // for child in &children {
            //     let space_left = height - child.size().get_dim(y);
            //     let pos_y = padding_y + opts.alignment.get_dim(y).normalized() * space_left;
            //     child.set_position_dim(y, pos_y);
            // }
        }
    }
}



// =================================================================================================
// === Public API ==================================================================================
// =================================================================================================

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

/// Generates getters and setters for display object transformations, such as `x()`, `xy()`,
/// `set_x()`, `rotation_z()`, `set_scale_x()`, etc.
macro_rules! gen_object_trans {
    ($trans:ident $(,$tx_name:ident)?) => {
        paste! {
            fn $trans(&self) -> Vector3<f32> {
                self.display_object().def.$trans()
            }

            fn [<set_ $trans>](&self, t: Vector3<f32>) {
                self.display_object().def.[<set_ $trans>](t);
            }

            fn [<update_ $trans>]<F: FnOnce(Vector3<f32>) -> Vector3<f32>>(&self, f: F) {
                self.display_object().def.[<update_ $trans>](f)
            }

            fn [<modify_ $trans>]<F: FnOnce(&mut Vector3<f32>)>(&self, f: F) {
                self.display_object().def.[<modify_ $trans>](f)
            }

            fn [<set_ $trans _dim>]<D>(&self, dim: D, value: f32)
            where Vector3<f32>: DimSetter<D> {
                self.display_object().def.[<set_ $trans _dim>](dim, value)
            }

            fn [<update_ $trans _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(f32) -> f32)
            where Vector3<f32>: DimSetter<D> {
                self.display_object().def.[<update_ $trans _dim>](dim, f)
            }

            fn [<modify_ $trans _dim>]<D: Copy>(&self, dim: D, f: impl FnOnce(&mut f32))
            where Vector3<f32>: DimSetter<D> {
                self.display_object().def.[<modify_ $trans _dim>](dim, f)
            }
        }
        enso_types::with_swizzling_for_dim!(1, gen_getters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim!(2, gen_getters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim!(3, gen_getters, $trans $(,$tx_name)?);

        enso_types::with_swizzling_for_dim_unique!(1, gen_setters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim_unique!(2, gen_setters, $trans $(,$tx_name)?);
        enso_types::with_swizzling_for_dim_unique!(3, gen_setters, $trans $(,$tx_name)?);
    };
}

macro_rules! gen_getters {
    ([$tx:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_getters! {@ $tx $( $name $name $dim )* }
    };
    ([$tx:tt, $tx_name:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_getters! {@ $tx $( [<$tx_name _ $name>] $name $dim )* }
    };
    (@ $tx:tt $( $fn_name:tt $name:tt $dim:tt )*) => { paste! {
        $( fn $fn_name(&self) -> [<Vector $dim>]<f32> { self.$tx().$name() } )*
    }};
}

macro_rules! gen_setters {
    ([$tx:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_setters! {@ $tx $( [<set_ $name>] [<modify_ $name>] [<update_ $name>] $name $dim )* }
    };
    ([$tx:tt, $tx_name:tt] $_dim:tt $( $name:ident $dim:tt $_dim_ix:tt $_dim_ord:tt )*) => {
        gen_setters! {@ $tx $( [<set_ $tx_name _ $name>] [<modify_ $tx_name _ $name>]
            [<update_ $tx_name _ $name>] $name $dim )* }
    };
    (@ $tx:tt $( $set_name:tt $mod_name:tt $update_name:tt $name:tt $dim:tt )*) => { paste! {
        $(
            fn $set_name(&self, value: [<Vector $dim>]<f32>) {
                self.[<modify_ $tx>](|p| p.[<set_ $name>](value));
            }

            fn $mod_name<F>(&self, f: F)
            where F: FnOnce(&mut [<Vector $dim>]<f32>) {
                let mut value = self.$name();
                f(&mut value);
                self.$set_name(value);
            }

            fn $update_name<F>(&self, f: F)
            where F: FnOnce([<Vector $dim>]<f32>) -> [<Vector $dim>]<f32> {
                self.$set_name(f(self.$name()));
            }
        )*
    }};
}

impl<T: Object + ?Sized> ObjectOps for T {}

/// Implementation of operations available for every struct which implements `display::Object`.
/// To learn more about the design, please refer to the documentation of [`Instance`].
#[allow(missing_docs)]
pub trait ObjectOps: Object {
    // === Transformations ===

    gen_object_trans!(position);
    gen_object_trans!(rotation, rotation);
    gen_object_trans!(scale, scale);

    fn transformation_matrix(&self) -> Matrix4<f32> {
        self.display_object().def.transformation_matrix()
    }

    fn global_position(&self) -> Vector3<f32> {
        self.display_object().def.global_position()
    }


    // === Information ===

    /// Globally unique identifier of this display object.
    fn id(&self) -> Id {
        self.display_object().def.id()
    }


    // === Hierarchy ===

    /// Get the layer this object is displayed in. May be equal to layer explicitly set by the user
    /// or a layer inherited from the parent.
    fn display_layer(&self) -> Option<Layer> {
        self.display_object().def.display_layer()
    }

    /// Add another display object as a child to this display object. Children will inherit all
    /// transformations of their parents.
    fn add_child<T: Object + ?Sized>(&self, child: &T) {
        self.display_object().def.add_child(child.display_object());
    }

    fn add_children<T: Object>(&self, children: impl IntoIterator<Item = T>) {
        self.display_object().def.add_children(children);
    }

    fn replace_children<T: Object>(&self, children: impl IntoIterator<Item = T>) {
        self.display_object().def.replace_children(children);
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
        self.display_object().def.is_visible()
    }


    // === EventModel ===

    /// Emit a new event. See docs of [`event::Event`] to learn more.
    fn emit_event<T>(&self, event: T)
    where T: 'static {
        self.display_object().def.emit_event(event)
    }

    /// Get event stream for bubbling events. See docs of [`event::Event`] to learn more.
    fn on_event<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.display_object().def.on_event()
    }

    /// Get event stream for capturing events. You should rather not need this function. Use
    /// [`on_event`] instead. See docs of [`event::Event`] to learn more.
    fn on_event_capturing<T>(&self) -> frp::Stream<event::Event<T>>
    where T: frp::Data {
        self.display_object().def.on_event_capturing()
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


    // === Layout ===

    // /// Get the current auto-layout settings.
    // fn layout(&self) -> Option<AutoLayout> {
    //     self.display_object().def.layout.auto.get()
    // }

    /// Place children in a horizontal layout.
    fn use_auto_layout(&self) -> LayoutObjectBuilder<Horizontal> {
        let instance = self.display_object();
        instance.def.set_layout(AutoLayout::horizontal());
        LayoutObjectBuilder::new(instance)
    }

    /// Place children in a vertical layout.
    fn set_layout_vertical(&self) -> LayoutObjectBuilder<Vertical> {
        let instance = self.display_object();
        instance.def.set_layout(AutoLayout::vertical());
        LayoutObjectBuilder::new(instance)
    }

    /// Remove the auto-layout from this display object.
    fn set_layout_manual(&self) {
        self.display_object().def.set_layout(None);
    }

    /// Get the current resizing settings.
    fn resizing(&self) -> Vector2<Resizing> {
        self.display_object().def.resizing()
    }

    /// The current size of the display object. It will be updated after the object is refreshed.
    fn size(&self) -> Vector2<f32> {
        self.display_object().def.size()
    }

    fn bbox_origin(&self) -> Vector2<f32> {
        self.display_object().def.bbox_origin()
    }

    /// Set the current resizing mode.
    fn set_size(&self, size: impl IntoVector2<f32>) -> &Self {
        self.display_object().def.set_size(size);
        self
    }

    /// Set the current resizing mode.
    fn set_size_x_hug(&self, x: f32) -> &Self {
        self.display_object().def.set_size_x_hug(x);
        self
    }

    fn set_size_x_to_hug(&self) -> &Self {
        self.display_object().def.set_size_x_to_hug();
        self
    }

    fn set_size_y_to_hug(&self) -> &Self {
        self.display_object().def.set_size_y_to_hug();
        self
    }

    fn allow_grow(&self) -> &Self {
        self.display_object().def.allow_grow();
        self
    }

    fn allow_grow_x(&self) -> &Self {
        self.display_object().def.allow_grow_x();
        self
    }

    fn allow_grow_y(&self) -> &Self {
        self.display_object().def.allow_grow_y();
        self
    }

    /// Set the current resizing mode.
    fn set_size_hug_y(&self, y: f32) -> &Self {
        self.display_object().def.set_size_hug_y(y);
        self
    }

    /// Set the current resizing mode.
    fn set_size_hug(&self) {
        self.display_object().def.set_size_hug()
    }

    // FIXME: move to interal api trait
    fn refresh_layout(&self) {
        self.display_object().def.refresh_layout()
    }
}

/// Trait exposing the generic API for working with layouts. Until you are creating layouts shared
/// by multiple display objects and you need to store their configurations, you'd not need to use
/// it.
pub trait GenericLayoutApi: Object {
    /// Layout setter.
    fn set_layout(&self, layout: impl Into<Option<AutoLayout>>) {
        self.display_object().def.set_layout(layout)
    }

    /// Resizing setter.
    fn set_resizing(&self, resizing: impl IntoResizing) {
        self.display_object().def.set_resizing(resizing)
    }
}

impl<T: Object> GenericLayoutApi for T {}



// =======================
// === Hierarchy Tests ===
// =======================

#[cfg(test)]
mod hierarchy_tests {
    use super::*;
    use crate::display::world::World;
    use std::f32::consts::PI;

    #[test]
    fn hierarchy_test() {
        let node1 = Instance::new();
        let node2 = Instance::new();
        let node3 = Instance::new();
        node1.add_child(&node2);
        assert_eq!(node2.my_index(), Some(ChildIndex(0)));

        node1.add_child(&node2);
        assert_eq!(node2.my_index(), Some(ChildIndex(0)));

        node1.add_child(&node3);
        assert_eq!(node3.my_index(), Some(ChildIndex(1)));

        node1.remove_child(&node3);
        assert_eq!(node3.my_index(), None);
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

        node1.modify_position(|t| t.x += 7.0);
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

        node2.modify_position(|t| t.y += 5.0);
        node1.update(scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(7.0, 5.0, 0.0));

        node3.modify_position(|t| t.x += 1.0);
        node1.update(scene);
        assert_eq!(node1.global_position(), Vector3::new(7.0, 0.0, 0.0));
        assert_eq!(node2.global_position(), Vector3::new(7.0, 5.0, 0.0));
        assert_eq!(node3.global_position(), Vector3::new(8.0, 5.0, 0.0));

        node2.modify_rotation(|t| t.z += PI / 2.0);
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
        node1.show();
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
        node1.show();
        node1.update(scene);
        node1.check_if_was_shown();

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
        node1.show();
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
        node1.show();
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

        root.show();

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

        node3.modify_position(|t| t.x += 1.0);
        node4.modify_position(|t| t.x += 3.0);
        node5.modify_position(|t| t.x += 5.0);
        node6.modify_position(|t| t.x += 7.0);

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
        assert_eq!(node1.display_layer().as_ref(), Some(&layer1));
        assert_eq!(node2.display_layer().as_ref(), Some(&layer1));
        assert_eq!(node3.display_layer().as_ref(), Some(&layer1));

        node2.add_to_display_layer(&layer2);
        node1.update(scene);
        assert_eq!(node1.display_layer().as_ref(), Some(&layer1));
        assert_eq!(node2.display_layer().as_ref(), Some(&layer2));
        assert_eq!(node3.display_layer().as_ref(), Some(&layer1));
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
        obj_3.event.source.emit(&event);
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



// ====================
// === Layout Tests ===
// ====================

#[cfg(test)]
mod layout_tests {
    use super::*;
    use crate::display::world::World;


    // === Utils ===

    /// Struct providing setup and utilities for testing a simple layout of objects – a root, and
    /// three of its children:
    ///
    /// ```text
    /// ╭─ ROOT ──────────────────────────────────╮
    /// │  ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮  │
    /// │  ╰─────────╯  ╰─────────╯  ╰─────────╯  │
    /// ╰─────────────────────────────────────────╯
    /// ```
    macro_rules! gen_test_flat_children {
        ($total:tt [$($num:tt),*]) => { paste! {
            #[derive(Debug)]
            pub struct [<TestFlatChildren $total>] {
                world: World,
                root:  Instance,
                $([<node $num>]: Instance),*
            }

            impl [<TestFlatChildren $total>] {
                fn new() -> Self {
                    let world = World::new();
                    let root = Instance::new();
                    $(let [<node $num>] = Instance::new();)*
                    world.add_child(&root);
                    $(root.add_child(&[<node $num>]);)*
                    Self { world, root, $([<node $num>]),* }
                }

                fn reset_positions(&self) {
                    self.root.set_position(Vector3::zero());
                    $(self.[<node $num>].set_position(Vector3::zero());)*
                }

                fn run(&self) -> &Self {
                    self.world.display_object().update(&self.world.default_scene);
                    self
                }

                fn assert_root_position(&self, x:f32, y:f32) -> &Self {
                    assert_eq!(self.root.position().xy().as_slice(), &[x,y]);
                    self
                }

                fn assert_root_size(&self, x:f32, y:f32) -> &Self {
                    assert_eq!(self.root.size().as_slice(), &[x,y]);
                    self
                }

                $(
                    fn [<assert_node $num _position>](&self, x:f32, y:f32) -> &Self {
                        assert_eq!(self.[<node $num>].position().xy().as_slice(), &[x,y]);
                        self
                    }

                    fn [<assert_node $num _size>](&self, x:f32, y:f32) -> &Self {
                        assert_eq!(self.[<node $num>].size().as_slice(), &[x,y]);
                        self
                    }
                )*
            }
        }};
    }

    gen_test_flat_children!(2 [1,2]);
    gen_test_flat_children!(3 [1,2,3]);


    // === Tests ===

    /// Input:
    ///
    /// ```text
    /// ╭▷ ROOT ──────────────────────────╮
    /// │   ╭▷ L ◀ ▶ ──╮   ╭R─ ▶ ◀ ───╮   │
    /// │   │ ╭ ◀ ▶ ╮  │   ▽ ╭────╮   │   │
    /// │   │ │ L1  │  │   │ │ R1 ▲   │   │
    /// │   │ │     │  │   │ │    ▼   │   │
    /// │   │ │     │  ▼   │ ╰────╯   ▲   │
    /// │   │ │     │  ▲   │ ╭────╮   ▼   │
    /// │   │ │     │  │   │ │ R2 ▲   │   │
    /// │   │ │     │  │   │ │    ▼   │   │
    /// │   │ ╰─────╯  │   │ ╰────╯   │   │
    /// │   ╰──────────╯   ╰──────────╯   │
    /// ╰─────────────────────────────────╯
    /// ```
    ///
    /// Output:
    /// The dimensions in parentheses were provided manually.
    ///
    /// ```text
    /// ╭▷ ROOT ─────────────────────────────────────────╮
    /// │                          ╭R─ ▶ ◀ ──────╮       │
    /// │                          ▽ ╭────╮      │       │
    /// │                          │ │ R2 ▲ 50   │       │
    /// │  ╭▷ L ◀ ▶ ────────╮      │ │    ▼      │       │
    /// │  │ ╭ ◀ ▶ ╮        │      │ ╰────╯      │       │
    /// │  │ │ L1  │        ▼      │  (30)       │       │
    /// │  │ │     │ (50)   ▲ 50   │             ▲ 100   │
    /// │  │ ╰─────╯        │      │ ╭────╮      ▼       │ (100)
    /// │  │   70           │      │ │ R1 ▲ 50   │       │
    /// │  ╰────────────────╯      │ │    ▼      │       │
    /// │         70               │ ╰────╯      │       │
    /// │                          │  (20)       │       │
    /// │                          ╰─────────────╯       │
    /// │                               30               │
    /// ╰────────────────────────────────────────────────╯
    ///                      (100)
    /// ```   
    #[test]
    fn test_mixed_layouts() {
        let world = World::new();
        let root = Instance::new_named("Root");
        let l = Instance::new_named("L");
        let l1 = Instance::new_named("L1");
        let r = Instance::new_named("R");
        let r1 = Instance::new_named("R1");
        let r2 = Instance::new_named("R2");
        root.add_child(&l);
        root.add_child(&r);
        l.add_child(&l1);
        r.add_child(&r1);
        r.add_child(&r2);

        root.use_auto_layout().set_alignment_center();
        root.set_size((100.0, 100.0));

        l.use_auto_layout();
        l.set_size_x_hug(0.0).allow_grow_x();
        l1.set_size((0.0, 50.0)).allow_grow_x();

        r.set_layout_vertical();
        r.set_size_hug_y(0.0).allow_grow_y();
        r1.set_size((20.0, 0.0)).allow_grow_y();
        r2.set_size((30.0, 0.0)).allow_grow_y();

        root.update(&world.default_scene);

        println!("L size: {:?}", l.size());
        assert_eq!(root.position().xy(), Vector2(0.0, 0.0));
        assert_eq!(l.position().xy(), Vector2(0.0, 25.0));
        assert_eq!(r.position().xy(), Vector2(70.0, 0.0));
        assert_eq!(l1.position().xy(), Vector2(0.0, 0.0));
        assert_eq!(r1.position().xy(), Vector2(0.0, 0.0));
        assert_eq!(r2.position().xy(), Vector2(0.0, 50.0));

        assert_eq!(root.size(), Vector2(100.0, 100.0));
        assert_eq!(l.size(), Vector2(70.0, 50.0));
        assert_eq!(r.size(), Vector2(30.0, 100.0));
        assert_eq!(r1.size(), Vector2(20.0, 50.0));
        assert_eq!(r2.size(), Vector2(30.0, 50.0));
    }

    #[test]
    fn test_column() {
        let world = World::new();
        let root = Instance::new_named("Root");
        let node1 = Instance::new_named("node1");
        let node2 = Instance::new_named("node2");
        root.add_child(&node1);
        root.add_child(&node2);

        root.use_auto_layout();
        root.set_size((200.0, 200.0));
        node1.set_size((100.0, 100.0));
        node2.set_size((100.0, 100.0));

        root.update(&world.default_scene);

        println!("node1: {:?}", node1);
        println!("node1: {:?}", node2);

        assert_eq!(1, 2);
    }

    // /// ```text
    // /// ╭▷ ROOT ─────────── ▶ ◀ ──────────────────────╮
    // /// │       ⋯5            ⋯5            ⋯5        │
    // /// │   ╭─ node1 ─╮   ╭─ node2 ─╮   ╭─ node3 ─╮   ▼
    // /// │ ⋯ │         │ ⋯ │         │ ⋯ │         │ ⋯ │
    // /// │ 3 ╰─────────╯ 1 ╰─────────╯ 1 ╰─────────╯ 3 ▲
    // /// │       ⋯5            ⋯5            ⋯5        │
    // /// ╰─────────────────────────────────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_hug_resizing() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().padding_xy(3.0, 5.0).spacing(1.0);
    //     test.node1.set_size((20.0, 200.0));
    //     test.node2.set_size((30.0, 300.0));
    //     test.node3.set_size((50.0, 500.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(3.0, 5.0)
    //         .assert_node2_position(24.0, 5.0)
    //         .assert_node3_position(55.0, 5.0)
    //         .assert_root_size(108.0, 510.0)
    //         .assert_node1_size(20.0, 200.0)
    //         .assert_node2_size(30.0, 300.0)
    //         .assert_node3_size(50.0, 500.0);
    // }
    //
    // /// ```text
    // /// ╭─ ROOT ─ ▶ ◀ ────╮
    // /// ▽       ⋯5        │
    // /// │   ╭─ node3 ─╮   │
    // /// │ ⋯ │         │ ⋯ │
    // /// │ 3 ╰─────────╯ 3 │
    // /// │       ⋯1        │
    // /// │   ╭─ node2 ─╮   ▼
    // /// │ ⋯ │         │ ⋯ │
    // /// │ 3 ╰─────────╯ 3 ▲
    // /// │       ⋯1        │
    // /// │   ╭─ node1 ─╮   │
    // /// │ ⋯ │         │ ⋯ │
    // /// │ 3 ╰─────────╯ 3 │
    // /// │       ⋯5        │
    // /// ╰─────────────────╯
    // /// ```
    // #[test]
    // fn test_vertical_hug_resizing() {
    //     let test = TestFlatChildren3::new();
    //     test.root.set_layout_vertical().padding_xy(3.0, 5.0).spacing(1.0);
    //     test.node1.set_size((20.0, 200.0));
    //     test.node2.set_size((30.0, 300.0));
    //     test.node3.set_size((50.0, 500.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(3.0, 5.0)
    //         .assert_node2_position(3.0, 206.0)
    //         .assert_node3_position(3.0, 507.0)
    //         .assert_root_size(56.0, 1012.0)
    //         .assert_node1_size(20.0, 200.0)
    //         .assert_node2_size(30.0, 300.0)
    //         .assert_node3_size(50.0, 500.0);
    // }
    //
    // /// ```text
    // /// ╭▷ ROOT ─────────── ▶ ◀ ──────────────────╮
    // /// │  ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮  │
    // /// │  │         ▼  │         ▼  │         ▼  ▼
    // /// │  │         ▲  │         ▲  │         ▲  ▲
    // /// │  ╰─────────╯  ╰── ▶ ◀ ──╯  ╰── ◀ ▶ ──╯  │
    // /// ╰─────────────────────────────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_nested_hug_resizing() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout();
    //     test.node1.set_size_x_hug(200.0);
    //     test.node2.set_size_hug();
    //     test.node3.set_size_fill_hug();
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(0.0, 0.0)
    //         .assert_node2_position(200.0, 0.0)
    //         .assert_node3_position(200.0, 0.0)
    //         .assert_root_size(200.0, 0.0)
    //         .assert_node1_size(200.0, 0.0)
    //         .assert_node2_size(0.0, 0.0)
    //         .assert_node3_size(0.0, 0.0);
    // }
    //
    // /// ```text
    // /// ╭─ ROOT ─ ▶ ◀ ──╮
    // /// ▽  ╭─ node1 ─╮  │
    // /// │  │         │  │
    // /// │  │         │  │
    // /// │  ╰── ▶ ◀ ──╯  │
    // /// │  ╭─ node2 ─╮  │
    // /// │  │         ▼  ▼
    // /// │  │         ▲  ▲
    // /// │  ╰── ▶ ◀ ──╯  │
    // /// │  ╭─ node3 ─╮  │
    // /// │  │         ▲  │
    // /// │  │         ▼  │
    // /// │  ╰── ▶ ◀ ──╯  │
    // /// ╰───────────────╯
    // /// ```
    // #[test]
    // fn test_vertical_nested_hug_resizing() {
    //     let test = TestFlatChildren3::new();
    //     test.root.set_layout_vertical();
    //     test.node1.set_size_hug_y(200.0);
    //     test.node2.set_size_hug();
    //     test.node3.set_size_hug_fill();
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(0.0, 0.0)
    //         .assert_node2_position(0.0, 200.0)
    //         .assert_node3_position(0.0, 200.0)
    //         .assert_root_size(0.0, 200.0)
    //         .assert_node1_size(0.0, 200.0)
    //         .assert_node2_size(0.0, 0.0)
    //         .assert_node3_size(0.0, 0.0);
    // }
    //
    // /// ```text
    // /// ╭▷ ROOT ──────────────────────────────────────────╮
    // /// │ ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮           │
    // /// │ ╰─────────╯  ╰─────────╯  ╰─────────╯           │
    // /// ╰─────────────────────────────────────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_packed_left_alignment() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().alignment_start_center();
    //     test.root.set_size((1000.0, 1000.0));
    //     test.node1.set_size((100.0, 100.0));
    //     test.node2.set_size((100.0, 100.0));
    //     test.node3.set_size((100.0, 100.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(0.0, 450.0)
    //         .assert_node2_position(100.0, 450.0)
    //         .assert_node3_position(200.0, 450.0)
    //         .assert_root_size(1000.0, 1000.0)
    //         .assert_node1_size(100.0, 100.0)
    //         .assert_node2_size(100.0, 100.0)
    //         .assert_node3_size(100.0, 100.0);
    // }
    //
    // /// ```text
    // /// ╭▷ ROOT ──────────────────────────────────────────╮
    // /// │      ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮      │
    // /// │      ╰─────────╯  ╰─────────╯  ╰─────────╯      │
    // /// ╰─────────────────────────────────────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_packed_center_alignment() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().alignment_center();
    //     test.root.set_size((1000.0, 1000.0));
    //     test.node1.set_size((100.0, 100.0));
    //     test.node2.set_size((100.0, 100.0));
    //     test.node3.set_size((100.0, 100.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(350.0, 450.0)
    //         .assert_node2_position(450.0, 450.0)
    //         .assert_node3_position(550.0, 450.0)
    //         .assert_root_size(1000.0, 1000.0)
    //         .assert_node1_size(100.0, 100.0)
    //         .assert_node2_size(100.0, 100.0)
    //         .assert_node3_size(100.0, 100.0);
    // }
    //
    // /// ```text
    // /// ╭▷ ROOT ──────────────────────────────────────────╮
    // /// │           ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮ │
    // /// │           ╰─────────╯  ╰─────────╯  ╰─────────╯ │
    // /// ╰─────────────────────────────────────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_packed_right_alignment() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().alignment_end_center();
    //     test.root.set_size((1000.0, 1000.0));
    //     test.node1.set_size((100.0, 100.0));
    //     test.node2.set_size((100.0, 100.0));
    //     test.node3.set_size((100.0, 100.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(700.0, 450.0)
    //         .assert_node2_position(800.0, 450.0)
    //         .assert_node3_position(900.0, 450.0)
    //         .assert_root_size(1000.0, 1000.0)
    //         .assert_node1_size(100.0, 100.0)
    //         .assert_node2_size(100.0, 100.0)
    //         .assert_node3_size(100.0, 100.0);
    // }
    //
    // /// ```text
    // /// ╭▷ ROOT ────────────╮
    // /// │ ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮
    // /// │ │         │  │    │    │  │         │
    // /// │ ╰─────────╯  ╰─────────╯  ╰─────────╯
    // /// ╰───────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_packed_left_alignment_overflow() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().alignment_start_center();
    //     test.root.set_size((150.0, 100.0));
    //     test.node1.set_size((100.0, 100.0));
    //     test.node2.set_size((100.0, 100.0));
    //     test.node3.set_size((100.0, 100.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(0.0, 0.0)
    //         .assert_node2_position(100.0, 0.0)
    //         .assert_node3_position(200.0, 0.0)
    //         .assert_root_size(150.0, 100.0)
    //         .assert_node1_size(100.0, 100.0)
    //         .assert_node2_size(100.0, 100.0)
    //         .assert_node3_size(100.0, 100.0);
    // }
    //
    // /// ```text
    // ///      ╭▷ ROOT ──────────────────╮
    // /// ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮
    // /// │    │    │  │         │  │    │    │
    // /// ╰─────────╯  ╰─────────╯  ╰─────────╯
    // ///      ╰─────────────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_packed_center_alignment_overflow() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().alignment_center();
    //     test.root.set_size((200.0, 100.0));
    //     test.node1.set_size((100.0, 100.0));
    //     test.node2.set_size((100.0, 100.0));
    //     test.node3.set_size((100.0, 100.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(-50.0, 0.0)
    //         .assert_node2_position(50.0, 0.0)
    //         .assert_node3_position(150.0, 0.0)
    //         .assert_root_size(200.0, 100.0)
    //         .assert_node1_size(100.0, 100.0)
    //         .assert_node2_size(100.0, 100.0)
    //         .assert_node3_size(100.0, 100.0);
    // }
    //
    // /// ```text
    // ///                   ╭▷ ROOT ────────────╮
    // /// ╭─ node1 ─╮  ╭─ node2 ─╮  ╭─ node3 ─╮ │
    // /// │         │  │    │    │  │         │ │
    // /// ╰─────────╯  ╰─────────╯  ╰─────────╯ │
    // ///                   ╰───────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_packed_right_alignment_overflow() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().alignment_end_center();
    //     test.root.set_size((150.0, 100.0));
    //     test.node1.set_size((100.0, 100.0));
    //     test.node2.set_size((100.0, 100.0));
    //     test.node3.set_size((100.0, 100.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(-150.0, 0.0)
    //         .assert_node2_position(-50.0, 0.0)
    //         .assert_node3_position(50.0, 0.0)
    //         .assert_root_size(150.0, 100.0)
    //         .assert_node1_size(100.0, 100.0)
    //         .assert_node2_size(100.0, 100.0)
    //         .assert_node3_size(100.0, 100.0);
    // }
    //
    // /// ```text
    // /// ╭▷ ROOT ────────────────────────────────────────────╮
    // /// │ ╭─ node1 ─╮        ╭─ node2 ─╮        ╭─ node3 ─╮ │
    // /// │ ╰─────────╯        ╰─────────╯        ╰─────────╯ │
    // /// ╰───────────────────────────────────────────────────╯
    // /// ```
    // #[test]
    // fn test_horizontal_spaced_alignment() {
    //     let test = TestFlatChildren3::new();
    //     test.root.use_auto_layout().alignment_end_center();
    //     test.root.set_size((1000.0, 100.0));
    //     test.node1.set_size((100.0, 100.0));
    //     test.node2.set_size((100.0, 100.0));
    //     test.node3.set_size((100.0, 100.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(700.0, 0.0)
    //         .assert_node2_position(800.0, 0.0)
    //         .assert_node3_position(900.0, 0.0)
    //         .assert_root_size(1000.0, 100.0)
    //         .assert_node1_size(100.0, 100.0)
    //         .assert_node2_size(100.0, 100.0)
    //         .assert_node3_size(100.0, 100.0);
    // }
    //
    // /// ```text
    // ///      ╭─ ROOT ────────────────────────╮
    // ///      │                   ╭─ node3 ─╮ │
    // /// ╭─ node1 ─╮              │         │ │
    // /// │    │    │              │         │ │
    // /// │    │    │  ╭─ node2 ─╮ ╰─────────╯ │
    // /// ╰─────────╯  │         │             │
    // ///      ╰───────│─────────│─────────────╯
    // ///              ╰─────────╯
    // /// ```
    // #[test]
    // fn test_layout_manual_fixed() {
    //     let test = TestFlatChildren3::new();
    //     test.root.set_size((40.0, 20.0));
    //     test.node1.set_size((20.0, 20.0));
    //     test.node2.set_size((20.0, 20.0));
    //     test.node3.set_size((20.0, 20.0));
    //     test.node1.set_xy(Vector2(-10.0, 0.0));
    //     test.node2.set_xy(Vector2(10.0, -10.0));
    //     test.node3.set_xy(Vector2(30.0, 10.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(-10.0, 0.0)
    //         .assert_node2_position(10.0, -10.0)
    //         .assert_node3_position(30.0, 10.0)
    //         .assert_root_size(40.0, 20.0)
    //         .assert_node1_size(20.0, 20.0)
    //         .assert_node2_size(20.0, 20.0)
    //         .assert_node3_size(20.0, 20.0);
    // }
    //
    // /// ```text
    // /// ╭─────┬─ ROOT ─── ▶ ◀ ────────────────╮
    // /// │     ┆                   ╭─ node3 ─╮ │
    // /// │╭─ node1 ─╮              │         │ │
    // /// ││    ┆    │              │         │ ▼
    // /// ││    ┆    │  ╭─ node2 ─╮ ╰─────────╯ ▲
    // /// │╰────┼────╯  │         │             │
    // /// │     ◎┈┈┈┈┈┈┈│┈┈┈┈┈┈┈┈┈│┈┈┈┈┈┈┈┈┈┈┈┈┈┤
    // /// │             ╰─────────╯             │
    // /// ╰─────────────────────────────────────╯
    // /// ```
    // #[test]
    // fn test_layout_manual_hug() {
    //     let test = TestFlatChildren3::new();
    //     test.node1.set_size((20.0, 20.0));
    //     test.node2.set_size((20.0, 20.0));
    //     test.node3.set_size((20.0, 20.0));
    //     test.node1.set_xy(Vector2(-10.0, 0.0));
    //     test.node2.set_xy(Vector2(10.0, -10.0));
    //     test.node3.set_xy(Vector2(30.0, 10.0));
    //     test.run()
    //         .assert_root_position(0.0, 0.0)
    //         .assert_node1_position(-10.0, 0.0)
    //         .assert_node2_position(10.0, -10.0)
    //         .assert_node3_position(30.0, 10.0)
    //         .assert_root_size(60.0, 40.0)
    //         .assert_node1_size(20.0, 20.0)
    //         .assert_node2_size(20.0, 20.0)
    //         .assert_node3_size(20.0, 20.0);
    // }
}
