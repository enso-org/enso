//! Implementation of display objects, elements that have visual representation and can form
//! hierarchical layouts. The implementation is very careful about performance, it tracks the
//! transformation changes and updates only the needed subset of the display object tree on demand.

use crate::data::dirty::traits::*;
use crate::display::object::layout::*;
use crate::prelude::*;

use crate::display::layout::alignment;
use crate::display::object::event;
use crate::display::object::transformation;
use crate::display::scene::layer::Layer;
use crate::display::scene::layer::WeakLayer;
use crate::display::scene::Scene;

use data::opt_vec::OptVec;
use nalgebra::Matrix4;
use nalgebra::Vector3;
use transformation::CachedTransformation;
use unit2::Fraction;
use unit2::Percent;



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
/// ## Size
/// Every display object can be configured with a horizontal and vertical size mode. There are
/// three modes available:
/// - `Hug` (default). In this mode, the display object size will be set to the bounding box of its
///   children. In case of no children, the display object will be resized to (0,0). The only
///   exception is setting the hug size on a display object with a manual layout. Then, its size
///   will be set to the children bounding box clipped to the positive X- and Y-axis.
/// - `Fill`. In this mode, the display object will fill the free space in its parent. If several
///   children are set to this mode, the free space will be divided evenly between them. If the
///   parent size was set to `Hug`, all its children with size set to `Fill` will be resized to 0.
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
/// Documentation of the layout uses drawings with graphical symbols for different size and
/// layout modes. For example, the illustration below shows a ROOT display object with a horizontal
/// layout and fixed size. The L child has a horiontal layout, fill horizontal size, and a
/// fixed vertical size. The R child has a vertical layout, hug horizontal size, and a fill
/// vertical size.
///
/// ```text
/// ╭▷ ROOT ──────────────────────────╮
/// │   ╭▷ L ◀ ▶ ──╮   ╭R─ ▶ ◀ ───╮   │   Auto-layout Legend:         
/// │   │ ╭ ◀ ▶ ╮  │   ▽ ╭────╮   │   │   ┄── ▷ ──┄ : Horizontal auto-layout.
/// │   │ │ L1  │  │   │ │ R2 ▲   │   │   ┄── ▽ ──┄ : Vertical auto-layout.
/// │   │ │     │  │   │ │    ▼   │   │   ┄───────┄ : Manual layout.   
/// │   │ │     │  ▼   │ ╰────╯   ▲   │
/// │   │ │     │  ▲   │ ╭────╮   ▼   │   Size Legend:             
/// │   │ │     │  │   │ │ R1 ▲   │   │   ┄── ◀ ▶ ──┄ : Fill size.
/// │   │ │     │  │   │ │    ▼   │   │   ┄── ▶ ◀ ──┄ : Hug size.  
/// │   │ ╰─────╯  │   │ ╰────╯   │   │   ┄─────────┄ : Fixed size.
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
    layout:    LayoutModel,
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
        let layout = LayoutModel::default();
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
            .field("size", &self.layout.computed_size.get().as_slice())
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

// ===================
// === LayoutModel ===
// ===================

/// Layout properties of this display object. It can be considered similar to the [`AutoLayout`]
/// struct, which describes the layout properties of children.
#[derive(Debug, Derivative)]
#[derivative(Default)]
pub struct LayoutModel {
    auto_layout:   RefCell<Option<AutoLayout>>,
    alignment:     Cell<alignment::OptDim2>,
    margin:        Cell<Vector2<SideSpacing>>,
    padding:       Cell<Vector2<SideSpacing>>,
    #[derivative(Default(
        value = "Cell::new((f32::INFINITY, f32::INFINITY).into_vector_trans())"
    ))]
    max_size:      Cell<Vector2<Unit>>,
    min_size:      Cell<Vector2<Unit>>,
    size:          Cell<Vector2<Size>>,
    grow_factor:   Cell<Vector2<f32>>,
    shrink_factor: Cell<Vector2<f32>>,
    computed_size: Cell<Vector2<f32>>,
    bbox_origin:   Cell<Vector2<f32>>,
}


// === Margin and padding ===

impl LayoutModel {
    pub fn set_margin_all(&self, value: Unit) {
        let margin = SideSpacing::from(value);
        self.margin.set(Vector2(margin, margin));
    }

    pub fn set_margin_trbl(&self, top: Unit, right: Unit, bottom: Unit, left: Unit) {
        let horizontal = SideSpacing::new(left, right);
        let vertical = SideSpacing::new(bottom, top);
        self.margin.set(Vector2(horizontal, vertical));
    }

    pub fn set_padding_all(&self, value: Unit) {
        let padding = SideSpacing::from(value);
        self.padding.set(Vector2(padding, padding));
    }

    pub fn set_padding_trbl(&self, top: Unit, right: Unit, bottom: Unit, left: Unit) {
        let horizontal = SideSpacing::new(left, right);
        let vertical = SideSpacing::new(bottom, top);
        self.padding.set(Vector2(horizontal, vertical));
    }
}

impl Model {
    fn modify_layout(&self, f: impl FnOnce(&LayoutModel)) {
        self.dirty.transformation.set();
        f(&self.layout);
    }

    fn set_layout(&self, layout: impl Into<Option<AutoLayout>>) {
        self.modify_layout(|l| {
            l.auto_layout.replace(layout.into());
        });
    }
}

macro_rules! gen_alignment_setters {
    ([$([$name:ident $x:tt $y:tt])*]) => {
        paste! {
            $(
                /// Alignment setter.
                fn [<align_ $name>](&self)  -> &Self {
                    self.display_object().modify_layout(|l| {
                        l.alignment.modify(|t| t.[<align_ $name>]());
                    });
                    self
                }
            )*
        }
    }
}

macro_rules! gen_spacing_props_for_layout_model2 {
    ([$tp: ident] [ $([$name:ident $axis:ident $loc:ident])* ]) => { paste! { $(
        /// Spacing setter.
        fn [<modify_ $tp _ $name>](&self, f: impl FnOnce(&mut Unit)) -> &Self {
            self.display_object().modify_layout(|layout| {
                layout.$tp.modify(|t| f(&mut t.$axis.$loc));
            });
            self
        }

        /// Spacing setter.
        fn [<set_ $tp _ $name>](&self, value: impl Into<Unit>) -> &Self {
            self.[<modify_ $tp _ $name>](|t| *t = value.into())
        }

        /// Spacing getter.
        fn [<update_ $tp _ $name>](&self, f: impl FnOnce(Unit) -> Unit) -> &Self {
            self.[<modify_ $tp _ $name>](|t| *t = f(*t))
        }
    )*}}
}

macro_rules! gen_prop_accessors {
    ($name:ident : Vector2<$ty:ty>) => {
        paste! {
            fn $name(&self) -> Vector2<$ty> {
                self.display_object().layout.$name.get()
            }

            fn [<modify_ $name>](&self, f: impl FnOnce(&mut Vector2<$ty>)) -> &Self {
                self.display_object().modify_layout(|l| l.$name.modify_(f));
                self
            }

            fn [<set_ $name>](&self, value: impl IntoVectorTrans2<$ty>) -> &Self {
                self.[<modify_ $name>](|t| *t = value.into_vector_trans())
            }

            fn [<update_ $name>](&self, f: impl FnOnce(Vector2<$ty>) -> Vector2<$ty>) -> &Self {
                self.[<modify_ $name>](|t| *t = f(*t))
            }

            fn [<modify_ $name _x>](&self, f: impl FnOnce(&mut $ty)) -> &Self {
                self.[<modify_ $name>](|t| f(&mut t.x))
            }

            fn [<set_ $name _x>](&self, v: impl Into<$ty>) -> &Self {
                self.[<modify_ $name _x>](|t| *t = v.into())
            }

            fn [<update_ $name _x>](&self, f: impl FnOnce($ty) -> $ty) -> &Self {
                self.[<modify_ $name _x>](|t| *t = f(*t))
            }

            fn [<modify_ $name _y>](&self, f: impl FnOnce(&mut $ty)) -> &Self {
                self.[<modify_ $name>](|t| f(&mut t.y))
            }

            fn [<set_ $name _y>](&self, v: impl Into<$ty>) -> &Self {
                self.[<modify_ $name _y>](|t| *t = v.into())
            }

            fn [<update_ $name _y>](&self, f: impl FnOnce($ty) -> $ty) -> &Self {
                self.[<modify_ $name _y>](|t| *t = f(*t))
            }
        }
    };
}


impl<T: Object + ?Sized> LayoutOps for T {}
pub trait LayoutOps: Object {
    crate::with_alignment_opt_dim2_named_matrix_sparse!(gen_alignment_setters);
    crate::with_display_object_side_spacing_matrix!(gen_spacing_props_for_layout_model2[margin]);
    crate::with_display_object_side_spacing_matrix!(gen_spacing_props_for_layout_model2[padding]);

    gen_prop_accessors!(max_size: Vector2<Unit>);
    gen_prop_accessors!(min_size: Vector2<Unit>);
    gen_prop_accessors!(size: Vector2<Size>);
    gen_prop_accessors!(grow_factor: Vector2<f32>);
    gen_prop_accessors!(shrink_factor: Vector2<f32>);

    fn bbox_origin(&self) -> Vector2<f32> {
        self.display_object().def.layout.bbox_origin.get()
    }

    fn computed_size(&self) -> Vector2<f32> {
        self.display_object().def.layout.computed_size.get()
    }


    fn set_size_x_hug(&self, x: f32) -> &Self {
        self.set_size((x, Size::Hug));
        self
    }

    fn set_size_x_to_hug(&self) -> &Self {
        self.modify_size(|t| t.x = Size::Hug);
        self
    }

    fn set_size_y_to_hug(&self) -> &Self {
        self.modify_size(|t| t.y = Size::Hug);
        self
    }

    fn allow_grow(&self) -> &Self {
        self.set_grow_factor((1.0, 1.0))
    }

    fn allow_grow_x(&self) -> &Self {
        self.set_grow_factor_x(1.0)
    }

    fn allow_grow_y(&self) -> &Self {
        self.set_grow_factor_y(1.0)
    }

    fn allow_shrink(&self) -> &Self {
        self.set_shrink_factor((1.0, 1.0))
    }

    fn allow_shrink_x(&self) -> &Self {
        self.set_shrink_factor_x(1.0)
    }

    fn allow_shrink_y(&self) -> &Self {
        self.set_shrink_factor_y(1.0)
    }

    fn set_size_hug_y(&self, y: f32) -> &Self {
        self.set_size((Size::Hug, y));
        self
    }

    fn set_size_hug(&self) -> &Self {
        self.set_size((Size::Hug, Size::Hug));
        self
    }

    fn set_margin_all(&self, value: impl Into<Unit>) -> &Self {
        self.display_object().modify_layout(|l| l.set_margin_all(value.into()));
        self
    }

    fn set_margin_trbl(
        &self,
        top: impl Into<Unit>,
        right: impl Into<Unit>,
        bottom: impl Into<Unit>,
        left: impl Into<Unit>,
    ) -> &Self {
        self.display_object().modify_layout(|l| {
            l.set_margin_trbl(top.into(), right.into(), bottom.into(), left.into())
        });
        self
    }

    fn set_padding_all(&self, value: impl Into<Unit>) -> &Self {
        self.display_object().modify_layout(|l| l.set_padding_all(value.into()));
        self
    }

    fn set_padding_trbl(
        &self,
        top: impl Into<Unit>,
        right: impl Into<Unit>,
        bottom: impl Into<Unit>,
        left: impl Into<Unit>,
    ) -> &Self {
        self.display_object().modify_layout(|l| {
            l.set_padding_trbl(top.into(), right.into(), bottom.into(), left.into())
        });
        self
    }
}



// ======================
// === Column and Row ===
// ======================

/// The auto-layout grid column or row definition.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[allow(missing_docs)]
pub struct ColumnOrRow {
    size:     Size,
    min_size: Option<f32>,
    max_size: Option<f32>,
    grow:     Option<f32>,
    shrink:   Option<f32>,
}

/// The auto-layout column definition.
#[derive(Clone, Copy, Debug, Default, Deref, DerefMut, PartialEq)]
pub struct Column {
    def: ColumnOrRow,
}

/// The auto-layout row definition.
#[derive(Clone, Copy, Debug, Default, Deref, DerefMut, PartialEq)]
pub struct Row {
    def: ColumnOrRow,
}



pub type ColumnRef = ColumnOrRowRef<X>;
pub type RowRef = ColumnOrRowRef<Y>;

#[derive(Debug)]
pub struct ColumnOrRowRef<Dim> {
    instance: Instance,
    index:    usize,
    axis:     PhantomData<Dim>,
}

impl<Dim> ColumnOrRowRef<Dim> {
    fn new(instance: Instance, index: usize) -> Self {
        Self { instance, index, axis: PhantomData }
    }
}

macro_rules! gen_column_or_row_builder_props {
    ($($name:ident : $ty:ty),*) => { paste! { $(
            pub fn [<modify_ $name>](&self, f: impl FnOnce(&mut $ty)) {
                self.modify(|t| f(&mut t.$name))
            }

            pub fn [<set_ $name>](&self, value: impl Into<$ty>) {
                self.modify(|t| t.$name = value.into())
            }

            pub fn [<update_ $name>](&self, f: impl FnOnce($ty) -> $ty) {
                self.modify(|t| t.$name = f(t.$name))
            }
    )*}};
}

pub trait ModifyBounds = where
    Self: Default,
    <(Column, Row) as DimRef<Self>>::Output: DerefMut + Deref<Target = ColumnOrRow>,
    (Column, Row): DimMut<Self>,
    (Vec<Column>, Vec<Row>): DimMut<Self, Output = Vec<<(Column, Row) as DimRef<Self>>::Output>>;

impl<Dim: ModifyBounds> ColumnOrRowRef<Dim> {
    fn modify(&self, f: impl FnOnce(&mut ColumnOrRow)) {
        self.instance
            .modify_auto_layout(|l| f(l.unchecked_column_or_row_mut(Dim::default(), self.index)));
    }

    gen_column_or_row_builder_props!(
        size: Size,
        min_size: Option<f32>,
        max_size: Option<f32>,
        grow: Option<f32>,
        shrink: Option<f32>
    );
}



// ==================
// === AutoLayout ===
// ==================

/// The auto-layout options. The auto-layout is similar to a mix of CSS flexbox and CSS grid with a
/// few additions. Read the docs of this module to learn more.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct AutoLayout {
    /// The default item alignment in grid cells. This can be overriden per-item.
    pub children_alignment: alignment::Dim2,
    /// The spacing between axes.
    pub gap: Vector2<Unit>,
    /// Indicates whether the axes should be placed in order or in a reversed order.
    pub reversed_columns_and_rows: Vector2<bool>,
    /// First column and row definition.
    pub first_column_and_row: (Column, Row),
    /// Other columns and rows definitions.
    pub other_columns_and_rows: (Vec<Column>, Vec<Row>),
    /// The number of columns and rows in the grid. If it's set to [`None`], the columns and rows
    /// will grow on demand.
    pub max_columns_and_rows: Vector2<Option<usize>>,
}

impl AutoLayout {
    fn unchecked_column_or_row_mut<'a, Dim, T>(
        &'a mut self,
        dim: Dim,
        index: usize,
    ) -> &mut ColumnOrRow
    where
        Dim: Default,
        T: DerefMut + 'a,
        T: Deref<Target = ColumnOrRow>,
        (Column, Row): DimMut<Dim, Output = T>,
        (Vec<Column>, Vec<Row>): DimMut<Dim, Output = Vec<T>>,
    {
        match index {
            0 => self.first_column_and_row.get_dim_mut(dim).deref_mut(),
            i => &mut self.other_columns_and_rows.get_dim_mut(dim)[i - 1],
        }
    }
}


// === Getters ===

/// A trait used to get columns and rows definitions from the auto layout. The definition consist
/// of the first column/row and other columns/rows if defined.
#[allow(missing_docs)]
pub trait ColumnAndRowDefinitionsGetter<T> {
    fn columns_or_rows(&self, dim: T) -> Vec<ColumnOrRow>;
}

impl ColumnAndRowDefinitionsGetter<X> for AutoLayout {
    fn columns_or_rows(&self, _dim: X) -> Vec<ColumnOrRow> {
        vec![*self.first_column_and_row.0]
            .extended(self.other_columns_and_rows.0.iter().map(|t| **t))
    }
}

impl ColumnAndRowDefinitionsGetter<Y> for AutoLayout {
    fn columns_or_rows(&self, _dim: Y) -> Vec<ColumnOrRow> {
        vec![*self.first_column_and_row.1]
            .extended(self.other_columns_and_rows.1.iter().map(|t| **t))
    }
}



// ================================
// === AutoLayout modifications ===
// ================================

#[allow(missing_docs)]
impl Model {
    fn modify_auto_layout<T>(&self, f: impl FnOnce(&mut AutoLayout) -> T) -> Option<T> {
        if let Some(layout) = &mut *self.layout.auto_layout.borrow_mut() {
            self.dirty.transformation.set();
            Some(f(layout))
        } else {
            warn!(
                "Cannot access auto-layout properties because this display object does not use \
                 auto-layout. Use `use_auto_layout` to enable it first."
            );
            None
        }
    }

    fn set_children_alignment(&self, value: alignment::Dim2) {
        self.modify_auto_layout(|l| l.children_alignment = value);
    }
}

impl InstanceDef {
    fn first_column_or_row<Dim>(&self) -> ColumnOrRowRef<Dim> {
        ColumnOrRowRef::new(self.clone_ref().into(), 0)
    }

    fn column_or_row<Dim, T>(&self, dim: Dim, index: usize) -> Option<ColumnOrRowRef<Dim>>
    where (Vec<Column>, Vec<Row>): DimRef<Dim, Output = Vec<T>> {
        self.modify_auto_layout(|l| {
            (l.other_columns_and_rows.get_dim(dim).len() <= index)
                .as_some(ColumnOrRowRef::new(self.clone_ref().into(), index))
        })
        .flatten()
    }

    fn add_column_or_row<Dim, T>(&self, dim: Dim) -> ColumnOrRowRef<Dim>
    where
        Dim: Copy,
        T: Default,
        (Vec<Column>, Vec<Row>): DimMut<Dim, Output = Vec<T>>, {
        let index = self.modify_auto_layout(|l| {
            l.other_columns_and_rows.get_dim_mut(dim).push(default());
            l.other_columns_and_rows.get_dim(dim).len()
        });
        ColumnOrRowRef::new(self.clone_ref().into(), index.unwrap_or(0))
    }
}


macro_rules! gen_layout_object_builder_alignment {
    ([$([$name:ident $x:ident $y:ident])*]) => { paste! { $(
        /// Children alignment setter.
        fn [<align_children_ $name>](&self) {
            self.display_object().def.set_children_alignment(alignment::Dim2::$name())
        }
    )*}}
}

impl<T: Object + ?Sized> AutoLayoutOps for T {}

/// Options for controllin auto-layout.
pub trait AutoLayoutOps: Object {
    /// Gap setter. Gap is the space between columns and rows.
    fn modify_gap(&self, f: impl FnOnce(&mut Vector2<Unit>)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.gap));
        self
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn set_gap(&self, value: impl IntoVectorTrans2<Unit>) -> &Self {
        self.modify_gap(|v| *v = value.into_vector_trans())
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn update_gap(&self, f: impl FnOnce(Vector2<Unit>) -> Vector2<Unit>) -> &Self {
        self.modify_gap(|v| *v = f(*v))
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn modify_gap_x(&self, f: impl FnOnce(&mut Unit)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.gap.x));
        self
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn set_gap_x(&self, value: impl Into<Unit>) -> &Self {
        self.modify_gap_x(|v| *v = value.into())
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn update_gap_x(&self, f: impl FnOnce(Unit) -> Unit) -> &Self {
        self.modify_gap_x(|v| *v = f(*v))
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn modify_gap_y(&self, f: impl FnOnce(&mut Unit)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.gap.y));
        self
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn set_gap_y(&self, value: impl Into<Unit>) -> &Self {
        self.modify_gap_y(|v| *v = value.into())
    }

    /// Gap setter. Gap is the space between columns and rows.
    fn update_gap_y(&self, f: impl FnOnce(Unit) -> Unit) -> &Self {
        self.modify_gap_y(|v| *v = f(*v))
    }

    /// Max columns setter. Controls how many items will be displayed in a column.
    fn modify_max_columns(&self, f: impl FnOnce(&mut Option<usize>)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.max_columns_and_rows.x));
        self
    }

    /// Max columns setter. Controls how many items will be displayed in a column.
    fn set_max_columns(&self, count: impl Into<Option<usize>>) -> &Self {
        self.modify_max_columns(|v| *v = count.into())
    }

    /// Max columns setter. Controls how many items will be displayed in a column.
    fn update_max_columns(&self, f: impl FnOnce(Option<usize>) -> Option<usize>) -> &Self {
        self.modify_max_columns(|v| *v = f(*v))
    }

    /// Max rows setter. Controls how many items will be displayed in a row.
    fn modify_max_rows(&self, f: impl FnOnce(&mut Option<usize>)) -> &Self {
        self.display_object().modify_auto_layout(|l| f(&mut l.max_columns_and_rows.y));
        self
    }

    /// Max rows setter. Controls how many items will be displayed in a row.
    fn set_max_rows(&self, count: impl Into<Option<usize>>) -> &Self {
        self.modify_max_rows(|v| *v = count.into())
    }

    /// Max rows setter. Controls how many items will be displayed in a row.
    fn update_max_rows(&self, f: impl FnOnce(Option<usize>) -> Option<usize>) -> &Self {
        self.modify_max_rows(|v| *v = f(*v))
    }

    /// Control whether columns should be displayed in a reversed order.
    fn set_columns_reversed(&self, rev: bool) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.set_x(rev));
        self
    }

    /// Control whether rows should be displayed in a reversed order.
    fn set_rows_reversed(&self, rev: bool) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.set_y(rev));
        self
    }

    /// Reverse the order of columns.
    fn reverse_columns(&self) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.update_x(|t| !t));
        self
    }

    /// Reverse the order of rows.
    fn reverse_rows(&self) -> &Self {
        self.display_object().modify_auto_layout(|l| l.reversed_columns_and_rows.update_y(|t| !t));
        self
    }

    fn first_column(&self) -> ColumnRef {
        self.display_object().def.first_column_or_row()
    }

    fn first_row(&self) -> RowRef {
        self.display_object().def.first_column_or_row()
    }

    fn column(&self, index: usize) -> Option<ColumnRef> {
        self.display_object().def.column_or_row(X, index)
    }

    fn row(&self, index: usize) -> Option<RowRef> {
        self.display_object().def.column_or_row(Y, index)
    }

    fn add_column(&self) -> ColumnRef {
        self.display_object().def.add_column_or_row(X)
    }

    fn add_row(&self) -> RowRef {
        self.display_object().def.add_column_or_row(Y)
    }

    crate::with_alignment_dim2_named_matrix!(gen_layout_object_builder_alignment);
}



// =========================
// === Layout Resolution ===
// =========================

impl Model {
    fn refresh_layout(&self) {
        self.refresh_self_size(true, 0.0);
        self.refresh_layout_internal(true);
        self.refresh_layout_internal(false);
    }

    fn refresh_self_size(&self, first_pass: bool, parent_size: f32) {
        if first_pass {
            let size = self.layout.size.get();
            match size.x {
                Size::Fixed(v) => {
                    println!("[X] Setting size.{:?} of {} to {}", "x", self.name, v);
                    self.layout
                        .computed_size
                        .set_x(v.resolve_const_and_percent(parent_size).unwrap_or(0.0));
                }
                _ => {
                    println!("[X2] Setting size.{:?} of {} to 0", "x", self.name);
                    self.layout.computed_size.set_x(0.0);
                }
            }
            match size.y {
                Size::Fixed(v) => {
                    println!("[X] Setting size.{:?} of {} to {}", "y", self.name, v);
                    self.layout
                        .computed_size
                        .set_y(v.resolve_const_and_percent(parent_size).unwrap_or(0.0));
                }
                _ => {
                    println!("[X2] Setting size.{:?} of {} to 0", "y", self.name);
                    self.layout.computed_size.set_y(0.0);
                }
            }
        }
    }

    fn refresh_layout_internal(&self, first_pass: bool) {
        println!("[{}] refresh_layout_internal. first pass? {}", self.name, first_pass);
        if !self.dirty.transformation.check() && !self.dirty.modified_children.check_all() {
            return;
        }
        if first_pass {
            if self.layout.auto_layout.borrow().is_some() {
                println!("Resetting bbox_origin");
                self.layout.bbox_origin.set(default());
            }
        }
        match &*self.layout.auto_layout.borrow() {
            None =>
                if first_pass {
                    self.refresh_layout_manual(X, first_pass);
                } else {
                    self.refresh_layout_manual(Y, first_pass);
                },
            Some(layout) =>
                if first_pass {
                    self.refresh_linear_layout(X, &layout, first_pass);
                } else {
                    self.refresh_linear_layout(Y, &layout, first_pass);
                },
        }
    }

    fn refresh_layout_manual<Dim: Copy>(&self, x: Dim, first_pass: bool)
    where
        Vector2<Size>: DimSetter<Dim>,
        Vector2<f32>: DimSetter<Dim>,
        Vector3<f32>: DimSetter<Dim>,
        Dim: Debug, {
        println!("[{}] refresh_layout_manual. first pass? {}", self.name, first_pass);

        let children = self.children();
        if children.is_empty() {
            if self.layout.size.get_dim(x).is_hug() {
                println!("[M] Setting size.{:?} of {} to {}", x, self.name, 0.0);
                self.layout.computed_size.set_dim(x, 0.0);
                println!("[M] bbox_origin.{:?} = 0.0", x);
                self.layout.bbox_origin.set_dim(x, 0.0);
            }
        } else {
            let mut min_x: f32 = f32::MAX;
            let mut max_x: f32 = f32::MIN;
            let mut children_to_grow = vec![];
            for child in &children {
                if child.layout.grow_factor.get_dim(x) > 0.0 {
                    children_to_grow.push(child);
                } else {
                    child.refresh_self_size(first_pass, 0.0); // FXME: 0.0
                    child.refresh_layout_internal(first_pass);
                    let child_pos = child.position().get_dim(x);
                    let child_size = child.computed_size().get_dim(x);
                    let child_bbox_origin = child.bbox_origin().get_dim(x);
                    let child_min_x = child_pos + child_bbox_origin;
                    let child_max_x = child_min_x + child_size;
                    min_x = min_x.min(child_min_x);
                    max_x = max_x.max(child_max_x);
                }
            }
            let new_size = max_x - min_x;
            let new_bbox_origin = min_x;
            if self.layout.size.get_dim(x).is_hug() {
                println!("[M] Setting size.{:?} of {} to {}", x, self.name, new_size);
                self.layout.computed_size.set_dim(x, new_size);
                println!("[M] bbox_origin.{:?} = {}", x, new_bbox_origin);
                self.layout.bbox_origin.set_dim(x, new_bbox_origin);
            } else {
                println!("[M] bbox_origin.{:?} = 0.0", x);
                self.layout.bbox_origin.set_dim(x, 0.0);
            }

            for child in children_to_grow {
                println!(
                    "[M] Setting size.{:?} of {} to {}",
                    x,
                    child.name,
                    self.layout.computed_size.get_dim(x)
                );
                child.layout.computed_size.set_dim(x, self.layout.computed_size.get_dim(x));
                child.refresh_layout_internal(first_pass);
            }
        }
    }
}


#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct ResolvedAxis {
    size:              Size,
    min_size:          f32,
    max_size:          f32,
    grow:              f32,
    shrink:            f32,
    computed_size:     f32,
    max_child_fr_size: Fraction,
}

#[derive(Debug, Deref, DerefMut)]
pub struct ResolvedColumn {
    #[deref]
    #[deref_mut]
    axis:     ResolvedAxis,
    children: Vec<Instance>,
}

impl Model {
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
    /// │   │ │     │  ▲   │ ╭────╮   ▼   ▲   Size Legend:             
    /// │   │ │     │  │   │ │ R2 ▲   │   │   ┄── ◀ ▶ ──┄ : Fill size.
    /// │   │ │     │  │   │ │    ▼   │   │   ┄── ▶ ◀ ──┄ : Hug size.  
    /// │   │ ╰─────╯  │   │ ╰────╯   │   │   ┄─────────┄ : Fixed size.
    /// │   ╰──────────╯   ╰──────────╯   │
    /// ╰─────────────────────────────────╯
    /// ```
    ///
    /// 1. In the first pass, we are updating the horizontal layout.
    ///    a) First, we are visiting the `L` object. It's X-axis size is set to `Fill`, so we
    ///       can't determine its width yet. Neither we can update the X-axis layout of its child,
    ///       as it may depend on the `L` object width.
    ///    b) Then, we are visiting the `R` object. It's X-axis size is set to `Hug`, so we need
    ///       to visit its children to find the widest one. It's layout is set to vertical. Unlike
    ///       in the case of the `L` and `R` objects, we are computing the size in the orthogonal
    ///       direction than the layout the children are placed in.
    ///    c) As the `ROOT` object's width size is set to `Fixed`, after finding the `R` object
    ///       width, we can compute the `L` object width.
    ///    d) Finally, we can update the `L` object children layout.
    ///
    /// 2. In the second pass, we are updating the vertical layout.
    ///    a) First, we are visiting the `L` object. It's Y-axis size is set to `Hug`, so we
    ///       need to visit its children to find the tallest one.
    ///    b) The `L1` object's Y-axis size is set to `Fixed`, so we can simply update its
    ///       height.
    ///    c) Next, we are visiting the `R` object. It's Y-axis size is set to `Fill`, so we can
    ///       compute it, as the `ROOT` object height is fixed.
    ///    d) Finally, we can update the `R` object children layout. Both children Y-axis size
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

    // !!!!!!!!!!!!!!!!!!!!!!!!!!!
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!
    // !!!!!!!!!!!!!!!!!!!!!!!!!!!
    // FIXME: origin points
    // FIXME: testing integration
    // FIXME: demo scene
    // FIXME: docs
    #[inline(always)]
    fn refresh_linear_layout<Dim: Copy>(&self, x: Dim, opts: &AutoLayout, first_pass: bool)
    where
        Vector2<Size>: DimSetter<Dim>,
        Vector2<alignment::OptDim1>: DimSetter<Dim>,
        Vector2<alignment::Dim1>: DimSetter<Dim>,
        Vector2<SideSpacing>: DimSetter<Dim>,
        Vector2<bool>: DimSetter<Dim>,
        Vector2<f32>: DimSetter<Dim>,
        Vector2<Unit>: DimSetter<Dim>,
        Vector3<f32>: DimSetter<Dim>,
        Dim: Debug,
        AutoLayout: ColumnAndRowDefinitionsGetter<Dim>, {
        println!("[{}] refresh_linear_layout. First pass: {}", self.name, first_pass);
        let children = self.children();
        if children.is_empty() {
            return;
        }
        let size = self.layout.size.get_dim(x);
        // === Recomputing X-axis elements size of the X-axis horizontal layout ===

        let defined_axes = opts.columns_or_rows(x);
        // FIXME: get_dim(X) here:
        let prim_axis_column_count =
            opts.max_columns_and_rows.get_dim(X).unwrap_or_else(|| children.len());
        let unresolved_columns = if first_pass {
            let column_axes = defined_axes.iter().cycle().enumerate().take(prim_axis_column_count);
            println!("prim_axis_column_count: {}", prim_axis_column_count);
            column_axes
                .map(|(i, t)| {
                    (
                        *t,
                        children
                            .iter()
                            .skip(i)
                            .step_by(prim_axis_column_count)
                            .cloned()
                            .collect_vec(),
                    )
                })
                .collect_vec()
        } else {
            let column_count = children.len().div_ceil(prim_axis_column_count);
            let column_axes = defined_axes.iter().cycle().enumerate().take(column_count);
            column_axes
                .map(|(i, t)| {
                    (
                        *t,
                        children
                            .iter()
                            .skip(i * prim_axis_column_count)
                            .take(prim_axis_column_count)
                            .cloned()
                            .collect_vec(),
                    )
                })
                .collect_vec()
        };
        // println!("unresolved_columns: {:#?}", unresolved_columns);
        let resolved_columns = unresolved_columns
            .into_iter()
            .map(|(axis, children)| {
                let mut grow = 0.0;
                let mut shrink = 0.0;
                let mut min_size = 0.0;
                let mut max_size = f32::INFINITY;
                let mut max_child_size = 0.0;
                let mut max_child_fr_size = Fraction::default();
                for child in &children {
                    let self_size = match self.layout.size.get_dim(x) {
                        Size::Fixed(unit) => unit.resolve_const_only(),
                        _ => 0.0,
                    };
                    child.refresh_self_size(first_pass, self_size);
                    match child.layout.size.get_dim(x) {
                        Size::Hug => child.refresh_layout_internal(first_pass),
                        Size::Fixed(unit) =>
                            if let Some(fr) = unit.as_fraction() {
                                if fr > max_child_fr_size {
                                    max_child_fr_size = fr;
                                }
                            },
                    }
                    let child_margin = child.layout.margin.get_dim(x).resolve_fixed();
                    let child_size = child.layout.computed_size.get_dim(x) + child_margin.total();
                    grow += child.layout.grow_factor.get_dim(x);
                    shrink += child.layout.shrink_factor.get_dim(x);
                    min_size = f32::max(
                        min_size,
                        child.layout.min_size.get_dim(x).resolve_const_only2().unwrap_or(0.0),
                    );
                    max_size = f32::min(
                        max_size,
                        child
                            .layout
                            .max_size
                            .get_dim(x)
                            .resolve_const_only2()
                            .unwrap_or(f32::INFINITY),
                    );
                    max_child_size = f32::max(max_child_size, child_size);
                }
                let child_count = children.len() as f32;
                let grow = axis.grow.unwrap_or_else(|| grow / child_count);
                let shrink = axis.shrink.unwrap_or_else(|| shrink / child_count);
                let min_size = axis.min_size.unwrap_or(min_size);
                let max_size = axis.max_size.unwrap_or(max_size);
                let max_child_fr_size = axis.size.as_fraction().unwrap_or(max_child_fr_size);

                let computed_size = match axis.size {
                    Size::Fixed(unit) => unit.resolve_const_only(),
                    Size::Hug => max_child_size,
                };
                let size = axis.size;
                let axis = ResolvedAxis {
                    size,
                    grow,
                    shrink,
                    computed_size,
                    min_size,
                    max_size,
                    max_child_fr_size,
                };
                ResolvedColumn { axis, children }
            })
            .collect_vec();

        let mut resolved_columns = if opts.reversed_columns_and_rows.get_dim(x) {
            resolved_columns.reversed()
        } else {
            resolved_columns
        };

        println!("resolved_columns: {:#?}", resolved_columns);


        let gap_def = opts.gap.get_dim(x);
        let padding_def = self.layout.padding.get_dim(x);
        let gap_count = (resolved_columns.len() - 1) as f32;
        let fixed_padding = padding_def.resolve_fixed().total();
        let fixed_gap = gap_count * gap_def.resolve_const_only();
        let mut space_left_with_pref_sizes =
            self.layout.computed_size.get_dim(x) - fixed_padding - fixed_gap;
        let mut total_grow_coeff = 0.0;
        let mut total_shrink_coeff = 0.0;
        let mut total_fr = Fraction::default();
        for column in &resolved_columns {
            space_left_with_pref_sizes -= column.computed_size;
            total_grow_coeff += column.grow;
            total_shrink_coeff += column.shrink;
            total_fr += column.max_child_fr_size;
        }
        if let Some(fr) = padding_def.start.as_fraction() {
            total_fr += fr;
        }
        if let Some(fr) = padding_def.end.as_fraction() {
            total_fr += fr;
        }
        if let Some(fr) = gap_def.as_fraction() {
            total_fr += fr * gap_count;
        }

        if self.layout.size.get_dim(x).is_hug() && space_left_with_pref_sizes < 0.0 {
            self.layout.computed_size.update_dim(x, |t| t - space_left_with_pref_sizes);
            space_left_with_pref_sizes = 0.0;
        }

        println!("space_left_with_pref_sizes: {}", space_left_with_pref_sizes);
        let self_size = self.layout.computed_size.get_dim(x);
        let padding = padding_def.resolve(self_size, space_left_with_pref_sizes, total_fr);
        println!("padding: {:#?}", padding);
        let gap = gap_def.resolve(self_size, space_left_with_pref_sizes, total_fr);
        let total_gap = gap_count * gap;
        let mut space_left = self_size - padding.total() - total_gap;

        let grow_coeff = if total_grow_coeff > 0.0 {
            f32::max(0.0, space_left_with_pref_sizes / total_grow_coeff)
        } else {
            0.0
        };
        let shrink_coeff = if total_shrink_coeff > 0.0 {
            f32::min(0.0, space_left_with_pref_sizes / total_shrink_coeff)
        } else {
            0.0
        };

        let mut pos_x = padding.start;
        for column in &mut resolved_columns {
            let grow_size = column.grow * grow_coeff;
            let shrink_size = column.shrink * shrink_coeff;
            let column_size = column.computed_size + grow_size + shrink_size;
            let column_size = f32::max(column.min_size, column_size);
            let column_size = f32::min(column.max_size, column_size);
            column.computed_size = column_size;
            space_left -= column_size;
        }

        println!("space_left: {}", space_left);
        println!("total_fr: {}", total_fr);
        for column in &resolved_columns {
            let fr_diff = if total_fr > Fraction::from(0.0) {
                space_left * column.max_child_fr_size.unchecked_raw() / total_fr.unchecked_raw()
            } else {
                0.0
            };
            println!("fr_diff: {}", fr_diff);
            let column_size = column.computed_size + fr_diff;
            let column_size = f32::max(column.min_size, column_size);
            let column_size = f32::min(column.max_size, column_size);
            println!("column: {} {:#?}", column_size, column);
            for child in &column.children {
                println!(">> child: {:?}", child);
                let child_size = child.layout.computed_size.get_dim(x);
                let child_unused_space = f32::max(0.0, column_size - child_size);
                let unresolved_margin = child.layout.margin.get_dim(x);
                let mut total_margin_fr = Fraction::from(0.0);
                if let Some(fr) = unresolved_margin.start.as_fraction() {
                    total_margin_fr += fr;
                }
                if let Some(fr) = unresolved_margin.end.as_fraction() {
                    total_margin_fr += fr;
                }
                let margin =
                    unresolved_margin.resolve(self_size, child_unused_space, total_margin_fr);
                let column_size_minus_margin = column_size - margin.start - margin.end;

                let child_can_grow = child.layout.grow_factor.get_dim(x) > 0.0;
                let child_can_shrink = child.layout.shrink_factor.get_dim(x) > 0.0;
                if child_can_grow && child_size < column_size_minus_margin {
                    // FIXME: resolve_const_only here
                    let size = f32::min(
                        column_size_minus_margin,
                        child.layout.max_size.get_dim(x).resolve_const_only(),
                    );
                    child.layout.computed_size.set_dim(x, size);
                }
                if let Some(fr) = child.layout.size.get_dim(x).as_fraction() {
                    if fr > Fraction::from(0.0) {
                        // FIXME: resolve_const_only here
                        let size = f32::min(
                            column_size_minus_margin,
                            child.layout.max_size.get_dim(x).resolve_const_only(),
                        );
                        child.layout.computed_size.set_dim(x, size);
                    }
                }
                if child_can_shrink && child_size > column_size_minus_margin {
                    // FIXME: resolve_const_only here
                    let size = f32::max(
                        column_size_minus_margin,
                        child.layout.min_size.get_dim(x).resolve_const_only(),
                    );
                    child.layout.computed_size.set_dim(x, size);
                }

                if child_size != child.layout.computed_size.get_dim(x) {
                    // Child size changed. There is one case when this might be a second call to
                    // refresh layout of the same child. If the child size is set to hug, the
                    // child can grow, and the column size is greater than earlier computed hugged
                    // child size, we need to refresh the child layout again.
                    child.refresh_layout_internal(first_pass);
                }
                let child_unused_space =
                    f32::max(0.0, column_size_minus_margin - child.layout.computed_size.get_dim(x));
                let def_alignment = opts.children_alignment.get_dim(x);
                let alignment = child.layout.alignment.get().get_dim(x).unwrap_or(def_alignment);
                let child_offset = child_unused_space * alignment.normalized();
                let bbox_origin = child.bbox_origin();
                println!("!!! bbox_origin: {:?}", bbox_origin);
                child.set_position_dim(
                    x,
                    pos_x - bbox_origin.get_dim(x) + child_offset + margin.start,
                );
                println!("<< child: {:?}", child);
            }
            pos_x += column_size + gap;
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



// =================================================================================================
// === Display Object Traits =======================================================================
// =================================================================================================

// =============================
// === Transformation Macros ===
// =============================

/// Generates getters and setters for display object transformations, such as `x()`, `xy()`,
/// `set_x()`, `rotation_z()`, `set_scale_x()`, etc.
macro_rules! gen_object_trans {
    ($trans:ident $(,$tx_name:ident)?) => {
        paste! {
            fn $trans(&self) -> Vector3<f32> {
                self.display_object().def.$trans()
            }

            fn [<set_ $trans>](&self, t: impl IntoVector3<f32>) {
                self.display_object().def.[<set_ $trans>](t.into_vector());
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
            fn $set_name(&self, value: impl [<IntoVector $dim>]<f32>) {
                self.[<modify_ $tx>](|p| p.[<set_ $name>](value.into_vector()));
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



// =================================
// === Children Alignment Macros ===
// =================================

macro_rules! gen_children_alignment_for_object {
    ([$([$name:ident $x:ident $y:ident])*]) => {
        paste! {
            $(
                /// Children alignment setter.
                fn [<align_children_ $name>](&self) -> &Self {
                    self.display_object().def.[<align_children_ $name>]();
                    self
                }
            )*
        }
    }
}



// =================
// === ObjectOps ===
// =================

impl<T: Object + ?Sized> ObjectOps for T {}

/// Implementation of operations available for every struct which implements `display::Object`.
/// To learn more about the design, please refer to the documentation of [`Instance`].
#[allow(missing_docs)]
pub trait ObjectOps: Object + AutoLayoutOps + LayoutOps {
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

    fn new_child(&self) -> Instance {
        let child = Instance::new();
        self.add_child(&child);
        child
    }

    fn new_child_named(&self, name: &'static str) -> Instance {
        let child = Instance::new_named(name);
        self.add_child(&child);
        child
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
        self.display_object().def.focused_instance()
    }


    // === Auto Layout settings ===

    /// Layout children using an auto-layout algorithm.
    fn use_auto_layout(&self) -> &Self {
        let instance = self.display_object();
        instance.def.set_layout(AutoLayout::default());
        self
    }

    /// Do not layout children automatically.
    fn set_layout_manual(&self) {
        self.display_object().def.set_layout(None);
    }


    // === Layout properties ===


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
}

impl<T: Object + ?Sized> GenericLayoutApi for T {}



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
                    let root = Instance::new_named("root");
                    $(let [<node $num>] = Instance::new_named(stringify!([<node $num>]));)*
                    world.add_child(&root);
                    $(root.add_child(&[<node $num>]);)*
                    Self { world, root, $([<node $num>]),* }
                }

                fn reset_positions(&self) {
                    self.root.set_position(Vector3(0.0, 0.0, 0.0));
                    $(self.[<node $num>].set_position(Vector3(0.0, 0.0, 0.0));)*
                }

                fn run(&self) -> &Self {
                    self.world.display_object().update(&self.world.default_scene);
                    self
                }

                fn assert_root_bbox_origin(&self, x:f32, y:f32) -> &Self {
                    assert_eq!(self.root.bbox_origin(), Vector2(x,y));
                    self
                }

                fn assert_root_position(&self, x:f32, y:f32) -> &Self {
                    assert_eq!(self.root.position().xy(), Vector2(x,y));
                    self
                }

                fn assert_root_computed_size(&self, x:f32, y:f32) -> &Self {
                    assert_eq!(self.root.computed_size(), Vector2(x,y));
                    self
                }

                $(
                    fn [<assert_node $num _position>](&self, x:f32, y:f32) -> &Self {
                        assert_eq!(self.[<node $num>].position().xy(), Vector2(x,y));
                        self
                    }

                    fn [<assert_node $num _computed_size>](&self, x:f32, y:f32) -> &Self {
                        assert_eq!(self.[<node $num>].computed_size(), Vector2(x,y));
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
    /// ╭──────────────────────────────╮
    /// │ root                      △  │
    /// │  ╭─────────┬▷  ╭── ▶ ◀ ───┤  │
    /// │  │ l       │   │ r    △   │  │
    /// │  │ ╭────┬▷ │   │ ╭────┤   │  │
    /// │  │ │ l1 │  │   │ │ R2 │   │  │
    /// │  │ │    │  ▼   │ ╰────╯   │  │
    /// │  │ │    │  ▲   │      △   │  │
    /// │  │ │    │  │   │ ╭────┤   │  │
    /// │  │ │    │  │   │ │ R1 │   │  │
    /// │  │ ╰────╯  │   │ ╰────╯   │  │
    /// │  ╰─────────╯   ╰──────────╯  │
    /// ╰──────────────────────────────╯
    /// ```
    ///
    /// Output:
    /// The dimensions in parentheses were provided manually.
    ///
    /// ```text
    /// ╭───────────────────────────────────────────╮
    /// │ root                                △     │
    /// │                    ╭── ▶ ◀ ─────────┤     │
    /// │                    │ r         △    │     │
    /// │  ╭────────────┬▷   │ ╭─────────┤    │     │
    /// │  │ l          │    │ │ R2      │ 5  │     │
    /// │  │ ╭────┬▷    │    │ ╰─────────╯    │     │
    /// │  │ │ l1 │     ▼    │     (3)        │ 10  │ (10)
    /// │  │ │    │ (4) ▲ 4  │       △        │     │
    /// │  │ ╰────╯     │    │ ╭─────┤        │     │
    /// │  │   7        │    │ │ R1  │ 5      │     │
    /// │  ╰────────────╯    │ ╰─────╯        │     │
    /// │        7           │   (2)          │     │
    /// │                    ╰────────────────╯     │
    /// │                             3             │
    /// ╰───────────────────────────────────────────╯
    ///                     (10)
    /// ```
    #[test]
    fn test_mixed_layouts() {
        let world = World::new();
        let root = Instance::new_named("Root");
        let l = root.new_child_named("L");
        let r = root.new_child_named("R");
        let l1 = l.new_child_named("L1");
        let r1 = r.new_child_named("R1");
        let r2 = r.new_child_named("R2");

        root.use_auto_layout();
        root.set_size((10.0, 10.0));

        l.align_center();
        l.use_auto_layout();
        l.set_size_y_to_hug().allow_grow_x();
        l1.set_size((0.0, 4.0)).allow_grow_x();

        r.use_auto_layout().set_max_columns(1);
        r.set_size_x_to_hug().allow_grow_y();
        r1.set_size((2.0, 0.0)).allow_grow_y();
        r2.set_size((3.0, 0.0)).allow_grow_y();

        root.update(&world.default_scene);

        println!("L size: {:?}", l.computed_size());
        assert_eq!(root.position().xy(), Vector2(0.0, 0.0));
        assert_eq!(l.position().xy(), Vector2(0.0, 3.0));
        assert_eq!(r.position().xy(), Vector2(7.0, 0.0));
        assert_eq!(l1.position().xy(), Vector2(0.0, 0.0));
        assert_eq!(r1.position().xy(), Vector2(0.0, 0.0));
        assert_eq!(r2.position().xy(), Vector2(0.0, 5.0));

        assert_eq!(root.computed_size(), Vector2(10.0, 10.0));
        assert_eq!(l.computed_size(), Vector2(7.0, 4.0));
        assert_eq!(r.computed_size(), Vector2(3.0, 10.0));
        assert_eq!(r1.computed_size(), Vector2(2.0, 5.0));
        assert_eq!(r2.computed_size(), Vector2(3.0, 5.0));
    }

    /// ```text
    /// ╭─────────────── ▶ ◀ ───────────────╮
    /// │ root                              │
    /// │                        ╭───────╮  │
    /// │             ╭───────╮  │ node3 │  │
    /// │  ╭───────╮  │ node2 │  │       │  ▼
    /// │  │ node1 │  │       │  │       │  ▲
    /// │  │       │  │       │  │       │  │
    /// │  ╰───────╯  ╰───────╯  ╰───────╯  │
    /// ╰───────────────────────────────────╯
    /// ```
    #[test]
    fn test_horizontal_layout_with_fixed_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(1.0, 0.0)
            .assert_node3_position(3.0, 0.0)
            .assert_root_computed_size(6.0, 3.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(3.0, 3.0);
    }

    /// ```text
    /// ╭──── ▶ ◀ ────────╮
    /// │ root            │
    /// │  ╭───────────╮  │
    /// │  │ node3     │  │
    /// │  │           │  │
    /// │  ╰───────────╯  │
    /// │  ╭─────────╮    │
    /// │  │ node2   │    ▼
    /// │  │         │    ▲
    /// │  ╰─────────╯    │
    /// │  ╭───────╮      │
    /// │  │ node1 │      │
    /// │  │       │      │
    /// │  ╰───────╯      │
    /// ╰─────────────────╯
    /// ```
    #[test]
    fn test_vertical_layout_with_fixed_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_max_columns(1);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(0.0, 1.0)
            .assert_node3_position(0.0, 3.0)
            .assert_root_computed_size(3.0, 6.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(3.0, 3.0);
    }

    /// ```text
    /// ╭─────────────── ▶ ◀ ───────────────╮
    /// │ root                              │
    /// │  ╭─ ▶ ◀ ─╮  ╭───────╮  ╭─ ▶ ◀ ─╮  │
    /// │  │ node1 │  │ node2 │  │ node3 ▼  ▼
    /// │  │       │  │       │  │       ▲  ▲
    /// │  ╰───────╯  ╰───────╯  ╰───────╯  │
    /// ╰───────────────────────────────────╯
    /// ```
    #[test]
    fn test_horizontal_layout_with_hug_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size_hug_y(1.0);
        test.node2.set_size((2.0, 2.0));
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(0.0, 0.0)
            .assert_node3_position(2.0, 0.0)
            .assert_root_computed_size(2.0, 2.0)
            .assert_node1_computed_size(0.0, 1.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(0.0, 0.0);
    }

    /// ```text
    /// ╭──────────────────────────────────────────╮
    /// │ root                                     │
    /// │  ╭─ ▶ ◀ ─╮  ╭───────┬───────▷┤ ╭─ ▶ ◀ ─╮ │
    /// │  │ node1 │  │ node2 │          │ node3 │ ▼
    /// │  │       │  │       │          │       │ ▲
    /// │  ╰───────╯  ╰───────╯          ╰───────╯ │
    /// ╰──────────────────────────────────────────╯
    /// ```
    #[test]
    fn test_horizontal_layout_with_children_that_grow() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_size_x_hug(10.0);
        test.node1.set_size_hug_y(1.0);
        test.node2.set_size((2.0, 2.0)).allow_grow_x();
        test.node3.set_size((3.0, 3.0));
        test.run()
            .assert_root_computed_size(10.0, 3.0)
            .assert_node1_computed_size(0.0, 1.0)
            .assert_node2_computed_size(7.0, 2.0)
            .assert_node3_computed_size(3.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(0.0, 0.0)
            .assert_node3_position(7.0, 0.0);
    }

    /// ```text
    /// ╭──────────────────────────────────────────╮
    /// │ root                                     │
    /// │  ╭─ ▶ ◀ ─╮  ╭───────┬─▷┤ ╭─ ▶ ◀ ─╮       │
    /// │  │ node1 │  │ node2 │    │ node3 │       ▼
    /// │  │       │  │       │    │       │       ▲
    /// │  ╰───────╯  ╰───────╯    ╰───────╯       │
    /// ╰──────────────────────────────────────────╯
    /// ```
    #[test]
    fn test_horizontal_layout_with_children_that_grow_to_a_limit() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_size_x_hug(10.0);
        test.node1.set_size_hug_y(1.0);
        test.node2.set_size((2.0, 2.0)).allow_grow_x().set_max_size_x(4.0);
        test.node3.set_size((3.0, 3.0));
        test.run()
            .assert_root_computed_size(10.0, 3.0)
            .assert_node1_computed_size(0.0, 1.0)
            .assert_node2_computed_size(4.0, 2.0)
            .assert_node3_computed_size(3.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(0.0, 0.0)
            .assert_node3_position(4.0, 0.0);
    }

    /// ```text
    /// ╭────────────────────────────╮
    /// │ root                       │
    /// │  ╭───────╮  ╭───┼◁──╮  ╭───┼───╮
    /// │  │ node1 │  │ node2 │  │ node3 │
    /// │  │       │  │       │  │   │   │
    /// │  ╰───────╯  ╰───────╯  ╰───┼───╯
    /// ╰────────────────────────────╯
    /// ```
    #[test]
    fn test_horizontal_layout_with_children_that_shrink_to_a_limit() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_size_x_hug(4.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0)).allow_shrink_x().set_min_size_x(1.0);
        test.node3.set_size((3.0, 3.0));
        test.run()
            .assert_root_computed_size(4.0, 3.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(1.0, 2.0)
            .assert_node3_computed_size(3.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(1.0, 0.0)
            .assert_node3_position(2.0, 0.0);
    }

    /// ```text
    /// ╭──────────────────────────────────────╮
    /// │ root                    △            │
    /// │             ╭── ▶ ◀ ────┼▷ ╭───────╮ │
    /// │             │ node2     │  │ node3 │ │     
    /// │             │╭─────────╮│  │       │ │       
    /// │  ╭───────╮  ││ node2_1 ││  │       │ │
    /// │  │ node1 │  │╰─────────╯│  │       │ │
    /// │  ╰───────╯  ╰───────────╯  ╰───────╯ │
    /// ╰──────────────────────────────────────╯
    /// ```
    #[test]
    fn test_hug_child_that_can_grow_in_a_hug_column() {
        let test = TestFlatChildren3::new();
        let node2_1 = Instance::new_named("node2_1");
        test.node2.add_child(&node2_1);

        test.root.use_auto_layout();
        test.root.set_size_x_hug(10.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.use_auto_layout();
        test.node2.allow_grow_x().allow_grow_y();
        node2_1.set_size((1.0, 1.0));
        test.node3.set_size((3.0, 3.0));
        test.run()
            .assert_root_computed_size(10.0, 3.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(6.0, 3.0)
            .assert_node3_computed_size(3.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(1.0, 0.0)
            .assert_node3_position(7.0, 0.0);
        assert_eq!(node2_1.computed_size(), Vector2(1.0, 1.0));
        assert_eq!(node2_1.position().xy(), Vector2(0.0, 0.0));
    }

    /// ```text
    /// ╭────────────┬───────  ▶ ◀ ────────┬───────────╮
    /// │ root       ┆ ╱╱╱╱╱╱           ╱╱ ┆           │
    /// │            ┆ ╱╱╱╱╱╱           ╱╱ ┆ ╭───────╮ │
    /// │            ┆ ╱╱╱╱╱╱ ╭───────╮ ╱╱ ┆ │ node3 │ │
    /// │  ╭───────╮ ┆ ╱╱╱╱╱╱ │ node2 │ ╱╱ ┆ │       │ ▼
    /// │  │ node1 │ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ ▲
    /// │  │       │ ┆ ╱╱╱╱╱╱ │       │ ╱╱ ┆ │       │ │
    /// │  ╰───────╯ ┆ ╱╱╱╱╱╱ ╰───────╯ ╱╱ ┆ ╰───────╯ │
    /// ╰────────────┴─────────────────────┴───────────╯
    /// ```
    #[test]
    fn test_horizontal_layout_with_fixed_children_and_margin() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.node2.set_margin_left(10.0);
        test.node2.set_margin_right(1.0);
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(11.0, 0.0)
            .assert_node3_position(14.0, 0.0)
            .assert_root_computed_size(17.0, 3.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(3.0, 3.0);
    }

    /// ```text
    /// ╭───────────────┬─── ▶ ◀ ───┬───────────────╮
    /// │ root ╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱ │
    /// │ ╱╱╱           ┆           ┆ ╭───────╮ ╱╱╱ │
    /// │ ╱╱╱           ┆ ╭───────╮ ┆ │ node3 │ ╱╱╱ │
    /// │ ╱╱╱ ╭───────╮ ┆ │ node2 │ ┆ │       │ ╱╱╱ ▼
    /// │ ╱╱╱ │ node1 │ ┆ │       │ ┆ │       │ ╱╱╱ ▲
    /// │ ╱╱╱ │       │ ┆ │       │ ┆ │       │ ╱╱╱ │
    /// │ ╱╱╱ ╰───────╯ ┆ ╰───────╯ ┆ ╰───────╯ ╱╱╱ │
    /// │ ╱╱╱╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱┆╱╱╱╱╱╱╱╱╱╱╱╱╱╱ │
    /// ╰───────────────┴───────────┴───────────────╯
    /// ```
    #[test]
    fn test_horizontal_layout_with_fixed_children_and_padding() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.set_padding_all(10.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((3.0, 3.0));
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(10.0, 10.0)
            .assert_node2_position(11.0, 10.0)
            .assert_node3_position(13.0, 10.0)
            .assert_root_computed_size(26.0, 23.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(3.0, 3.0);
    }

    /// ```text
    /// ╭────────────────────────╮
    /// │ root                   │
    /// │  ╭───────╮             │
    /// │  │ node3 │             │
    /// │  │       │             │
    /// │  ╰───────╯             │
    /// │  ╭───────╮  ╭───────╮  │
    /// │  │ node1 │  │ node2 │  │
    /// │  │       │  │       │  │
    /// │  ╰───────╯  ╰───────╯  │
    /// ╰────────────────────────╯
    /// ```
    #[test]
    fn test_simple_grid_layout() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_max_columns(2);
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run()
            .assert_root_computed_size(4.0, 4.0)
            .assert_node1_computed_size(2.0, 2.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 2.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(2.0, 0.0)
            .assert_node3_position(0.0, 2.0);
    }

    /// ```text
    /// ╭────────────────────────╮
    /// │ root                   │
    /// │             ╭───────╮  │
    /// │             │ node3 │  │
    /// │             │       │  │
    /// │             ╰───────╯  │
    /// │  ╭───────╮  ╭───────╮  │
    /// │  │ node2 │  │ node1 │  │
    /// │  │       │  │       │  │
    /// │  ╰───────╯  ╰───────╯  │
    /// ╰────────────────────────╯
    /// ```
    #[test]
    fn test_simple_grid_layout_reversed_x() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_max_columns(2).reverse_columns();
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run()
            .assert_root_computed_size(4.0, 4.0)
            .assert_node1_computed_size(2.0, 2.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 2.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(2.0, 0.0)
            .assert_node2_position(0.0, 0.0)
            .assert_node3_position(2.0, 2.0);
    }

    /// ```text
    /// ╭────────────────────────╮
    /// │ root                   │
    /// │  ╭───────╮  ╭───────╮  │
    /// │  │ node1 │  │ node2 │  │
    /// │  │       │  │       │  │
    /// │  ╰───────╯  ╰───────╯  │
    /// │  ╭───────╮             │
    /// │  │ node3 │             │
    /// │  │       │             │
    /// │  ╰───────╯             │
    /// ╰────────────────────────╯
    /// ```
    #[test]
    fn test_simple_grid_layout_reversed_y() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_max_columns(2).reverse_rows();
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run()
            .assert_root_computed_size(4.0, 4.0)
            .assert_node1_computed_size(2.0, 2.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 2.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 2.0)
            .assert_node2_position(2.0, 2.0)
            .assert_node3_position(0.0, 0.0);
    }

    /// ```text
    /// ╭─────────────────────────────╮
    /// │ root       ╱╱╱╱╱╱           │
    /// │  ╭───────╮ ╱╱╱╱╱╱           │
    /// │  │ node3 │ ╱╱╱╱╱╱           │
    /// │  │       │ ╱╱╱╱╱╱           │
    /// │  ╰───────╯ ╱╱╱╱╱╱           │
    /// │ ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱ │
    /// │  ╭───────╮ ╱╱╱╱╱╱ ╭───────╮ │
    /// │  │ node1 │ ╱╱╱╱╱╱ │ node2 │ │
    /// │  │       │ ╱╱╱╱╱╱ │       │ │
    /// │  ╰───────╯ ╱╱╱╱╱╱ ╰───────╯ │
    /// ╰─────────────────────────────╯
    /// ```
    #[test]
    fn test_simple_grid_layout_with_gap() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_max_columns(2).set_gap((5.0, 3.0));
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.run()
            .assert_root_computed_size(9.0, 7.0)
            .assert_node1_computed_size(2.0, 2.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 2.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(7.0, 0.0)
            .assert_node3_position(0.0, 5.0);
    }

    /// ```text
    /// ╭────────────────────────╮
    /// │ root                   │
    /// │                        │
    /// │             ╭───────╮  │
    /// │  ╭───────╮  │ node2 │  ▼
    /// │  │ node1 │  │       │  ▲
    /// │  │       │  │       │  │
    /// │  ╰───────╯  ╰───────╯  │
    /// │     30%        70%     │
    /// ╰────────────────────────╯
    ///             10
    /// ```
    #[test]
    fn test_horizontal_layout_with_percentage_children() {
        let test = TestFlatChildren2::new();
        test.root.use_auto_layout().set_size_x(10.0);
        test.node1.set_size((30.pc(), 1.0));
        test.node2.set_size((70.pc(), 2.0));
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(3.0, 0.0)
            .assert_root_computed_size(10.0, 2.0)
            .assert_node1_computed_size(3.0, 1.0)
            .assert_node2_computed_size(7.0, 2.0);
    }

    /// ```text
    /// ╭───────────────────────────────────╮
    /// │ root                              │
    /// │                        ╭───────╮  │
    /// │             ╭───────╮  │ node3 │  │
    /// │  ╭───────╮  │ node2 │  │       │  ▼
    /// │  │ node1 │  │       │  │       │  ▲
    /// │  │       │  │       │  │       │  │
    /// │  ╰───────╯  ╰───────╯  ╰───────╯  │
    /// │     1fr        40%        2fr     │
    /// ╰───────────────────────────────────╯
    ///                  10
    /// ```
    #[test]
    fn test_horizontal_layout_with_fraction_and_percentage_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_size_x(10.0);
        test.node1.set_size((1.fr(), 1.0));
        test.node2.set_size((40.pc(), 2.0));
        test.node3.set_size((2.fr(), 3.0));
        test.run()
            .assert_root_computed_size(10.0, 3.0)
            .assert_node1_computed_size(2.0, 1.0)
            .assert_node2_computed_size(4.0, 2.0)
            .assert_node3_computed_size(4.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(2.0, 0.0)
            .assert_node3_position(6.0, 0.0);
    }

    /// ```text
    /// ╭─────────────── ▶ ◀ ───────────────╮
    /// │ root                              │
    /// │                        ╭───────╮  │
    /// │             ╭───────╮  │ node3 │  │
    /// │  ╭───────╮  │ node2 │  │       │  ▼
    /// │  │ node1 │  │       │  │       │  ▲
    /// │  │       │  │       │  │       │  │
    /// │  ╰───────╯  ╰───────╯  ╰───────╯  │
    /// │     1fr        40%        2fr     │
    /// ╰───────────────────────────────────╯
    /// ```
    #[test]
    fn test_horizontal_hug_layout_with_fraction_and_percentage_children() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.node1.set_size((1.fr(), 1.0));
        test.node2.set_size((40.pc(), 2.0));
        test.node3.set_size((2.fr(), 3.0));
        test.run()
            .assert_root_computed_size(0.0, 3.0)
            .assert_node1_computed_size(0.0, 1.0)
            .assert_node2_computed_size(0.0, 2.0)
            .assert_node3_computed_size(0.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(0.0, 0.0)
            .assert_node3_position(0.0, 0.0);
    }

    /// ```text
    /// ╭─────────────┬──── ▶ ◀ ────┬─────────────╮
    /// │ root        ┆             ┆             │
    /// │             ┆             ┆  ╭───────╮  │
    /// │             ┆  ╭───────╮  ┆  │ node3 │  │
    /// │  ╭───────╮  ┆  │ node2 │  ┆  │       │  ▼
    /// │  │ node1 │  ┆  │       │  ┆  │       │  ▲
    /// │  │       │  ┆  │       │  ┆  │       │  │
    /// │  ╰───────╯  ┆  ╰───────╯  ┆  ╰───────╯  │
    /// ╰─────────────┴─────────────┴─────────────╯
    ///       1fr           2fr           3fr
    /// ```
    #[test]
    fn test_fractional_column_layout() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_size_x(12.0);
        test.root.first_column().set_size(1.fr());
        test.root.add_column().set_size(2.fr());
        test.root.add_column().set_size(3.fr());
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((1.0, 2.0));
        test.node3.set_size((1.0, 3.0));
        test.run()
            .assert_root_computed_size(12.0, 3.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(1.0, 2.0)
            .assert_node3_computed_size(1.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(2.0, 0.0)
            .assert_node3_position(6.0, 0.0);
    }

    /// ```text
    /// ╭─────────────┬──── ▶ ◀ ────┬─────────────╮
    /// │ root        ┆             ┆             │
    /// │             ┆             ┆  ╭──────────┼─────╮
    /// │             ┆  ╭──────────┼──┼─╮ node3  │     │
    /// │  ╭───────╮  ┆  │ node2    ┆  │ │        ▼     │
    /// │  │ node1 │  ┆  │          ┆  │ │        ▲     │
    /// │  │       │  ┆  │          ┆  │ │        │     │
    /// │  ╰───────╯  ┆  ╰──────────┼──┴─╯────────┼─────╯
    /// ╰─────────────┴─────────────┴─────────────╯
    ///       2.0           2.0           2.0
    /// ```
    #[test]
    fn test_fixed_column_layout() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout();
        test.root.first_column().set_size(2.0);
        test.root.add_column().set_size(2.0);
        test.root.add_column().set_size(2.0);
        test.node1.set_size((1.0, 1.0));
        test.node2.set_size((3.0, 2.0));
        test.node3.set_size((4.0, 3.0));
        test.run()
            .assert_root_computed_size(6.0, 3.0)
            .assert_node1_computed_size(1.0, 1.0)
            .assert_node2_computed_size(3.0, 2.0)
            .assert_node3_computed_size(4.0, 3.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(0.0, 0.0)
            .assert_node2_position(2.0, 0.0)
            .assert_node3_position(4.0, 0.0);
    }


    /// ```text
    /// ╭─────────────────┬─────────────────┬─────────────────╮
    /// │╱╱ root        ╱╱┆╱╱             ╱╱┆╱╱             ╱╱│
    /// │╱╱             ╱╱┆╱╱             ╱╱┆╱╱  ╭───────╮  ╱╱│
    /// │╱╱             ╱╱┆╱╱  ╭───────╮  ╱╱┆╱╱  │ node3 │  ╱╱│
    /// │╱╱  ╭───────╮  ╱╱┆╱╱  │ node2 │  ╱╱┆╱╱  │       │  ╱╱▼
    /// │╱╱  │ node1 │  ╱╱┆╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱▲
    /// │╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱┆╱╱  │       │  ╱╱│
    /// │╱╱  ╰───────╯  ╱╱┆╱╱  ╰───────╯  ╱╱┆╱╱  ╰───────╯  ╱╱│
    /// │.5fr    2       1fr       2       1fr       2    .5fr│
    /// ╰─────────────────┴─────────────────┴─────────────────╯
    ///                            12
    /// ```
    #[test]
    fn test_fixed_column_layout_with_fraction_gap() {
        let test = TestFlatChildren3::new();
        test.root
            .use_auto_layout()
            .set_size_x(12.0)
            .set_padding_left(0.5.fr())
            .set_padding_right(0.5.fr())
            .set_gap_x(1.fr());
        test.node1.set_size((2.0, 1.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 3.0));
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(1.0, 0.0)
            .assert_node2_position(5.0, 0.0)
            .assert_node3_position(9.0, 0.0)
            .assert_root_computed_size(12.0, 3.0)
            .assert_node1_computed_size(2.0, 1.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 3.0);
    }

    /// ```text
    /// ╭─────────────────────┬─────────────────────┬─────────────────────╮
    /// │ ╱╱╱ root        ╱╱╱ ┆ ╱╱╱             ╱╱╱ ┆ ╱╱╱             ╱╱╱ │
    /// │ ╱╱╱             ╱╱╱ ┆ ╱╱╱             ╱╱╱ ┆ ╱╱╱  ╭───────╮  ╱╱╱ │
    /// │ ╱╱╱             ╱╱╱ ┆ ╱╱╱  ╭───────╮  ╱╱╱ ┆ ╱╱╱  │ node3 │  ╱╱╱ │
    /// │ ╱╱╱  ╭───────╮  ╱╱╱ ┆ ╱╱╱  │ node2 │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ▼
    /// │ ╱╱╱  │ node1 │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ▲
    /// │ ╱╱╱  │       │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ ┆ ╱╱╱  │       │  ╱╱╱ │
    /// │ ╱╱╱  ╰───────╯  ╱╱╱ ┆ ╱╱╱  ╰───────╯  ╱╱╱ ┆ ╱╱╱  ╰───────╯  ╱╱╱ │
    /// │ 1fr      2      2fr ┆ 3fr      2      4fr ┆ 5fr      2      6fr │
    /// ╰─────────────────────┴─────────────────────┴─────────────────────╯
    ///            5                     9                    13
    /// ```
    #[test]
    fn test_fixed_column_layout_with_fraction_margin() {
        let test = TestFlatChildren3::new();
        test.root.use_auto_layout().set_size_x(27.0);
        test.root.first_column().set_size(5.0);
        test.root.add_column().set_size(9.0);
        test.root.add_column().set_size(13.0);
        test.node1.set_size((2.0, 1.0)).set_margin_left(1.fr()).set_margin_right(2.fr());
        test.node2.set_size((2.0, 2.0)).set_margin_left(3.fr()).set_margin_right(4.fr());
        test.node3.set_size((2.0, 3.0)).set_margin_left(5.fr()).set_margin_right(6.fr());
        test.run()
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(1.0, 0.0)
            .assert_node2_position(8.0, 0.0)
            .assert_node3_position(19.0, 0.0)
            .assert_root_computed_size(27.0, 3.0)
            .assert_node1_computed_size(2.0, 1.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 3.0);
    }

    /// ```text
    ///    ╭───────────────────────────╮
    ///    │ root            ╭───────╮ │
    /// ╭──┼────╮            │ node3 │ │
    /// │ node1 │            │       │ │
    /// │  │    │  ╭───────╮ ╰───────╯ │
    /// ╰──┼────╯  │ node2 │           │
    ///    ╰───────┼───────┼───────────╯
    ///            ╰───────╯
    /// ```
    #[test]
    fn test_layout_manual_fixed() {
        let test = TestFlatChildren3::new();
        test.root.set_size((4.0, 2.0));
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.node1.set_xy((-1.0, 0.0));
        test.node2.set_xy((1.0, -1.0));
        test.node3.set_xy((3.0, 1.0));
        test.run()
            .assert_root_computed_size(4.0, 2.0)
            .assert_node1_computed_size(2.0, 2.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 2.0)
            .assert_root_bbox_origin(0.0, 0.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(-1.0, 0.0)
            .assert_node2_position(1.0, -1.0)
            .assert_node3_position(3.0, 1.0);
    }

    /// ```text
    /// ╭────┬───────────────────────────╮
    /// │    ┆ root            ╭───────╮ │
    /// │ ╭──┼────╮            │ node3 │ │
    /// │ │ node1 │            │       │ │
    /// │ │  ┆    │  ╭───────╮ ╰───────╯ │
    /// │ ╰──┼────╯  │ node2 │           │
    /// │    ◎┈┈┈┈┈┈┈┼┈┈┈┈┈┈┈┼┈┈┈┈┈┈┈┈┈┈┈┤
    /// │            ╰───────╯           │
    /// ╰────────────────────────────────╯
    /// ```
    #[test]
    fn test_layout_manual_hug() {
        let test = TestFlatChildren3::new();
        test.node1.set_size((2.0, 2.0));
        test.node2.set_size((2.0, 2.0));
        test.node3.set_size((2.0, 2.0));
        test.node1.set_xy((-1.0, 0.0));
        test.node2.set_xy((1.0, -1.0));
        test.node3.set_xy((3.0, 1.0));
        test.run()
            .assert_root_computed_size(6.0, 4.0)
            .assert_node1_computed_size(2.0, 2.0)
            .assert_node2_computed_size(2.0, 2.0)
            .assert_node3_computed_size(2.0, 2.0)
            .assert_root_bbox_origin(-1.0, -1.0)
            .assert_root_position(0.0, 0.0)
            .assert_node1_position(-1.0, 0.0)
            .assert_node2_position(1.0, -1.0)
            .assert_node3_position(3.0, 1.0);
    }


    /// ```text
    /// ╭────────────────────────────────┬────────────────────────────────╮
    /// │ root                           ┆                                │
    /// │  ╭────┬─────────────────────╮  ┆  ╭────┬─────────────────────╮  │
    /// │  │    ┆ node1               │  ┆  │    ┆ node2               │  │
    /// │  │ ╭──┼──────╮              │  ┆  │ ╭──┼──────╮              │  │
    /// │  │ │ node1_1 │              │  ┆  │ │ node2_2 │              │  │
    /// │  │ │  ┆      │  ╭─────────╮ │  ┆  │ │  ┆      │  ╭─────────╮ │  │
    /// │  │ ╰──┼──────╯  │ node1_2 │ │  ┆  │ ╰──┼──────╯  │ node2_2 │ │  │
    /// │  │    ◎┈┈┈┈┈┈┈┈┈┼┈┈┈┈┈┈┈┈┈┼┈┤  ┆  │    ◎┈┈┈┈┈┈┈┈┈┼┈┈┈┈┈┈┈┈┈┼┈┤  │
    /// │  │              ╰─────────╯ │  ┆  │              ╰─────────╯ │  │
    /// │  ╰──────────────────────────╯  ┆  ╰──────────────────────────╯  │
    /// ╰────────────────────────────────┴────────────────────────────────╯
    /// ```
    #[test]
    fn test_layout_with_children_with_shifted_bbox_origins() {
        let test = TestFlatChildren2::new();

        test.root.use_auto_layout();

        let node1_1 = test.node1.new_child_named("node1_1");
        let node1_2 = test.node1.new_child_named("node1_2");
        node1_1.set_size((2.0, 2.0));
        node1_2.set_size((2.0, 2.0));
        node1_1.set_xy((-1.0, 0.0));
        node1_2.set_xy((1.0, -1.0));

        let node2_1 = test.node2.new_child_named("node2_1");
        let node2_2 = test.node2.new_child_named("node2_2");
        node2_1.set_size((2.0, 2.0));
        node2_2.set_size((2.0, 2.0));
        node2_1.set_xy((-1.0, 0.0));
        node2_2.set_xy((1.0, -1.0));

        test.run();

        assert_eq!(node1_1.position().xy(), Vector2(-1.0, 0.0));
        assert_eq!(node1_2.position().xy(), Vector2(1.0, -1.0));
        assert_eq!(node1_1.computed_size(), Vector2(2.0, 2.0));
        assert_eq!(node1_2.computed_size(), Vector2(2.0, 2.0));
        assert_eq!(test.node1.bbox_origin(), Vector2(-1.0, -1.0));
        assert_eq!(test.node1.computed_size(), Vector2(4.0, 3.0));

        assert_eq!(node2_1.position().xy(), Vector2(-1.0, 0.0));
        assert_eq!(node2_2.position().xy(), Vector2(1.0, -1.0));
        assert_eq!(node2_1.computed_size(), Vector2(2.0, 2.0));
        assert_eq!(node2_2.computed_size(), Vector2(2.0, 2.0));
        assert_eq!(test.node2.bbox_origin(), Vector2(-1.0, -1.0));
        assert_eq!(test.node2.computed_size(), Vector2(4.0, 3.0));

        test.assert_node1_position(1.0, 1.0)
            .assert_node2_position(5.0, 1.0)
            .assert_root_computed_size(8.0, 3.0)
            .assert_root_bbox_origin(0.0, 0.0)
            .assert_root_position(0.0, 0.0);
    }
}
