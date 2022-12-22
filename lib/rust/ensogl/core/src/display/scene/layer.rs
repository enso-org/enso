//! Scene layers implementation. See docs of [`Layer`] to learn more.

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::dirty;
use crate::data::OptVec;
use crate::display;
use crate::display::camera::Camera2d;
use crate::display::scene::Scene;
use crate::display::shape::primitive::system::ShapeSystemFlavor;
use crate::display::shape::system::Shape;
use crate::display::shape::system::ShapeInstance;
use crate::display::shape::system::ShapeSystem;
use crate::display::shape::system::ShapeSystemId;
use crate::display::symbol;
use crate::display::symbol::RenderGroup;
use crate::display::symbol::SymbolId;

use enso_data_structures::dependency_graph::DependencyGraph;
use enso_shapely::shared;
use smallvec::alloc::collections::BTreeSet;
use std::any::TypeId;



// ==================
// === LayerFlags ===
// ==================

bitflags::bitflags! {
    /// A set of flags associated with each [`Layer`].
    #[derive(Shrinkwrap)]
    pub struct LayerFlags: u8 {
        /// When layer is `MAIN_PASS_VISIBLE`, it will be rendered during standard color buffer
        /// render pass. Layers without this flag are not rendered in main pass, but can still be
        /// rendered in other passes, e.g. when used as mask layers.
        const MAIN_PASS_VISIBLE = 1 << 0;
        /// This layer's camera will be updated every time its parent camera is updated, or the
        /// parent layer itself is changed. See [`LayerModel::set_camera`] for implementation of
        /// camera inheritance.
        const INHERIT_PARENT_CAMERA = 1 << 1;
    }
}



// =============
// === Layer ===
// =============

/// Display layers implementation. Layer consist of a [`Camera`] and a set of [`LayerItem`]s. Layers
/// are hierarchical and contain sublayers. Items of a layer containing sublayers layers are
/// displayed below items of sublayers layers. Layers are allowed to share references to the same
/// camera. and the same [`Symbol`]s.
///
///
/// # Symbol Management
/// [`Symbol`]s are the basic primitives managed by layers. Even if you add an user-defined shape
/// system, it will be internally represented as a group of symbols (details are provided in the
/// following section). Layers are allowed to share references to the same [`Symbol`]s. For example,
/// you can create a layer which displays the same symbols as another layer, but from a
/// different camera to create a "mini-map view" of a graph editor.
///
/// ```text
/// +------+.
/// |`.    | `.  Layer 1 (top)
/// |  `+--+---+ (Camera 1 and symbols [1,2,3])
/// +---+--+.  |
/// |`. |  | `.| Layer 2 (middle)
/// |  `+------+ (Camera 2 and symbols [3,4,5])
/// +---+--+.  |
///  `. |    `.| Layer 3 (bottom)
///    `+------+ (Camera 1 and symbols [3,6,7])
/// ```
///
///
/// # ShapeProxy and ShapeSystem Management
/// You are allowed to define custom [`ShapeProxy`]s, which are like [`Shape`]s, but may not be
/// bound to a [`Scene`] (and thus to WebGL context) yet. You can use the [`Layer::add_exclusive`]
/// to add any [`DisplayObject`] to that particular layer. During update, the display object
/// hierarchy will propagate the layer-assignment information, and all [`ShapeView`]s containing
/// user-defined dynamic shapes will be initialized during the display object update time. Each
/// layer contains a [`ShapeSystemRegistry`] which contains a mapping between all used user-defined
/// shape types (this is type-level mapping!) to its corresponding [`ShapeSystem`]s. This allows
/// multiple [`DynamicShapes`] to share the same [`ShapeSystem`]s. For example, adding different
/// components containing the same shape to the same layer, will make rendering of all the shapes in
/// a single draw-call. This provides a great control over performance and possible rendering
/// optimizations.
///
///
/// # Layer Ordering
/// Layers can be ordered by using the `set_sublayers` method on their parent. By default,
/// layers are ordered according to their creation order.
///
///
/// # Symbols Ordering
/// There are two ways to define symbol ordering in scene layers, a global, and local (per-layer)
/// one. In order to define a global depth-order dependency, you can use the
/// `add_elements_order_dependency`, and the `remove_elements_order_dependency` methods
/// respectively. In order to define local (per-layer) depth-order dependency, you can use methods
/// of the same names in every layer instance. After changing a dependency graph, the layer
/// management marks appropriate dirty flags and re-orders symbols on each new frame processed.
///
/// During symbol sorting, the global and local dependency graphs are merged together. The defined
/// rules are equivalently important, so local rules will not override global ones. In case of
/// lack of dependencies or circular dependencies, the symbol ids are considered (the ids are
/// increasing with every new symbol created).
///
/// Please note, that symbol ordering doesn't work cross-layer. Even if you define that symbol A has
/// to be above the symbol B, but you place symbol B on a layer above the layer of the symbol A, the
/// symbol A will be drawn first, below symbol B!
///
///
/// # Shapes Ordering
/// Ordering of shapes is more tricky than ordering of [`Symbol`]s. Each shape instance will be
/// assigned with a unique [`Symbol`] when placed on a stage, but the connection may change or can
/// be missing when the shape will be detached from the display object hierarchy or when the shape
/// will be moved between the layers. Read the "Shape Management" section below to learn why.
///
/// Shapes can be ordered by using the same methods as symbols (described above). In fact, the
/// depth-order dependencies can be seamlessly defined between both [`Symbol`]s and
/// [`ShapeProxy`]s thanks to the [`LayerItem`] abstraction. Moreover, there is a special
/// shapes ordering API allowing describing their dependencies without requiring references to their
/// instances (unlike the API described above). You can add or remove depth-order dependencies for
/// shapes based solely on their types by using the [`add_shapes_order_dependency`],and the
/// [`remove_shapes_order_dependency`] methods, respectively. Please note, that
///
/// Also, there is a macro [`shapes_order_dependencies!`] which allows convenient form for
/// defining the depth-order dependency graph for shapes based on their types.
///
///
/// # Compile Time Shapes Ordering Relations
/// There is also a third way to define depth-dependencies for shapes. However, unlike previous
/// methods, this one does not require you to own a reference to [`Scene`] or its [`Layer`]. Also,
/// it is impossible to remove during runtime dependencies created this way. This might sound
/// restrictive, but actually it is what you may often want to do. For example, when creating a
/// text area, you want to define that the cursor should always be above its background and there is
/// no situation when it should not be hold. In such a way, you should use this method to define
/// depth-dependencies. In order to define such compile tie shapes ordering relations, you have to
/// define them while defining the shape system. The easiest way to do it is by using the
/// [`shape!`] macro. Refer to its documentation to learn more.
///
///
/// # Layer Lifetime Management
/// Every [`Layer`] allows you to add symbols while removing them from other layers automatically.
/// The [`SublayersModel`] holds [`WeakLayer`], the weak form of a [`Layer`] that does not prevent
/// the layer from being dropped. That means a layer is not held alive just by being a part of the
/// scene hierarchy. When you drop last [`Layer`] reference, it will be automatically unregistered
/// from its parent and all its symbols will be removed from the scene.
///
/// # Masking Layers With ScissorBox
/// Layers rendering an be limited to a specific set of pixels by using the [`ScissorBox`] object.
/// Only the required pixels will be processed by the GPU which makes layer scissors a very
/// efficient clipping mechanism (definitely faster than masking with arbitrary shapes). All
/// [`ScissorBox`] elements are inherited by sublayers and can be refined (the common shape of
/// overlapping scissor boxes is automatically computed).
///
/// Please note that although this method is the fastest (almost zero-cost) masking way, it has
/// several downsides – it can be used only for rectangular areas, and also it works on whole pixels
/// only. The latter fact drastically limits its usability on elements with animations. Animating
/// rectangles requires displaying them sometimes with non-integer coordinates in order to get a
/// correct, smooth movement. Using [`ScissorBox`] on such elements would always cut them to whole
/// pixels which might result in a jaggy animation.
///
/// # Masking Layers With Arbitrary Shapes
/// Every layer can be applied with a "mask", another layer defining the visible area of the first
/// layer. The masked layer will be rendered first and it will be used to determine which pixels to
/// hide in the first layer. Unlike in many other solutions, masks are not black-white. Only the
/// alpha channel of the mask is used to determine which area should be hidden in the masked layer.
/// This design allows for a much easier definition of layers and also, it allows layers to be
/// assigned as both visible layers as masks, without the need to modify their shapes definitions.
/// As layers are hierarchical, you can also apply masks to group of layers.
///
/// Please note that the current implementation does not allow for hierarchical masks (masks applied
/// to already masked area or masks applied to masks). If you try using masks in hierarchical way,
/// the nested masks will be skipped and a warning will be emitted to the console.
#[derive(Clone, CloneRef, Deref)]
pub struct Layer {
    model: Rc<LayerModel>,
}

impl Layer {
    /// Create a new detached layer. It will inherit the camera of the parent layer once it is
    /// attached.
    pub fn new(name: impl Into<String>) -> Self {
        let flags = LayerFlags::MAIN_PASS_VISIBLE | LayerFlags::INHERIT_PARENT_CAMERA;
        Self::new_with_flags(name, flags)
    }

    /// Create a new layer with specified camera. If it will be later attached as a sublayer, it
    /// will not inherit the camera of the set parent layer.
    #[profile(Detail)]
    pub fn new_with_camera(name: impl Into<String>, camera: &Camera2d) -> Self {
        let flags = LayerFlags::MAIN_PASS_VISIBLE;
        let this = Self::new_with_flags(name, flags);
        this.set_camera(camera);
        this
    }

    /// Create a new layer with specified inheritance and render_only_as_mask flags.
    fn new_with_flags(name: impl Into<String>, flags: LayerFlags) -> Self {
        let model = LayerModel::new(name, flags);
        let model = Rc::new(model);
        Self { model }
    }

    /// Create a new weak pointer to this layer.
    pub fn downgrade(&self) -> WeakLayer {
        let model = Rc::downgrade(&self.model);
        WeakLayer { model }
    }

    /// Add the display object to this layer and remove it from a layer it was assigned to, if any.
    pub fn add(&self, object: impl display::Object) {
        object.display_object().add_to_display_layer(self);
    }

    /// Remove the display object from a layer it was assigned to, if any.
    pub fn remove(&self, object: impl display::Object) {
        object.display_object().remove_from_display_layer(self);
    }

    /// Instantiate the provided [`ShapeProxy`].
    pub fn instantiate<S>(
        &self,
        scene: &Scene,
        data: &S::ShapeData,
    ) -> (ShapeInstance<S>, LayerShapeBinding)
    where
        S: Shape,
    {
        let (shape_system_info, symbol_id, shape_instance, global_instance_id) =
            self.shape_system_registry.instantiate(scene, data);
        self.add_shape(shape_system_info, symbol_id);
        (shape_instance, LayerShapeBinding::new(self, global_instance_id))
    }

    /// Iterate over all layers and sublayers of this layer hierarchically. Parent layers will be
    /// visited before their corresponding sublayers. Does not visit masks. If you want to visit
    /// masks, use [`iter_sublayers_and_masks_nested`] instead. The visited layer sublayers will be
    /// borrowed during the iteration.
    pub fn iter_sublayers_nested(&self, mut f: impl FnMut(&Layer)) {
        self.iter_sublayers_nested_internal(&mut f)
    }

    fn iter_sublayers_nested_internal(&self, f: &mut impl FnMut(&Layer)) {
        f(self);
        self.for_each_sublayer(|layer| layer.iter_sublayers_nested_internal(f));
    }

    /// Iterate over all layers, sublayers, masks, and their sublayers of this layer hierarchically.
    /// Parent layers will be visited before their corresponding sublayers. The visited layer
    /// sublayers and masks will be borrowed during the iteration.
    pub fn iter_sublayers_and_masks_nested(&self, mut f: impl FnMut(&Layer)) {
        self.iter_sublayers_and_masks_nested_internal(&mut f)
    }

    fn iter_sublayers_and_masks_nested_internal(&self, f: &mut impl FnMut(&Layer)) {
        f(self);
        if let Some(mask) = &*self.mask.borrow() {
            if let Some(layer) = mask.upgrade() {
                layer.iter_sublayers_and_masks_nested_internal(f)
            }
        }
        self.for_each_sublayer(|layer| layer.iter_sublayers_and_masks_nested_internal(f));
    }
}

impl AsRef<Layer> for Layer {
    fn as_ref(&self) -> &Layer {
        self
    }
}

impl Debug for Layer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&*self.model, f)
    }
}

impl From<&Layer> for LayerId {
    fn from(t: &Layer) -> Self {
        t.id()
    }
}

impl Eq for Layer {}
impl PartialEq for Layer {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.model, &other.model)
    }
}



// =================
// === WeakLayer ===
// =================

/// A weak version of [`Layer`].
#[derive(Clone, CloneRef)]
pub struct WeakLayer {
    model: Weak<LayerModel>,
}

impl WeakLayer {
    /// Upgrade to strong reference.
    pub fn upgrade(&self) -> Option<Layer> {
        self.model.upgrade().map(|model| Layer { model })
    }

    /// Attach a `layer` as a sublayer. Will do nothing if the layer does not exist.
    pub fn add_sublayer(&self, sublayer: &Layer) {
        if let Some(layer) = self.upgrade() {
            layer.add_sublayer(sublayer)
        } else {
            warn!("Attempt to add a sublayer to deallocated layer.");
        }
    }

    /// Remove previously attached sublayer. Will do nothing if the layer does not exist.
    pub fn remove_sublayer(&self, sublayer: &Layer) {
        if let Some(layer) = self.upgrade() {
            layer.remove_sublayer(sublayer)
        } else {
            warn!("Attempt to remove a sublayer from deallocated layer.");
        }
    }
}

impl Debug for WeakLayer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WeakLayer")
    }
}

impl Eq for WeakLayer {}
impl PartialEq for WeakLayer {
    fn eq(&self, other: &Self) -> bool {
        self.model.ptr_eq(&other.model)
    }
}



// ==================
// === LayerModel ===
// ==================

/// Internal representation of [`Layer`].
///
/// Please note that the [`parent`] field contains reference to a very small part of parent layer,
/// namely to its [`Sublayers`] struct. Only this part is needed to properly update all the models.
#[allow(missing_docs)]
pub struct LayerModel {
    pub name: String,
    camera: RefCell<Camera2d>,
    pub shape_system_registry: ShapeSystemRegistry,
    shape_system_to_symbol_info_map: RefCell<HashMap<ShapeSystemId, ShapeSystemSymbolInfo>>,
    symbol_to_shape_system_map: RefCell<HashMap<SymbolId, ShapeSystemId>>,
    elements: RefCell<BTreeSet<LayerItem>>,
    symbols_renderable: Rc<RefCell<RenderGroup>>,
    depth_order: RefCell<DependencyGraph<LayerItem>>,
    depth_order_dirty: dirty::SharedBool<OnDepthOrderDirty>,
    parent: Rc<RefCell<Option<Sublayers>>>,
    global_element_depth_order: Rc<RefCell<DependencyGraph<LayerItem>>>,
    sublayers: Sublayers,
    mask: RefCell<Option<WeakLayer>>,
    scissor_box: RefCell<Option<ScissorBox>>,
    mem_mark: Rc<()>,
    pub flags: LayerFlags,
}

impl Debug for LayerModel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Layer")
            .field("name", &self.name)
            .field("id", &self.id().raw)
            .field("registry", &self.shape_system_registry)
            .field("elements", &self.elements.borrow().iter().collect_vec())
            .field("symbols_renderable", &self.symbols_renderable)
            .finish()
    }
}

impl Drop for LayerModel {
    fn drop(&mut self) {
        self.remove_from_parent();
    }
}

impl LayerModel {
    fn new(name: impl Into<String>, flags: LayerFlags) -> Self {
        let name = name.into();
        let camera = default();
        let shape_system_registry = default();
        let shape_system_to_symbol_info_map = default();
        let symbol_to_shape_system_map = default();
        let elements = default();
        let symbols_renderable = default();
        let depth_order = default();
        let parent = default();
        let on_mut = on_depth_order_dirty(&parent);
        let depth_order_dirty = dirty::SharedBool::new(on_mut);
        let global_element_depth_order = default();
        let sublayers = Sublayers::new(&parent);
        let mask = default();
        let scissor_box = default();
        let mem_mark = default();
        Self {
            name,
            camera,
            shape_system_registry,
            shape_system_to_symbol_info_map,
            symbol_to_shape_system_map,
            elements,
            symbols_renderable,
            depth_order,
            depth_order_dirty,
            parent,
            global_element_depth_order,
            sublayers,
            mask,
            scissor_box,
            mem_mark,
            flags,
        }
    }

    /// Unique identifier of this layer. It is memory-based, it will be unique even for layers in
    /// different instances of [`Scene`].
    pub fn id(&self) -> LayerId {
        LayerId::new(Rc::as_ptr(&self.mem_mark) as usize)
    }

    /// Vector of all symbols registered in this layer, ordered according to the defined depth-order
    /// dependencies. Please note that this function does not update the depth-ordering of the
    /// elements. Updates are performed by calling the `update` method on [`Layer`], which happens
    /// at least once per animation frame.
    pub fn symbols(&self) -> impl Deref<Target = RenderGroup> + '_ {
        self.symbols_renderable.borrow()
    }

    /// Return the [`SymbolId`] of the provided [`LayerItem`] if it was added to the current
    /// layer.
    pub fn symbol_id_of_element(&self, element: LayerItem) -> Option<SymbolId> {
        use LayerItem::*;
        match element {
            Symbol(id) => Some(id),
            ShapeSystem(id) => self.shape_system_to_symbol_info_map.borrow().get(&id).map(|t| t.id),
        }
    }

    /// Add depth-order dependency between two [`LayerItem`]s in this layer.
    pub fn add_elements_order_dependency(
        &self,
        below: impl Into<LayerItem>,
        above: impl Into<LayerItem>,
    ) {
        let below = below.into();
        let above = above.into();
        if self.depth_order.borrow_mut().insert_dependency(below, above) {
            self.depth_order_dirty.set();
        }
    }

    /// Remove a depth-order dependency between two [`LayerItem`]s in this layer. Returns `true`
    /// if the dependency was found, and `false` otherwise.
    pub fn remove_elements_order_dependency(
        &self,
        below: impl Into<LayerItem>,
        above: impl Into<LayerItem>,
    ) -> bool {
        let below = below.into();
        let above = above.into();
        let found = self.depth_order.borrow_mut().remove_dependency(below, above);
        if found {
            self.depth_order_dirty.set();
        }
        found
    }

    /// Add depth-order dependency between two shape-like definitions, where a "shape-like"
    /// definition means a [`Shape`], a [`ShapeProxy`], or user-defined shape system.
    pub fn add_shapes_order_dependency<S1, S2>(&self) -> (PhantomData<S1>, PhantomData<S2>)
    where
        S1: Shape,
        S2: Shape, {
        let s1_id = ShapeSystem::<S1>::id();
        let s2_id = ShapeSystem::<S2>::id();
        self.add_elements_order_dependency(s1_id, s2_id);
        self.depth_order_dirty.set();
        default()
    }

    /// Remove depth-order dependency between two shape-like definitions, where a "shape-like"
    /// definition means a [`Shape`], a [`ShapeProxy`], or user-defined shape system. Returns
    /// `true` if the dependency was found, and `false` otherwise.
    pub fn remove_shapes_order_dependency<S1, S2>(
        &self,
    ) -> (bool, PhantomData<S1>, PhantomData<S2>)
    where
        S1: Shape,
        S2: Shape, {
        let s1_id = ShapeSystem::<S1>::id();
        let s2_id = ShapeSystem::<S2>::id();
        let found = self.remove_elements_order_dependency(s1_id, s2_id);
        if found {
            self.depth_order_dirty.set();
        }
        (found, default(), default())
    }

    /// Camera getter of this layer.
    pub fn camera(&self) -> Camera2d {
        self.camera.borrow().clone_ref()
    }

    /// Camera setter of this layer. Will propagate the camera to all sublayers if they inherit
    /// the parent camera.
    fn set_camera(&self, camera: impl Into<Camera2d>) {
        let camera = camera.into();
        *self.camera.borrow_mut() = camera.clone_ref();
        self.for_each_sublayer(|layer| {
            if layer.flags.contains(LayerFlags::INHERIT_PARENT_CAMERA) {
                layer.set_camera(camera.clone_ref());
            }
        });
    }

    /// Add the symbol to this layer.
    pub fn add_symbol(&self, symbol_id: impl Into<SymbolId>) {
        self.add_element(symbol_id.into(), None)
    }

    /// Add the shape to this layer.
    pub(crate) fn add_shape(
        &self,
        shape_system_info: ShapeSystemInfo,
        symbol_id: impl Into<SymbolId>,
    ) {
        self.add_element(symbol_id.into(), Some(shape_system_info))
    }

    /// Internal helper for adding elements to this layer.
    fn add_element(&self, symbol_id: SymbolId, shape_system_info: Option<ShapeSystemInfo>) {
        self.depth_order_dirty.set();
        match shape_system_info {
            None => {
                self.elements.borrow_mut().insert(LayerItem::Symbol(symbol_id));
            }
            Some(info) => {
                let symbol_info = ShapeSystemSymbolInfo::new(symbol_id, info.ordering);
                self.shape_system_to_symbol_info_map.borrow_mut().insert(info.id, symbol_info);
                self.symbol_to_shape_system_map.borrow_mut().insert(symbol_id, info.id);
                self.elements.borrow_mut().insert(LayerItem::ShapeSystem(info.id));
            }
        }
    }

    /// Remove the symbol from the current layer.
    pub fn remove_symbol(&self, symbol_id: impl Into<SymbolId>) {
        self.depth_order_dirty.set();
        let symbol_id = symbol_id.into();

        self.elements.borrow_mut().remove(&LayerItem::Symbol(symbol_id));
        if let Some(shape_system_id) =
            self.symbol_to_shape_system_map.borrow_mut().remove(&symbol_id)
        {
            self.shape_system_to_symbol_info_map.borrow_mut().remove(&shape_system_id);
            self.elements.borrow_mut().remove(&LayerItem::ShapeSystem(shape_system_id));
        }
    }

    /// Remove the [`ShapeSystem`] registered in this layer together with all of its [`Symbol`]s.
    pub fn remove_shape_system(&self, shape_system_id: ShapeSystemId) {
        self.depth_order_dirty.set();
        self.elements.borrow_mut().remove(&LayerItem::ShapeSystem(shape_system_id));
        if let Some(symbol_id) =
            self.shape_system_to_symbol_info_map.borrow_mut().remove(&shape_system_id)
        {
            self.symbol_to_shape_system_map.borrow_mut().remove(&symbol_id.id);
        }
    }

    /// Consume all dirty flags and update the ordering of elements if needed. Returns [`true`] if
    /// the layer or its sub-layers were modified during this call.
    pub fn update(&self) -> bool {
        self.update_internal(None)
    }

    /// Consume all dirty flags and update the ordering of elements if needed.
    #[profile(Debug)]
    pub(crate) fn update_internal(
        &self,
        global_element_depth_order: Option<&DependencyGraph<LayerItem>>,
    ) -> bool {
        let mut was_dirty = false;

        if self.depth_order_dirty.check() {
            was_dirty = true;
            self.depth_order_dirty.unset();
            self.depth_sort(global_element_depth_order);
        }

        if self.sublayers.element_depth_order_dirty.check() {
            was_dirty = true;
            self.sublayers.element_depth_order_dirty.unset();
            self.for_each_sublayer(|layer| {
                layer.update_internal(Some(&*self.global_element_depth_order.borrow()));
            });
            if let Some(layer) = &*self.mask.borrow() {
                if let Some(layer) = layer.upgrade() {
                    layer.update_internal(Some(&*self.global_element_depth_order.borrow()));
                }
            }
        }

        was_dirty
    }

    /// Compute a combined [`DependencyGraph`] for the layer taking into consideration the global
    /// dependency graph (from root [`Layer`]), the local one (per layer), and individual shape
    /// preferences (see the "Compile Time Shapes Ordering Relations" section in docs of [`Layer`]
    /// to learn more).
    fn combined_depth_order_graph(
        &self,
        global_element_depth_order: Option<&DependencyGraph<LayerItem>>,
    ) -> DependencyGraph<LayerItem> {
        let mut graph = if let Some(global_element_depth_order) = global_element_depth_order {
            let mut graph = global_element_depth_order.clone();
            graph.extend(self.depth_order.borrow().clone().into_iter());
            graph
        } else {
            self.depth_order.borrow().clone()
        };
        for element in &*self.elements.borrow() {
            if let LayerItem::ShapeSystem(id) = element {
                if let Some(info) = self.shape_system_to_symbol_info_map.borrow().get(id) {
                    for &id2 in &info.below {
                        graph.insert_dependency(*element, id2.into());
                    }
                    for &id2 in &info.above {
                        graph.insert_dependency(id2.into(), *element);
                    }
                }
            }
        }
        graph
    }

    fn depth_sort(&self, global_element_depth_order: Option<&DependencyGraph<LayerItem>>) {
        let graph = self.combined_depth_order_graph(global_element_depth_order);
        let elements_sorted = self.elements.borrow().iter().copied().collect_vec();
        let sorted_elements = graph.into_unchecked_topo_sort(elements_sorted);
        let sorted_symbols = sorted_elements
            .into_iter()
            .filter_map(|element| match element {
                LayerItem::Symbol(symbol_id) => Some(symbol_id),
                LayerItem::ShapeSystem(id) => {
                    let out = self.shape_system_to_symbol_info_map.borrow().get(&id).map(|t| t.id);
                    if out.is_none() {
                        warn!("Trying to perform depth-order of non-existing element '{:?}'.", id)
                    }
                    out
                }
            })
            .collect();
        self.symbols_renderable.borrow_mut().set(sorted_symbols);
    }
}


// === Grouping Utilities ===

impl LayerModel {
    /// Query [`Layer`] by [`LayerId`].
    pub fn get_sublayer(&self, layer_id: LayerId) -> Option<Layer> {
        self.sublayers.borrow().get(layer_id)
    }

    /// Vector of all layers, ordered according to the defined depth-order dependencies. Please note
    /// that this function does not update the depth-ordering of the layers. Updates are performed
    /// by calling the `update` method on [`Layer`], which happens at least once per animation
    /// frame.
    pub fn sublayers(&self) -> Vec<Layer> {
        self.sublayers.borrow().all()
    }

    /// Iterate over all sublayers, ordered according to the defined depth-order dependencies. The
    /// layer sublayers list will be borrowed during the iteration.
    ///
    /// Please note that this function does not update the depth-ordering of the layers. Updates are
    /// performed by calling the `update` method on [`Layer`], which happens at least once per
    /// animation frame.
    pub fn for_each_sublayer(&self, mut f: impl FnMut(Layer)) {
        for layer in self.sublayers.borrow().iter_all() {
            f(layer);
        }
    }

    /// Attach a `layer` as a sublayer. If that layer is already attached as a sublayer of any
    /// layer, it will be detached first.
    pub fn add_sublayer(&self, layer: &Layer) {
        layer.remove_from_parent();
        self.sublayers.borrow_mut().add(layer);
        layer.set_parent(self);
    }

    /// Remove previously attached sublayer.
    ///
    /// The implementation is the opposite of [`LayerModel::add_sublayer`]: we modify both fields of
    /// [`SublayersModel`] and also unset parent. If the layer was not attached as a sublayer, this
    /// function does nothing.
    pub fn remove_sublayer(&self, layer: &Layer) {
        let removed = self.sublayers.borrow_mut().remove(layer.id());
        if removed {
            *layer.parent.borrow_mut() = None;
        }
    }

    fn remove_all_sublayers(&self) {
        for layer in self.sublayers.borrow().layers.iter() {
            if let Some(layer) = layer.upgrade() {
                layer.remove_parent()
            }
        }
        mem::take(&mut *self.sublayers.model.borrow_mut());
    }

    fn set_parent(&self, parent: &LayerModel) {
        *self.parent.borrow_mut() = Some(parent.sublayers.clone_ref());
        if self.flags.contains(LayerFlags::INHERIT_PARENT_CAMERA) {
            self.set_camera(parent.camera());
        }
    }

    fn remove_parent(&self) {
        *self.parent.borrow_mut() = None;
    }

    fn remove_from_parent(&self) {
        if let Some(sublayers) = self.parent.borrow_mut().take() {
            sublayers.borrow_mut().remove(self.id());
        }
    }

    fn remove_mask(&self) {
        mem::take(&mut *self.mask.borrow_mut());
    }

    /// Set all sublayers layer of this layer. Old sublayers layers will be unregistered.
    pub fn set_sublayers(&self, layers: &[&Layer]) {
        self.remove_all_sublayers();
        for layer in layers {
            self.add_sublayer(layer)
        }
    }

    /// Create a new sublayer to this layer. It will inherit this layer's camera.
    pub fn create_sublayer(&self, name: impl Into<String>) -> Layer {
        let layer = Layer::new_with_flags(
            name,
            LayerFlags::MAIN_PASS_VISIBLE | LayerFlags::INHERIT_PARENT_CAMERA,
        );
        self.add_sublayer(&layer);
        layer
    }

    /// Create a new sublayer to this layer. Override the camera for this layer, breaking the camera
    /// inheritance chain. Updates to the parent camera will not affect this layer.
    pub fn create_sublayer_with_camera(&self, name: impl Into<String>, camera: &Camera2d) -> Layer {
        let layer = Layer::new_with_camera(name, camera);
        self.add_sublayer(&layer);
        layer
    }


    /// Create a new sublayer to this layer. It will inherit this layer's camera, but will not be
    /// rendered as a part of standard layer stack. Instead, it will only be rendered as a mask for
    /// other layers. See [`Layer::set_mask`] for more information. Note that this will not set up
    /// the mask connection. You still have to separately call `set_mask` on any layer you want to
    /// mask with this layer.
    pub fn create_mask_sublayer(&self, name: impl Into<String>) -> Layer {
        let layer = Layer::new_with_flags(name, LayerFlags::INHERIT_PARENT_CAMERA);
        self.add_sublayer(&layer);
        layer
    }

    /// The layer's mask, if any.
    pub fn mask(&self) -> Option<Layer> {
        self.mask.borrow().as_ref().and_then(|t| t.upgrade())
    }

    /// Set a mask layer of this layer. Old mask layer will be unregistered.
    pub fn set_mask(&self, mask: &Layer) {
        self.remove_mask();
        *self.mask.borrow_mut() = Some(mask.downgrade());
    }

    /// The layer's [`ScissorBox`], if any.
    pub fn scissor_box(&self) -> Option<ScissorBox> {
        *self.scissor_box.borrow()
    }

    /// Set the [`ScissorBox`] of this layer.
    pub fn set_scissor_box(&self, scissor_box: Option<&ScissorBox>) {
        *self.scissor_box.borrow_mut() = scissor_box.cloned();
    }

    /// Add depth-order dependency between two [`LayerItem`]s in this layer. Returns `true`
    /// if the dependency was inserted successfully (was not already present), and `false`
    /// otherwise. All sublayers will inherit these rules.
    pub fn add_global_elements_order_dependency(
        &self,
        below: impl Into<LayerItem>,
        above: impl Into<LayerItem>,
    ) -> bool {
        let below = below.into();
        let above = above.into();
        let fresh = self.global_element_depth_order.borrow_mut().insert_dependency(below, above);
        if fresh {
            self.sublayers.element_depth_order_dirty.set();
        }
        fresh
    }

    /// Remove a depth-order dependency between two [`LayerItem`]s in this layer. Returns `true`
    /// if the dependency was found, and `false` otherwise.
    pub fn remove_global_elements_order_dependency(
        &self,
        below: impl Into<LayerItem>,
        above: impl Into<LayerItem>,
    ) -> bool {
        let below = below.into();
        let above = above.into();
        let found = self.global_element_depth_order.borrow_mut().remove_dependency(below, above);
        if found {
            self.sublayers.element_depth_order_dirty.set();
        }
        found
    }

    /// # Future Improvements
    /// This implementation can be simplified to `S1:KnownShapeSystemId` (not using [`Content`] at
    /// all), after the compiler gets updated to newer version. Returns `true` if the dependency was
    /// inserted successfully (was not already present), and `false` otherwise.
    pub fn add_global_shapes_order_dependency<S1, S2>(
        &self,
    ) -> (bool, PhantomData<S1>, PhantomData<S2>)
    where
        S1: Shape,
        S2: Shape, {
        let s1_id = ShapeSystem::<S1>::id();
        let s2_id = ShapeSystem::<S2>::id();
        let fresh = self.add_global_elements_order_dependency(s1_id, s2_id);
        (fresh, default(), default())
    }

    /// # Future Improvements
    /// This implementation can be simplified to `S1:KnownShapeSystemId` (not using [`Content`] at
    /// all), after the compiler gets updated to newer version. Returns `true` if the dependency was
    /// found, and `false` otherwise.
    pub fn remove_global_shapes_order_dependency<S1, S2>(
        &self,
    ) -> (bool, PhantomData<S1>, PhantomData<S2>)
    where
        S1: Shape,
        S2: Shape, {
        let s1_id = ShapeSystem::<S1>::id();
        let s2_id = ShapeSystem::<S2>::id();
        let found = self.remove_global_elements_order_dependency(s1_id, s2_id);
        (found, default(), default())
    }
}

/// The callback setting `element_depth_order_dirty` in parents.
pub type OnDepthOrderDirty = impl Fn();
fn on_depth_order_dirty(parent: &Rc<RefCell<Option<Sublayers>>>) -> OnDepthOrderDirty {
    let parent = parent.clone();
    move || {
        if let Some(parent) = parent.borrow().as_ref() {
            // It's safe to do it having parent borrowed, because the only possible callback called
            // [`OnElementDepthOrderDirty`], which don't borrow_mut at any point.
            parent.element_depth_order_dirty.set()
        }
    }
}

impl AsRef<LayerModel> for Layer {
    fn as_ref(&self) -> &LayerModel {
        &self.model
    }
}

impl std::borrow::Borrow<LayerModel> for Layer {
    fn borrow(&self) -> &LayerModel {
        &self.model
    }
}



// =========================
// === LayerShapeBinding ===
// =========================

/// Information about an instance of a shape bound to a particular layer.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct LayerShapeBinding {
    pub layer:              WeakLayer,
    pub global_instance_id: symbol::GlobalInstanceId,
}

impl LayerShapeBinding {
    /// Constructor.
    pub fn new(layer: &Layer, global_instance_id: symbol::GlobalInstanceId) -> Self {
        let layer = layer.downgrade();
        Self { layer, global_instance_id }
    }
}



// =================
// === Sublayers ===
// =================

/// The callback propagating `element_depth_order_dirty` flag to parent.
pub type OnElementDepthOrderDirty = impl Fn();
fn on_element_depth_order_dirty(
    parent: &Rc<RefCell<Option<Sublayers>>>,
) -> OnElementDepthOrderDirty {
    let parent = parent.clone_ref();
    move || {
        if let Some(parent) = parent.borrow().as_ref() {
            // It's safe to do it having parent borrowed, because the only possible callback called
            // [`OnElementDepthOrderDirty`], which don't borrow_mut at any point.
            parent.element_depth_order_dirty.set()
        }
    }
}

/// Abstraction for layer sublayers.
#[derive(Clone, CloneRef, Debug)]
pub struct Sublayers {
    model:                     Rc<RefCell<SublayersModel>>,
    element_depth_order_dirty: dirty::SharedBool<OnElementDepthOrderDirty>,
}

impl Deref for Sublayers {
    type Target = Rc<RefCell<SublayersModel>>;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl Eq for Sublayers {}
impl PartialEq for Sublayers {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.model, &other.model)
    }
}

impl Sublayers {
    /// Constructor.
    pub fn new(parent: &Rc<RefCell<Option<Sublayers>>>) -> Self {
        let model = default();
        let dirty_on_mut = on_element_depth_order_dirty(parent);
        let element_depth_order_dirty = dirty::SharedBool::new(dirty_on_mut);
        Self { model, element_depth_order_dirty }
    }
}



// ======================
// === SublayersModel ===
// ======================

/// Internal representation of [`Sublayers`].
#[derive(Debug, Default)]
pub struct SublayersModel {
    layers:          OptVec<WeakLayer>,
    layer_placement: HashMap<LayerId, usize>,
}

impl SublayersModel {
    /// Vector of all layers, ordered according to the defined depth-order dependencies. Please note
    /// that this function does not update the depth-ordering of the layers. Updates are performed
    /// by calling the `update` method on [`LayerModel`], which happens once per animation frame.
    pub fn all(&self) -> Vec<Layer> {
        self.iter_all().collect()
    }

    /// Iterator of all layers, ordered according to the defined depth-order dependencies. Please
    /// note that this function does not update the depth-ordering of the layers. Updates are
    /// performed by calling the `update` method on [`LayerModel`], which happens once per animation
    /// frame.
    pub fn iter_all(&self) -> impl Iterator<Item = Layer> + '_ {
        self.layers.iter().filter_map(|t| t.upgrade())
    }


    fn layer_ix(&self, layer_id: LayerId) -> Option<usize> {
        self.layer_placement.get(&layer_id).copied()
    }

    fn add(&mut self, layer: &Layer) {
        let ix = self.layers.insert(layer.downgrade());
        self.layer_placement.insert(layer.id(), ix);
    }

    fn remove(&mut self, layer_id: LayerId) -> bool {
        if let Some(ix) = self.layer_ix(layer_id) {
            self.layers.remove(ix);
            true
        } else {
            false
        }
    }

    /// Query a [`Layer`] based on its [`LayerId`].
    pub fn get(&self, layer_id: LayerId) -> Option<Layer> {
        self.layer_ix(layer_id).and_then(|ix| self.layers.safe_index(ix).and_then(|t| t.upgrade()))
    }
}



// ==============
// === Masked ===
// ==============

/// A layer with an attached mask. Each shape in the `mask_layer` defines the renderable area
/// of the `masked_layer`. See [`Layer`] docs for the info about masking.
///
/// One of the use cases might be an `ensogl_scroll_area::ScrollArea` component
/// implementation. To clip the area's content (so that it is displayed only inside its borders) we
/// place the area's content in the `masked_object` layer; and we place a rectangular mask in the
/// `mask` layer.
///
/// We need to store `mask_layer`, because [`LayerModel::set_mask`] uses [`WeakLayer`] internally,
/// so the [`Layer`] would be deallocated otherwise.
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct Masked {
    #[deref]
    pub masked_layer: Layer,
    pub mask_layer:   Layer,
}

impl AsRef<Layer> for Masked {
    fn as_ref(&self) -> &Layer {
        &self.masked_layer
    }
}

impl Masked {
    /// Constructor.
    pub fn new() -> Self {
        let masked_layer = Layer::new("MaskedLayer");
        let mask_layer = masked_layer.create_mask_sublayer("MaskLayer");
        masked_layer.set_mask(&mask_layer);
        Self { masked_layer, mask_layer }
    }

    /// Constructor.
    pub fn new_with_cam(camera: &Camera2d) -> Self {
        let masked_layer = Layer::new_with_camera("MaskedLayer", camera);
        let mask_layer = masked_layer.create_mask_sublayer("MaskLayer");
        masked_layer.set_mask(&mask_layer);
        Self { masked_layer, mask_layer }
    }
}

impl Default for Masked {
    fn default() -> Self {
        Self::new()
    }
}



// ===============
// === LayerId ===
// ===============

use enso_shapely::newtype_prim;
newtype_prim! {
    /// The ID of a layer. Under the hood, it is the index of the layer.
    LayerId(usize);
}



// =================
// === LayerItem ===
// =================

/// Abstraction over [`SymbolId`] and [`ShapeSystemId`]. Read docs of [`Layer`] to learn about its
/// usage scenarios.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Hash, Ord)]
#[allow(missing_docs)]
pub enum LayerItem {
    Symbol(SymbolId),
    ShapeSystem(ShapeSystemId),
}

impl From<ShapeSystemId> for LayerItem {
    fn from(t: ShapeSystemId) -> Self {
        Self::ShapeSystem(t)
    }
}



// =====================
// === ShapeRegistry ===
// =====================

/// An entry containing [`Any`]-encoded [`ShapeSystem`] and information about symbol instance count
/// of this [`ShapeSystem`].
pub struct ShapeSystemRegistryEntry {
    shape_system:   Box<dyn Any>,
    instance_count: usize,
}

impl Debug for ShapeSystemRegistryEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.instance_count, f)
    }
}

/// Mutable reference to decoded [`ShapeSystemRegistryEntry`].
#[derive(Debug)]
pub struct ShapeSystemRegistryEntryRefMut<'t, T> {
    shape_system:   &'t mut T,
    instance_count: &'t mut usize,
}

shared! { ShapeSystemRegistry
/// A per [`Scene`] [`Layer`] user defined shape system registry. It is used as a cache for existing
/// shape system instances. When creating a shape instance, we often want it to share the same shape
/// system than other instances in order for all of them to be drawn with just a single WebGL draw
/// call. After adding a [`ShapeProxy`] to a layer, it will get instantiated (its shape will be
/// created), and because of this structure, it will share the same shape system as other shapes of
/// the same type on the same layer. Read the docs of [`ShapeProxy`] to learn more.
#[derive(Default,Debug)]
pub struct ShapeSystemRegistryData {
    shape_system_map : HashMap<(TypeId, ShapeSystemFlavor),ShapeSystemRegistryEntry>,
    shape_system_flavors: HashMap<TypeId, Vec<ShapeSystemFlavor>>,
}

impl {
    /// Instantiate the provided [`ShapeProxy`].
    pub fn instantiate<S>
    (&mut self, scene:&Scene, data: &S::ShapeData) -> (ShapeSystemInfo, SymbolId, ShapeInstance<S>, symbol::GlobalInstanceId)
    where S : Shape {
        self.with_get_or_register_mut::<S,_,_>(scene, data, |entry| {
            let system = entry.shape_system;
            let system_id = ShapeSystem::<S>::id();
            let (shape_instance, global_instance_id) = system.instantiate();
            let symbol_id = system.sprite_system().symbol.id;
            let above = S::always_above().to_vec();
            let below = S::always_below().to_vec();
            let ordering = ShapeSystemStaticDepthOrdering {above,below};
            let shape_system_info = ShapeSystemInfo::new(system_id,ordering);
            *entry.instance_count += 1;
            (shape_system_info, symbol_id, shape_instance, global_instance_id)
        })
    }

    /// Decrement internal register of used [`Symbol`] instances previously instantiated with the
    /// [`instantiate`] method. In case there are no more instances associated with any system of
    /// type `S`, the caller of this function should perform necessary cleanup.
    pub(crate) fn drop_instance<S>(
        &mut self,
        flavor: ShapeSystemFlavor
    ) -> (bool, ShapeSystemId, PhantomData<S>)
    where
        S : Shape
    {
        let system_id = ShapeSystem::<S>::id();
        let entry_is_empty = self.get_mut::<S>(flavor).map_or(true, |entry| {
            *entry.instance_count = entry.instance_count.saturating_sub(1);
            *entry.instance_count == 0
        });

        // Intentional short-circuit - avoid computing `total_system_instances` when we know there
        // are still more instances in the currently processed entry.
        let no_more_instances = entry_is_empty && self.total_system_instances(*system_id) == 0;

        (no_more_instances, system_id, PhantomData)
    }
}}

impl ShapeSystemRegistryData {
    fn get_mut<S>(
        &mut self,
        flavor: ShapeSystemFlavor,
    ) -> Option<ShapeSystemRegistryEntryRefMut<ShapeSystem<S>>>
    where
        S: Shape,
    {
        let id = TypeId::of::<S>();
        self.shape_system_map.get_mut(&(id, flavor)).and_then(|t| {
            let shape_system = t.shape_system.downcast_mut::<ShapeSystem<S>>();
            let instance_count = &mut t.instance_count;
            shape_system.map(move |shape_system| ShapeSystemRegistryEntryRefMut {
                shape_system,
                instance_count,
            })
        })
    }

    // T: ShapeSystemInstance
    fn register<S>(
        &mut self,
        scene: &Scene,
        data: &S::ShapeData,
    ) -> ShapeSystemRegistryEntryRefMut<ShapeSystem<S>>
    where
        S: Shape,
    {
        let id = TypeId::of::<S>();
        let flavor = S::flavor(data);
        let system = ShapeSystem::<S>::new(scene, data);
        let any = Box::new(system);
        let entry = ShapeSystemRegistryEntry { shape_system: any, instance_count: 0 };
        self.shape_system_map.entry((id, flavor)).insert_entry(entry);
        self.shape_system_flavors.entry(id).or_default().push(flavor);
        // The following line is safe, as the object was just registered.
        self.get_mut(flavor).unwrap()
    }

    /// Get total number of shape instances from shape systems of given type and all flavors.
    fn total_system_instances(&self, system_id: TypeId) -> usize {
        let Some(flavors) = self.shape_system_flavors.get(&system_id) else { return 0 };
        flavors.iter().map(|f| self.shape_system_map[&(system_id, *f)].instance_count).sum()
    }

    fn with_get_or_register_mut<S, F, Out>(
        &mut self,
        scene: &Scene,
        data: &S::ShapeData,
        f: F,
    ) -> Out
    where
        F: FnOnce(ShapeSystemRegistryEntryRefMut<ShapeSystem<S>>) -> Out,
        S: Shape,
    {
        let flavor = S::flavor(data);
        match self.get_mut(flavor) {
            Some(entry) => f(entry),
            None => f(self.register(scene, data)),
        }
    }
}



// =======================
// === ShapeSystemInfo ===
// =======================

/// [`ShapeSystemInfoTemplate`] specialized for [`ShapeSystemId`].
pub type ShapeSystemInfo = ShapeSystemInfoTemplate<ShapeSystemId>;

/// [`ShapeSystemInfoTemplate`] specialized for [`SymbolId`].
pub type ShapeSystemSymbolInfo = ShapeSystemInfoTemplate<SymbolId>;

/// When adding a [`ShapeProxy`] to a [`Layer`], it will get instantiated to [`Shape`] by reusing
/// the shape system (read docs of [`ShapeSystemRegistry`] to learn more). This struct contains
/// information about the compile time depth ordering relations. See the "Compile Time Shapes
/// Ordering Relations" section in docs of [`Layer`] to learn more.
#[derive(Clone, Debug)]
pub struct ShapeSystemStaticDepthOrdering {
    above: Vec<ShapeSystemId>,
    below: Vec<ShapeSystemId>,
}

/// [`ShapeSystemStaticDepthOrdering`] associated with an id.
#[derive(Clone, Debug)]
pub struct ShapeSystemInfoTemplate<T> {
    id:       T,
    ordering: ShapeSystemStaticDepthOrdering,
}

impl<T> Deref for ShapeSystemInfoTemplate<T> {
    type Target = ShapeSystemStaticDepthOrdering;
    fn deref(&self) -> &Self::Target {
        &self.ordering
    }
}

impl<T> ShapeSystemInfoTemplate<T> {
    fn new(id: T, ordering: ShapeSystemStaticDepthOrdering) -> Self {
        Self { id, ordering }
    }
}



// ======================
// === Shape Ordering ===
// ======================

/// Shape ordering utility. Currently, this macro supports ordering of shapes for a given stage.
/// For example, the following usage:
///
/// ```text
/// shapes_order_dependencies! {
///     scene => {
///         output::port::single_port -> shape;
///         output::port::multi_port  -> shape;
///         shape                     -> input::port::hover;
///         input::port::hover        -> input::port::viz;
///     }
/// }
/// ```
///
/// Will expand to:
///
/// ```text
/// scene.layers.add_shapes_order_dependency::<output::port::single_port::Shape, shape::Shape>();
/// scene.layers.add_shapes_order_dependency::<output::port::multi_port::Shape, shape::Shape>();
/// scene.layers.add_shapes_order_dependency::<shape::Shape, input::port::hover::Shape>();
/// scene
///     .layers
///     .add_shapes_order_dependency::<input::port::hover::Shape, input::port::viz::Shape>();
/// ```
///
/// A shape listed on the left side of an arrow (`->`) will be ordered below the shape listed on
/// the right side of the arrow.
#[macro_export]
macro_rules! shapes_order_dependencies {
    ($scene:expr => {
        $( $p1:ident $(:: $ps1:ident)* -> $p2:ident $(:: $ps2:ident)*; )*
    }) => {$(
        $scene.layers.add_global_shapes_order_dependency::<$p1$(::$ps1)*::Shape, $p2$(::$ps2)*::Shape>();
    )*};
}



// ==================
// === ScissorBox ===
// ==================

/// A rectangular area used to limit rendering of a [`Layer`]. The area contains information about
/// rendering limits from each side of the image (left, right, top, and bottom).
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
pub struct ScissorBox {
    pub min_x: i32,
    pub min_y: i32,
    pub max_x: i32,
    pub max_y: i32,
}

impl ScissorBox {
    /// Constructor.
    pub fn new() -> Self {
        let min_x = 0;
        let min_y = 0;
        let max_x = i32::MAX;
        let max_y = i32::MAX;
        Self { min_x, min_y, max_x, max_y }
    }

    /// Constructor.
    pub fn new_with_position_and_size(position: Vector2<i32>, size: Vector2<i32>) -> Self {
        let min_x = position.x;
        let min_y = position.y;
        let max_x = min_x + size.x;
        let max_y = min_y + size.y;
        Self { min_x, min_y, max_x, max_y }
    }
}

impl ScissorBox {
    /// The size of the scissor box.
    pub fn size(&self) -> Vector2<i32> {
        let width = (self.max_x - self.min_x).max(0);
        let height = (self.max_y - self.min_y).max(0);
        Vector2(width, height)
    }

    /// The position of the scissor box computed from the left bottom corner.
    pub fn position(&self) -> Vector2<i32> {
        Vector2(self.min_x.max(0), self.min_y.max(0))
    }
}

impl Default for ScissorBox {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialSemigroup<ScissorBox> for ScissorBox {
    fn concat_mut(&mut self, other: Self) {
        self.min_x = Ord::max(self.min_x, other.min_x);
        self.min_y = Ord::max(self.min_y, other.min_y);
        self.max_x = Ord::min(self.max_x, other.max_x);
        self.max_y = Ord::min(self.max_y, other.max_y);
    }
}

impl PartialSemigroup<&ScissorBox> for ScissorBox {
    fn concat_mut(&mut self, other: &Self) {
        self.concat_mut(*other)
    }
}
