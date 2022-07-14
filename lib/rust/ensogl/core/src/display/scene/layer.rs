//! Scene layers implementation. See docs of [`Group`] to learn more.

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::dirty;
use crate::data::OptVec;
use crate::display;
use crate::display::camera::Camera2d;
use crate::display::scene::Scene;
use crate::display::shape::system::DynShapeSystemInstance;
use crate::display::shape::system::DynShapeSystemOf;
use crate::display::shape::system::KnownShapeSystemId;
use crate::display::shape::system::ShapeSystemId;
use crate::display::shape::ShapeSystemInstance;
use crate::display::symbol;
use crate::display::symbol::RenderGroup;
use crate::display::symbol::SymbolId;

use enso_data_structures::dependency_graph::DependencyGraph;
use enso_shapely::shared;
use smallvec::alloc::collections::BTreeSet;
use std::any::TypeId;



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
/// # DynamicShape and ShapeSystem Management
/// You are allowed to define custom [`DynamicShape`]s, which are like [`Shape`]s, but may not be
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
/// Group can be ordered by using the `set_sublayers` method.
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
/// [`DynamicShape`]s thanks to the [`LayerItem`] abstraction. Moreover, there is a special
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
/// methods, this one does not require you to own a reference to [`Scene`] or its [`Group`]. Also,
/// it is impossible to remove during runtime dependencies created this way. This might sound
/// restrictive, but actually it is what you may often want to do. For example, when creating a
/// text area, you want to define that the cursor should always be above its background and there is
/// no situation when it should not be hold. In such a way, you should use this method to define
/// depth-dependencies. In order to define such compile tie shapes ordering relations, you have to
/// define them while defining the shape system. The easiest way to do it is by using the
/// [`define_shape_system!`] macro. Refer to its documentation to learn more.
///
///
/// # Layer Lifetime Management
/// Both [`Group`] and every [`Layer`] instance are strongly interconnected. This is needed for a
/// nice API. For example, [`Layer`] allows you to add symbols while removing them from other layers
/// automatically. Although the [`SublayersModel`] registers [`WeakLayer`], the weak form is used
/// only to break cycles and never points to a dropped [`Layer`], as layers update the information
/// on a drop.
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
#[derive(Clone, CloneRef)]
pub struct Layer {
    model: Rc<LayerModel>,
}

impl Deref for Layer {
    type Target = LayerModel;
    fn deref(&self) -> &Self::Target {
        &self.model
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

impl Layer {
    /// Constructor.
    pub fn new(logger: Logger) -> Self {
        let model = LayerModel::new(logger);
        let model = Rc::new(model);
        Self { model }
    }

    /// Constructor.
    #[profile(Detail)]
    pub fn new_with_cam(logger: Logger, camera: &Camera2d) -> Self {
        let this = Self::new(logger);
        this.set_camera(camera);
        this
    }

    /// Create a new weak pointer to this layer.
    pub fn downgrade(&self) -> WeakLayer {
        let model = Rc::downgrade(&self.model);
        WeakLayer { model }
    }

    /// Add the display object to this layer without removing it from other layers.
    pub fn add(&self, object: impl display::Object) {
        object.display_object().add_to_display_layer(self);
    }

    /// Add the display object to this layer and remove it from any other layers.
    pub fn add_exclusive(&self, object: impl display::Object) {
        object.display_object().add_to_display_layer_exclusive(self);
    }

    /// Instantiate the provided [`DynamicShape`].
    pub fn instantiate<T>(&self, scene: &Scene, shape: &T) -> LayerDynamicShapeInstance
    where T: display::shape::system::DynamicShape {
        let (shape_system_info, symbol_id, global_instance_id) =
            self.shape_system_registry.instantiate(scene, shape);
        self.add_shape(shape_system_info, symbol_id);
        LayerDynamicShapeInstance::new(self, global_instance_id)
    }

    /// Iterate over all layers and sublayers of this layer hierarchically. Parent layers will be
    /// visited before their corresponding sublayers. Does not visit masks. If you want to visit
    /// masks, use [`iter_sublayers_and_masks_nested`] instead.
    pub fn iter_sublayers_nested(&self, f: impl Fn(&Layer)) {
        self.iter_sublayers_nested_internal(&f)
    }

    fn iter_sublayers_nested_internal(&self, f: &impl Fn(&Layer)) {
        f(self);
        for layer in self.sublayers() {
            layer.iter_sublayers_nested_internal(f)
        }
    }

    /// Iterate over all layers, sublayers, masks, and their sublayers of this layer hierarchically.
    /// Parent layers will be visited before their corresponding sublayers.
    pub fn iter_sublayers_and_masks_nested(&self, f: impl Fn(&Layer)) {
        self.iter_sublayers_and_masks_nested_internal(&f)
    }

    fn iter_sublayers_and_masks_nested_internal(&self, f: &impl Fn(&Layer)) {
        f(self);
        if let Some(mask) = &*self.mask.borrow() {
            if let Some(layer) = mask.upgrade() {
                layer.iter_sublayers_and_masks_nested_internal(f)
            }
        }
        for layer in self.sublayers() {
            layer.iter_sublayers_and_masks_nested_internal(f)
        }
    }
}

impl From<&Layer> for LayerId {
    fn from(t: &Layer) -> Self {
        t.id()
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
            warning!(sublayer.logger, "Attempt to add a sublayer to deallocated layer.");
        }
    }

    /// Remove previously attached sublayer. Will do nothing if the layer does not exist.
    pub fn remove_sublayer(&self, sublayer: &Layer) {
        if let Some(layer) = self.upgrade() {
            layer.remove_sublayer(sublayer)
        } else {
            warning!(sublayer.logger, "Attempt to remove a sublayer from deallocated layer.");
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
/// Please note that the [`parents`] field contains reference to a very small part of parent layer,
/// namely to its [`Sublayers`] struct. Only this part is needed to properly update all the models.
#[derive(Clone)]
#[allow(missing_docs)]
pub struct LayerModel {
    logger: Logger,
    pub camera: RefCell<Camera2d>,
    pub shape_system_registry: ShapeSystemRegistry,
    shape_system_to_symbol_info_map: RefCell<HashMap<ShapeSystemId, ShapeSystemSymbolInfo>>,
    symbol_to_shape_system_map: RefCell<HashMap<SymbolId, ShapeSystemId>>,
    elements: RefCell<BTreeSet<LayerItem>>,
    symbols_renderable: Rc<RefCell<RenderGroup>>,
    depth_order: RefCell<DependencyGraph<LayerItem>>,
    depth_order_dirty: dirty::SharedBool<OnDepthOrderDirty>,
    parents: Rc<RefCell<Vec<Sublayers>>>,
    global_element_depth_order: Rc<RefCell<DependencyGraph<LayerItem>>>,
    sublayers: Sublayers,
    mask: RefCell<Option<WeakLayer>>,
    scissor_box: RefCell<Option<ScissorBox>>,
    mem_mark: Rc<()>,
}

impl Debug for LayerModel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Layer")
            .field("id", &self.id().raw)
            .field("registry", &self.shape_system_registry)
            .field("elements", &self.elements.borrow().iter().collect_vec())
            .field("symbols_renderable", &self.symbols_renderable)
            .finish()
    }
}

impl Drop for LayerModel {
    fn drop(&mut self) {
        let id = self.id();
        for parent in &mut *self.parents.borrow_mut() {
            let mut model = parent.borrow_mut();
            model.remove(id);
        }
    }
}

impl LayerModel {
    fn new(logger: Logger) -> Self {
        let logger_dirty = Logger::new_sub(&logger, "dirty");
        let camera = RefCell::new(Camera2d::new(&logger));
        let shape_system_registry = default();
        let shape_system_to_symbol_info_map = default();
        let symbol_to_shape_system_map = default();
        let elements = default();
        let symbols_renderable = default();
        let depth_order = default();
        let parents = default();
        let on_mut = on_depth_order_dirty(&parents);
        let depth_order_dirty = dirty::SharedBool::new(logger_dirty, on_mut);
        let global_element_depth_order = default();
        let sublayers = Sublayers::new(Logger::new_sub(&logger, "registry"), &parents);
        let mask = default();
        let scissor_box = default();
        let mem_mark = default();
        Self {
            logger,
            camera,
            shape_system_registry,
            shape_system_to_symbol_info_map,
            symbol_to_shape_system_map,
            elements,
            symbols_renderable,
            depth_order,
            depth_order_dirty,
            parents,
            global_element_depth_order,
            sublayers,
            mask,
            scissor_box,
            mem_mark,
        }
    }

    /// Unique identifier of this layer. It is memory-based, it will be unique even for layers in
    /// different instances of [`Scene`].
    pub fn id(&self) -> LayerId {
        LayerId::new(Rc::as_ptr(&self.mem_mark) as usize)
    }

    /// Vector of all symbols registered in this layer, ordered according to the defined depth-order
    /// dependencies. Please note that this function does not update the depth-ordering of the
    /// elements. Updates are performed by calling the `update` method on [`Group`], which usually
    /// happens once per animation frame.
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
    /// definition means a [`Shape`], a [`DynamicShape`], or user-defined shape system.
    ///
    /// # Future Improvements
    /// This implementation can be simplified to `S1:KnownShapeSystemId` (not using [`Content`] at
    /// all), after the compiler gets updated to newer version.
    pub fn add_shapes_order_dependency<S1, S2>(&self) -> (PhantomData<S1>, PhantomData<S2>)
    where
        S1: HasContent,
        S2: HasContent,
        Content<S1>: KnownShapeSystemId,
        Content<S2>: KnownShapeSystemId, {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
        self.add_elements_order_dependency(s1_id, s2_id);
        self.depth_order_dirty.set();
        default()
    }

    /// Remove depth-order dependency between two shape-like definitions, where a "shape-like"
    /// definition means a [`Shape`], a [`DynamicShape`], or user-defined shape system. Returns
    /// `true` if the dependency was found, and `false` otherwise.
    ///
    /// # Future Improvements
    /// This implementation can be simplified to `S1:KnownShapeSystemId` (not using [`Content`] at
    /// all), after the compiler gets updated to newer version.
    pub fn remove_shapes_order_dependency<S1, S2>(
        &self,
    ) -> (bool, PhantomData<S1>, PhantomData<S2>)
    where
        S1: HasContent,
        S2: HasContent,
        Content<S1>: KnownShapeSystemId,
        Content<S2>: KnownShapeSystemId, {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
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

    /// Camera setter of this layer.
    pub fn set_camera(&self, camera: impl Into<Camera2d>) {
        let camera = camera.into();
        *self.camera.borrow_mut() = camera;
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
            for layer in self.sublayers() {
                layer.update_internal(Some(&*self.global_element_depth_order.borrow()));
            }
            if let Some(layer) = &*self.mask.borrow() {
                if let Some(layer) = layer.upgrade() {
                    layer.update_internal(Some(&*self.global_element_depth_order.borrow()));
                }
            }
        }

        was_dirty
    }

    /// Compute a combined [`DependencyGraph`] for the layer taking into consideration the global
    /// dependency graph (from [`Group`]), the local one (per layer), and individual shape
    /// preferences (see the "Compile Time Shapes Ordering Relations" section in docs of [`Group`]
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
                        warning!(
                            self.logger,
                            "Trying to perform depth-order of non-existing element '{id:?}'."
                        )
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
    /// by calling the `update` method on [`Group`], which usually happens once per animation
    /// frame.
    pub fn sublayers(&self) -> Vec<Layer> {
        self.sublayers.borrow().all()
    }

    /// Attach a `layer` as a sublayer.
    pub fn add_sublayer(&self, layer: &Layer) {
        let ix = self.sublayers.borrow_mut().layers.insert(layer.downgrade());
        self.sublayers.borrow_mut().layer_placement.insert(layer.id(), ix);
        layer.add_parent(&self.sublayers);
    }

    /// Remove previously attached sublayer.
    ///
    /// The implementation is the opposite of [`LayerModel::add_sublayer`]: we modify both fields of
    /// [`SublayersModel`] and also unset parent.
    pub fn remove_sublayer(&self, layer: &Layer) {
        self.sublayers.borrow_mut().remove(layer.id());
        layer.remove_parent(&self.sublayers);
    }

    fn remove_all_sublayers(&self) {
        for layer in self.sublayers.borrow().layers.iter() {
            if let Some(layer) = layer.upgrade() {
                layer.remove_parent(&self.sublayers)
            }
        }
        mem::take(&mut *self.sublayers.model.borrow_mut());
    }

    fn add_parent(&self, parent: &Sublayers) {
        let parent = parent.clone_ref();
        self.parents.borrow_mut().push(parent);
    }

    fn remove_parent(&self, parent: &Sublayers) {
        self.parents.borrow_mut().remove_item(parent);
    }

    fn remove_mask(&self) {
        if let Some(mask) = &*self.mask.borrow() {
            if let Some(mask) = mask.upgrade() {
                mask.remove_parent(&self.sublayers)
            }
        }
        mem::take(&mut *self.mask.borrow_mut());
    }

    /// Set all sublayers layer of this layer. Old sublayers layers will be unregistered.
    pub fn set_sublayers(&self, layers: &[&Layer]) {
        self.remove_all_sublayers();
        for layer in layers {
            self.add_sublayer(layer)
        }
    }

    /// Create a new sublayer to this layer, with the same camera.
    pub fn create_sublayer(&self) -> Layer {
        let logger = self.logger.sub("Sublayer");
        let layer = Layer::new_with_cam(logger, &self.camera.borrow());
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
        mask.add_parent(&self.sublayers);
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
        S1: HasContent,
        S2: HasContent,
        Content<S1>: KnownShapeSystemId,
        Content<S2>: KnownShapeSystemId, {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
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
        S1: HasContent,
        S2: HasContent,
        Content<S1>: KnownShapeSystemId,
        Content<S2>: KnownShapeSystemId, {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
        let found = self.remove_global_elements_order_dependency(s1_id, s2_id);
        (found, default(), default())
    }
}

/// The callback setting `element_depth_order_dirty` in parents.
pub type OnDepthOrderDirty = impl Fn();
fn on_depth_order_dirty(parents: &Rc<RefCell<Vec<Sublayers>>>) -> OnDepthOrderDirty {
    let parents = parents.clone();
    move || {
        for parent in &*parents.borrow() {
            // It's safe to do it having parents borrowed, because the only possible callback called
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



// =================================
// === LayerDynamicShapeInstance ===
// =================================

/// Information about an instance of a dynamic shape bound to a particular layer.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct LayerDynamicShapeInstance {
    pub layer:              WeakLayer,
    pub global_instance_id: symbol::GlobalInstanceId,
}

impl LayerDynamicShapeInstance {
    /// Constructor.
    pub fn new(layer: &Layer, global_instance_id: symbol::GlobalInstanceId) -> Self {
        let layer = layer.downgrade();
        Self { layer, global_instance_id }
    }
}



// =================
// === Sublayers ===
// =================

/// The callback propagating `element_depth_order_dirty` flag to parents.
pub type OnElementDepthOrderDirty = impl Fn();
fn on_element_depth_order_dirty(parents: &Rc<RefCell<Vec<Sublayers>>>) -> OnElementDepthOrderDirty {
    let parents = parents.clone_ref();
    move || {
        for sublayers in parents.borrow().iter() {
            // It's safe to do it having parents borrowed, because the only possible callback called
            // [`OnElementDepthOrderDirty`], which don't borrow_mut at any point.
            sublayers.element_depth_order_dirty.set()
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
    pub fn new(logger: impl AnyLogger, parents: &Rc<RefCell<Vec<Sublayers>>>) -> Self {
        let element_dirty_logger = Logger::new_sub(&logger, "dirty");
        let model = default();
        let dirty_on_mut = on_element_depth_order_dirty(parents);
        let element_depth_order_dirty = dirty::SharedBool::new(element_dirty_logger, dirty_on_mut);
        Self { model, element_depth_order_dirty }
    }
}



// ======================
// === SublayersModel ===
// ======================

/// Internal representation of [`Group`].
#[derive(Debug, Default)]
pub struct SublayersModel {
    layers:          OptVec<WeakLayer>,
    layer_placement: HashMap<LayerId, usize>,
}

impl SublayersModel {
    /// Vector of all layers, ordered according to the defined depth-order dependencies. Please note
    /// that this function does not update the depth-ordering of the layers. Updates are performed
    /// by calling the `update` method on [`Group`], which usually happens once per animation
    /// frame.
    pub fn all(&self) -> Vec<Layer> {
        self.layers.iter().filter_map(|t| t.upgrade()).collect()
    }

    fn layer_ix(&self, layer_id: LayerId) -> Option<usize> {
        self.layer_placement.get(&layer_id).copied()
    }

    fn remove(&mut self, layer_id: LayerId) {
        if let Some(ix) = self.layer_ix(layer_id) {
            self.layers.remove(ix);
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
    /// Constructor. The passed [`camera`] is used to render created layers.
    pub fn new(logger: &Logger, camera: &Camera2d) -> Self {
        let masked_layer = Layer::new_with_cam(logger.sub("MaskedLayer"), camera);
        let mask_layer = Layer::new_with_cam(logger.sub("MaskLayer"), camera);
        masked_layer.set_mask(&mask_layer);
        Self { masked_layer, mask_layer }
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

/// Abstraction over [`SymbolId`] and [`ShapeSystemId`]. Read docs of [`Group`] to learn about its
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
/// call. After adding a [`DynamicShape`] to a layer, it will get instantiated (its shape will be
/// created), and because of this structure, it will share the same shape system as other shapes of
/// the same type on the same layer. Read the docs of [`DynamicShape`] to learn more.
#[derive(Default,Debug)]
pub struct ShapeSystemRegistryData {
    shape_system_map : HashMap<TypeId,ShapeSystemRegistryEntry>,
}

impl {
    // TODO: This API requires Scene to be passed as argument, which is ugly. Consider splitting
    //       the Scene into few components.
    /// Query the registry for a user defined shape system of a given type. In case the shape system
    /// was not yet used, it will be created.
    pub fn shape_system<T>(&mut self, scene:&Scene, _phantom:PhantomData<T>) -> DynShapeSystemOf<T>
    where T : display::shape::system::DynamicShape {
        self.with_get_or_register_mut::<DynShapeSystemOf<T>,_,_>
            (scene,|entry| {entry.shape_system.clone_ref()})
    }

    /// Instantiate the provided [`DynamicShape`].
    pub fn instantiate<T>
    (&mut self, scene:&Scene, shape:&T) -> (ShapeSystemInfo, SymbolId, symbol::GlobalInstanceId)
    where T : display::shape::system::DynamicShape {
        self.with_get_or_register_mut::<DynShapeSystemOf<T>,_,_>(scene,|entry| {
            let system = entry.shape_system;
            let system_id = DynShapeSystemOf::<T>::id();
            let global_instance_id = system.instantiate(shape);
            let symbol_id = system.shape_system().sprite_system.symbol.id;
            let above = DynShapeSystemOf::<T>::above();
            let below = DynShapeSystemOf::<T>::below();
            let ordering = ShapeSystemStaticDepthOrdering {above,below};
            let shape_system_info = ShapeSystemInfo::new(system_id,ordering);
            *entry.instance_count += 1;
            (shape_system_info, symbol_id, global_instance_id)
        })
    }

    /// Decrement internal register of used [`Symbol`] instances previously instantiated with the
    /// [`instantiate`] method. In case the counter drops to 0, the caller of this function should
    /// perform necessary cleanup.
    pub(crate) fn drop_instance<T>(&mut self) -> (usize,ShapeSystemId,PhantomData<T>)
    where T : display::shape::system::DynamicShape {
        let system_id      = DynShapeSystemOf::<T>::id();
        let instance_count = if let Some(entry) = self.get_mut::<DynShapeSystemOf<T>>() {
            *entry.instance_count -= 1;
            *entry.instance_count
        } else { 0 };
        (instance_count,system_id,PhantomData)
    }
}}

impl ShapeSystemRegistryData {
    fn get_mut<T>(&mut self) -> Option<ShapeSystemRegistryEntryRefMut<T>>
    where T: ShapeSystemInstance {
        let id = TypeId::of::<T>();
        self.shape_system_map.get_mut(&id).and_then(|t| {
            let shape_system = t.shape_system.downcast_mut::<T>();
            let instance_count = &mut t.instance_count;
            shape_system.map(move |shape_system| ShapeSystemRegistryEntryRefMut {
                shape_system,
                instance_count,
            })
        })
    }

    fn register<T>(&mut self, scene: &Scene) -> ShapeSystemRegistryEntryRefMut<T>
    where T: ShapeSystemInstance {
        let id = TypeId::of::<T>();
        let system = <T as ShapeSystemInstance>::new(scene);
        let any = Box::new(system);
        let entry = ShapeSystemRegistryEntry { shape_system: any, instance_count: 0 };
        self.shape_system_map.entry(id).insert_entry(entry);
        // The following line is safe, as the object was just registered.
        self.get_mut().unwrap()
    }

    fn with_get_or_register_mut<T, F, Out>(&mut self, scene: &Scene, f: F) -> Out
    where
        F: FnOnce(ShapeSystemRegistryEntryRefMut<T>) -> Out,
        T: ShapeSystemInstance, {
        match self.get_mut() {
            Some(entry) => f(entry),
            None => f(self.register(scene)),
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

/// When adding a [`DynamicShape`] to a [`Layer`], it will get instantiated to [`Shape`] by reusing
/// the shape system (read docs of [`ShapeSystemRegistry`] to learn more). This struct contains
/// information about the compile time depth ordering relations. See the "Compile Time Shapes
/// Ordering Relations" section in docs of [`Group`] to learn more.
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
/// ```ignore
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
/// ```ignore
/// scene.layers.add_shapes_order_dependency::<output::port::single_port::View, shape::View>();
/// scene.layers.add_shapes_order_dependency::<output::port::multi_port::View, shape::View>();
/// scene.layers.add_shapes_order_dependency::<shape::View, input::port::hover::View>();
/// scene.layers.add_shapes_order_dependency::<input::port::hover::View, input::port::viz::View>();
/// ```
///
/// A shape listed on the left side of an arrow (`->`) will be ordered below the shape listed on
/// the right side of the arrow.
#[macro_export]
macro_rules! shapes_order_dependencies {
    ($scene:expr => {
        $( $p1:ident $(:: $ps1:ident)* -> $p2:ident $(:: $ps2:ident)*; )*
    }) => {$(
        $scene.layers.add_global_shapes_order_dependency::<$p1$(::$ps1)*::View, $p2$(::$ps2)*::View>();
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
