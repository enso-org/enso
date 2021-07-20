//! Scene layers implementation. See docs of [`Layers`] to learn more.

use crate::data::dirty::traits::*;
use crate::prelude::*;

use crate::data::OptVec;
use crate::data::dirty;
use crate::display::camera::Camera2d;
use crate::display::scene::Scene;
use crate::display::shape::ShapeSystemInstance;
use crate::display::shape::system::DynShapeSystemInstance;
use crate::display::shape::system::DynShapeSystemOf;
use crate::display::shape::system::KnownShapeSystemId;
use crate::display::shape::system::ShapeSystemId;
use crate::display::symbol::SymbolId;
use crate::display;
use crate::system::gpu::data::attribute;

use enso_data::dependency_graph::DependencyGraph;
use enso_shapely::shared;
use smallvec::alloc::collections::BTreeSet;
use std::any::TypeId;



// =====================
// === ShapeRegistry ===
// =====================

shared! { ShapeSystemRegistry
/// A per [`Scene`] [`Layer`] user defined shape system registry. It is used as a cache for existing
/// shape system instances. When creating a shape instance, we often want it to share the same shape
/// system than other instances in order for all of them to be drawn with just a single WebGL draw
/// call. After adding a [`DynamicShape`] to a layer, it will get instantiated (its shape will be
/// created), and because of this structure, it will share the same shape system as other shapes of
/// the same type on the same layer. Read the docs of [`DynamicShape`] to learn more.
#[derive(Debug,Default)]
pub struct ShapeSystemRegistryData {
    shape_system_map : HashMap<TypeId,Box<dyn Any>>,
}

impl {
    fn get<T>(&self) -> Option<T>
    where T : ShapeSystemInstance {
        let id = TypeId::of::<T>();
        self.shape_system_map.get(&id).and_then(|t| t.downcast_ref::<T>()).map(|t| t.clone_ref())
    }

    fn register<T>(&mut self, scene:&Scene) -> T
    where T : ShapeSystemInstance {
        let id     = TypeId::of::<T>();
        let system = <T as ShapeSystemInstance>::new(scene);
        let any    = Box::new(system.clone_ref());
        self.shape_system_map.insert(id,any);
        system
    }

    fn get_or_register<T>(&mut self, scene:&Scene) -> T
    where T : ShapeSystemInstance {
        self.get().unwrap_or_else(|| self.register(scene))
    }

    // TODO: This API requires Scene to be passed as argument, which is ugly. Consider splitting
    //       the Scene into few components.
    /// Query the registry for a user defined shape system of a given type. In case the shape system
    /// was not yet used, it will be created.
    pub fn shape_system<T>(&mut self, scene:&Scene, _phantom:PhantomData<T>) -> DynShapeSystemOf<T>
    where T : display::shape::system::DynamicShape {
        self.get_or_register::<DynShapeSystemOf<T>>(scene)
    }

    /// Instantiate the provided [`DynamicShape`].
    pub fn instantiate<T>
    (&mut self, scene:&Scene, shape:&T) -> (ShapeSystemInfo,SymbolId,attribute::InstanceIndex)
    where T : display::shape::system::DynamicShape {
        let system            = self.get_or_register::<DynShapeSystemOf<T>>(scene);
        let system_id         = DynShapeSystemOf::<T>::id();
        let instance_id       = system.instantiate(shape);
        let symbol_id         = system.shape_system().sprite_system.symbol.id;
        let above             = DynShapeSystemOf::<T>::above();
        let below             = DynShapeSystemOf::<T>::below();
        let ordering          = ShapeSystemStaticDepthOrdering {above,below};
        let shape_system_info = ShapeSystemInfo::new(system_id,ordering);
        (shape_system_info,symbol_id,instance_id)
    }
}}



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
/// Ordering Relations" section in docs of [`Layers`] to learn more.
#[derive(Clone,Debug)]
pub struct ShapeSystemStaticDepthOrdering {
    above : Vec<ShapeSystemId>,
    below : Vec<ShapeSystemId>,
}

/// [`ShapeSystemStaticDepthOrdering`] associated with an id.
#[derive(Clone,Debug)]
pub struct ShapeSystemInfoTemplate<T> {
    id       : T,
    ordering : ShapeSystemStaticDepthOrdering,
}

impl<T> Deref for ShapeSystemInfoTemplate<T> {
    type Target = ShapeSystemStaticDepthOrdering;
    fn deref(&self) -> &Self::Target {
        &self.ordering
    }
}

impl<T> ShapeSystemInfoTemplate<T> {
    fn new(id:T, ordering:ShapeSystemStaticDepthOrdering) -> Self {
        Self {id,ordering}
    }
}



// ====================
// === LayerElement ===
// ====================

/// Abstraction over [`SymbolId`] and [`ShapeSystemId`]. Read docs of [`Layers`] to learn about its
/// usage scenarios.
#[derive(Clone,Copy,Debug,PartialEq,PartialOrd,Eq,Hash,Ord)]
#[allow(missing_docs)]
pub enum LayerElement {
    Symbol      (SymbolId),
    ShapeSystem (ShapeSystemId)
}

impl From<ShapeSystemId> for LayerElement {
    fn from(t:ShapeSystemId) -> Self {
        Self::ShapeSystem(t)
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



// =============
// === Layer ===
// =============

/// A single scene layer. See documentation of [`Layers`] to learn more.
#[derive(Debug,Clone,CloneRef)]
pub struct Layer {
    model : Rc<LayerModel>
}

impl Deref for Layer {
    type Target = LayerModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl Layer {
    fn new
    (logger:&Logger, id:LayerId, reg:&Rc<RefCell<LayersRegistry>>, on_mut:Box<dyn Fn()>) -> Self {
        let model = LayerModel::new(logger,id,reg,on_mut);
        let model = Rc::new(model);
        Self {model}
    }

    fn downgrade(&self) -> WeakLayer {
        let model = Rc::downgrade(&self.model);
        WeakLayer {model}
    }
}

impl From<&Layer> for LayerId {
    fn from(t:&Layer) -> Self {
        t.id
    }
}



// =================
// === WeakLayer ===
// =================

/// A weak version of [`Layer`].
#[derive(Clone,CloneRef)]
struct WeakLayer {
    model : Weak<LayerModel>
}

impl WeakLayer {
    pub fn upgrade(&self) -> Option<Layer> {
        self.model.upgrade().map(|model| Layer {model})
    }
}

impl Debug for WeakLayer {
    fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"WeakLayer")
    }
}



// ==================
// === LayerModel ===
// ==================

/// Internal representation of [`Layer`].
#[derive(Debug,Clone)]
#[allow(missing_docs)]
pub struct LayerModel {
    pub id                          : LayerId,
    logger                          : Logger,
    pub camera                      : RefCell<Camera2d>,
    pub shape_system_registry       : ShapeSystemRegistry,
    shape_system_to_symbol_info_map : RefCell<HashMap<ShapeSystemId,ShapeSystemSymbolInfo>>,
    symbol_to_shape_system_map      : RefCell<HashMap<SymbolId,ShapeSystemId>>,
    elements                        : RefCell<BTreeSet<LayerElement>>,
    symbols_ordered                 : RefCell<Vec<SymbolId>>,
    depth_order                     : RefCell<DependencyGraph<LayerElement>>,
    depth_order_dirty               : dirty::SharedBool<Box<dyn Fn()>>,
    all_layers_registry             : Rc<RefCell<LayersRegistry>>,
}

impl Drop for LayerModel {
    fn drop(&mut self) {
        let mut model = self.all_layers_registry.borrow_mut();
        model.registry.remove(*self.id);
        model.sorted_layers.remove_item(&self.id);
        for element in &*self.elements.borrow() {
            if let Some(symbol_id) = self.symbol_id_of_element(*element) {
                if let Some(vec) = model.symbols_placement.get_mut(&symbol_id) {
                    vec.remove_item(&self.id);
                }
            }
        }
    }
}

impl LayerModel {
    fn new
    ( logger              : impl AnyLogger
    , id                  : LayerId
    , all_layers_registry : &Rc<RefCell<LayersRegistry>>
    , on_mut              : Box<dyn Fn()>
    ) -> Self {
        let logger                          = Logger::sub(logger,"layer");
        let logger_dirty                    = Logger::sub(&logger,"dirty");
        let camera                          = RefCell::new(Camera2d::new(&logger));
        let shape_system_registry           = default();
        let shape_system_to_symbol_info_map = default();
        let symbol_to_shape_system_map      = default();
        let elements                        = default();
        let symbols_ordered                 = default();
        let depth_order                     = default();
        let depth_order_dirty               = dirty::SharedBool::new(logger_dirty,on_mut);
        let all_layers_registry                = all_layers_registry.clone();
        Self {id,logger,camera,shape_system_registry,shape_system_to_symbol_info_map
             ,symbol_to_shape_system_map,elements,symbols_ordered,depth_order,depth_order_dirty
             ,all_layers_registry}
    }

    /// Vector of all symbols registered in this layer, ordered according to the defined depth-order
    /// dependencies. Please note that this function does not update the depth-ordering of the
    /// elements. Updates are performed by calling the `update` method on [`Layers`], which usually
    /// happens once per animation frame.
    pub fn symbols(&self) -> Vec<SymbolId> {
        self.symbols_ordered.borrow().clone()
    }

    /// Return the [`SymbolId`] of the provided [`LayerElement`] if it was added to the current
    /// layer.
    pub fn symbol_id_of_element(&self, element:LayerElement) -> Option<SymbolId> {
        use LayerElement::*;
        match element {
            Symbol(id)      => Some(id),
            ShapeSystem(id) => self.shape_system_to_symbol_info_map.borrow().get(&id).map(|t|t.id)
        }
    }

    /// Add depth-order dependency between two [`LayerElement`]s in this layer.
    pub fn add_elements_order_dependency
    (&self, below:impl Into<LayerElement>, above:impl Into<LayerElement>) {
        let below = below.into();
        let above = above.into();
        if self.depth_order.borrow_mut().insert_dependency(below,above) {
            self.depth_order_dirty.set();
        }
    }

    /// Remove a depth-order dependency between two [`LayerElement`]s in this layer. Returns `true`
    /// if the dependency was found, and `false` otherwise.
    pub fn remove_elements_order_dependency
    (&self, below:impl Into<LayerElement>, above:impl Into<LayerElement>) -> bool {
        let below = below.into();
        let above = above.into();
        let found = self.depth_order.borrow_mut().remove_dependency(below,above);
        if found { self.depth_order_dirty.set(); }
        found
    }

    /// Add depth-order dependency between two shape-like definitions, where a "shape-like"
    /// definition means a [`Shape`], a [`DynamicShape`], or user-defined shape system.
    ///
    /// # Future Improvements
    /// This implementation can be simplified to `S1:KnownShapeSystemId` (not using [`Content`] at
    /// all), after the compiler gets updated to newer version.
    pub fn add_shapes_order_dependency<S1,S2>(&self) -> (PhantomData<S1>,PhantomData<S2>)
    where S1          : HasContent,
          S2          : HasContent,
          Content<S1> : KnownShapeSystemId,
          Content<S2> : KnownShapeSystemId {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
        self.add_elements_order_dependency(s1_id,s2_id);
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
    pub fn remove_shapes_order_dependency<S1,S2>
    (&self) -> (bool,PhantomData<S1>,PhantomData<S2>)
    where S1          : HasContent,
          S2          : HasContent,
          Content<S1> : KnownShapeSystemId,
          Content<S2> : KnownShapeSystemId {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
        let found = self.remove_elements_order_dependency(s1_id,s2_id);
        if found { self.depth_order_dirty.set(); }
        (found,default(),default())
    }

    /// Camera getter of this layer.
    pub fn camera(&self) -> Camera2d {
        self.camera.borrow().clone_ref()
    }

    /// Camera setter of this layer.
    pub fn set_camera(&self, camera:impl Into<Camera2d>) {
        let camera = camera.into();
        *self.camera.borrow_mut() = camera;
    }

    /// Add the display object to this layer and remove it from any other layers.
    pub fn add_exclusive(&self, object:impl display::Object) {
        object.display_object().add_to_scene_layer_exclusive(self.id);
    }

    /// Add the symbol to this layer.
    pub fn add_symbol(&self, symbol_id:impl Into<SymbolId>) {
        self.add_element(symbol_id.into(),None)
    }

    /// Add the symbol to this layer and remove it from other layers.
    pub fn add_symbol_exclusive(&self, symbol_id:impl Into<SymbolId>) {
        self.add_element_exclusive(symbol_id.into(),None)
    }

    /// Add the shape to this layer.
    pub(crate) fn add_shape
    (&self, shape_system_info:ShapeSystemInfo, symbol_id:impl Into<SymbolId>) {
        self.add_element(symbol_id.into(),Some(shape_system_info))
    }

    /// Add the shape to this layer and remove it from other layers.
    pub(crate) fn add_shape_exclusive
    (&self, shape_system_info:ShapeSystemInfo, symbol_id:impl Into<SymbolId>) {
        self.add_element_exclusive(symbol_id.into(),Some(shape_system_info))
    }

    /// Internal helper for adding elements to this layer and removing them from other layers.
    fn add_element_exclusive(&self, symbol_id:SymbolId, shape_system_info:Option<ShapeSystemInfo>) {
        self.remove_symbol_from_all_layers(symbol_id);
        self.add_element(symbol_id,shape_system_info);
    }

    /// Internal helper for adding elements to this layer.
    fn add_element(&self, symbol_id:SymbolId, shape_system_info:Option<ShapeSystemInfo>) {
        self.depth_order_dirty.set();
        match shape_system_info {
            None       => { self.elements.borrow_mut().insert(LayerElement::Symbol(symbol_id)); }
            Some(info) => {
                let symbol_info = ShapeSystemSymbolInfo::new(symbol_id,info.ordering);
                self.shape_system_to_symbol_info_map.borrow_mut().insert(info.id,symbol_info);
                self.symbol_to_shape_system_map.borrow_mut().insert(symbol_id,info.id);
                self.elements.borrow_mut().insert(LayerElement::ShapeSystem(info.id));
            }
        }
        self.all_layers_registry.borrow_mut().symbols_placement.entry(symbol_id).or_default().push(self.id);
    }

    /// Remove the symbol from the current layer.
    pub fn remove_symbol(&self, symbol_id:impl Into<SymbolId>) {
        self.depth_order_dirty.set();
        let symbol_id = symbol_id.into();

        self.elements.borrow_mut().remove(&LayerElement::Symbol(symbol_id));
        if let Some(shape_system_id) = self.symbol_to_shape_system_map.borrow_mut().remove(&symbol_id) {
            self.shape_system_to_symbol_info_map.borrow_mut().remove(&shape_system_id);
            self.elements.borrow_mut().remove(&LayerElement::ShapeSystem(shape_system_id));
        }

        if let Some(placement) = self.all_layers_registry.borrow_mut().symbols_placement.get_mut(&symbol_id) {
            placement.remove_item(&self.id);
        }
    }

    /// Remove the symbol from all layers it was attached to.
    fn remove_symbol_from_all_layers(&self, symbol_id:SymbolId) {
        let placement = self.all_layers_registry.borrow().symbols_placement.get(&symbol_id).cloned();
        if let Some(placement) = placement {
            for layer_id in placement {
                let opt_layer = self.all_layers_registry.borrow().registry[*layer_id].upgrade();
                if let Some(layer) = opt_layer {
                    layer.remove_symbol(symbol_id)
                }
            }
        }
    }

    /// Consume all dirty flags and update the ordering of elements if needed.
    pub(crate) fn update(&self, global_element_depth_order:&DependencyGraph<LayerElement>) {
        if self.depth_order_dirty.check() {
            self.depth_order_dirty.unset();
            self.depth_sort(global_element_depth_order);
        }
    }

    /// Compute a combined [`DependencyGraph`] for the layer taking int consideration the global
    /// dependency graph (from [`Layers`]), the local one (per layer), and individual shape
    /// preferences (see the "Compile Time Shapes Ordering Relations" section in docs of [`Layers`]
    /// to learn more).
    fn combined_depth_order_graph(&self, global_element_depth_order:&DependencyGraph<LayerElement>)
    -> DependencyGraph<LayerElement> {
        let mut graph = global_element_depth_order.clone();
        graph.extend(self.depth_order.borrow().clone().into_iter());
        for element in &*self.elements.borrow() {
            if let LayerElement::ShapeSystem(id) = element {
                if let Some(info) = self.shape_system_to_symbol_info_map.borrow().get(id) {
                    for &id2 in &info.below { graph.insert_dependency(*element,id2.into()); }
                    for &id2 in &info.above { graph.insert_dependency(id2.into(),*element); }
                }
            }
        };
        graph
    }

    fn depth_sort(&self, global_element_depth_order:&DependencyGraph<LayerElement>) {
        let graph           = self.combined_depth_order_graph(global_element_depth_order);
        let elements_sorted = self.elements.borrow().iter().copied().collect_vec();
        let sorted_elements = graph.into_unchecked_topo_sort(elements_sorted);
        let sorted_symbols  = sorted_elements.into_iter().filter_map(|element| {
            match element {
                LayerElement::Symbol(symbol_id) => Some(symbol_id),
                LayerElement::ShapeSystem(id) => {
                    let out = self.shape_system_to_symbol_info_map.borrow().get(&id).map(|t|t.id);
                    if out.is_none() {
                        warning!(self.logger,
                            "Trying to perform depth-order of non-existing element '{id:?}'."
                        )
                    }
                    out
                }
            }
        }).collect();
        *self.symbols_ordered.borrow_mut() = sorted_symbols;
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



// ==============
// === Layers ===
// ==============

/// [`Scene`] layers implementation. Scene can consist of one or more layers. Each layer is assigned
/// with a camera and set of [`Symbol`]s to be displayed. Layers can share cameras and symbols.
///
/// For example, you can create a layer which displays the same symbols as another layer, but from a
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
/// # Layer Ordering
/// Layers can be ordered by using the `add_layers_order_dependency`, and the
/// `remove_layers_order_dependency` methods, respectively. The API allows defining a depth-order
/// dependency graph which will be resolved during a frame update. All symbols from lower layers
/// will be drawn to the screen before symbols from the upper layers.
///
///
/// # Symbols Ordering
/// There are two ways to define symbol ordering in scene layers, a global, and local (per-layer)
/// one. In order to define a global depth-order dependency, you can use the
/// `add_elements_order_dependency`, and the `remove_elements_order_dependency` methods respectively.
/// In order to define local (per-layer) depth-order dependency, you can use methods of the same
/// names in every layer instance. After changing a dependency graph, the layer management marks
/// appropriate dirty flags and re-orders symbols on each new frame processed.
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
/// [`DynamicShape`]s thanks to the [`LayerElement`] abstraction. Moreover, there is a special
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
/// methods, this one does not require you to own a reference to [`Scene`] or its [`Layers`]. Also,
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
/// Both [`Layers`] and every [`Layer`] instance are strongly interconnected. This is needed for a
/// nice API. For example, [`Layer`] allows you to add symbols while removing them from other layers
/// automatically. Although the [`LayersRegistry`] registers [`WeakLayer`], the weak form is used only
/// to break cycles and never points to a dropped [`Layer`], as layers update the information on
/// a drop.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Layers {
    pub main : Layer,
    model    : LayersModel,
}

impl Deref for Layers {
    type Target = LayersModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl Layers {
    /// Constructor.
    pub fn new(logger:impl AnyLogger) -> Self {
        let model = LayersModel::new(logger);
        let main  = model.new_layer();
        Self {main,model}
    }
}



// ===================
// === LayersModel ===
// ===================

/// Internal representation of [`Layers`].
#[derive(Clone,CloneRef,Debug)]
pub struct LayersModel {
    logger                     : Logger,
    global_element_depth_order : Rc<RefCell<DependencyGraph<LayerElement>>>,
    model                      : Rc<RefCell<LayersRegistry>>,
    element_depth_order_dirty  : dirty::SharedBool,
    layers_depth_order_dirty   : dirty::SharedBool,
}

impl LayersModel {
    /// Constructor.
    pub fn new(logger:impl AnyLogger) -> Self {
        let logger                     = Logger::sub(logger,"views");
        let element_dirty_logger       = Logger::sub(&logger,"element_dirty");
        let layers_dirty_logger        = Logger::sub(&logger,"layers_dirty");
        let global_element_depth_order = default();
        let model                      = default();
        let element_depth_order_dirty  = dirty::SharedBool::new(element_dirty_logger,());
        let layers_depth_order_dirty   = dirty::SharedBool::new(layers_dirty_logger,());
        Self {logger,global_element_depth_order,model,element_depth_order_dirty
             ,layers_depth_order_dirty}
    }

    /// Query [`Layer`] by [`LayerId`].
    pub fn get(&self, layer_id:LayerId) -> Option<Layer> {
        self.model.borrow().get(layer_id)
    }

    /// Vector of all layers, ordered according to the defined depth-order dependencies. Please note
    /// that this function does not update the depth-ordering of the layers. Updates are performed
    /// by calling the `update` method on [`Layers`], which usually happens once per animation
    /// frame.
    pub fn all(&self) -> Vec<Layer> {
        self.model.borrow().all()
    }

    /// Create a new [`Layer`].
    pub fn new_layer(&self) -> Layer {
        let (_,layer) = self.model.borrow_mut().registry.insert_with_ix(|ix| {
            let id     = LayerId::from(ix);
            let dirty  = &self.element_depth_order_dirty;
            let on_mut = Box::new(f!(dirty.set()));
            let layer  = Layer::new(&self.logger,id,&self.model,on_mut);
            (layer.downgrade(),layer)
        });
        self.layers_depth_order_dirty.set();
        layer
    }

    /// Update the layers. This checks all dirty flags, sorts the layers and sort symbols in all
    /// layers affected by previous changes. This function is usually called once per animation
    /// frame.
    pub(crate) fn update(&self) {
        if self.layers_depth_order_dirty.check() {
            self.layers_depth_order_dirty.unset();
            let model         = &mut *self.model.borrow_mut();
            let layers_sorted = model.registry.iter().filter_map(|t|t.upgrade().map(|t|t.id));
            let layers_sorted = layers_sorted.collect_vec();
            let sorted_layers = model.layer_depth_order.unchecked_topo_sort(layers_sorted);
            model.sorted_layers = sorted_layers;
        }

        if self.element_depth_order_dirty.check() {
            self.element_depth_order_dirty.unset();
            for layer in self.all() {
                layer.update(&*self.global_element_depth_order.borrow())
            }
        }
    }

    /// Add depth-order dependency between two [`Layer`]s. Returns `true` if the dependency was
    /// inserted successfully (was not already present), and `false` otherwise.
    pub fn add_layers_order_dependency
    (&self, below:impl Into<LayerId>, above:impl Into<LayerId>) -> bool {
        let below = below.into();
        let above = above.into();
        let fresh = self.model.borrow_mut().layer_depth_order.insert_dependency(below,above);
        if fresh { self.layers_depth_order_dirty.set(); }
        fresh
    }

    /// Remove depth-order dependency between two [`Layer`]s. Returns `true` if the dependency was
    /// found, and `false` otherwise.
    pub fn remove_layers_order_dependency
    (&self, below:impl Into<LayerId>, above:impl Into<LayerId>) -> bool {
        let below = below.into();
        let above = above.into();
        let found = self.model.borrow_mut().layer_depth_order.remove_dependency(below,above);
        if found { self.layers_depth_order_dirty.set(); }
        found
    }

    /// Add depth-order dependency between two [`LayerElement`]s in this layer. Returns `true`
    /// if the dependency was inserted successfully (was not already present), and `false`
    /// otherwise.
    pub fn add_elements_order_dependency
    (&self, below:impl Into<LayerElement>, above:impl Into<LayerElement>) -> bool {
        let below = below.into();
        let above = above.into();
        let fresh = self.global_element_depth_order.borrow_mut().insert_dependency(below,above);
        if fresh { self.element_depth_order_dirty.set(); }
        fresh
    }

    /// Remove a depth-order dependency between two [`LayerElement`]s in this layer. Returns `true`
    /// if the dependency was found, and `false` otherwise.
    pub fn remove_elements_order_dependency
    (&self, below:impl Into<LayerElement>, above:impl Into<LayerElement>) -> bool {
        let below = below.into();
        let above = above.into();
        let found = self.global_element_depth_order.borrow_mut().remove_dependency(below,above);
        if found { self.element_depth_order_dirty.set(); }
        found
    }

    /// # Future Improvements
    /// This implementation can be simplified to `S1:KnownShapeSystemId` (not using [`Content`] at
    /// all), after the compiler gets updated to newer version. Returns `true` if the dependency was
    /// inserted successfully (was not already present), and `false` otherwise.
    pub fn add_shapes_order_dependency<S1,S2>(&self) -> (bool,PhantomData<S1>,PhantomData<S2>) where
    S1          : HasContent,
    S2          : HasContent,
    Content<S1> : KnownShapeSystemId,
    Content<S2> : KnownShapeSystemId {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
        let fresh = self.add_elements_order_dependency(s1_id,s2_id);
        (fresh,default(),default())
    }

    /// # Future Improvements
    /// This implementation can be simplified to `S1:KnownShapeSystemId` (not using [`Content`] at
    /// all), after the compiler gets updated to newer version. Returns `true` if the dependency was
    /// found, and `false` otherwise.
    pub fn remove_shapes_order_dependency<S1,S2>
    (&self) -> (bool,PhantomData<S1>,PhantomData<S2>) where
    S1          : HasContent,
    S2          : HasContent,
    Content<S1> : KnownShapeSystemId,
    Content<S2> : KnownShapeSystemId {
        let s1_id = <Content<S1>>::shape_system_id();
        let s2_id = <Content<S2>>::shape_system_id();
        let found = self.remove_elements_order_dependency(s1_id,s2_id);
        (found,default(),default())
    }
}



// ======================
// === LayersRegistry ===
// ======================

/// Internal representation of [`Layers`].
#[derive(Debug,Default)]
pub struct LayersRegistry {
    registry          : OptVec<WeakLayer>,
    symbols_placement : HashMap<SymbolId,Vec<LayerId>>,
    sorted_layers     : Vec<LayerId>,
    layer_depth_order : DependencyGraph<LayerId>,
}

impl LayersRegistry {
    /// Vector of all layers, ordered according to the defined depth-order dependencies. Please note
    /// that this function does not update the depth-ordering of the layers. Updates are performed
    /// by calling the `update` method on [`Layers`], which usually happens once per animation
    /// frame.
    pub fn all(&self) -> Vec<Layer> {
        self.sorted_layers.iter().filter_map(|id| self.registry[**id].upgrade()).collect()
    }

    /// Query a [`Layer`] based on its [`LayerId`].
    pub fn get(&self, layer_id:LayerId) -> Option<Layer> {
        self.registry.safe_index(*layer_id).and_then(|t|t.upgrade())
    }
}



// ==============
// === Macros ===
// ==============

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
#[macro_export]
macro_rules! shapes_order_dependencies {
    ($scene:expr => {
        $( $p1:ident $(:: $ps1:ident)* -> $p2:ident $(:: $ps2:ident)*; )*
    }) => {$(
        $scene.layers.add_shapes_order_dependency::<$p1$(::$ps1)*::View, $p2$(::$ps2)*::View>();
    )*};
}
