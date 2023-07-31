use crate::prelude::*;

use crate::display;
use crate::display::camera::Camera2d;
use crate::display::scene::layer::LayerSymbolPartition;
use crate::display::scene::HardcodedLayers;
use crate::display::scene::Layer;
use crate::display::shape::compound::rectangle;
use crate::display::shape::system::Shape;

use ensogl::display::scene::layer;



/// An `Rc` handle over [`GraphLayersData`].
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct GraphLayers {
    #[deref]
    data: Rc<GraphLayersData>,
}

/// All layers used by different parts of the graph editor.
#[derive(Debug)]
pub struct GraphLayersData {
    // == Main camera layers ==
    /// Layers used for shapes rendered below all nodes, such as node selection.
    pub main_backdrop: NodeBackdropLayers,

    /// The layer that is used for for all non-active edge shapes. Those must be drawn above node
    /// selection, but below backgrounds of all nodes. When an edge becomes active (it is being
    /// dragged), it is moved to the `edge_above_nodes` layer.
    ///
    /// Note that small portion of the dragged edge (the very end near the source edge) is always
    /// being drawn on this layer, since it must be occluded by its source's node background.
    pub edge_below_nodes: Layer,

    /// A set of layers used for all node shapes. The nodes at given tree depth are rendered above
    pub main_nodes: MainNodeLayers,

    /// The layer that is used for for all edge shapes that needs to be above nodes.
    pub edge_above_nodes:        Layer,
    /// An dedicated edge layer that will be masked out with the edge source shape. Sublayer of
    /// `edge_above_nodes`.
    pub masked_edge_above_nodes: Layer,
    /// A cutout layer that removes parts of drawn edge shapes. Mask for `masked_edge_above_nodes`.
    pub edge_above_nodes_cutout: Layer,

    // == Edited node  camera layers ==
    /// A set of layers used for all edited shapes, with a dedicated camera.
    pub edited_backdrop: NodeBackdropLayers,
    pub edited_nodes:    MainNodeLayers,
}

/// Layers used by node backdrop shapes. Separated from node layers, so that other layers can
/// be inserted between the backdrop and main node layers (e.g. edges).
#[derive(Debug)]
pub struct NodeBackdropLayers {
    /// The actual layer that is used as the base for all backdrop partitions, below any edges.
    /// This layer uses a special blend mode, which guarantees that shapes with alpha with
    /// identical colors will nicely blend together without visible overdraw. When shapes of
    /// different colors are combined together, the result may look a little funky, but
    /// acceptable for backdrops. If something needs to be drawn below nodes without special blend,
    /// a new layer must be created.
    pub backdrop_base: Layer,

    /// The visual layer for all rectangle shapes displayed below node background and edges, such
    /// as node selection.
    ///
    /// Note: We are using a single partition instead of the layer directly, so that we can easily
    /// add more partitions either below or above the backdrop if necessary, and to not mix the
    /// types of layers and partitions in different parts of the code.
    pub backdrop: LayerSymbolPartition<rectangle::Shape>,
}

#[derive(Debug)]
pub struct MainNodeLayers {
    /// The actual layer that is used as the base for all shapes within the node itself.
    pub node_base: Layer,

    /// Layer below the node body, but above the backdrop.
    pub below_body: LayerSymbolPartition<rectangle::Shape>,

    /// Hover layer for output port shapes.
    pub output_hover: LayerSymbolPartition<rectangle::Shape>,

    /// Hover layer below the node body, but above the output port.
    pub below_body_hover: LayerSymbolPartition<rectangle::Shape>,

    /// Main node's body, including its background color shape and output port.
    pub body: LayerSymbolPartition<rectangle::Shape>,

    /// The layer used for interactive elements of the main node body, such as output port.
    pub body_hover: LayerSymbolPartition<rectangle::Shape>,

    /// The layer used for all widget shapes, always above the node body.
    pub widget_base: Layer,

    /// The stack of partitions used for all widget rectangle shapes. The widgets at given tree
    /// depth are rendered above the widgets at lower depths. Every depth has an allocation of two
    /// partitions, one for the widget visual elements and one for mouse interaction (hover
    /// shapes).
    pub widget_rectangles: PartitionStack<rectangle::Shape>,

    /// The layer used for elements which should always be rendered above all other node shapes.
    pub above_base: Layer,

    /// The layer used for the action bar, which is rendered above all other node shapes.
    pub action_bar: LayerSymbolPartition<rectangle::Shape>,

    /// The stack of hover areas used by the node port. The ports at given tree depth are rendered
    /// above the ports at lower depths.
    pub port_hover: PartitionStack<rectangle::Shape>,
}

impl GraphLayers {
    /// Create a layer stack used by the graph editor.
    ///
    /// Uses `main` and `node_searcher` hardcoded layers. It is assumed that the `node_searcher`
    /// layer has an unique camera and is rendered above `main`.
    pub fn new(hardcoded: &HardcodedLayers) -> Self {
        let base = &hardcoded.main;
        let searcher = &hardcoded.node_searcher;

        let edit_camera = Camera2d::new();

        let main_backdrop = NodeBackdropLayers::new(base, None);
        let edge_below_nodes = base.create_sublayer("edge_below_nodes");
        let main_nodes = MainNodeLayers::new(base, None);
        let edge_above_nodes = base.create_sublayer("edge_above_nodes");

        let masked_edge_above_nodes = edge_above_nodes.create_sublayer("masked_edge_above_nodes");
        let edge_above_nodes_cutout =
            edge_above_nodes.create_mask_sublayer("edge_above_nodes_cutout");
        masked_edge_above_nodes.set_inverted_mask(&edge_above_nodes_cutout);

        let edited_backdrop = NodeBackdropLayers::new(base, Some(&edit_camera));
        let edited_nodes = MainNodeLayers::new(searcher, Some(&edit_camera));

        let data = GraphLayersData {
            main_backdrop,
            edge_below_nodes,
            main_nodes,
            edge_above_nodes,
            masked_edge_above_nodes,
            edge_above_nodes_cutout,
            edited_backdrop,
            edited_nodes,
        };
        Self { data: Rc::new(data) }
    }
}

impl NodeBackdropLayers {
    fn new(layer: &Layer, camera: Option<&Camera2d>) -> Self {
        let backdrop_base = layer.create_sublayer_with_optional_camera("backdrop_base", camera);
        backdrop_base.set_blend_mode(layer::BlendMode::MAX);

        Self { backdrop: backdrop_base.create_symbol_partition("backdrop"), backdrop_base }
    }
}

impl MainNodeLayers {
    fn new(layer: &Layer, camera: Option<&Camera2d>) -> Self {
        let node_base = layer.create_sublayer_with_optional_camera("node_base", camera);
        let widget_base = layer.create_sublayer_with_optional_camera("widget_base", camera);
        let above_base = layer.create_sublayer_with_optional_camera("above", camera);

        Self {
            below_body: node_base.create_symbol_partition("below_body"),
            output_hover: node_base.create_symbol_partition("output_hover"),
            below_body_hover: node_base.create_symbol_partition("below_body_hover"),
            body: node_base.create_symbol_partition("body"),
            body_hover: node_base.create_symbol_partition("body_hover"),
            widget_rectangles: PartitionStack::new(&widget_base),
            action_bar: above_base.create_symbol_partition("action_bar"),
            port_hover: PartitionStack::new(&above_base),
            node_base,
            widget_base,
            above_base,
        }
    }

    /// Camera getter of this set of layers.
    pub fn camera(&self) -> Camera2d {
        self.node_base.camera()
    }

    /// Get a set of layer partitions for node widgets at given tree depth. The widgets that are
    /// deeper in the tree are rendered above the widgets at lower depths.
    pub fn layers_for_widgets_at_depth(&self, depth: usize) -> CommonLayers {
        let visual = self.widget_rectangles.get(depth * 2);
        let hover = self.widget_rectangles.get(depth * 2 + 1);
        CommonLayers { visual, hover }
    }

    /// Get a layer partition for node port hover areas at given tree depth.
    pub fn port_hover_layer(&self, depth: usize) -> LayerSymbolPartition<rectangle::Shape> {
        self.port_hover.get(depth)
    }
}


/// A combined set of layers with dedicated partitions for visual and interactive elements
#[derive(Debug, Clone)]
pub struct CommonLayers {
    /// Layer partition used for visual shapes. Always rendered below the hover partition.
    pub visual: LayerSymbolPartition<rectangle::Shape>,
    /// Layer partition that is supposed to be used for transparent shapes with mouse interaction.
    /// The hover shapes will be rendered above the visual shapes of the same layer, but below the
    /// visual shapes of the layer above.
    pub hover:  LayerSymbolPartition<rectangle::Shape>,
}


// ======================
// === PartitionStack ===
// ======================

// A stack of layer partitions that can be queried by an integer depth. The relative depth order of
// partitions is guaranteed to be the same as their index - the partition of index 0 is the lowest
// one, the partition of index 1 is the one above it, etc.
//
// NOTE: This structure is only suitable for relatively low index values. It will always create
// partitions up to the requested index, even if the lower indices are not used. This is necessary
// to maintain correct rendering order of partitions.
#[derive(Debug)]
pub struct PartitionStack<S> {
    base_layer: Layer,
    partitions: RefCell<Vec<LayerSymbolPartition<S>>>,
}

impl<S: Shape> PartitionStack<S> {
    /// Create a new partition stack on given base layer. All partitions created by this stack will
    /// be above all partitions created on that layer so far.
    ///
    /// NOTE: If more partitions are created on the base layer after this stack is created, the
    /// order of those relative to the partitions created by this stack is undefined. It is
    /// recommended to never create any additional partitions on the base layer after this stack is
    /// created.
    fn new(base_layer: &Layer) -> Self {
        Self { base_layer: base_layer.clone(), partitions: RefCell::new(Vec::new()) }
    }

    /// Get a partition of given index. Partitions with higher indices are rendered above partitions
    /// with lower indices.
    pub fn get(&self, index: usize) -> LayerSymbolPartition<S> {
        self.get_ref(index).clone()
    }

    /// Add a shape to a partition of given index. Partitions with higher indices are rendered above
    /// partitions with lower indices.
    pub fn add(&self, index: usize, shape: &display::object::Instance) {
        self.get_ref(index).add(shape);
    }

    fn get_ref(&self, index: usize) -> RefMut<LayerSymbolPartition<S>> {
        let mut partitions = self.partitions.borrow_mut();
        if partitions.len() <= index {
            self.extend(&mut partitions, index);
        }
        RefMut::map(partitions, |partitions| &mut partitions[index])
    }

    /// Separate the cold path into separate non-inline function to avoid codegen bloat.
    #[inline(never)]
    #[cold]
    fn extend(&self, partitions: &mut RefMut<Vec<LayerSymbolPartition<S>>>, index: usize) {
        // Sanity check if we aren't creating too many partitions. This is not a hard limit, but
        // if we are creating more than 1000 partitions, something is probably wrong.
        if index > 1000 {
            error!("Unreasonably layer partition index requested. This is probably a bug.");
        }
        // Ensure that the partitions are always created in order of their index. We cannot skip
        // unused indices here.
        partitions.resize_with(index + 1, || self.base_layer.create_symbol_partition("stack"));
    }
}
