//! The FRP runtime, a singleton containing all node and network data.

use crate::prelude::*;
use enso_generics::traits::*;

use crate::callstack::CallStack;
use crate::callstack::DefInfo;
use crate::data::Data;
use crate::metrics;
use crate::node::input;

use enso_data_structures::unrolled_linked_list::UnrolledLinkedList;
use enso_data_structures::unrolled_slot_map::UnrolledSlotMap;
use enso_data_structures::unrolled_slot_map::VersionedIndex;



pub(crate) trait EventConsumer = Fn(&Runtime, &NodeData, &dyn Data) + 'static;



// ============
// === Edge ===
// ============

/// A connection between FRP nodes.
#[derive(Clone, Copy, Debug, Default, Zeroable)]
#[allow(missing_docs)]
pub struct Edge {
    pub target:     NodeId,
    /// Determines whether the edge is connected to a sample port.
    pub is_sampler: bool,
}



// ================
// === NodeData ===
// ================

/// A helper macro to define node data. It is defined with a macro to guarantee that all fields are
/// used either by the `ImClearable` or `Reusable` implementations.
///
/// For performance reasons, when a node is dropped, not all of its fields are cleared, as some of
/// the fields will be set when the node is reused. See docs of [`ImClearable`] and [`Reusable`] to
/// learn more.
macro_rules! def_node {
    (
        $(#$meta:tt)*
        pub struct $name:ident {
            reusable {
                $(
                    $(#$reusable_field_meta:tt)*
                    $reusable_field:ident: $reusable_field_ty:ty
                ),* $(,)?
            }

            clearable {
                $(
                    $(#$clearable_field_meta:tt)*
                    $clearable_field:ident: $clearable_field_ty:ty
                ),* $(,)?
            }

        }
    ) => {
        $(#$meta)*
        pub struct $name {
            $(
                $(#$reusable_field_meta)*
                pub(crate) $reusable_field: $reusable_field_ty,
            )*
            $(
                $(#$clearable_field_meta)*
                pub(crate) $clearable_field: $clearable_field_ty,
            )*
        }

        impl ImClearable for $name {
            #[inline(always)]
            fn clear_im(&self) {
                $(self.$clearable_field.clear_im();)*
            }
        }

        impl Reusable for NodeData {
            type Args = ($($reusable_field_ty,)*);
            #[inline(always)]
            fn reuse(&mut self, _args: Self::Args) {
                $(
                    let (arg, _args) = _args.pop_first_field();
                    self.$reusable_field = arg;
                )*
            }
        }

    };
}

type NodeOutputs = ZeroOverheadRefCell<UnrolledLinkedList<Edge, 8, usize, prealloc::Zeroed>>;

def_node! {
    /// An FRP node.
    #[derive(Default, Zeroable)]
    pub struct NodeData {
        reusable {
            tp: ZeroableOption<Box<dyn EventConsumer>>,
            network_id: NetworkId,
            def: DefInfo,
        }
        clearable {
            inputs: ZeroOverheadRefCell<UnrolledLinkedList<NodeId, 8, usize, prealloc::Zeroed>>,
            /// Outputs to which an event should be emitted. This includes both [`Listener`] and
            /// [`ListenerAndSampler`] connections.
            outputs: NodeOutputs,
            /// Outputs to which an event should NOT be emitted. This includes only [`Sampler`]
            /// connections.
            sampler_outputs: NodeOutputs,
            output_cache: ZeroOverheadRefCell<ZeroableOption<Box<dyn Data>>>,
            /// Number of samplers connected to this nodes. This includes both the count of
            /// [`ListenerAndSampler`] connections in [`Self::outputs`] and the count of all
            /// connections in [`Self::sampler_outputs`].
            sampler_count: Cell<usize>,
        }
    }
}

impl Debug for NodeData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeData")
    }
}

impl NodeData {
    // Either `inline(always)` or `inline(never)` flags makes this code slower.
    fn on_event(&self, runtime: &Runtime, event: &dyn Data) {
        self.tp.as_ref().map(|f| f(runtime, self, event));
    }
}



// ===================
// === NetworkData ===
// ===================

#[derive(Debug, Default)]
struct NetworkData {
    nodes: Vec<NodeId>,
}

impl Clearable for NetworkData {
    #[inline(always)]
    fn clear(&mut self) {
        self.nodes.clear();
    }
}



// ===============
// === Runtime ===
// ===============

/// The size of a single array in the array-linked list of FRP networks. This value was picked
/// empirically.
const NETWORK_LINKED_ARRAY_SIZE: usize = 1024;

/// The size of a single array in the array-linked list of FRP nodes. This value was picked
/// empirically.
const NODES_LINKED_ARRAY_SIZE: usize = 131072; // 2 ^ 17

thread_local! {
    static RUNTIME: Runtime  = default();
}

#[inline(always)]
pub(crate) fn with_runtime<T>(f: impl FnOnce(&Runtime) -> T) -> T {
    RUNTIME.with(|runtime| f(runtime))
}

/// Phantom type used to provide type-level hints for network-related structs.
#[derive(Clone, Copy, Debug)]
pub struct NETWORK;

/// Phantom type used to provide type-level hints for node-related structs.
#[derive(Clone, Copy, Debug)]
pub struct NODE;

/// A unique identifier of a network.
pub type NetworkId = VersionedIndex<NETWORK>;

/// A unique identifier of a node.
pub type NodeId = VersionedIndex<NODE>;

/// A global FRP network. Both networks and nodes are stored in a global slot-maps. This allows for
/// both fast initialization of the slot-maps, as well as re-using already allocated memory in case
/// a network or a node is dropped and created again.
#[derive(Default, Debug)]
pub struct Runtime {
    networks: UnrolledSlotMap<
        ZeroOverheadRefCell<NetworkData>,
        NETWORK_LINKED_ARRAY_SIZE,
        NETWORK,
        prealloc::Default,
    >,
    nodes: UnrolledSlotMap<
        ZeroOverheadRefCell<NodeData>,
        NODES_LINKED_ARRAY_SIZE,
        NODE,
        prealloc::Zeroed,
    >,
    metrics:  metrics::Metrics,
    stack:    CallStack,
}

impl Runtime {
    #[inline(always)]
    pub(crate) fn new_network(&self) -> NetworkId {
        self.metrics.inc_networks();
        self.networks.reserve()
    }

    #[inline(always)]
    pub(crate) fn drop_network(&self, id: NetworkId) {
        if let Some(network) = self.networks.get(id) {
            #[allow(unused_mut)]
            let mut network_data = network.borrow_mut();
            for node_id in &network_data.nodes {
                self.drop_node(*node_id);
            }
            network_data.clear();
            self.networks.invalidate(id);
        }
    }

    #[inline(always)]
    fn drop_node(&self, id: NodeId) {
        if let Some(node) = self.nodes.get(id) {
            node.borrow().clear_im();
            self.nodes.invalidate(id);
        }
    }

    #[inline(always)]
    fn with_borrowed_node(&self, node_id: NodeId, f: impl FnOnce(&NodeData)) {
        if let Some(node) = self.nodes.get(node_id) {
            f(&node.borrow());
        } else {
            // If enabled, it slows down event emitting by 20%.
            #[cfg(debug_assertions)]
            error!("Trying to access a non-existent FRP node {:?}.", node_id);
        }
    }

    #[inline(always)]
    pub(crate) fn new_node(
        &self,
        net_id: NetworkId,
        def: DefInfo,
        f: impl EventConsumer,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeId {
        self.metrics.inc_nodes();
        if let Some(network) = self.networks.get(net_id) {
            let id = self.nodes.reserve_and_init_im_(|node| {
                #[allow(unused_mut)]
                let mut node = node.borrow_mut();
                node.reuse((ZeroableOption::Some(Box::new(f)), net_id, def));
                init(&mut node);
            });
            network.borrow_mut().nodes.push(id);
            id
        } else {
            error!("Trying to create a node in a non-existent FRP network.");
            NodeId::new_not_occupied()
        }
    }

    #[inline(always)]
    pub(crate) fn connect(&self, src_tp: input::Type, tgt_id: NodeId) {
        let src_id = src_tp.node_id();
        let is_sampler = src_tp.is_sampler();
        let output_connection = Edge { target: tgt_id, is_sampler };
        self.with_borrowed_node(src_id, |src| {
            if is_sampler {
                src.sampler_count.modify(|t| *t += 1);
            }
            let outputs = if src_tp.is_listener() { &src.outputs } else { &src.sampler_outputs };
            outputs.borrow().push(output_connection);
        });
        self.with_borrowed_node(tgt_id, |tgt| {
            tgt.inputs.borrow().push(src_id);
        });
    }

    /// Emit the event to all listeners of the given node. The event type is not checked, so you
    /// have to guarantee that the type is correct.
    ///
    /// # Safety
    /// The event type is not checked, so you have to guarantee that the type is correct.
    #[inline(always)]
    #[allow(unsafe_code)]
    pub(crate) unsafe fn unchecked_emit_borrow(&self, src_node_id: NodeId, event: &dyn Data) {
        // TODO: Maybe we can check the type correctness with debug-assertions enabled?
        self.with_borrowed_node(src_node_id, |src_node| {
            self.unchecked_emit(src_node, event);
        })
    }

    /// Emit the event to all listeners of the given node. The event type is not checked, so you
    /// have to guarantee that the type is correct.
    #[inline(always)]
    pub(crate) fn unchecked_emit(&self, src_node: &NodeData, event: &dyn Data) {
        // Clone the incoming data if there are any sampler outputs.
        if src_node.sampler_count.get() > 0 {
            src_node.output_cache.replace(ZeroableOption::Some(event.boxed_clone()));
        }

        self.stack.with(src_node.def, || {
            let mut cleanup_outputs = false;
            let mut cleanup_sampler_outputs = false;
            for &output in &*src_node.outputs.borrow() {
                let target = self.nodes.get(output.target);
                if target.map(|tgt_node| tgt_node.borrow().on_event(self, event)).is_none() {
                    cleanup_outputs = true;
                }
            }
            for &output in &*src_node.sampler_outputs.borrow() {
                if self.nodes.get(output.target).is_none() {
                    cleanup_sampler_outputs = true;
                }
            }

            let cleanup = |do_cleanup: bool, outputs: &NodeOutputs| {
                if do_cleanup {
                    outputs.borrow_mut().retain(|output| {
                        let retain = self.nodes.exists(output.target);
                        if !retain && output.is_sampler {
                            src_node.sampler_count.update(|t| t - 1);
                        }
                        retain
                    });
                }
            };

            cleanup(cleanup_outputs, &src_node.outputs);
            cleanup(cleanup_sampler_outputs, &src_node.sampler_outputs);
        });
    }

    /// Perform the provided function with the borrowed output cache of the given node. If the node
    /// did not store its cache, a default value will be used and an error will be emitted.
    ///
    /// # Safety
    /// The cache will be casted to the provided type without checking it, so you need to guarantee
    /// type safety when using this function.
    #[inline(always)]
    #[allow(unsafe_code)]
    pub(crate) unsafe fn with_borrowed_node_output_coerced<T>(
        &self,
        node_id: NodeId,
        f: impl FnOnce(&T),
    ) where
        T: Default,
    {
        self.with_borrowed_node(node_id, |node| {
            let output = node.output_cache.borrow();
            if let Some(output) = output.as_ref().map(|t| &**t) {
                #[allow(trivial_casts)]
                let output_coerced = unsafe { &*(output as *const dyn Data as *const T) };
                f(output_coerced)
            } else {
                f(&default())
            };
        })
    }
}
