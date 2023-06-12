//! # Introduction
//! This module implements a sophisticated event handling framework, known as Functional Reactive
//! Programming system. It allows creating data flow diagrams from predefined nodes. Over years many
//! different FRP implementations appeared, especially in the Haskell community. The implementations
//! differ both in concepts they use, as well as the provided functionality. The following
//! implementation focuses on providing high performance and maintainability (ability to reason
//! about correctness of complex data flow graphs).
//!
//! In order to grasp some of the main ideas we strongly encourage you to read the following FRP
//! introduction. Although this implementation differs in few details, understanding these concepts
//! would be helpful: https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md
//!
//!
//! # FRP Network and FRP nodes
//! FRP logic is encoded by a [`Network`] of connected FRP [`Node`]s. Each node has zero or more
//! inputs and a single output. You can think of nodes like functions that are processing incoming
//! data. FRP nodes are grouped into [`Network`]s and they are dropped when the network is dropped.
//!
//! The [`Network`] is parametrized with a model, a data structure that can be modified by some of
//! FPR nodes, such as `map`.
//!
//!
//!
//! # Events and Behaviors
//! Every FRP implementation has its own evaluation logic. Some FRP implementations provide the user
//! with an explicit distinction between "events" and "behaviors". "Events" are simple values passed
//! trough the network, while "behaviors" are values that can be accessed at any time (like the
//! current mouse position). Such a distinction requires the user to explicitly specify where a
//! behavior should be created, for example by creating a "hold" node, which on every incoming event
//! clones its value and stores it in case someone would like to sample it. Such a distinction has
//! two major drawbacks:
//!
//! 1. It increases the verbosity of the FRP network, as users need to explicitly specify where a
//!    behavior should be created.
//! 2. It does not allow for some network optimizations. For example, if a node that requires the
//!    output to be sampled is removed, the hold node is no longer needed and the value does not
//!    need to ble cloned anymore.
//!
//! That's why this implementation makes this distinction implicit. The FRP network passes events
//! and some output ports are considered "behaviors" if at least one "sample" input port is
//! connected to the output port. There are three types of input ports:
//!
//! - [`Listen`]: A node can have only one listen port. If a node has this port it can also have
//!   zero or more [`Sample`] ports (but no [`ListenAndSample`] ports). In case an event is emitted
//!   to this port, the node will sample all of its [`Sample`] ports, evaluate its expression, and
//!   emit the output value.
//!
//! - [`Sample`]: In contrast to listen ports, if an event is emitted to a sample port, the node
//!   will not evaluate its expression. Sample ports are used only to sample the last emitted value
//!   in case a listen port is triggered.
//!
//! - [`ListenAndSample`]: This port is a combination of [`Listen`] and [`Sample`] ports. Unlike the
//!   [`Listen`] port, a node can have multiple [`ListenAndSample`] ports. In case an event is
//!   emitted on this port, the node will sample all of its [`ListenAndSample`] and [`Sample`]
//!   ports, evaluate its expression, and emit the output value.
//!
//!
//! # Imperative FRP evaluation order
//! This library implements so called "imperative FRP evaluation order". This means that a node
//! evaluates as soon as it receives an event on one of its listen ports. After evaluating, the node
//! emits the output value which triggers evaluation of all nodes connected to its output ports. For
//! example, given the following FRP network:
//!
//! ```text
//! ╭───────╮                 [L] - Listen port.
//! │ Node1 │
//! ╰───┬───╯
//!     ├──────────╮
//!     ▼ [L]      ▼ [L]
//! ╭───────╮  ╭───────╮
//! │ Node2 │  │ Node3 │
//! ╰───┬───╯  ╰───┬───╯
//! ```
//!
//! If an event is emitted to the listen port of `Node1`:
//! - The event from `Node1` is emitted to `Node2`.
//! - `Node2` evaluates and emits the event to all of its children nodes, which will evaluate and
//!   emit their events to subsequent nodes.
//! - The event from `Node1` is emitted to `Node3`.
//! - `Node3` evaluates and emits the event to all of its children nodes, which will evaluate and
//!   emit their events to subsequent nodes.
//!
//!
//! # The "diamond problem"
//! ...
//!
//!
//! # The "initialization problem"
//! ...
//!
//!
//! # The "unused sub-network performance problem"
//! ...
//!
//!
//! # Reactive FRP evaluation order
//! An alternative way to imperative FRP evaluation order is a reactive one. This library does NOT
//! implement such a mode, but it is important to understand this difference, as we plan to
//! introduce this mode in the future. In this mode, the evaluation of some nodes is triggered
//! automatically to make the FRP network contain the most up-to-date values. This mode eliminates
//! the above described problems:
//!
//! - It eliminates the "diamond problem" as a node will evaluate only after all of its inputs will
//!   be computed.
//! - It eliminates the "initialization problem", as the network will propagate initial values
//!   automatically after creation.
//! - It eliminates the "unused sub-network performance problem" as it can leave some nodes not
//!   evaluated it the system discovers that the output values will not be used by subsequent nodes.
//!
//! However, this mode has its own drawbacks:
//!
//! - It is impossible for the user to determine the order of FRP nodes evaluation.
//! - The FRP runtime has significantly more work at runtime, which will cause more computations to
//!   be performed, making passing values between nodes slower. However, this can be probably
//!   mitigated by the benefits described above.
//!
//!
//! # Passing data between nodes as references and clones
//! By default, the data is passed between nodes by reference. In case a node output is connected to
//! at least one sample port, the data will be cloned before it is emitted, so the sample ports will
//! be able to sample it on demand. If no sample ports are connected to the node output, the data
//! will not be cloned.

#![feature(allocator_api)]
#![feature(test)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(downcast_unchecked)]
#![feature(type_alias_impl_trait)]
#![feature(core_intrinsics)]

mod callstack;
mod metrics;

pub use enso_prelude as prelude;

use enso_data_structures::unrolled_linked_list::UnrolledLinkedList;
use enso_data_structures::unrolled_slot_map::UnrolledSlotMap;
use enso_data_structures::unrolled_slot_map::VersionedIndex;
use enso_generics::traits::*;
use enso_prelude::*;
use smallvec::SmallVec;


use crate::callstack::CallStack;
use crate::callstack::DefInfo;

use enso_frp as frp_old;



// ============
// === Data ===
// ============

/// Trait for types that can be used as event data in the FRP system. The inherited bounds are as
/// follow:
/// - [`Debug`]: For debug purposes in order to be able to trace the data flow when needed.
/// - [`Clone + 'static`]: Needed in case the data needs to be cloned if it is passed to a sample
///   port.
pub trait Data: DataBounds {
    fn boxed_clone(&self) -> Box<dyn Data>;
}

/// Alias for bounds required on [`Data`] types.
pub trait DataBounds = where Self: Debug + 'static;

impl<T: DataBounds + Clone> Data for T {
    fn boxed_clone(&self) -> Box<dyn Data> {
        Box::new(self.clone())
    }
}



// ===============
// === Network ===
// ===============


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



// ============
// === Edge ===
// ============

/// A connection between FRP nodes.
#[derive(Clone, Copy, Debug, Default, Zeroable)]
pub struct Edge {
    pub target:     NodeId,
    /// Determines whether the edge is connected to a sample port.
    pub is_sampler: bool,
}



// ============
// === Node ===
// ============

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
                $reusable_field: $reusable_field_ty,
            )*
            $(
                $(#$clearable_field_meta)*
                $clearable_field: $clearable_field_ty,
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
            fn reuse(&mut self, args: Self::Args) {
                $(
                    let (arg, args) = args.pop_first_field();
                    self.$reusable_field = arg;
                )*
            }
        }

    };
}

type NodeOutputs = OptRefCell<UnrolledLinkedList<Edge, 8, usize, prealloc::Zeroed>>;

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
            inputs: OptRefCell<UnrolledLinkedList<NodeId, 8, usize, prealloc::Zeroed>>,
            /// Outputs to which an event should be emitted. This includes both [`Listener`] and
            /// [`ListenerAndSampler`] connections.
            outputs: NodeOutputs,
            /// Outputs to which an event should NOT be emitted. This includes only [`Sampler`]
            /// connections.
            sampler_outputs: NodeOutputs,
            output_cache: OptRefCell<ZeroableOption<Box<dyn Data>>>,
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



// ===============
// === Runtime ===
// ===============

/// The size of a single array in the array-linked list of FRP networks.
const NETWORK_LINKED_ARRAY_SIZE: usize = 1024;

/// The size of a single array in the array-linked list of FRP nodes.
const NODES_LINKED_ARRAY_SIZE: usize = 131072; // 2 ^ 17

thread_local! {
    static RUNTIME: Runtime  = default();
}

#[inline(always)]
pub(crate) fn with_runtime<T>(f: impl FnOnce(&Runtime) -> T) -> T {
    RUNTIME.with(|runtime| f(runtime))
}

/// Phantom type used to provide type-level hints for network-related structs.
pub struct NETWORK;

/// Phantom type used to provide type-level hints for node-related structs.
pub struct NODE;

/// A unique identifier of a network.
pub type NetworkId = VersionedIndex<NETWORK>;

/// A unique identifier of a node.
pub type NodeId = VersionedIndex<NODE>;

/// A global FRP network. Both networks and nodes are stored in a global slot-maps. This allows for
/// both fast initialization of the slot-maps, as well as re-using already allocated memory in case
/// a network or a node is dropped and created again.
#[derive(Default)]
pub struct Runtime {
    networks: UnrolledSlotMap<
        OptRefCell<NetworkData>,
        NETWORK_LINKED_ARRAY_SIZE,
        NETWORK,
        prealloc::Default,
    >,
    nodes: UnrolledSlotMap<OptRefCell<NodeData>, NODES_LINKED_ARRAY_SIZE, NODE, prealloc::Zeroed>,
    metrics:  metrics::Metrics,
    stack:    CallStack,
}

impl Runtime {
    #[inline(always)]
    fn new_network(&self) -> NetworkId {
        self.metrics.inc_networks();
        self.networks.reserve()
    }

    #[inline(always)]
    fn drop_network(&self, id: NetworkId) {
        if let Some(network) = self.networks.get(id) {
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
            f(&*node.borrow());
        } else {
            // If enabled, it slows down event emitting by 20%.
            #[cfg(debug_assertions)]
            error!("Trying to access a non-existent FRP node {:?}.", node_id);
        }
    }

    #[inline(always)]
    fn new_node(
        &self,
        net_id: NetworkId,
        def: DefInfo,
        f: impl EventConsumer,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeId {
        self.metrics.inc_nodes();
        if let Some(network) = self.networks.get(net_id) {
            let id = self.nodes.reserve_and_init_im_(|node| {
                let mut node = node.borrow_mut();
                node.reuse((ZeroableOption::Some(Box::new(f)), net_id, def));
                init(&mut *node);
            });
            network.borrow_mut().nodes.push(id);
            id
        } else {
            error!("Trying to create a node in a non-existent FRP network.");
            NodeId::new_not_occupied()
        }
    }

    #[inline(always)]
    fn connect(&self, src_tp: InputType, tgt_id: NodeId) {
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
    #[inline(always)]
    fn unchecked_emit_borrow(&self, src_node_id: NodeId, event: &dyn Data) {
        self.with_borrowed_node(src_node_id, |src_node| {
            self.unchecked_emit(src_node, event);
        })
    }

    /// Emit the event to all listeners of the given node. The event type is not checked, so you
    /// have to guarantee that the type is correct.
    #[inline(always)]
    fn unchecked_emit(&self, src_node: &NodeData, event: &dyn Data) {
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
    unsafe fn with_borrowed_node_output_coerced<T>(&self, node_id: NodeId, f: impl FnOnce(&T))
    where T: Default {
        self.with_borrowed_node(node_id, |node| {
            let output = node.output_cache.borrow();
            if let Some(output) = output.as_ref().map(|t| &**t) {
                let output_coerced = unsafe { &*(output as *const dyn Data as *const T) };
                f(output_coerced)
            } else {
                f(&default())
            };
        })
    }
}



// ===============
// === Network ===
// ===============

/// Alias for [`Network`] with the default parametrization;
pub type Network_ = Network;

/// An FRP network, a group of FRP nodes.
///
/// # WARNING: Be careful when cloning the network.
/// As long as the network lives, all nodes in the network will live as well. Cloning the network
/// and passing it to FRP node closure will cause memory leaks. The ability to clone the network is
/// provided for backward compatibility only and will be removed in the future.
#[derive(CloneRef, Debug, Default, Deref)]
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Network<Model = ()> {
    rc: Rc<NetworkModel<Model>>,
}

impl<Model: Default> Network<Model> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}

/// Internal representation of [`Network`].
#[derive(Debug)]
pub struct NetworkModel<Model> {
    pub(crate) id:    NetworkId,
    pub(crate) model: Rc<OptRefCell<Model>>,
}

impl<Model: Default> NetworkModel<Model> {
    #[inline(never)]
    fn new() -> Self {
        let id = with_runtime(|rt| rt.new_network());
        let model = default();
        NetworkModel { id, model }
    }
}

impl<Model: Default> Default for NetworkModel<Model> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Model> Drop for NetworkModel<Model> {
    fn drop(&mut self) {
        with_runtime(|rt| rt.drop_network(self.id));
    }
}



// ================================
// === Node Definition Generics ===
// ================================



#[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, PartialEq)]
pub struct Listen<T>(T);

#[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, PartialEq)]
pub struct Sample<T>(T);

#[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, PartialEq)]
pub struct ListenAndSample<T>(T);

impl<T> Listen<T> {
    #[inline(always)]
    fn map<S>(self, f: impl FnOnce(T) -> S) -> Listen<S> {
        Listen(f(self.0))
    }
}

impl<T> Sample<T> {
    #[inline(always)]
    fn map<S>(self, f: impl FnOnce(T) -> S) -> Sample<S> {
        Sample(f(self.0))
    }
}

impl<T> ListenAndSample<T> {
    #[inline(always)]
    fn map<S>(self, f: impl FnOnce(T) -> S) -> ListenAndSample<S> {
        ListenAndSample(f(self.0))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InputType {
    Listen(NodeId),
    ListenAndSample(NodeId),
    Sample(NodeId),
}

impl InputType {
    #[inline(always)]
    pub fn node_id(self) -> NodeId {
        match self {
            InputType::Listen(t) => t,
            InputType::ListenAndSample(t) => t,
            InputType::Sample(t) => t,
        }
    }

    #[inline(always)]
    pub fn is_sampler(self) -> bool {
        match self {
            InputType::Listen(_) => false,
            InputType::ListenAndSample(_) => true,
            InputType::Sample(_) => true,
        }
    }

    #[inline(always)]
    pub fn is_listener(self) -> bool {
        match self {
            InputType::Listen(_) => true,
            InputType::ListenAndSample(_) => true,
            InputType::Sample(_) => false,
        }
    }
}

impl<T: Node> From<Listen<T>> for InputType {
    #[inline(always)]
    fn from(t: Listen<T>) -> Self {
        InputType::Listen(t.0.id())
    }
}

impl<T: Node> From<ListenAndSample<T>> for InputType {
    #[inline(always)]
    fn from(t: ListenAndSample<T>) -> Self {
        InputType::ListenAndSample(t.0.id())
    }
}

impl<T: Node> From<Sample<T>> for InputType {
    #[inline(always)]
    fn from(t: Sample<T>) -> Self {
        InputType::Sample(t.0.id())
    }
}



// ====================
// === EventContext ===
// ====================

/// A wrapper for the runtime and current node data with explicit information about the event type.
/// It is mainly used to make the API for defining nodes easy and type-safe.
struct EventContext<'a, Output> {
    tp:      PhantomData<Output>,
    runtime: &'a Runtime,
    node:    &'a NodeData,
}

impl<'a, Output> EventContext<'a, Output> {
    /// Constructor. The `Output` type is not checked, so you need to be careful when using this
    /// function.
    #[inline(always)]
    fn unchecked_new(runtime: &'a Runtime, node: &'a NodeData) -> Self {
        EventContext { tp: PhantomData, runtime, node }
    }
}

impl<'a, Output: Data> EventContext<'a, Output> {
    /// Emit the output event.
    #[inline(always)]
    fn emit(self, value: &Output) {
        self.runtime.unchecked_emit(self.node, value);
    }
}



// ======================
// === NodeDefinition ===
// ======================



/// Abstraction allowing easy FRP node definition. It generates appropriate logic based on the input
/// types. The [`Inputs`] is a tuple containing any supported combination of [`Listen`], [`Sample`],
/// and [`ListenAndSample`]. Refer to their docs to learn more about their meaning.
trait NodeDefinition<Incl, Model, Output, F> {
    fn new_node_with_init<Kind>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeTemplate<Kind, Output>;
}

impl<Model> Network<Model> {
    #[inline(always)]
    fn _new_node_with_init<Incl, Kind, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        f: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, NodeTemplate<Kind, Output>>
    where
        Inputs: NodeDefinition<Incl, Model, Output, F>,
    {
        let node = Inputs::new_node_with_init(self, inps, f, |n| {
            init(n);
        });
        NodeInNetwork::new(self, node)
    }

    #[inline(always)]
    fn new_node_with_init<Kind, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        f: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, NodeTemplate<Kind, Output>>
    where
        Inputs: NodeDefinition<ModelNotIncluded, Model, Output, F>,
    {
        self._new_node_with_init::<ModelNotIncluded, Kind, Inputs, Output, F, _Out>(inps, f, init)
    }

    #[inline(always)]
    fn new_node_with_model_with_init<Kind, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        f: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, NodeTemplate<Kind, Output>>
    where
        Inputs: NodeDefinition<ModelIncluded, Model, Output, F>,
    {
        self._new_node_with_init::<ModelIncluded, Kind, Inputs, Output, F, _Out>(inps, f, init)
    }

    #[inline(always)]
    fn new_node<Kind, Inputs, Output, F>(
        &self,
        inps: Inputs,
        f: F,
    ) -> NodeInNetwork<Model, NodeTemplate<Kind, Output>>
    where
        Inputs: NodeDefinition<ModelNotIncluded, Model, Output, F>,
    {
        self.new_node_with_init(inps, f, |_| {})
    }

    #[inline(always)]
    fn new_node_with_model<Kind, Inputs, Output, F>(
        &self,
        inps: Inputs,
        f: F,
    ) -> NodeInNetwork<Model, NodeTemplate<Kind, Output>>
    where
        Inputs: NodeDefinition<ModelIncluded, Model, Output, F>,
    {
        self.new_node_with_model_with_init(inps, f, |_| {})
    }
}


struct ModelIncluded;
struct ModelNotIncluded;

struct ModelSkipped;

trait Includable<Model> {
    type Included;
    type Cloned: 'static;
    fn clone_model(model: &Rc<OptRefCell<Model>>) -> Self::Cloned;
    fn with_model_borrow_mut<Out>(
        model: &Self::Cloned,
        f: impl FnOnce(&mut Self::Included) -> Out,
    ) -> Out;
}

impl<Model> Includable<Model> for ModelNotIncluded {
    type Included = ModelSkipped;
    type Cloned = ModelSkipped;
    #[inline(always)]
    fn clone_model(_model: &Rc<OptRefCell<Model>>) -> Self::Cloned {
        ModelSkipped
    }
    #[inline(always)]
    fn with_model_borrow_mut<Out>(
        _model: &Self::Cloned,
        f: impl FnOnce(&mut Self::Included) -> Out,
    ) -> Out {
        f(&mut ModelSkipped)
    }
}

impl<Model: 'static> Includable<Model> for ModelIncluded {
    type Included = Model;
    type Cloned = Rc<OptRefCell<Model>>;
    #[inline(always)]
    fn clone_model(model: &Rc<OptRefCell<Model>>) -> Self::Cloned {
        model.clone()
    }
    #[inline(always)]
    fn with_model_borrow_mut<Out>(
        model: &Self::Cloned,
        f: impl FnOnce(&mut Self::Included) -> Out,
    ) -> Out {
        f(&mut *model.borrow_mut())
    }
}

type Included<Incl, Model> = <Incl as Includable<Model>>::Included;

impl<Incl, Model, Output, F> NodeDefinition<Incl, Model, Output, F> for ()
where
    Incl: Includable<Model>,
    F: Fn(EventContext<Output>, &mut Included<Incl, Model>) + 'static,
{
    #[inline(always)]
    fn new_node_with_init<Kind>(
        net: &Network<Model>,
        _inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeTemplate<Kind, Output> {
        let model = Incl::clone_model(&net.model);
        net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, _: &dyn Data| {
                Incl::with_model_borrow_mut(&model, |model| {
                    f(EventContext::unchecked_new(rt, node), model)
                })
            },
            init,
        )
    }
}

impl<Incl, Model, N0, Output, F> NodeDefinition<Incl, Model, Output, F> for (Listen<N0>,)
where
    Incl: Includable<Model>,
    N0: Node,
    F: 'static + Fn(EventContext<Output>, &mut Included<Incl, Model>, &N0::Output),
{
    #[inline(always)]
    fn new_node_with_init<Kind>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeTemplate<Kind, Output> {
        let model = Incl::clone_model(&net.model);
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, data: &dyn Data| {
                let data = unsafe { &*(data as *const dyn Data as *const N0::Output) };
                Incl::with_model_borrow_mut(&model, |model| {
                    f(EventContext::unchecked_new(rt, node), model, data);
                })
            },
            init,
        );
        with_runtime(|rt| inputs.field_iter(|input| rt.connect(*input, node.id)));
        node
    }
}

impl<Incl, Model, N0, N1, Output, F> NodeDefinition<Incl, Model, Output, F>
    for (Listen<N0>, Sample<N1>)
where
    Incl: Includable<Model>,
    N0: Node,
    N1: NodeWithDefaultOutput,
    F: 'static + Fn(EventContext<Output>, &mut Included<Incl, Model>, &N0::Output, &N1::Output),
{
    #[inline(always)]
    fn new_node_with_init<Kind>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeTemplate<Kind, Output> {
        let model = Incl::clone_model(&net.model);
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, data: &dyn Data| unsafe {
                let t0 = &*(data as *const dyn Data as *const N0::Output);
                rt.with_borrowed_node_output_coerced(inputs.1.node_id(), |t1| {
                    Incl::with_model_borrow_mut(&model, |model| {
                        f(EventContext::unchecked_new(rt, node), model, t0, t1)
                    })
                });
            },
            init,
        );
        with_runtime(|rt| inputs.field_iter(|input| rt.connect(*input, node.id)));
        node
    }
}

impl<Incl, Model, N0, N1, N2, Output, F> NodeDefinition<Incl, Model, Output, F>
    for (Listen<N0>, Sample<N1>, Sample<N2>)
where
    Incl: Includable<Model>,
    N0: Node,
    N1: NodeWithDefaultOutput,
    N2: NodeWithDefaultOutput,
    F: 'static
        + Fn(EventContext<Output>, &mut Included<Incl, Model>, &N0::Output, &N1::Output, &N2::Output),
{
    #[inline(always)]
    fn new_node_with_init<Kind>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeTemplate<Kind, Output> {
        let model = Incl::clone_model(&net.model);
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, data: &dyn Data| unsafe {
                let t0 = &*(data as *const dyn Data as *const N0::Output);
                rt.with_borrowed_node_output_coerced(inputs.1.node_id(), |t1| {
                    rt.with_borrowed_node_output_coerced(inputs.2.node_id(), |t2| {
                        Incl::with_model_borrow_mut(&model, |model| {
                            f(EventContext::unchecked_new(rt, node), model, t0, t1, t2)
                        })
                    })
                });
            },
            init,
        );
        with_runtime(|rt| inputs.field_iter(|input| rt.connect(*input, node.id)));
        node
    }
}



// ============
// === Node ===
// ============

/// Any FRP node. This is a generalization for [`NodeTemplate`] with hidden `Kind` type.
pub trait Node: Copy {
    type Output;
    fn id(self) -> NodeId;
}

/// An alias for [`Node`] with default output type bounds.
pub trait NodeWithDefaultOutput = Node where <Self as Node>::Output: Default;

/// A generic node representation that is parametrized with the node kind and node output. All FRP
/// nodes use this struct under the hood.
#[derive(Derivative)]
#[derivative(Copy(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[repr(transparent)]
pub struct NodeTemplate<Kind, Output> {
    _marker: PhantomData<(Kind, Output)>,
    id:      NodeId,
}

impl<Kind, Output> Node for NodeTemplate<Kind, Output> {
    type Output = Output;
    #[inline(always)]
    fn id(self) -> NodeId {
        self.id
    }
}

impl<Kind, Output> NodeTemplate<Kind, Output> {
    #[inline(never)]
    pub fn emit(&self, value: &Output)
    where Output: Data {
        with_runtime(|rt| rt.unchecked_emit_borrow(self.id, value));
    }
}

impl<Model> Network<Model> {
    /// Constructor of the node. The type safety is not guaranteed. You have to ensure that the
    /// [`f`] function arguments and output type are correct.
    #[inline(always)]
    fn new_node_with_init_unchecked<Kind, Output>(
        &self,
        f: impl EventConsumer,
        init: impl FnOnce(&mut NodeData),
    ) -> NodeTemplate<Kind, Output> {
        let _marker = PhantomData;
        let id = with_runtime(|rt| rt.new_node(self.id, DefInfo::unlabelled(), f, init));
        NodeTemplate { _marker, id }
    }
}



// =====================
// === NodeInNetwork ===
// =====================

#[derive(Deref, DerefMut)]
pub struct NodeInNetwork<'t, Model, T> {
    #[deref]
    #[deref_mut]
    elem:    T,
    network: &'t Network<Model>,
}

impl<'a, Model, T: Node> Node for NodeInNetwork<'a, Model, T> {
    type Output = T::Output;
    #[inline(always)]
    fn id(self) -> NodeId {
        self.elem.id()
    }
}

impl<'t, Model, T: Copy> Copy for NodeInNetwork<'t, Model, T> {}
impl<'t, Model, T: Clone> Clone for NodeInNetwork<'t, Model, T> {
    fn clone(&self) -> Self {
        Self { elem: self.elem.clone(), network: self.network }
    }
}

impl<'t, Model, T> NodeInNetwork<'t, Model, T> {
    fn new(network: &'t Network<Model>, elem: T) -> Self {
        Self { network, elem }
    }
}

// === Stream ===

pub struct StreamType;

pub type Stream<Output = ()> = NodeTemplate<StreamType, Output>;


// === Source ===

#[derive(Clone, Copy, Default)]
pub struct SourceType;
pub type Source<Output = ()> = NodeTemplate<SourceType, Output>;

impl<Model> Network<Model> {
    #[inline(always)]
    pub fn source<T>(&self) -> NodeInNetwork<Model, Source<T>> {
        self.new_node((), |_, _| {})
    }

    pub fn source_(&self) -> NodeInNetwork<Model, Source> {
        self.new_node((), |_, _| {})
    }
}


// === Sampler ===

#[derive(Clone, Copy, Default)]
pub struct SamplerType;
pub type Sampler<Output = ()> = NodeTemplate<SamplerType, Output>;

impl<Model> Network<Model> {
    #[inline(always)]
    pub fn sampler<T>(&self) -> NodeInNetwork<Model, Sampler<T>> {
        self.new_node_with_init((), |_, _| {}, |node| node.sampler_count.update(|t| t + 1))
    }
}

// === Trace ===

impl<Model> Network<Model> {
    /// Pass all incoming events to the output. Print them to console.
    pub fn trace<T0: Data>(
        &self,
        source: impl Node<Output = T0>,
    ) -> NodeInNetwork<Model, Stream<T0>> {
        self.new_node((Listen(source),), move |event, _, t0| {
            println!("TRACE: {:?}", t0);
            event.emit(t0);
        })
    }

    /// Pass all incoming events to the output. Print them to console if [`cond`] is true.
    pub fn trace_if<T0: Data>(
        &self,
        src: impl Node<Output = T0>,
        cond: impl Node<Output = bool>,
    ) -> NodeInNetwork<Model, Stream<T0>> {
        self.new_node((Listen(src), Sample(cond)), move |event, _, src, cond| {
            if *cond {
                println!("TRACE: {:?}", src);
            }
            event.emit(src);
        })
    }
}


// === Trace ===

#[derive(Debug, Clone, Derivative)]
#[derivative(Default(bound = ""))]
pub struct DebugCollectData<T> {
    cell: Rc<OptRefCell<Vec<T>>>,
}

impl<T> DebugCollectData<T> {
    pub fn assert_eq(&self, expected: &[T])
    where T: PartialEq + Debug {
        let actual = self.cell.borrow();
        assert_eq!(actual.as_slice(), expected);
    }
}

impl<Model> Network<Model> {
    pub fn debug_collect<T0: Data + Clone + 'static>(
        &self,
        source: impl Node<Output = T0>,
    ) -> (NodeInNetwork<Model, Stream<Vec<T0>>>, DebugCollectData<T0>) {
        let data: DebugCollectData<T0> = default();
        let out = data.clone();
        let node = self.new_node((Listen(source),), move |event, _, t0| {
            data.cell.borrow_mut().push(t0.clone());
            event.emit(&*data.cell.borrow());
        });
        (node, out)
    }
}



// === Map2 ===

impl<'a, Model, N1> NodeInNetwork<'a, Model, N1>
where
    Model: 'static,
    N1: Node,
{
    #[inline(never)]
    pub fn map<F, Output>(self, f: F) -> NodeInNetwork<'a, Model, Stream<Output>>
    where
        Output: Data,
        F: 'static + Fn(&mut Model, &N1::Output) -> Output, {
        self.network.new_node_with_model((Listen(self),), move |event, m, t1| event.emit(&f(m, t1)))
    }

    // #[inline(never)]
    // pub fn map2<N2, F, Output>(self, n2: N2, f: F) -> NodeInNetwork<'a, Model, Stream<Output>>
    // where
    //     N2: NodeWithDefaultOutput,
    //     Output: Data,
    //     F: 'static + Fn(&mut Model, &N1::Output, &N2::Output) -> Output, {
    //     self.network.new_node_with_model((Listen(self), Sample(n2)), move |event, m, t1, t2| {
    //         event.emit(&f(m, t1, t2))
    //     })
    // }

    #[inline(never)]
    pub fn map_<F, Output>(self, f: F) -> NodeInNetwork<'a, Model, Stream<Output>>
    where
        Output: Data,
        F: 'static + Fn(&N1::Output) -> Output, {
        self.network.new_node((Listen(self),), move |event, _, t1| event.emit(&f(t1)))
    }

    #[inline(never)]
    pub fn map2_<N2, F, Output>(self, n2: N2, f: F) -> NodeInNetwork<'a, Model, Stream<Output>>
    where
        N2: NodeWithDefaultOutput,
        Output: Data,
        F: 'static + Fn(&N1::Output, &N2::Output) -> Output, {
        self.network
            .new_node((Listen(self), Sample(n2)), move |event, _, t1, t2| event.emit(&f(t1, t2)))
    }
}
//
macro_rules! def_map_nodes {
    ($($is:literal),*) => {
        def_map_nodes! { @ [[1 []]] $($is)* }
    };

    (@ [ [$i:tt [$($is:tt)*]] $($iss:tt)* ] $n:tt $($ns:tt)*) => {
        def_map_nodes! { @ [ [$n [$($is)* $n]] [$i [$($is)*]] $($iss)* ] $($ns)* }
    };

    (@ [ [$i:tt [$($is:tt)*]] $($iss:tt)* ]) => {
        def_map_nodes! { @ [ $($iss)* ] }
        paste! { def_map_nodes! { # $i [$([<N $is>])*] } }
    };

    (@ []) => {};

    (# $i:tt [$($ns:tt)*]) => { paste! {
        #[inline(never)]
        pub fn [<map $i>] < $($ns,)* F, Output>
        (self, $($ns:$ns,)* f: F) -> NodeInNetwork<'a, Model, Stream<Output>>
        where
            $($ns: NodeWithDefaultOutput,)*
            Output: Data,
            F: 'static + Fn(&mut Model, &N1::Output, $(&$ns::Output,)*) -> Output, {
            self.network.new_node_with_model((Listen(self), $(Sample($ns),)*),
                move |event, m, t1, $($ns,)*| { event.emit(&f(m, t1, $($ns,)*)) }
            )
        }
    }};
}

impl<'a, Model, N1> NodeInNetwork<'a, Model, N1>
where
    Model: 'static,
    N1: Node,
{
    def_map_nodes![2, 3];
}

trait EventConsumer = Fn(&Runtime, &NodeData, &dyn Data) + 'static;



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::callstack::Location;

    #[test]
    fn test() {
        let net = Network_::new();
        let src1 = net.source::<usize>();
        let src2 = net.source::<usize>();
        let m1 = src1.map2_(src2, |a, b| 10 * a + b);
        let (_, results) = net.debug_collect(m1);
        results.assert_eq(&[]);
        src1.emit(&1);
        results.assert_eq(&[10]);
        src2.emit(&2);
        results.assert_eq(&[10]);
        src1.emit(&3);
        results.assert_eq(&[10, 32]);
    }

    #[test]
    fn test_network_drop() {
        let net1 = Network_::new();
        let net1_src = net1.source::<usize>();
        let net1_tgt = net1_src.map_(|t| t + 1);
        let (_, net1_results) = net1.debug_collect(net1_tgt);

        let net2 = Network_::new();
        let net2_tgt = net1_src.map_(|t| t * 3);
        let (_, net2_results) = net2.debug_collect(net2_tgt);

        net1_results.assert_eq(&[]);
        net2_results.assert_eq(&[]);
        net1_src.emit(&1);
        net1_results.assert_eq(&[2]);
        net2_results.assert_eq(&[3]);

        drop(net2);
        net1_src.emit(&2);
        net1_results.assert_eq(&[2, 3]);
        net2_results.assert_eq(&[3]);
    }

    #[test]
    fn test2() {
        for i in 0..10 {
            let net = Network_::new();
            let src1 = net.source::<usize>();
            // let src2 = net.source::<usize>();
            // let m1 = net.map2(src1, src2, map_fn);
        }
    }
}


pub fn pub_bench() {
    let net = Network_::new();
    let n1 = net.source::<usize>();
    let n2 = n1.map_(|t| t + 1);
    let mut prev = n2;
    for _ in 0..100_0 {
        let next = prev.map_(|t| t + 1);
        prev = next;
    }

    let _keep = &net;
    for i in 0..1_000 {
        n1.emit(&i);
    }
}

pub fn pub_bench_old() {
    let net = frp_old::Network::new("label");
    frp_old::extend! { net
        n1 <- source::<usize>();
        n2 <- map(&n1, |t| t + 1);
    }
    let mut prev = n2;
    for _ in 0..100_0 {
        frp_old::extend! { net
            next <- map(&prev, |t| t + 1);
        }
        prev = next;
    }

    let _keep = &net;
    for i in 0..1_000 {
        n1.emit(i);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use test::Bencher;

    const REPS: usize = 100_000;

    fn map_fn(a: &usize, b: &usize) -> usize {
        if *a > 100 {
            10 * a + b
        } else {
            5 * a + 2 * b
        }
    }

    #[bench]
    fn bench_emit_plain_fn(bencher: &mut Bencher) {
        let mut sum = 0;
        bencher.iter(move || {
            for i in 0..REPS {
                sum += map_fn(&i, &i);
            }
            assert_ne!(sum, 0);
        });
    }

    // 3739029
    // 1744908
    // 1661864
    // 1595170
    // 1604364
    // 1493188
    // 1454128
    #[bench]
    fn bench_emit_frp_pod(bencher: &mut Bencher) {
        let net = Network_::new();
        let src1 = net.source::<usize>();
        let src2 = net.source::<usize>();
        let m1 = src1.map2_(src2, map_fn);
        src2.emit(&2);

        bencher.iter(move || {
            for i in 0..REPS {
                src1.emit(&i);
            }
        });
    }

    // 1036659
    #[bench]
    fn bench_emit_frp_pod_old(bencher: &mut Bencher) {
        let net = frp_old::Network::new("label");
        frp_old::extend! { net
            src1 <- source::<usize>();
            src2 <- source::<usize>();
            m1 <- map2(&src1, &src2, map_fn);
        }
        src2.emit(2);

        bencher.iter(move || {
            let _keep = &net;
            for i in 0..REPS {
                src1.emit(i);
            }
        });
    }

    // 10:   5047258
    // 50:  47114087
    #[bench]
    fn bench_emit_frp_chain_pod(bencher: &mut Bencher) {
        let net = Network_::new();
        let n1 = net.source::<usize>();
        let n2 = n1.map_(|t| t + 1);
        let mut prev = n2;
        for _ in 0..8 {
            let next = prev.map_(|t| t + 1);
            prev = next;
        }

        bencher.iter(move || {
            for i in 0..REPS {
                n1.emit(&i);
            }
        });
    }

    // 5: 2843612
    // 10: 6370118
    // 50: 79284775
    #[bench]
    fn bench_emit_frp_chain_pod_old(bencher: &mut Bencher) {
        let net = frp_old::Network::new("label");
        frp_old::extend! { net
            n1 <- source::<usize>();
            n2 <- map(&n1, |t| t + 1);
        }
        let mut prev = n2;
        for _ in 0..8 {
            frp_old::extend! { net
                next <- map(&prev, |t| t + 1);
            }
            prev = next;
        }

        bencher.iter(move || {
            let _keep = &net;
            for i in 0..REPS {
                n1.emit(i);
            }
        });
    }

    // 1488912
    #[bench]
    fn bench_emit_non_pod_frp(bencher: &mut Bencher) {
        let net = Network_::new();
        let src1 = net.source::<String>();
        let src2 = net.source::<usize>();
        let m1 = src1.map2_(src2, |s, i| s.len() + i);
        src2.emit(&2);

        bencher.iter(move || {
            let str = "test".to_owned();
            for i in 0..REPS {
                src1.emit(&str);
            }
        });
    }

    // 1077895
    #[bench]
    fn bench_emit_frp_non_pod_old(bencher: &mut Bencher) {
        let net = frp_old::Network::new("label");
        frp_old::extend! { net
            src1 <- source::<Rc<String>>();
            src2 <- source::<usize>();
            m1 <- map2(&src1, &src2, |s, i| s.len() + i);
        }
        src2.emit(2);

        bencher.iter(move || {
            let str = Rc::new("test".to_owned());
            let _keep = &net;
            for i in 0..REPS {
                src1.emit(&str);
            }
        });
    }

    // 1450107 // Including removal of frp network taking 60% of time.
    #[bench]
    fn bench_create_frp(bencher: &mut Bencher) {
        bencher.iter(move || {
            // with_runtime(|rt| rt.unsafe_clear());
            let net = Network_::new();
            for i in 0..100_000 {
                let src1 = net.source::<usize>();
                // let src2 = net.source::<usize>();
                // let m1 = src1.map2_(src2, map_fn);
            }
        });
    }

    // 8784825
    #[bench]
    fn bench_create_frp_old(bencher: &mut Bencher) {
        bencher.iter(move || {
            let net = frp_old::Network::new("label");
            for i in 0..100_000 {
                frp_old::extend! { net
                    src1 <- source::<usize>();
                }
                // let src2 = net.source::<usize>();
                // let m1 = net.map2(src1, src2, map_fn);
            }
        });
    }

    // 42284
    #[bench]
    fn bench_push_to_vector(bencher: &mut Bencher) {
        bencher.iter(move || {
            let mut vec = Vec::with_capacity(100_000);
            for i in 0..100_000 {
                vec.push(i);
            }
            assert_eq!(vec.len(), 100_000);
        });
    }


    #[bench]
    fn b1(bencher: &mut Bencher) {
        let mut vec = Vec::<Box<dyn Fn(usize) -> usize>>::new();
        bencher.iter(move || {
            for i in 0..100_000 {
                vec.push(Box::new(move |x| x + i));
            }
        });
    }

    #[bench]
    fn b2(bencher: &mut Bencher) {
        let mut vec = Vec::<Box<dyn FF2>>::new();
        bencher.iter(move || {
            for i in 0..100_000 {
                vec.push(Box::new(S(i)));
            }
        });
    }

    // 93085
    // 42930
    // 57315
    // 45130
}

// 4835920
// 4134314
// 4146143

// 2107576
// 2102113
// 2280935

// 3039606
// 3411847
// 1941622



type FF = dyn Fn(usize) -> usize;

pub struct S(usize);

trait FF2 {
    fn ff(&self, x: usize) -> usize;
}

impl FF2 for S {
    fn ff(&self, x: usize) -> usize {
        self.0 + x
    }
}
