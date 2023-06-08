#![feature(allocator_api)]
#![feature(test)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(downcast_unchecked)]
#![feature(type_alias_impl_trait)]
#![feature(core_intrinsics)]

mod callstack;

pub use enso_prelude as prelude;

use enso_data_structures::unrolled_linked_list::UnrolledLinkedList;
use enso_data_structures::unrolled_slot_map::UnrolledSlotMap;
use enso_data_structures::unrolled_slot_map::VersionedIndex;
use enso_generics::traits::*;
use enso_prelude::*;
use slotmap::SlotMap;
use smallvec::SmallVec;

use crate::callstack::CallStack;
use crate::callstack::DefInfo;

use enso_frp as frp_old;
// use std::any::Any as Data;

// #[derive(Clone, Copy, Debug)]
// pub struct Data(usize);



// pub trait Data = Any;
pub trait Data: Debug {
    fn boxed_clone(&self) -> Box<dyn Data>;
}


impl<T> Data for T
where T: Debug + Clone + 'static
{
    fn boxed_clone(&self) -> Box<dyn Data> {
        Box::new(self.clone())
    }
}

// ===============
// === Metrics ===
// ===============

#[derive(Default, Clone)]
pub struct Metrics {
    pub total_networks:      Cell<u64>,
    pub total_network_refs:  Cell<u64>,
    pub total_networks_ever: Cell<u64>,
    pub total_nodes:         Cell<u64>,
    pub total_nodes_ever:    Cell<u64>,
}

impl std::fmt::Debug for Metrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Metrics")
            .field("total_networks", &self.total_networks.get())
            .field("total_network_refs", &self.total_network_refs.get())
            .field("total_networks_ever", &self.total_networks_ever.get())
            .field("total_nodes", &self.total_nodes.get())
            .field("total_nodes_ever", &self.total_nodes_ever.get())
            .finish()
    }
}

impl Metrics {
    fn inc_networks(&self) {
        self.total_networks.set(self.total_networks.get().wrapping_add(1));
        self.total_networks_ever.set(self.total_networks_ever.get().wrapping_add(1));
    }

    fn dec_networks(&self) {
        self.total_networks.set(self.total_networks.get().wrapping_sub(1));
    }

    fn inc_network_refs(&self) {
        self.total_network_refs.set(self.total_network_refs.get().wrapping_add(1));
    }

    fn dec_network_refs(&self) {
        self.total_network_refs.set(self.total_network_refs.get().wrapping_sub(1));
    }

    fn inc_nodes(&self) {
        self.total_nodes.set(self.total_nodes.get().wrapping_add(1));
        self.total_nodes_ever.set(self.total_nodes_ever.get().wrapping_add(1));
    }

    fn sub_nodes(&self, dec: u64) {
        self.total_nodes.set(self.total_nodes.get().wrapping_sub(dec));
    }
}



struct NetworkData {
    nodes: Vec<NodeId>,
    refs:  usize, // FIXME: remove
}



#[derive(Clone, Copy, Debug, Default, Zeroable)]
pub struct Edge {
    pub target:     NodeId,
    pub is_sampler: bool,
}

#[derive(Default, Zeroable)]
pub struct NodeData {
    tp:              ZeroableOption<Box<dyn EventConsumer>>,
    network_id:      NetworkId,
    def:             DefInfo,
    inputs:          OptRefCell<UnrolledLinkedList<NodeId, 8, usize, prealloc::Zeroed>>,
    /// Outputs to which an event should be emitted. This includes both [`Listener`] and
    /// [`ListenerAndSampler`] connections.
    outputs:         OptRefCell<UnrolledLinkedList<Edge, 8, usize, prealloc::Zeroed>>,
    /// Outputs to which an event should NOT be emitted. This includes only [`Sampler`]
    /// connections.
    sampler_outputs: OptRefCell<UnrolledLinkedList<Edge, 8, usize, prealloc::Zeroed>>,
    output_cache:    OptRefCell<ZeroableOption<Box<dyn Data>>>,
    /// Number of samplers connected to this nodes. This includes both the count of
    /// [`ListenerAndSampler`] connections in [`Self::outputs`] and the count of all connections in
    /// [`Self::sampler_outputs`].
    sampler_count:   Cell<usize>,
    // WARNING!
    // When adding new fields, be sure that they are correctly handled by the [`ImClearable`] and
    // [`Reusable`] trait implementations.
}

impl ImClearable for NodeData {
    fn clear_im(&self) {
        self.inputs.borrow_mut().clear();
        self.outputs.borrow_mut().clear();
        self.sampler_outputs.borrow_mut().clear();
        self.output_cache.replace(default());
        self.sampler_count.set(0);
    }
}

impl Reusable for NodeData {
    type Args = (ZeroableOption<Box<dyn EventConsumer>>, NetworkId, DefInfo);
    fn reuse(&mut self, args: Self::Args) {
        self.tp = args.0;
        self.network_id = args.1;
        self.def = args.2;
    }
}

impl Debug for NodeData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeData")
    }
}

impl NodeData {
    // fn new(tp: impl EventConsumer + 'static, network_id: NetworkId, def: DefInfo) -> Self {
    //     Self {
    //         tp: ZeroableOption::Some(Box::new(tp)),
    //         network_id,
    //         def,
    //         inputs: default(),
    //         outputs: default(),
    //         output: default(),
    //         sampler_count: default(),
    //     }
    // }

    #[inline(always)]
    fn on_event(&self, runtime: &Runtime, event: &dyn Data) {
        // println!("on_event: {:?}", event);
        match &self.tp {
            ZeroableOption::Some(f) => f(runtime, self, event),
            ZeroableOption::None => {}
        }
    }
}
slotmap::new_key_type! { pub struct NetworkId; }

// FIXME: this is wrong!
unsafe impl Zeroable for NetworkId {}

// slotmap::new_key_type! { pub struct NodeId; }

type TargetsVec = SmallVec<[NodeId; 1]>;



// ===============
// === Runtime ===
// ===============

thread_local! {
    static RUNTIME: Runtime  = default();
}

#[inline(always)]
pub fn with_runtime<T>(f: impl FnOnce(&Runtime) -> T) -> T {
    RUNTIME.with(|runtime| f(runtime))
}

// fn rc_runtime() -> Rc<Runtime> {
//     RUNTIME.with(|runtime| runtime.clone())
// }

pub struct NodesMap;

#[derive(Default)]
pub struct Runtime {
    networks: OptRefCell<SlotMap<NetworkId, NetworkData>>,
    nodes:    UnrolledSlotMap<OptRefCell<NodeData>, 131072, NodesMap, prealloc::Zeroed>,
    // consumers:         RefCell<SecondaryMap<NodeId, BumpRc<dyn RawEventConsumer>>>,
    /// Map for each node to its behavior, if any. For nodes that are natively behaviors, points to
    /// itself. For non-behavior streams, may point to a sampler node that listens to it.
    // behaviors:         RefCell<SecondaryMap<NodeId, NodeId>>,
    /// For each node that is a behavior, stores the current value of the behavior. A node can be
    /// checked for being a behavior by checking if it is present in this map.
    // behavior_values:   RefCell<SecondaryMap<NodeId, BumpRc<dyn Data>>>,
    // targets: RefCell<SecondaryMap<NodeId, TargetsVec>>,
    // target_store_pool: RefCell<Vec<TargetsVec>>,
    metrics: Metrics,
    stack:    CallStack,
    // current_node: Cell<NodeId>,
}

impl Runtime {
    // fn unsafe_clear(&self) {
    //     self.networks.borrow_mut().clear();
    //     self.nodes.clear();
    //     // self.metrics = Metrics::default();
    //     // self.stack.clear();
    //     // self.current_node.set(default());
    // }

    #[inline(always)]
    fn new_network(&self) -> NetworkId {
        self.metrics.inc_networks();
        self.metrics.inc_network_refs();
        let mut networks = self.networks.borrow_mut();
        networks.insert(NetworkData { nodes: Vec::new(), refs: 1 })
    }

    fn drop_network(&self, id: NetworkId) {
        let net_data = self.networks.borrow_mut().remove(id);
        if let Some(net_data) = net_data {
            for node_id in net_data.nodes {
                self.drop_node(node_id);
            }
        }
    }

    fn drop_node(&self, id: NodeId) {
        if let Some(node) = self.nodes.get(id) {
            node.borrow().clear_im();
            self.nodes.invalidate(id);
        }
    }

    #[inline(always)]
    fn new_node(
        &self,
        tp: impl EventConsumer + 'static,
        net_id: NetworkId,
        def: DefInfo,
    ) -> NodeId {
        self.metrics.inc_nodes();
        let mut networks = self.networks.borrow_mut();
        if let Some(network) = networks.get_mut(net_id) {
            let id = self.nodes.reserve();
            if let Some(node) = self.nodes.get(id) {
                node.borrow_mut().reuse((ZeroableOption::Some(Box::new(tp)), net_id, def))
            }
            network.nodes.push(id);
            id
        } else {
            panic!()
            // NodeId::null()
        }
    }

    #[inline(always)]
    fn connect(&self, src_id: impl Into<InputType>, tgt_id: NodeId) {
        let src_id = src_id.into();
        let is_listener = src_id.is_listener();
        let is_sampler = src_id.is_sampler();
        let output_connection = Edge { target: tgt_id, is_sampler };
        if let Some(src) = self.nodes.get(src_id.node_id()) {
            let src = src.borrow();
            if is_sampler {
                src.sampler_count.modify(|t| *t += 1);
            }
            let outputs = if is_listener { &src.outputs } else { &src.sampler_outputs };
            outputs.borrow().push(output_connection);
        }
        if let Some(tgt) = self.nodes.get(tgt_id) {
            tgt.borrow().inputs.borrow().push(src_id.node_id());
        }
    }

    #[inline(always)]
    fn unchecked_emit(&self, src_node_id: NodeId, event: &dyn Data) {
        if let Some(src_node) = self.nodes.get(src_node_id) {
            self.unchecked_emit_internal(&*src_node.borrow(), event);
        }
    }

    #[inline(always)]
    fn unchecked_emit_internal(&self, src_node: &NodeData, event: &dyn Data) {
        if src_node.sampler_count.get() > 0 {
            *src_node.output_cache.borrow_mut() = ZeroableOption::Some(event.boxed_clone());
        }

        self.stack.with(src_node.def, || {
            let mut cleanup_outputs = false;
            for &output in &*src_node.outputs.borrow() {
                let done = self.nodes.get(output.target).map(|tgt_node| {
                    tgt_node.borrow().on_event(self, event);
                });
                if done.is_none() {
                    cleanup_outputs = true;
                }
            }

            if cleanup_outputs {
                src_node.outputs.borrow_mut().retain(|output| {
                    let retain = self.nodes.exists(output.target);
                    if !retain && output.is_sampler {
                        src_node.sampler_count.update(|t| t - 1);
                    }
                    retain
                });
            }

            let mut cleanup_sampler_outputs = false;
            for &output in &*src_node.sampler_outputs.borrow() {
                if self.nodes.get(output.target).is_none() {
                    cleanup_sampler_outputs = true;
                }
            }

            if cleanup_sampler_outputs {
                src_node.sampler_outputs.borrow_mut().retain(|output| {
                    let retain = self.nodes.exists(output.target);
                    if !retain && output.is_sampler {
                        src_node.sampler_count.update(|t| t - 1);
                    }
                    retain
                });
            }
        });
    }

    #[inline(always)]
    unsafe fn with_borrowed_node_output_cache_coerced<T: Default>(
        &self,
        node_id: NodeId,
        f: impl FnOnce(&T),
    ) {
        if let Some(node) = self.nodes.get(node_id) {
            let borrowed_node = node.borrow();
            let output = borrowed_node.output_cache.borrow();
            if let Some(output) = output.as_ref().map(|t| &**t) {
                let output_coerced = unsafe { &*(output as *const dyn Data as *const T) };
                f(output_coerced)
            } else {
                f(&default())
            };
        }
    }
}



// ===============
// === Network ===
// ===============

#[derive(Debug, Deref, Default)]
pub struct Network {
    rc: Rc<NetworkGuard>,
}

impl Network {
    pub fn new() -> Self {
        default()
    }
}

#[derive(Debug)]
pub struct NetworkGuard {
    pub(crate) id: NetworkId,
}

impl NetworkGuard {
    #[inline(never)]
    pub fn new() -> Self {
        let id = with_runtime(|rt| rt.new_network());
        NetworkGuard { id }
    }
}

impl Default for NetworkGuard {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for NetworkGuard {
    fn drop(&mut self) {
        with_runtime(|rt| rt.drop_network(self.id));
    }
}

pub trait Node: Copy {
    type Output;
    fn id(self) -> NodeId;
}

pub trait NodeWithDefaultOutput = Node where <Self as Node>::Output: Default;


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
        self.runtime.unchecked_emit_internal(self.node, value);
    }
}



// ======================
// === NodeDefinition ===
// ======================

/// Abstraction allowing easy FRP node definition. It generates appropriate logic based on the input
/// types. The [`Inputs`] is a tuple containing any supported combination of [`Listen`], [`Sample`],
/// and [`ListenAndSample`]. Refer to their docs to learn more about their meaning.
trait NodeDefinition<Output, F> {
    fn def_node<Kind>(net: &Network, inputs: Self, f: F) -> NodeTemplate<Kind, Output>;
}

impl Network {
    #[inline(always)]
    fn def_node<Kind, Inputs, Output, F>(&self, inps: Inputs, f: F) -> NodeTemplate<Kind, Output>
    where Inputs: NodeDefinition<Output, F> {
        Inputs::def_node(self, inps, f)
    }
}

impl<Output, F> NodeDefinition<Output, F> for ()
where F: Fn(EventContext<Output>) + 'static
{
    #[inline(always)]
    fn def_node<Kind>(net: &Network, _inputs: Self, f: F) -> NodeTemplate<Kind, Output> {
        net.new_node(move |rt: &Runtime, node: &NodeData, _: &dyn Data| {
            f(EventContext::unchecked_new(rt, node))
        })
    }
}

impl<N0, Output, F> NodeDefinition<Output, F> for (Listen<N0>,)
where
    N0: Node,
    F: Fn(EventContext<Output>, &N0::Output) + 'static,
{
    #[inline(always)]
    fn def_node<Kind>(net: &Network, inputs: Self, f: F) -> NodeTemplate<Kind, Output> {
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node(move |rt: &Runtime, node: &NodeData, data: &dyn Data| {
            let data = unsafe { &*(data as *const dyn Data as *const N0::Output) };
            f(EventContext::unchecked_new(rt, node), data);
        });
        with_runtime(|rt| inputs.field_iter(|input| rt.connect(*input, node.id)));
        node
    }
}

impl<N0, N1, Output, F> NodeDefinition<Output, F> for (Listen<N0>, Sample<N1>)
where
    N0: Node,
    N1: NodeWithDefaultOutput,
    F: Fn(EventContext<Output>, &N0::Output, &N1::Output) + 'static,
{
    #[inline(always)]
    fn def_node<Kind>(net: &Network, inputs: Self, f: F) -> NodeTemplate<Kind, Output> {
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node(move |rt: &Runtime, node: &NodeData, data: &dyn Data| unsafe {
            let t0 = &*(data as *const dyn Data as *const N0::Output);
            rt.with_borrowed_node_output_cache_coerced(inputs.1.node_id(), |t1| {
                f(EventContext::unchecked_new(rt, node), t0, t1)
            });
        });
        with_runtime(|rt| inputs.field_iter(|input| rt.connect(*input, node.id)));
        node
    }
}

impl<N0, N1, N2, Output, F> NodeDefinition<Output, F> for (Listen<N0>, Sample<N1>, Sample<N2>)
where
    N0: Node,
    N1: NodeWithDefaultOutput,
    N2: NodeWithDefaultOutput,
    F: Fn(EventContext<Output>, &N0::Output, &N1::Output, &N2::Output) + 'static,
{
    #[inline(always)]
    fn def_node<Kind>(net: &Network, inputs: Self, f: F) -> NodeTemplate<Kind, Output> {
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node(move |rt: &Runtime, node: &NodeData, data: &dyn Data| unsafe {
            let t0 = &*(data as *const dyn Data as *const N0::Output);
            rt.with_borrowed_node_output_cache_coerced(inputs.1.node_id(), |t1| {
                rt.with_borrowed_node_output_cache_coerced(inputs.2.node_id(), |t2| {
                    f(EventContext::unchecked_new(rt, node), t0, t1, t2)
                })
            });
        });
        with_runtime(|rt| inputs.field_iter(|input| rt.connect(*input, node.id)));
        node
    }
}

// === NodeTemplate ===

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
    // #[inline(always)]
    // fn new_template(tp: impl EventConsumer + 'static, network_id: NetworkId) -> Self {
    //     let _marker = PhantomData;
    //     let id = with_runtime(|rt| rt.new_node(tp, network_id, DefInfo::unlabelled()));
    //     Self { _marker, id }
    // }

    #[inline(never)]
    pub fn emit(&self, value: &Output)
    where Output: Data {
        with_runtime(|rt| rt.unchecked_emit(self.id, value));
    }
}

impl Network {
    #[inline(always)]
    fn new_node<Kind, Output>(
        &self,
        tp: impl EventConsumer + 'static,
    ) -> NodeTemplate<Kind, Output> {
        let _marker = PhantomData;
        let id = with_runtime(|rt| rt.new_node(tp, self.id, DefInfo::unlabelled()));
        NodeTemplate { _marker, id }
    }
}


// === Stream ===

pub struct StreamType;

pub type Stream<Output> = NodeTemplate<StreamType, Output>;


// === Source ===

#[derive(Clone, Copy, Default)]
pub struct SourceType;
pub type Source<Output> = NodeTemplate<SourceType, Output>;

impl Network {
    #[inline(always)]
    pub fn source<T>(&self) -> Source<T> {
        self.def_node((), |_| {})
    }
}


// === Trace ===

impl Network {
    pub fn trace<T0: Data>(&self, source: impl Node<Output = T0>) -> Stream<T0> {
        self.def_node((Listen(source),), move |event, t0| {
            println!("TRACE: {:?}", t0);
            event.emit(t0);
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

impl Network {
    pub fn debug_collect<T0: Data + Clone + 'static>(
        &self,
        source: impl Node<Output = T0>,
    ) -> (Stream<Vec<T0>>, DebugCollectData<T0>) {
        let data: DebugCollectData<T0> = default();
        let out = data.clone();
        let node = self.def_node((Listen(source),), move |event, t0| {
            data.cell.borrow_mut().push(t0.clone());
            event.emit(&*data.cell.borrow());
        });
        (node, out)
    }
}



// === Map2 ===

impl Network {
    #[inline(never)]
    pub fn map<T0, Output: Data>(
        &self,
        n0: impl Node<Output = T0>,
        f: impl Fn(&T0) -> Output + 'static,
    ) -> Stream<Output> {
        self.def_node((Listen(n0),), move |event, t0| event.emit(&f(t0)))
    }

    #[inline(never)]
    pub fn map2<T0, T1: Default, Output: Data>(
        &self,
        n0: impl Node<Output = T0>,
        n1: impl Node<Output = T1>,
        f: impl Fn(&T0, &T1) -> Output + 'static,
    ) -> Stream<Output> {
        self.def_node((Listen(n0), Sample(n1)), move |event, t0, t1| event.emit(&f(t0, t1)))
    }
}


// struct EventData<'a> {
//     pub runtime:     &'a Runtime,
//     pub data:        &'a NodeData,
//     pub source_id:   NodeId,
//     pub source_edge: Edge,
// }

trait EventConsumer = Fn(&Runtime, &NodeData, &dyn Data);



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::callstack::Location;

    #[test]
    fn test() {
        let net = Network::new();
        let src1 = net.source::<usize>();
        let src2 = net.source::<usize>();
        let m1 = net.map2(src1, src2, |a, b| 10 * a + b);
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
        let net1 = Network::new();
        let net1_src = net1.source::<usize>();
        let net1_tgt = net1.map(net1_src, |t| t + 1);
        let (_, net1_results) = net1.debug_collect(net1_tgt);

        let net2 = Network::new();
        let net2_tgt = net2.map(net1_src, |t| t * 3);
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
            let net = Network::new();
            let src1 = net.source::<usize>();
            // let src2 = net.source::<usize>();
            // let m1 = net.map2(src1, src2, map_fn);
        }
    }
}


pub fn pub_bench() {
    let net = Network::new();
    let n1 = net.source::<usize>();
    let n2 = net.map(n1, |t| t + 1);
    let mut prev = n2;
    for _ in 0..100_0 {
        let next = net.map(prev, |t| t + 1);
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
        let net = Network::new();
        let src1 = net.source::<usize>();
        let src2 = net.source::<usize>();
        let m1 = net.map2(src1, src2, map_fn);
        src2.emit(&2);

        bencher.iter(move || {
            let _keep = &net;
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

    // 10:   6297170
    // 50:  62830175
    #[bench]
    fn bench_emit_frp_chain_pod(bencher: &mut Bencher) {
        let net = Network::new();
        let n1 = net.source::<usize>();
        let n2 = net.map(n1, |t| t + 1);
        let mut prev = n2;
        for _ in 0..8 {
            let next = net.map(prev, |t| t + 1);
            prev = next;
        }

        bencher.iter(move || {
            let _keep = &net;
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
        let net = Network::new();
        let src1 = net.source::<String>();
        let src2 = net.source::<usize>();
        let m1 = net.map2(src1, src2, |s, i| s.len() + i);
        src2.emit(&2);

        bencher.iter(move || {
            let str = "test".to_owned();
            let _keep = &net;
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

    // 694589
    #[bench]
    fn bench_create_frp(bencher: &mut Bencher) {
        bencher.iter(move || {
            // with_runtime(|rt| rt.unsafe_clear());
            let net = Network::new();
            for i in 0..100_000 {
                let src1 = net.source::<usize>();
                // let src2 = net.source::<usize>();
                // let m1 = net.map2(src1, src2, map_fn);
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

pub type NodeId = VersionedIndex<NodesMap>;
