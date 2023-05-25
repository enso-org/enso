use bumpalo::Bump;
use enso_prelude::*;
use ouroboros::self_referencing;
use slotmap::Key;
use slotmap::SecondaryMap;
use slotmap::SlotMap;
use smallvec::SmallVec;
use std::cell::UnsafeCell;
use std::process::Output;

use crate::callstack::CallStack;
use crate::callstack::DefInfo;

pub use enso_prelude as prelude;

use enso_frp as frp_old;
use enso_frp::prelude::bytemuck::ZeroableInOption;
// use std::any::Any as Data;

// #[derive(Clone, Copy, Debug)]
// pub struct Data(usize);



#[derive(Default, Zeroable)]
#[repr(transparent)]
pub struct OptRefCell<T> {
    inner: UnsafeCell<T>,
}

impl<T> OptRefCell<T> {
    #[inline(always)]
    pub fn borrow(&self) -> &T {
        unsafe { &*self.inner.get() }
    }

    #[inline(always)]
    pub fn borrow_mut(&self) -> &mut T {
        unsafe { &mut *self.inner.get() }
    }

    #[inline(always)]
    pub fn with_borrowed<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        f(self.borrow())
    }

    #[inline(always)]
    pub fn with_borrowed_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        f(self.borrow_mut())
    }
}

impl<T: Debug> Debug for OptRefCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.borrow(), f)
    }
}


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
    refs:  usize,
}



#[derive(Clone, Copy, Debug, Default, Zeroable)]
pub struct OutputConnection {
    pub target:           NodeId,
    pub behavior_request: bool,
}

#[derive(Default, Zeroable)]
pub struct NodeData {
    tp:                ZeroableOption<Box<dyn EventConsumer>>,
    network_id:        NetworkId,
    def:               DefInfo,
    inputs:            LinkedCellArray<NodeId, 8>,
    outputs:           LinkedCellArray<OutputConnection, 8>,
    output:            OptRefCell<ZeroableOption<Box<dyn Data>>>,
    behavior_requests: Cell<usize>,
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
    //         behavior_requests: default(),
    //     }
    // }

    #[inline(always)]
    fn on_event(
        &self,
        runtime: &Runtime,
        source_id: NodeId,
        source_edge: OutputConnection,
        event: &dyn Data,
    ) {
        // println!("on_event: {:?}", event);
        match &self.tp {
            ZeroableOption::Some(f) => f(runtime, self, source_id, source_edge, event),
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


#[derive(Default)]
pub struct Runtime {
    networks:     OptRefCell<SlotMap<NetworkId, NetworkData>>,
    nodes:        CellSlotMap,
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
    stack:        CallStack,
    current_node: Cell<NodeId>,
}

impl Runtime {
    fn unsafe_clear(&self) {
        self.networks.borrow_mut().clear();
        self.nodes.clear();
        // self.metrics = Metrics::default();
        // self.stack.clear();
        self.current_node.set(default());
    }

    #[inline(always)]
    fn new_network(&self) -> NetworkId {
        self.metrics.inc_networks();
        self.metrics.inc_network_refs();
        let mut networks = self.networks.borrow_mut();
        networks.insert(NetworkData { nodes: Vec::new(), refs: 1 })
    }

    #[inline(always)]
    fn new_node(
        &self,
        tp: impl EventConsumer + 'static,
        network_id: NetworkId,
        def: DefInfo,
    ) -> NodeId {
        self.metrics.inc_nodes();
        let mut networks = self.networks.borrow_mut();
        if let Some(network) = networks.get_mut(network_id) {
            let id = self.nodes.insert_default();
            self.nodes.with_item_borrow_mut(id, |node| {
                node.tp = ZeroableOption::Some(Box::new(tp));
                node.network_id = network_id;
                node.def = def;
            });
            network.nodes.push(id);
            id
        } else {
            panic!()
            // NodeId::null()
        }
    }

    #[inline(always)]
    fn connect(&self, src_id: NodeId, tgt_id: NodeId, behavior_request: bool) {
        let output_connection = OutputConnection { target: tgt_id, behavior_request };
        self.nodes.with_item_borrow_mut(src_id, |src| {
            if behavior_request {
                src.behavior_requests.modify(|t| *t += 1);
            }
            src.outputs.push(output_connection)
        });
        self.nodes.with_item_borrow_mut(tgt_id, |tgt| tgt.inputs.push(src_id));
    }

    #[inline(always)]
    fn unchecked_emit(&self, node_id: NodeId, event: &dyn Data) {
        // println!("unchecked_emit {:?} {:?}", node_id, event);
        // println!("unchecked_emit {:?} {:?}", node_id, event);
        self.nodes.with_item_borrow(node_id, |node| {
            if node.behavior_requests.get() > 0 {
                *node.output.borrow_mut() = ZeroableOption::Some(event.boxed_clone());
            }

            let cleanup_targets = self.stack.with(node.def, || {
                let mut cleanup_targets = false;
                node.outputs.for_item_borrow(|&node_output_id| {
                    let done = self.nodes.with_item_borrow(node_output_id.target, |node_output| {
                        node_output.on_event(self, node_id, node_output_id, event);
                    });
                    if done.is_none() {
                        cleanup_targets = true;
                    }
                });

                cleanup_targets
            });
            //
            // Return temporary buffer to pool.
            // node_outputs.clear();
            // self.target_store_pool.borrow_mut().push(node_outputs);
        });
        //
        // // Remove targets that have been dropped.
        // if cleanup_targets {
        //     let mut targets = self.targets.borrow_mut();
        //     let node_targets = targets.get_mut(node_id);
        //     if let Some(targets) = node_targets {
        //         let consumers = self.consumers.borrow();
        //         targets.retain(|target| consumers.contains_key(*target));
        //     }
        // }
    }

    #[inline(always)]
    fn unchecked_emit2(&self, node_id: NodeId, node: &NodeData, event: &dyn Data) {
        // println!("unchecked_emit {:?} {:?}", node_id, event);
        // println!("unchecked_emit {:?} {:?}", node_id, event);
        if node.behavior_requests.get() > 0 {
            *node.output.borrow_mut() = ZeroableOption::Some(event.boxed_clone());
        }

        self.stack.with(node.def, || {
            let mut index = 0;
            let mut cleanup_targets = vec![];
            node.outputs.for_item_borrow(|&node_output_id| {
                let done = self.nodes.with_item_borrow(node_output_id.target, |node_output| {
                    node_output.on_event(self, node_id, node_output_id, event);
                });
                if done.is_none() {
                    cleanup_targets.push(index);
                }
                index += 1;
            });

            println!("CLEANUP TARGETS: {:?}", cleanup_targets);
        });
        //
        // Return temporary buffer to pool.
        // node_outputs.clear();
        // self.target_store_pool.borrow_mut().push(node_outputs);
        //
        // // Remove targets that have been dropped.
        // if cleanup_targets {
        //     let mut targets = self.targets.borrow_mut();
        //     let node_targets = targets.get_mut(node_id);
        //     if let Some(targets) = node_targets {
        //         let consumers = self.consumers.borrow();
        //         targets.retain(|target| consumers.contains_key(*target));
        //     }
        // }
    }

    #[inline(always)]
    unsafe fn with_borrowed_node_output_coerced<T: Default>(
        &self,
        node_id: NodeId,
        f: impl FnOnce(&T),
    ) {
        self.nodes.with_item_borrow(node_id, |node| {
            let output = node.output.borrow();
            if let ZeroableOption::Some(output) = output.as_ref().map(|t| &**t) {
                let output_coerced = unsafe { &*(output as *const dyn Data as *const T) };
                f(output_coerced)
            } else {
                f(&default())
            };
        });
    }
}



// ===============
// === Network ===
// ===============

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Network {
    pub(crate) id: NetworkId,
}

impl Network {
    #[inline(never)]
    pub fn new() -> Self {
        let id = with_runtime(|rt| rt.new_network());
        Network { id }
    }
}


pub trait Node: Copy {
    type Output;
    fn id(self) -> NodeId;
}


// pub trait N2: Copy {
// }
//
//
// #[derive(Derivative)]
// #[derivative(Copy(bound = ""))]
// #[derivative(Clone(bound = ""))]
// #[derivative(Debug(bound = ""))]
// pub struct NodeTemplate<Output> {
//     _output: PhantomData<Output>,
//     id:      NodeId,
// }
//


// ================================
// === Node Definition Generics ===
// ================================

#[derive(Debug, Clone, Copy, Deref, DerefMut)]
struct Event<T>(T);

#[derive(Debug, Clone, Copy, Deref, DerefMut)]
struct Behavior<T>(T);

trait NodeDefinition<Kind, Inputs, Output, F> {
    fn define_node(&self, inputs: Inputs, f: F) -> NodeTemplate<Kind, Output>;
}

impl<Kind, Output, F> NodeDefinition<Kind, (), Output, F> for &Network
where F: Fn(TypedNodeData<Output>, &NodeData, NodeId, OutputConnection) + 'static
{
    #[inline(always)]
    fn define_node(&self, _inputs: (), f: F) -> NodeTemplate<Kind, Output> {
        let node = NodeTemplate::new_template(
            move |runtime: &Runtime,
                  node_data: &NodeData,
                  source_id: NodeId,
                  source_edge: OutputConnection,
                  _event: &dyn Data| {
                f(xx_unchecked_into(runtime), node_data, source_id, source_edge)
            },
            self.id,
        );
        node
    }
}

impl<Kind, N0, T0, Output, F> NodeDefinition<Kind, Event<N0>, Output, F> for &Network
where
    N0: Node<Output = T0>,
    F: Fn(TypedNodeData<Output>, &NodeData, NodeId, OutputConnection, &T0) + 'static,
{
    #[inline(always)]
    fn define_node(&self, inputs: Event<N0>, f: F) -> NodeTemplate<Kind, Output> {
        let node = NodeTemplate::new_template(
            move |runtime: &Runtime,
                  node_data: &NodeData,
                  source_id: NodeId,
                  source_edge: OutputConnection,
                  event: &dyn Data| {
                let event = unsafe { &*(event as *const dyn Data as *const T0) };
                f(xx_unchecked_into(runtime), node_data, source_id, source_edge, event);
            },
            self.id,
        );
        with_runtime(|rt| rt.connect(inputs.0.id(), node.id, false));
        node
    }
}

impl<Kind, N0, N1, T0, T1: Default, Output, F>
    NodeDefinition<Kind, (Event<N0>, Behavior<N1>), Output, F> for &Network
where
    N0: Node<Output = T0>,
    N1: Node<Output = T1>,
    F: Fn(TypedNodeData<Output>, &NodeData, NodeId, OutputConnection, &T0, &T1) + 'static,
{
    #[inline(always)]
    fn define_node(&self, inputs: (Event<N0>, Behavior<N1>), f: F) -> NodeTemplate<Kind, Output> {
        let node = NodeTemplate::new_template(
            move |runtime: &Runtime,
                  node_data: &NodeData,
                  source_id: NodeId,
                  source_edge: OutputConnection,
                  event: &dyn Data| {
                if source_id == node_data.inputs.get(0) {
                    unsafe {
                        let t0 = &*(event as *const dyn Data as *const T0);
                        runtime.with_borrowed_node_output_coerced(node_data.inputs.get(1), |t1| {
                            f(xx_unchecked_into(runtime), node_data, source_id, source_edge, t0, t1)
                        });
                    }
                }
            },
            self.id,
        );
        with_runtime(|rt| rt.connect(inputs.0.id(), node.id, false));
        with_runtime(|rt| rt.connect(inputs.1.id(), node.id, true));
        node
    }
}

impl<Kind, N0, N1, N2, T0, T1: Default, T2: Default, Output, F>
    NodeDefinition<Kind, (Event<N0>, Behavior<N1>, Behavior<N2>), Output, F> for &Network
where
    N0: Node<Output = T0>,
    N1: Node<Output = T1>,
    N2: Node<Output = T2>,
    F: Fn(TypedNodeData<Output>, &NodeData, NodeId, OutputConnection, &T0, &T1, &T2) + 'static,
{
    #[inline(always)]
    fn define_node(
        &self,
        inputs: (Event<N0>, Behavior<N1>, Behavior<N2>),
        f: F,
    ) -> NodeTemplate<Kind, Output> {
        let node = NodeTemplate::new_template(
            move |runtime: &Runtime,
                  node_data: &NodeData,
                  source_id: NodeId,
                  source_edge: OutputConnection,
                  event: &dyn Data| {
                if source_id == node_data.inputs.get(0) {
                    unsafe {
                        let t0 = &*(event as *const dyn Data as *const T0);
                        runtime.with_borrowed_node_output_coerced(node_data.inputs.get(1), |t1| {
                            runtime.with_borrowed_node_output_coerced(
                                node_data.inputs.get(2),
                                |t2| {
                                    f(
                                        xx_unchecked_into(runtime),
                                        node_data,
                                        source_id,
                                        source_edge,
                                        t0,
                                        t1,
                                        t2,
                                    )
                                },
                            )
                        });
                    }
                }
            },
            self.id,
        );
        with_runtime(|rt| rt.connect(inputs.0.id(), node.id, false));
        with_runtime(|rt| rt.connect(inputs.1.id(), node.id, true));
        with_runtime(|rt| rt.connect(inputs.2.id(), node.id, true));
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
    #[inline(always)]
    fn new_template(tp: impl EventConsumer + 'static, network_id: NetworkId) -> Self {
        let _marker = PhantomData;
        let id = with_runtime(|rt| rt.new_node(tp, network_id, DefInfo::unlabelled()));
        Self { _marker, id }
    }

    #[inline(never)]
    pub fn emit(&self, value: &Output)
    where Output: Data {
        with_runtime(|rt| rt.unchecked_emit(self.id, value));
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
        self.define_node((), |_, _, _, _| {})
    }
}


// === Trace ===

impl Network {
    pub fn trace<T0: Data>(&self, source: impl Node<Output = T0>) -> Stream<T0> {
        self.define_node(Event(source), move |event, target, _, e, t0| {
            println!("TRACE: {:?}", t0);
            event.emit(e.target, target, t0);
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
        let node = self.define_node(Event(source), move |event, target, _, e, t0| {
            data.cell.borrow_mut().push(t0.clone());
            event.emit(e.target, target, &*data.cell.borrow());
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
        self.define_node(Event(n0), move |event, target, _, e, t0| {
            event.emit(e.target, target, &f(t0))
        })
    }

    #[inline(never)]
    pub fn map2<T0, T1: Default, Output: Data>(
        &self,
        n0: impl Node<Output = T0>,
        n1: impl Node<Output = T1>,
        f: impl Fn(&T0, &T1) -> Output + 'static,
    ) -> Stream<Output> {
        self.define_node((Event(n0), Behavior(n1)), move |event, target, _, e, t0, t1| {
            event.emit(e.target, target, &f(t0, t1))
        })
    }
}

// #[derive(Deref)]
#[repr(transparent)]
struct TypedNodeData<'a, Output> {
    runtime: &'a Runtime,
    // target:  NodeId,
    tp:      PhantomData<Output>,
}

impl<'a, Output: Data> TypedNodeData<'a, Output> {
    #[inline(always)]
    fn emit(self, target_id: NodeId, target: &NodeData, value: &Output) {
        self.runtime.unchecked_emit2(target_id, target, value);
    }
}

#[inline(always)]
fn xx_unchecked_into<'a, Output>(
    runtime: &'a Runtime,
    // target: NodeId,
) -> TypedNodeData<'a, Output> {
    TypedNodeData { runtime, tp: PhantomData } // target
}

// struct EventData<'a> {
//     pub runtime:     &'a Runtime,
//     pub data:        &'a NodeData,
//     pub source_id:   NodeId,
//     pub source_edge: OutputConnection,
// }

trait EventConsumer = Fn(&Runtime, &NodeData, NodeId, OutputConnection, &dyn Data);

// trait EventConsumer {
//     fn on_event(&self, event_data: EventData, event: &dyn Data);
// }
//
// impl<F> EventConsumer for F
// where F: Fn(EventData, &dyn Data)
// {
//     fn on_event(&self, event_data: EventData, event: &dyn Data) {
//         self(event_data, event)
//     }
// }



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

    // 10:   6290372
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
    // 50: 74304958
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
            with_runtime(|rt| rt.unsafe_clear());
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Zeroable)]
pub struct NodeId {
    index:   usize,
    version: usize,
}

#[derive(Debug, Default, Zeroable)]
pub struct Slot {
    value:   NodeData,
    version: usize,
}

#[derive(Debug, Default)]
pub struct CellSlotMap {
    free_indexes: OptRefCell<Vec<usize>>,
    list:         Box<LinkedCellArray<Slot, 131072>>,
}

impl CellSlotMap {
    #[inline(always)]
    pub fn new() -> Self {
        default()
    }

    #[inline(always)]
    pub fn clear(&self) {
        self.free_indexes.borrow_mut().clear();
        self.list.clear();
    }

    #[inline(always)]
    pub fn insert(&self, val: NodeData) -> NodeId {
        let mut free_indexes = self.free_indexes.borrow_mut();
        if let Some(index) = free_indexes.pop() {
            let version = self.list.with_item_borrow_mut(index, |slot| {
                slot.value = val;
                slot.version += 1;
                slot.version
            });
            NodeId { index, version }
        } else {
            let index = self.list.len();
            self.list.push(Slot { value: val, version: 0 });
            NodeId { index, version: 0 }
        }
    }

    #[inline(always)]
    pub fn insert_default(&self) -> NodeId {
        let mut free_indexes = self.free_indexes.borrow_mut();
        if let Some(index) = free_indexes.pop() {
            todo!()
            // let version = self.list.with_item_borrow_mut(index, |slot| {
            //     slot.value = val;
            //     slot.version += 1;
            //     slot.version
            // });
            // NodeId { index, version }
        } else {
            let index = self.list.len();
            self.list.push_default();
            NodeId { index, version: 0 }
        }
    }

    #[inline(always)]
    pub fn insert_at(&self, key: NodeId, val: NodeData) -> bool {
        self.list.with_item_borrow_mut(key.index, |slot| {
            if slot.version != key.version {
                false
            } else {
                slot.value = val;
                true
            }
        })
    }

    #[inline(always)]
    pub fn with_item_borrow<R>(&self, key: NodeId, f: impl FnOnce(&NodeData) -> R) -> Option<R> {
        self.list.with_item_borrow(key.index, |slot| {
            (slot.version == key.version).then(|| f(&slot.value))
        })
    }

    #[inline(always)]
    pub fn with_item_borrow_mut<R>(
        &self,
        key: NodeId,
        f: impl FnOnce(&mut NodeData) -> R,
    ) -> Option<R> {
        self.list.with_item_borrow_mut(key.index, |slot| {
            (slot.version == key.version).then(|| f(&mut slot.value))
        })
    }
}


// =================
// === LinkedVec ===
// =================

pub trait LinkedArrayDefault<T, const N: usize> = where [OptRefCell<T>; N]: Default;

#[derive(Debug)]
pub struct Segment<T, const N: usize> {
    items: Vec<OptRefCell<T>>,
    next:  OptRefCell<Option<Box<Segment<T, N>>>>,
}

impl<T: Zeroable, const N: usize> Default for Segment<T, N> {
    #[inline(always)]
    fn default() -> Self {
        let items = {
            let layout = std::alloc::Layout::array::<OptRefCell<T>>(N).unwrap();
            unsafe { Vec::from_raw_parts(std::alloc::alloc_zeroed(layout) as *mut _, N, N) }
        };
        let next = default();
        Self { items, next }
    }
}

#[derive(Debug, Derivative)]
#[derivative(Default(bound = "T: Default"))]
pub struct LinkedCellArray<T, const N: usize = 32> {
    size:          Cell<usize>,
    first_segment: OptRefCell<ZeroableOption<Segment<T, N>>>,
}

// FIXME: make it future sound-proof
unsafe impl<T, const N: usize> Zeroable for LinkedCellArray<T, N> {}

impl<T: Default + Zeroable + Debug, const N: usize> LinkedCellArray<T, N> {
    #[inline(always)]
    pub fn clear(&self) {
        self.size.set(0);
    }

    #[inline(always)]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.size.get()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.size.get() == 0
    }

    #[inline(always)]
    pub fn push(&self, elem: T) {
        self.with_first_segment(|s| s.push(self.size.get(), elem));
        self.size.modify(|t| *t += 1);
    }

    #[inline(always)]
    fn push_default(&self) -> usize {
        let index = self.size.get();
        if index % N == 0 {
            self.with_first_segment(|s| s.add_tail_segment());
        }
        self.size.modify(|t| *t += 1);
        index
    }
}

impl<T: Zeroable + Debug, const N: usize> LinkedCellArray<T, N> {
    #[inline(always)]
    fn with_first_segment<R>(&self, f: impl FnOnce(&Segment<T, N>) -> R) -> R {
        if self.first_segment.borrow().is_none() {
            *self.first_segment.borrow_mut() = ZeroableOption::Some(Segment::default());
        }
        f(&*self.first_segment.borrow().as_ref().unwrap())
    }

    #[inline(always)]
    fn with_first_segment_mut<R>(&self, f: impl FnOnce(&mut Segment<T, N>) -> R) -> R {
        if self.first_segment.borrow().is_none() {
            *self.first_segment.borrow_mut() = ZeroableOption::Some(Segment::default());
        }
        f(&mut *self.first_segment.borrow_mut().as_mut().unwrap())
    }

    #[inline(always)]
    pub fn set(&self, index: usize, elem: T) {
        self.with_first_segment(|s| s.set(index, elem));
    }

    #[inline(always)]
    pub fn get_cloned(&self, index: usize) -> T
    where T: Clone {
        self.with_item_borrow(index, Clone::clone)
    }

    #[inline(always)]
    pub fn with_item_borrow<U>(&self, index: usize, f: impl FnOnce(&T) -> U) -> U {
        self.with_first_segment(|s| s.with_item_borrow(index, f))
    }

    #[inline(always)]
    pub fn with_item_borrow_mut<U>(&self, index: usize, f: impl FnOnce(&mut T) -> U) -> U {
        self.with_first_segment(|s| s.with_item_borrow_mut(index, f))
    }

    #[inline(always)]
    pub fn for_item_borrow(&self, f: impl FnMut(&T)) {
        self.with_first_segment(|s| s.for_item_borrow(self.size.get(), f));
    }

    #[inline(always)]
    pub fn for_item_borrow_mut(&self, f: impl FnMut(&mut T)) {
        self.with_first_segment(|s| s.for_item_borrow_mut(self.size.get(), f));
    }

    pub fn retain<F>(&self, mut f: F)
    where F: FnMut(&T) -> bool {
        self.with_first_segment_mut(|s| {
            s.retain_stage1(self.size.get(), f);
            self.size.set(s.retain_stage2());
        })
    }

    pub fn to_vec(&self) -> Vec<T>
    where T: Clone {
        let mut vec = Vec::with_capacity(self.size.get());
        self.for_item_borrow(|t| vec.push(t.clone()));
        vec
    }
}

impl<T: Copy + Zeroable + Debug, const N: usize> LinkedCellArray<T, N> {
    #[inline(always)]
    pub fn get(&self, index: usize) -> T {
        self.with_item_borrow(index, |t| *t)
    }
}

impl<T: Zeroable + Debug, const N: usize> Segment<T, N> {
    #[inline(always)]
    fn push(&self, offset: usize, elem: T) {
        if offset < N {
            *self.items[offset].borrow_mut() = elem;
        } else {
            let no_next = self.next.borrow().is_none();
            if no_next {
                *self.next.borrow_mut() = Some(default());
            }
            self.next.with_borrowed(|t| t.as_ref().unwrap().push(offset - N, elem));
        }
    }

    #[inline(always)]
    fn add_tail_segment(&self) {
        let no_next = self.next.borrow().is_none();
        if no_next {
            *self.next.borrow_mut() = Some(default());
        } else {
            self.next.with_borrowed(|t| t.as_ref().unwrap().add_tail_segment());
        }
    }

    /// For each segment, retain the items. Segment lengths will be shortened if necessary, however,
    /// two adjacent not-full segments will not be merged. Merging will be performed in stage 2.
    #[inline(always)]
    pub fn retain_stage1<F>(&mut self, len: usize, mut f: F)
    where F: FnMut(&T) -> bool {
        if len < N {
            unsafe { self.items.set_len(len) }
        }
        self.items.retain(|t| f(&*t.borrow()));
        if let Some(next) = self.next.borrow_mut().as_mut() {
            next.retain_stage1(len - N, f);
        }
    }

    /// For each segment, merge two adjacent not-full segments.
    #[inline(always)]
    pub fn retain_stage2(&mut self) -> usize {
        while self.items.len() < N {
            let new_next_segment = self.next.borrow_mut().as_mut().and_then(|next| {
                let end = next.items.len().min(N - self.items.len());
                self.items.extend(next.items.drain(0..end));
                next.items.is_empty().then(|| mem::take(&mut *next.next.borrow_mut()))
            });
            if let Some(new_next_segment) = new_next_segment {
                *self.next.borrow_mut() = new_next_segment;
            }
            if self.next.borrow().is_none() {
                break;
            }
        }
        let len = self.items.len();
        for _ in len..N {
            self.items.push(Zeroable::zeroed());
        }
        len + self.next.borrow_mut().as_mut().map(|next| next.retain_stage2()).unwrap_or_default()
    }
}

impl<T, const N: usize> Segment<T, N> {
    #[inline(always)]
    fn set(&self, offset: usize, elem: T) {
        self.with_item_borrow_mut(offset, |t| *t = elem);
    }

    #[inline(always)]
    fn with_item_borrow<U>(&self, offset: usize, f: impl FnOnce(&T) -> U) -> U {
        if offset < N {
            f(&*self.items[offset].borrow())
        } else {
            self.next.with_borrowed(|t| t.as_ref().unwrap().with_item_borrow(offset - N, f))
        }
    }

    #[inline(always)]
    fn with_item_borrow_mut<U>(&self, offset: usize, f: impl FnOnce(&mut T) -> U) -> U {
        if offset < N {
            f(&mut *self.items[offset].borrow_mut())
        } else {
            self.next.with_borrowed(|t| t.as_ref().unwrap().with_item_borrow_mut(offset - N, f))
        }
    }

    #[inline(always)]
    fn for_item_borrow(&self, count: usize, mut f: impl FnMut(&T)) {
        for item in self.items.iter().take(count) {
            f(&*item.borrow());
        }
        if count > N {
            self.next.with_borrowed(|t| t.as_ref().unwrap().for_item_borrow(count - N, f));
        }
    }

    #[inline(always)]
    fn for_item_borrow_mut(&self, count: usize, mut f: impl FnMut(&mut T)) {
        for item in self.items.iter().take(count) {
            f(&mut *item.borrow_mut());
        }
        if count > N {
            self.next.with_borrowed(|t| t.as_ref().unwrap().for_item_borrow_mut(count - N, f));
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests2 {
    use super::*;

    #[test]
    fn test_push() {
        let array = LinkedCellArray::<usize, 2>::new();
        array.push(1);
        assert_eq!(&array.to_vec(), &[1]);
        array.push(2);
        assert_eq!(&array.to_vec(), &[1, 2]);
        array.push(3);
        assert_eq!(&array.to_vec(), &[1, 2, 3]);
        array.push(4);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4]);
        array.push(5);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_retain() {
        let array = LinkedCellArray::<usize, 2>::new();
        array.push(1);
        array.push(2);
        array.push(3);
        array.push(4);
        array.push(5);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4, 5]);
        array.retain(|t| *t > 0);
        assert_eq!(&array.to_vec(), &[1, 2, 3, 4, 5]);
        array.retain(|t| *t > 3);
        assert_eq!(&array.to_vec(), &[4, 5]);
        array.push(6);
        array.push(7);
        array.push(8);
        assert_eq!(&array.to_vec(), &[4, 5, 6, 7, 8]);
        array.retain(|t| t % 2 == 0);
        assert_eq!(&array.to_vec(), &[4, 6, 8]);
        array.retain(|_| false);
        assert!(&array.is_empty());
    }
}
