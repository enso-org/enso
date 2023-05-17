#![feature(allocator_api)]
#![feature(test)]
#![feature(let_chains)]

mod callstack;

use bumpalo::Bump;
use enso_prelude::*;
use ouroboros::self_referencing;
use slotmap::Key;
use slotmap::SecondaryMap;
use slotmap::SlotMap;
use std::cell::UnsafeCell;

use smallvec::SmallVec;

use callstack::CallStack;
use callstack::DefInfo;

pub use enso_prelude as prelude;
// use std::any::Any as Data;

#[derive(Clone, Copy, Debug)]
pub struct Data(usize);


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

enum NodeType {
    Source,
    Inc,
    Trace,
    Map(Box<dyn Fn(Data) -> Data>),
}

impl Debug for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeType")
    }
}

#[derive(Debug)]
struct NodeData {
    tp:         NodeType,
    network_id: NetworkId,
    def:        DefInfo,
    inputs:     TargetsVec,
    outputs:    TargetsVec,
}

impl NodeData {
    pub fn new(tp: NodeType, network_id: NetworkId, def: DefInfo) -> Self {
        Self { tp, network_id, def, inputs: default(), outputs: default() }
    }

    fn on_event(&self, rt: Rt, node_id: NodeId, event: Data) -> Option<Data> {
        match &self.tp {
            NodeType::Source => None,
            NodeType::Inc => Some(Data(event.0 + 1)),
            NodeType::Trace => {
                println!("TRACE: {:?}", event);
                Some(event)
            }
            NodeType::Map(f) => Some(f(event)),
        }
    }
}

slotmap::new_key_type! { pub struct NetworkId; }
slotmap::new_key_type! { pub struct NodeId; }

type TargetsVec = SmallVec<[NodeId; 1]>;


// ==========
// === Rt ===
// ==========

/// Public-facing opaque handle to a runtime. Provides no access to internals. Allows to cut down
/// on `with_runtime` calls across the codebase, allowing for more performant code which is easier
/// for the compiler to optimize.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Rt<'a>(&'a Runtime);

impl<'a> From<&'a Runtime> for Rt<'a> {
    fn from(runtime: &'a Runtime) -> Self {
        Rt(runtime)
    }
}

impl<'a> Rt<'a> {
    pub fn stack_trace(&self) -> &CallStack {
        &self.0.stack
    }

    pub fn in_stack_frame<T>(&self, _def: DefInfo, f: impl FnOnce() -> T) -> T {
        #[cfg(feature = "stack-trace")]
        {
            self.0.stack.with(_def, || f())
        }

        #[cfg(not(feature = "stack-trace"))]
        {
            f()
        }
    }

    pub fn metrics(&self) -> Metrics {
        self.0.metrics.clone()
    }
}



// ===============
// === Runtime ===
// ===============

thread_local! {
    static RUNTIME: Rc<Runtime>  = default();
}

#[inline(always)]
pub fn with_runtime<T>(f: impl FnOnce(Rt) -> T) -> T {
    RUNTIME.with(|runtime| f(Rt(runtime)))
}

fn rc_runtime() -> Rc<Runtime> {
    RUNTIME.with(|runtime| runtime.clone())
}


#[derive(Default)]
pub(crate) struct Runtime {
    networks:          RefCell<SlotMap<NetworkId, NetworkData>>,
    nodes:             RefCell<SlotMap<NodeId, NodeData>>,
    // consumers:         RefCell<SecondaryMap<NodeId, BumpRc<dyn RawEventConsumer>>>,
    /// Map for each node to its behavior, if any. For nodes that are natively behaviors, points to
    /// itself. For non-behavior streams, may point to a sampler node that listens to it.
    behaviors:         RefCell<SecondaryMap<NodeId, NodeId>>,
    /// For each node that is a behavior, stores the current value of the behavior. A node can be
    /// checked for being a behavior by checking if it is present in this map.
    // behavior_values:   RefCell<SecondaryMap<NodeId, BumpRc<dyn Data>>>,
    // targets: RefCell<SecondaryMap<NodeId, TargetsVec>>,
    target_store_pool: RefCell<Vec<TargetsVec>>,
    metrics:           Metrics,
    stack:             CallStack,
    current_node:      Cell<NodeId>,
}

impl Runtime {
    fn new_network(&self) -> NetworkId {
        self.metrics.inc_networks();
        self.metrics.inc_network_refs();
        let mut networks = self.networks.borrow_mut();
        networks.insert(NetworkData { nodes: Vec::new(), refs: 1 })
    }

    fn new_node(&self, tp: NodeType, network_id: NetworkId, def: DefInfo) -> NodeId {
        self.metrics.inc_nodes();
        let mut nodes = self.nodes.borrow_mut();

        let mut networks = self.networks.borrow_mut();
        if let Some(network) = networks.get_mut(network_id) {
            let id = nodes.insert(NodeData::new(tp, network_id, def));
            network.nodes.push(id);
            id
        } else {
            NodeId::null()
        }
    }

    fn connect(&self, src_id: NodeId, tgt_id: NodeId) {
        self.nodes.with_borrowed_mut(|nodes| {
            if let Some(src) = nodes.get_mut(src_id) {
                src.outputs.push(tgt_id);
            }
            if let Some(tgt) = nodes.get_mut(tgt_id) {
                tgt.inputs.push(src_id);
            }
        });
    }

    fn emit_event(&self, init_node_id: NodeId, init_event: Data) {
        let mut nodes_to_eval = vec![(init_node_id, init_event)];

        while let Some((node_id, event)) = nodes_to_eval.pop() {
            let node_outputs = self.nodes.with_borrowed(|nodes| {
                nodes.get(node_id).map(|node| {
                    let vec_from_pool = self.target_store_pool.borrow_mut().pop();
                    let mut node_outputs = vec_from_pool.unwrap_or_default();
                    node_outputs.extend_from_slice(node.outputs.as_slice());
                    (node_outputs, node.def)
                })
            });
            if let Some((mut node_outputs, def)) = node_outputs {
                // Emit events to all targets. In case there are no targets, we are done.

                // store the targets to emit to in a temporary buffer, so we can
                // release the borrow.


                let cleanup_targets = self.stack.with(def, || {
                    let mut cleanup_targets = false;
                    for &node_output_id in &node_outputs {
                        let nodes = self.nodes.borrow();
                        let node_output = nodes.get(node_output_id);
                        if let Some(node_output) = node_output {
                            if let Some(output) = node_output.on_event(Rt(self), node_id, event) {
                                nodes_to_eval.push((node_output_id, output));
                            }
                        } else {
                            cleanup_targets = true;
                        }
                    }
                    cleanup_targets
                });
                //
                // Return temporary buffer to pool.
                node_outputs.clear();
                self.target_store_pool.borrow_mut().push(node_outputs);
            }
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
    }
}



// ===============
// === Network ===
// ===============

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Network {
    pub(crate) id: NetworkId,
    #[derivative(Debug = "ignore")]
    rt:            Rc<Runtime>,
    #[cfg(feature = "stack-trace")]
    span:          Cell<Option<DefInfo>>,
    #[cfg(not(feature = "stack-trace"))]
    span:          (),
}

impl Network {
    pub fn new() -> Self {
        let rt = rc_runtime();
        let id = rt.new_network();
        Network { rt, id, span: default() }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::callstack::Location;

    #[test]
    fn test() {
        println!("hello");
        let net = Network::new();
        let src = with_runtime(|rt| rt.0.new_node(NodeType::Source, net.id, DefInfo::unlabelled()));
        let inc = with_runtime(|rt| rt.0.new_node(NodeType::Inc, net.id, DefInfo::unlabelled()));
        let trc = with_runtime(|rt| rt.0.new_node(NodeType::Trace, net.id, DefInfo::unlabelled()));
        let inc2 = with_runtime(|rt| {
            rt.0.new_node(
                NodeType::Map(Box::new(|t: Data| Data(t.0 * 2))),
                net.id,
                DefInfo::unlabelled(),
            )
        });
        let trc2 = with_runtime(|rt| rt.0.new_node(NodeType::Trace, net.id, DefInfo::unlabelled()));

        with_runtime(|rt| rt.0.connect(src, inc));
        with_runtime(|rt| rt.0.connect(inc, trc));
        with_runtime(|rt| rt.0.connect(trc, inc2));
        with_runtime(|rt| rt.0.connect(inc2, trc2));
        with_runtime(|rt| rt.0.emit_event(src, Data(1)));
        // let source = net.source::<i32>();
        assert_eq!(1, 2);
    }
}
