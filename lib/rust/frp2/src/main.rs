#![feature(allocator_api)]
#![feature(test)]

use bumpalo::Bump;
use enso_prelude::*;
use ouroboros::self_referencing;
use slotmap::SlotMap;
use std::cell::UnsafeCell;

use smallvec::SmallVec;



slotmap::new_key_type! {
    pub struct Network;
    pub struct Node;
}

#[derive(Clone, Debug, Default)]
pub struct NetworkData {
    nodes: Rc<RefCell<Vec<Node>>>,
}

#[derive(Debug, Default)]
pub struct Runtime {
    pub networks: RefCell<SlotMap<Network, NetworkData>>,
    pub nodes:    RefCell<SlotMap<Node, NodeData>>,
}


thread_local! {
    static RUNTIME: Runtime = default();
    static CURRENT_NETWORK: RefCell<Option<NetworkData>> = default();
}

fn with_runtime<T>(f: impl FnOnce(&Runtime) -> T) -> T {
    RUNTIME.with(f)
}


impl Network {
    pub fn new() -> Self {
        with_runtime(|runtime| runtime.networks.borrow_mut().insert(default()))
    }

    pub fn with<T>(&self, f: impl FnOnce() -> T) -> T {
        with_runtime(|runtime| {
            let network: NetworkData = runtime.networks.borrow()[*self].clone();
            CURRENT_NETWORK.with(|current_network| {
                let old_network = current_network.replace(Some(network));
                let result = f();
                current_network.replace(old_network);
                result
            })
        })
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub enum NodeType {
    #[default]
    Source,
    Trace,
}

#[derive(Debug, Default)]
pub struct NodeData {
    tp:      NodeType,
    inputs:  SmallVec<[Node; 4]>,
    outputs: SmallVec<[Node; 4]>,
    data:    Option<Box<dyn Any>>,
}

impl NodeData {
    fn emit<D>(&self, data: &D) {
        with_runtime(|runtime| {
            for output in &self.outputs {
                runtime.nodes.borrow()[*output].on_data(data);
            }
        })
    }

    fn on_data<D>(&self, data: D) {
        match self.tp {
            NodeType::Source => {}
            NodeType::Trace => {
                println!("trace");
            }
        }
    }
}

fn new_node(f: impl FnOnce(&mut NodeData)) -> Node {
    RUNTIME.with(|runtime| {
        CURRENT_NETWORK.with(|current_network| {
            let current_network = current_network.borrow();
            let network = current_network.as_ref().unwrap();
            let mut node_data = NodeData::default();
            f(&mut node_data);
            let node = runtime.nodes.borrow_mut().insert(node_data);
            network.nodes.borrow_mut().push(node);
            node
        })
    })
}


fn source() -> Node {
    new_node(|node| node.tp = NodeType::Source)
}

impl Node {
    pub fn trace(&self) -> Node {
        new_node(|node| {
            node.tp = NodeType::Trace;
            node.inputs.push(*self);
        })
    }
}

// fn test() {
//     let bump = Bump::new();
//     let v = Vec::new_in(&bump);
// }

fn main() {
    let network = Network::new();
    network.with(|| {
        let src = source();
        let trc = src.trace();
    });
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!(1, 2);
    }
}


#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use test::Bencher;

    #[bench]
    fn creation_via_mem_zeroed(bencher: &mut Bencher) {
        let reps = 1_000_000;
        bencher.iter(move || {
            for i in 0..reps {
                let mut v: SmallVec<[u8; 4]> = unsafe { mem::zeroed() };
                v.push(1);
                assert_eq!(v.len(), 1);
            }
        });
    }

    #[bench]
    fn creation_via_new(bencher: &mut Bencher) {
        let reps = 1_000_000;
        bencher.iter(move || {
            let mut nodes = Vec::with_capacity(reps);
            for i in 0..reps {
                let node = NodeData::default();
                nodes.push(node);
            }
            mem::take(&mut nodes);
        });
    }
}
