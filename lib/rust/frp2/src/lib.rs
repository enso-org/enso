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
#![feature(auto_traits)]
#![feature(negative_impls)]

mod callstack;
mod metrics;
mod network;
mod node;

pub use enso_prelude as prelude;

use enso_generics::traits::*;

use crate::node::Node;
use enso_prelude::*;
use network::with_runtime;
use network::Network;
use network::NetworkId;
use network::Network_;
use network::NodeData;
use network::NodeId;
use network::Runtime;

use crate::callstack::CallStack;
use crate::callstack::DefInfo;
use crate::node::NodeInNetwork;
use crate::node::NodeTemplate;
use crate::node::NodeWithDefaultOutput;

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



// =============
// === Nodes ===
// =============
// =============
// =============
// =============

// ==============
// === Stream ===
// ==============

/// Marker type for the [`Stream`] node.
pub struct STREAM;

auto trait NotStream {}
impl !NotStream for STREAM {}

/// The most generic type of a node. Every node can be converted to this type while losing some
/// abilities. For example, a [`Sampler`] node provides the ability to sample the last node output.
/// However, if you need to store different node types in an array, you can convert them to samplers
/// with zero-overhead.
pub type Stream<Output = ()> = NodeTemplate<STREAM, Output>;

impl<Kind: NotStream, Output> Deref for NodeTemplate<Kind, Output> {
    type Target = Stream<Output>;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Self as *const Self::Target) }
    }
}

impl<Kind: NotStream, Output> From<NodeTemplate<Kind, Output>> for Stream<Output> {
    #[inline(always)]
    fn from(node: NodeTemplate<Kind, Output>) -> Self {
        NodeTemplate { _marker: PhantomData, id: node.id }
    }
}


// ==============
// === Source ===
// ==============

#[derive(Clone, Copy, Default)]
pub struct SOURCE;
pub type Source<Output = ()> = NodeTemplate<SOURCE, Output>;

impl<Model> Network<Model> {
    #[inline(always)]
    pub fn source<T>(&self) -> NodeInNetwork<Model, Source<T>> {
        self.new_node((), |_, _| {})
    }

    #[inline(always)]
    pub fn source_(&self) -> NodeInNetwork<Model, Source> {
        self.new_node((), |_, _| {})
    }
}


// === Sampler ===

#[derive(Clone, Copy, Default)]
pub struct SAMPLER;
pub type Sampler<Output = ()> = NodeTemplate<SAMPLER, Output>;

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
