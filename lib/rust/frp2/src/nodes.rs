//! FRP nodes definitions. They are the main user-facing API for creating FRP networks.

use crate::prelude::*;

use crate::data::Data;
use crate::network::Model;
use crate::network::Network;
use crate::node::input::Listen;
use crate::node::input::ListenAndSample;
use crate::node::input::Sample;
use crate::node::Node;
use crate::node::NodeInNetwork;
use crate::node::NodeOf;
use crate::node::NodeWithDefaultOutput;
use crate::node::TypedNode;



// ==============
// === Stream ===
// ==============

/// Marker type for the [`Stream`] node.
#[derive(Clone, Copy, Debug, Default)]
pub struct STREAM;

/// Auto trait for discovering that the given marker is not [`STREAM`].
pub auto trait NotStream {}
impl !NotStream for STREAM {}

/// The most generic type of a node. Every node can be converted to this type while losing some
/// abilities. For example, a [`Sampler`] node provides the ability to sample the last node output.
/// However, if you need to store different node types in an array, you can convert them to samplers
/// with zero-overhead.
pub type Stream<Output = ()> = TypedNode<STREAM, Output>;

impl<Type, Output> TypedNode<Type, Output> {
    /// Convert the node to a [`Stream`].
    #[inline(always)]
    pub fn as_stream(self) -> Stream<Output>
    where Type: NotStream {
        self.into()
    }
}

impl<Type: NotStream, Output> Deref for TypedNode<Type, Output> {
    type Target = Stream<Output>;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // # Safety
        // The values differ only in phantom parametrization, so it is safe to coerce them.
        #[allow(unsafe_code)]
        #[allow(trivial_casts)]
        unsafe {
            &*(self as *const Self as *const Self::Target)
        }
    }
}

impl<Type: NotStream, Output> From<TypedNode<Type, Output>> for Stream<Output> {
    #[inline(always)]
    fn from(node: TypedNode<Type, Output>) -> Self {
        TypedNode { _marker: ZST(), id: node.id }
    }
}


// ==============
// === Source ===
// ==============

/// Marker type for the [`Source`] node.
#[derive(Clone, Copy, Debug, Default)]
pub struct SOURCE;

/// A node that allows the user to explicitly emit values on.
pub type Source<Output = ()> = TypedNode<SOURCE, Output>;

impl<M: Model> Network<M> {
    /// Starting point in the FRP network. It provides the API to explicitly emit events. Often, it
    /// is used to indicate a GUI action, like pressing a button.
    #[inline(always)]
    pub fn source<T>(&self) -> NodeInNetwork<M, Source<T>> {
        self.new_node((), |_, _| {})
    }

    /// Specialization of [`source`] for with the unit output type.
    #[inline(always)]
    pub fn source_(&self) -> NodeInNetwork<M, Source> {
        self.new_node((), |_, _| {})
    }
}

// TODO: Limit allowing emitting events to source node only. To be finished in:
//       https://github.com/enso-org/enso/issues/7043


// === Sampler ===

/// Marker type for the [`Sampler`] node.
#[derive(Clone, Copy, Debug, Default)]
pub struct SAMPLER;

/// A node that allows the user to sample the last value of the node output.
pub type Sampler<Output = ()> = TypedNode<SAMPLER, Output>;

impl<'a, M: Model, N1: NodeWithDefaultOutput> NodeInNetwork<'a, M, N1> {
    /// On every event, remember it, and pass it trough.
    #[inline(never)]
    pub fn sampler(self) -> NodeInNetwork<'a, M, Stream<N1::Output>> {
        self.new_node((ListenAndSample(self),), move |event, _, t1| event.emit(t1))
    }
}


// === Trace ===

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
    /// On every event, print it to console, and pass it trough.
    pub fn trace(self) -> NodeInNetwork<'a, M, Stream<N1::Output>> {
        self.new_node((Listen(self),), move |event, _, t0| {
            println!("TRACE: {t0:?}");
            event.emit(t0);
        })
    }

    /// On every event, print it to console if the `cond` input is `true`, and pass it trough.
    pub fn trace_if(self, cond: impl NodeOf<bool>) -> NodeInNetwork<'a, M, Stream<N1::Output>> {
        self.new_node((Listen(self), Sample(cond)), move |event, _, src, cond| {
            if *cond {
                println!("TRACE: {src:?}");
            }
            event.emit(src);
        })
    }
}


// === DebugCollect ===

/// A node that collects all the events it receives. Usd mainly for debug purposes.
#[derive(Debug, Clone, Derivative)]
#[derivative(Default(bound = ""))]
pub struct DebugCollectData<T> {
    cell: Rc<ZeroOverheadRefCell<Vec<T>>>,
}

impl<T> DebugCollectData<T> {
    /// Assert that the collected events are equal to the given slice.
    pub fn assert_eq(&self, expected: &[T])
    where T: PartialEq + Debug {
        let actual = self.cell.borrow();
        assert_eq!(actual.as_slice(), expected);
    }
}

impl<M: Model> Network<M> {
    /// On every event, remember it, and pass it trough.
    pub fn debug_collect<T0: Data + Clone + 'static>(
        &self,
        source: impl Node<Output = T0>,
    ) -> (NodeInNetwork<M, Stream<Vec<T0>>>, DebugCollectData<T0>) {
        let data: DebugCollectData<T0> = default();
        let out = data.clone();
        let node = self.new_node((Listen(source),), move |event, _, t0| {
            data.cell.borrow_mut().push(t0.clone());
            event.emit(&*data.cell.borrow());
        });
        (node, out)
    }
}



// === Map ===

macro_rules! def_map_nodes {
    ($($is:literal),*) => {
        def_map_nodes! { @ [[map 1 []]] $($is)* }
    };

    (@ [ [$name:ident $i:tt [$($is:tt)*]] $($iss:tt)* ] $n:tt $($ns:tt)*) => { paste !{
        def_map_nodes! { @ [ [[<map $n>] $n [$($is)* $n]] [$name $i [$($is)*]] $($iss)* ] $($ns)* }
    }};

    (@ [ [$name:ident $i:tt [$($is:tt)*]] $($iss:tt)* ]) => {
        def_map_nodes! { @ [ $($iss)* ] }
        paste! { def_map_nodes! { # $name $i [$([<N $is>])*] } }
    };

    (@ []) => {};

    (# $name:ident $i:tt [$($ns:tt)*]) => { paste! {
        /// On every event on the first input, sample other inputs, evaluate the provided function,
        /// and pass the result trough. The function contains mutable reference to the network
        /// model. If you don't need the model, use the `mapX_` family of functions instead.
        #[inline(never)]
        #[allow(non_snake_case)]
        #[allow(clippy::too_many_arguments)]
        pub fn $name < $($ns,)* F, Output>
        (self, $($ns:$ns,)* f: F) -> NodeInNetwork<'a, M, Stream<Output>>
        where
            $($ns: NodeWithDefaultOutput,)*
            Output: Data,
            F: 'static + Fn(&mut M, &N1::Output, $(&$ns::Output,)*) -> Output, {
            self.network.new_node_with_model((Listen(self), $(Sample($ns),)*),
                move |event, m, t1, $($ns,)*| { event.emit(&f(m, t1, $($ns,)*)) }
            )
        }

        /// On every event on the first input, sample other inputs, evaluate the provided function,
        /// and pass the result trough. The function does not contain mutable reference to the
        /// network model. If you need the model, use the `mapX` family of functions instead.
        #[inline(never)]
        #[allow(non_snake_case)]
        #[allow(clippy::too_many_arguments)]
        pub fn [<$name _>] < $($ns,)* F, Output>
        (self, $($ns:$ns,)* f: F) -> NodeInNetwork<'a, M, Stream<Output>>
        where
            $($ns: NodeWithDefaultOutput,)*
            Output: Data,
            F: 'static + Fn(&N1::Output, $(&$ns::Output,)*) -> Output, {
            self.network.new_node((Listen(self), $(Sample($ns),)*),
                move |event, _, t1, $($ns,)*| { event.emit(&f(t1, $($ns,)*)) }
            )
        }
    }};
}

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
    def_map_nodes![2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
}

// TODO: Allow more than one listen port - for example, `any` node uses this pattern.
