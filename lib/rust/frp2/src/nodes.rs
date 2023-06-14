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

auto trait NotStream {}
impl !NotStream for STREAM {}

/// The most generic type of a node. Every node can be converted to this type while losing some
/// abilities. For example, a [`Sampler`] node provides the ability to sample the last node output.
/// However, if you need to store different node types in an array, you can convert them to samplers
/// with zero-overhead.
pub type Stream<Output = ()> = TypedNode<STREAM, Output>;

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
        TypedNode { _marker: PhantomData, id: node.id }
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


// === Sampler ===

/// Marker type for the [`Sampler`] node.
#[derive(Clone, Copy, Debug, Default)]
pub struct SAMPLER;

/// A node that allows the user to sample the last value of the node output.
pub type Sampler<Output = ()> = TypedNode<SAMPLER, Output>;

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
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
            println!("TRACE: {:?}", t0);
            event.emit(t0);
        })
    }

    pub fn trace_if(self, cond: impl NodeOf<bool>) -> NodeInNetwork<'a, M, Stream<N1::Output>> {
        self.new_node((Listen(self), Sample(cond)), move |event, _, src, cond| {
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

impl<M: Model> Network<M> {
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



// === Map2 ===

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
    #[inline(never)]
    pub fn map<F, Output>(self, f: F) -> NodeInNetwork<'a, M, Stream<Output>>
    where
        Output: Data,
        F: 'static + Fn(&mut M, &N1::Output) -> Output, {
        self.new_node_with_model((Listen(self),), move |event, m, t1| event.emit(&f(m, t1)))
    }

    #[inline(never)]
    pub fn map_<F, Output>(self, f: F) -> NodeInNetwork<'a, M, Stream<Output>>
    where
        Output: Data,
        F: 'static + Fn(&N1::Output) -> Output, {
        self.new_node((Listen(self),), move |event, _, t1| event.emit(&f(t1)))
    }

    #[inline(never)]
    pub fn map2_<N2, F, Output>(self, n2: N2, f: F) -> NodeInNetwork<'a, M, Stream<Output>>
    where
        N2: NodeWithDefaultOutput,
        Output: Data,
        F: 'static + Fn(&N1::Output, &N2::Output) -> Output, {
        self.new_node((Listen(self), Sample(n2)), move |event, _, t1, t2| event.emit(&f(t1, t2)))
    }
}

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
        #[allow(non_snake_case)]
        pub fn [<map $i>] < $($ns,)* F, Output>
        (self, $($ns:$ns,)* f: F) -> NodeInNetwork<'a, M, Stream<Output>>
        where
            $($ns: NodeWithDefaultOutput,)*
            Output: Data,
            F: 'static + Fn(&mut M, &N1::Output, $(&$ns::Output,)*) -> Output, {
            self.network.new_node_with_model((Listen(self), $(Sample($ns),)*),
                move |event, m, t1, $($ns,)*| { event.emit(&f(m, t1, $($ns,)*)) }
            )
        }
    }};
}

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
    def_map_nodes![2, 3];
}
