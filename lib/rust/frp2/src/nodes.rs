use crate::prelude::*;

use crate::data::Data;
use crate::network::Network;
use crate::node::input::Listen;
use crate::node::input::Sample;
use crate::node::Node;
use crate::node::NodeInNetwork;
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

#[derive(Clone, Copy, Debug, Default)]
pub struct SOURCE;
pub type Source<Output = ()> = TypedNode<SOURCE, Output>;

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

#[derive(Clone, Copy, Debug, Default)]
pub struct SAMPLER;
pub type Sampler<Output = ()> = TypedNode<SAMPLER, Output>;

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
