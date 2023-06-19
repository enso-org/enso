//! FRP nodes definitions. They are the main user-facing API for creating FRP networks.

use crate::prelude::*;

use crate::data::Data;
use crate::network::Model;
use crate::network::Network;
use crate::node::input;
use crate::node::input::Listen;
use crate::node::input::ListenAndSample;
use crate::node::input::Sample;
use crate::node::Node;
use crate::node::NodeInNetwork;
use crate::node::NodeOf;
use crate::node::NodeWithDefaultOutput;
use crate::node::TypedNode;
use crate::runtime::with_runtime;
use enso_generics::*;



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

impl<Output> Source<Output> {
    pub fn emit(&self, value: &Output)
    where Output: Data {
        self.emit_internal(value);
    }
}



// ===============
// === Sampler ===
// ===============

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

impl<Output> Sampler<Output> {
    /// Get the last emitted value. If no value was emitted yet, returns the default value.
    pub fn value(&self) -> Output
    where Output: Clone + Default {
        self.value_internal()
    }
}



// ====================
// === DebugCollect ===
// ====================

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

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
    /// On every event, remember it, and pass it trough. It returns bot the newly created node and
    /// a struct allowing for easy access to the recorded values. Used mainly for debug purposes.
    pub fn debug_collect(
        self,
    ) -> (NodeInNetwork<'a, M, Stream<Vec<N1::Output>>>, DebugCollectData<N1::Output>)
    where N1::Output: Clone {
        let data = DebugCollectData::default();
        let out = data.clone();
        let node = self.new_node((Listen(self),), move |event, _, t0| {
            data.cell.borrow_mut().push(t0.clone());
            event.emit(&*data.cell.borrow());
        });
        (node, out)
    }
}



// ===========
// === Map ===
// ===========

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



// ===============
// === AllWith ===
// ===============

// FIXME: rename `all_with` to `all` and `all` to `zip`.
macro_rules! def_all_with_nodes {
    ($($is:literal),*) => {
        def_all_with_nodes! { @ [[all_with 1 []]] $($is)* }
    };

    (@ [ [$name:ident $i:tt [$($is:tt)*]] $($iss:tt)* ] $n:tt $($ns:tt)*) => { paste !{
        def_all_with_nodes! { @ [ [[<all_with $n>] $n [$($is)* $n]] [$name $i [$($is)*]] $($iss)* ] $($ns)* }
    }};

    (@ [ [$name:ident $i:tt [$($is:tt)*]] $($iss:tt)* ]) => {
        def_all_with_nodes! { @ [ $($iss)* ] }
        paste! { def_all_with_nodes! { # $name $i [$([<N $is>])*] } }
    };

    (@ []) => {};

    (# $name:ident $i:tt [$($ns:tt)*]) => { paste! {
        /// On every event on the first input, sample all inputs, evaluate the provided function,
        /// and pass the result trough. The function contains mutable reference to the network
        /// model. If you don't need the model, use the `all_withX_` family of functions instead.
        #[inline(never)]
        #[allow(non_snake_case)]
        #[allow(clippy::too_many_arguments)]
        pub fn $name < $($ns,)* F, Output>
        (self, $($ns:$ns,)* f: F) -> NodeInNetwork<'a, M, Stream<Output>>
        where
            $($ns: NodeWithDefaultOutput,)*
            Output: Data,
            F: 'static + Fn(&mut M, &N1::Output, $(&$ns::Output,)*) -> Output, {
            self.network.new_node_with_model((ListenAndSample(self), $(ListenAndSample($ns),)*),
                move |event, m, t1, $($ns,)*| { event.emit(&f(m, t1, $($ns,)*)) }
            )
        }

        /// On every event on the first input, sample all inputs, evaluate the provided function,
        /// and pass the result trough. The function does not contain mutable reference to the
        /// network model. If you need the model, use the `all_withX` family of functions instead.
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

impl<'a, M: Model, N1: NodeWithDefaultOutput> NodeInNetwork<'a, M, N1> {
    def_all_with_nodes![2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
}



// ==============
// === AnyMut ===
// ==============

/// Marker type for the [`AnyMut`] node.
#[derive(Clone, Copy, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct ANY_MUT;

pub type AnyMut<Output = ()> = TypedNode<ANY_MUT, Output>;

impl<M: Model> Network<M> {
    /// Merges multiple input streams into a single output stream. All input streams have to share
    /// the same output data type. This is a dynamic node, allowing new input streams to be attached
    /// after its creation with the [`Self::attach`] method.
    ///
    /// Please note that `any_mut` can be used to create recursive FRP networks ...
    // FIXME: check if we can really create recursive frp networks here.
    #[inline(always)]
    pub fn any_mut<T: Data>(&self) -> NodeInNetwork<M, AnyMut<T>> {
        self.new_node(PhantomData::<(Listen<Stream<T>>,)>, |event, _, t| event.emit(t))
    }
}

impl<Output: Data> AnyMut<Output> {
    pub fn attach<N: NodeOf<Output>>(&self, node: N) {
        with_runtime(|rt| rt.connect(input::Type::Listen(node.id()), self.id()));
    }
}



// ===========
// === Any ===
// ===========

macro_rules! def_any_nodes {
    ($($is:literal),*) => {
        def_any_nodes! { @ [[any 1 []]] $($is)* }
    };

    (@ [ [$name:ident $i:tt [$($is:tt)*]] $($iss:tt)* ] $n:tt $($ns:tt)*) => { paste !{
        def_any_nodes! { @ [ [[<any $n>] $n [$($is)* $n]] [$name $i [$($is)*]] $($iss)* ] $($ns)* }
    }};

    (@ [ [$name:ident $i:tt [$($is:tt)*]] $($iss:tt)* ]) => {
        def_any_nodes! { @ [ $($iss)* ] }
        paste! { def_any_nodes! { # $name $i [$([<N $is>])*] } }
    };

    (@ []) => {};

    (# $name:ident $i:tt [$($ns:tt)*]) => { paste! {
        /// Merges multiple input event streams into a single output event stream. All input event
        /// streams have to share the same output data type.
        #[inline(never)]
        #[allow(non_snake_case)]
        #[allow(clippy::too_many_arguments)]
        pub fn $name < $($ns,)* Output>(self, $($ns:$ns,)*) -> NodeInNetwork<'a, M, Stream<Output>>
        where
            N1: NodeOf<Output>,
            $($ns: NodeOf<Output>,)*
            Output: Data {
            self.network.new_node((Listen(self), $(Listen($ns),)*),
                move |event, _, t| { event.emit(t) }
            )
        }

        /// Merges multiple input event streams into a single output event stream. All input event
        /// streams have to share the same output data type. The event data is dropped and on every
        /// event, an unit event is emitted.
        #[inline(never)]
        #[allow(non_snake_case)]
        #[allow(clippy::too_many_arguments)]
        pub fn [<$name _>] < $($ns,)* Output>(self, $($ns:$ns,)*) -> NodeInNetwork<'a, M, Stream>
        where
            N1: NodeOf<Output>,
            $($ns: NodeOf<Output>,)*
            Output: Data {
            self.network.new_node((Listen(self), $(Listen($ns),)*),
                move |event, _, _| { event.emit(&()) }
            )
        }
    }};
}

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
    def_any_nodes![2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
}



// ===================
// === Other Nodes ===
// ===================

impl<'a, M: Model, N1: Node> NodeInNetwork<'a, M, N1> {
    /// On every event, pass it trough. This node can be used to branch the event stream into
    /// multiple streams to control their evaluation order. For example:
    ///
    /// ```text
    /// let on_up_src = mouse.on_up();
    /// let on_up = on_up_src.identity();
    /// let on_up_cleaning_phase = on_up_src.identity();
    /// on_up_cleaning_phase.map_ (|_| println!("On up cleaning phase."));
    /// ```
    ///
    /// Now, we can safely attach any events to `on_up` and we can be sure that all these events
    /// will be handled before events attached to `on_up_cleaning_phase`.
    pub fn identity(self) -> NodeInNetwork<'a, M, Stream<N1::Output>> {
        self.new_node((Listen(self),), move |event, _, t0| event.emit(t0))
    }

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

    /// On every event, emit next element of an infinite stream of `true`, `false`, `true`, `false`,
    /// [...]. The first emitted value will be `true`.
    pub fn toggle(self) -> NodeInNetwork<'a, M, Stream<bool>> {
        let value = Cell::new(false);
        self.new_node((Listen(self),), move |event, _, _| {
            value.update(|t| !t);
            event.emit(&value.get());
        })
    }

    /// On every event, emit next element of an infinite stream of `false`, `true`, `false`, `true`,
    /// [...]. The first emitted value will be `false`.
    pub fn toggle_true(self) -> NodeInNetwork<'a, M, Stream<bool>> {
        let value = Cell::new(true);
        self.new_node((Listen(self),), move |event, _, _| {
            value.update(|t| !t);
            event.emit(&value.get());
        })
    }

    /// On every event, increase the internal event count and emit the new count value.
    pub fn count(self) -> NodeInNetwork<'a, M, Stream<usize>> {
        let count = Cell::new(0);
        self.new_node((Listen(self),), move |event, _, _| {
            count.update(|t| t + 1);
            event.emit(&count.get());
        })
    }

    /// On every event, drop it and emit the provided value instead.
    pub fn constant<T: Data>(self, value: T) -> NodeInNetwork<'a, M, Stream<T>> {
        self.new_node((Listen(self),), move |event, _, _| {
            event.emit(&value);
        })
    }

    /// On every event, remember it and emit the previously remembered one. In case of the first
    /// event, the default value will be emitted.
    pub fn previous(self) -> NodeInNetwork<'a, M, Stream<N1::Output>>
    where N1::Output: Default {
        let previous: RefCell<N1::Output> = RefCell::new(default());
        self.new_node((Listen(self),), move |event, _, t0| {
            let value = previous.replace(t0.clone());
            event.emit(&value);
        })
    }

    // FIXME: Make sure that the compatible API layer supports it as the ordering is different here
    /// On every event, drop it, sample the value of the provided behavior, and emit it instead.
    pub fn sample<N2: Node>(self, behavior: N2) -> NodeInNetwork<'a, M, Stream<N2::Output>>
    where N2::Output: Default {
        self.new_node((Listen(self), Sample(behavior)), move |event, _, _, sampled| {
            event.emit(sampled);
        })
    }

    /// On every event, pass it trough if the `cond` input is `true`.
    pub fn gate(self, cond: impl NodeOf<bool>) -> NodeInNetwork<'a, M, Stream<N1::Output>> {
        self.new_node((Listen(self), Sample(cond)), move |event, _, src, cond| {
            if *cond {
                event.emit(src);
            }
        })
    }

    /// On every event, pass it trough if the `cond` input is `false`.
    pub fn gate_not(self, cond: impl NodeOf<bool>) -> NodeInNetwork<'a, M, Stream<N1::Output>> {
        self.new_node((Listen(self), Sample(cond)), move |event, _, src, cond| {
            if !cond {
                event.emit(src);
            }
        })
    }

    /// On every event, emit its unwrapped value. In case the value cannot be unwrapped (e.g. from
    /// the incoming `None` value), no output event will be emitted.
    pub fn unwrap(self) -> NodeInNetwork<'a, M, Stream<Item<N1::Output>>>
    where
        N1::Output: OptItemRef,
        Item<N1::Output>: Data, {
        self.new_node((Listen(self),), move |event, _, t0| {
            if let Some(val) = t0.opt_item() {
                event.emit(val);
            }
        })
    }

    /// On every event, iterate over its items and emit each item as a separate event.
    pub fn iter<T: Data>(self) -> NodeInNetwork<'a, M, Stream<T>>
    where for<'t> &'t N1::Output: IntoIterator<Item = &'t T> {
        self.new_node((Listen(self),), move |event, _, t0| {
            for val in t0 {
                event.emit(val);
            }
        })
    }

    /// On every event, fold all of its items by using [`Monoid`] and emit the folded result.
    pub fn fold<T: Data + Monoid>(self) -> NodeInNetwork<'a, M, Stream<T>>
    where for<'t> &'t N1::Output: IntoIterator<Item = &'t T> {
        self.new_node((Listen(self),), move |event, _, t0| {
            let val = t0.into_iter().fold(default(), |t: T, s| t.concat(s));
            event.emit(&val);
        })
    }

    /// On every event, get the N-th field of the value and emit it.
    pub fn field_at<const N: usize>(self) -> NodeInNetwork<'a, M, Stream<FieldAt<N, N1::Output>>>
    where
        N1::Output: GetFieldAt<N>,
        FieldAt<N, N1::Output>: Data, {
        self.new_node((Listen(self),), move |event, _, t0| {
            event.emit(t0.field_at::<N>());
        })
    }

    /// On every event, get the 0-index field of the value and emit it.
    pub fn _0(self) -> NodeInNetwork<'a, M, Stream<FieldAt<0, N1::Output>>>
    where
        N1::Output: GetFieldAt<0>,
        FieldAt<0, N1::Output>: Data, {
        self.field_at::<0>()
    }

    // TODO: _1, _2, ...

    // FIXME: Double check if it works, as the behavior is changed. It emits event even if the first
    //        value == default().
    /// On every event, remember it and pass it trough if its different than the previously
    /// remembered one.
    pub fn on_change(self) -> NodeInNetwork<'a, M, Stream<N1::Output>>
    where N1::Output: PartialEq {
        let previous: RefCell<Option<N1::Output>> = RefCell::new(None);
        self.new_node((Listen(self),), move |event, _, t0| {
            let changed = previous.borrow().as_ref() == Some(t0);
            if changed {
                *previous.borrow_mut() = Some(t0.clone());
                event.emit(t0);
            }
        })
    }

    /// On every event `e: &N1::Output`, emit `&e.into()`.
    pub fn ref_into<T: Data>(self) -> NodeInNetwork<'a, M, Stream<T>>
    where for<'t> &'t N1::Output: Into<T> {
        self.new_node((Listen(self),), move |event, _, t0| {
            event.emit(&t0.into());
        })
    }

    /// On every event `e: &N1::Output`, emit `&e.clone().into()`.
    pub fn cloned_into<T: Data>(self) -> NodeInNetwork<'a, M, Stream<T>>
    where N1::Output: Into<T> {
        self.new_node((Listen(self),), move |event, _, t0| {
            event.emit(&t0.clone().into());
        })
    }

    /// On every event `e: &N1::Output`, emit `&Some(e.into())`.
    pub fn ref_into_some<T: Data>(self) -> NodeInNetwork<'a, M, Stream<Option<T>>>
    where for<'t> &'t N1::Output: Into<T> {
        self.new_node((Listen(self),), move |event, _, t0| {
            event.emit(&Some(t0.into()));
        })
    }

    /// On every event `e: &N1::Output`, emit `&Some(e.clone().into())`.
    pub fn cloned_into_some<T: Data>(self) -> NodeInNetwork<'a, M, Stream<Option<T>>>
    where N1::Output: Into<T> {
        self.new_node((Listen(self),), move |event, _, t0| {
            event.emit(&Some(t0.clone().into()));
        })
    }

    // === Bool Utils ===

    /// On every event, emit `true`.
    pub fn to_true(self) -> NodeInNetwork<'a, M, Stream<bool>> {
        self.constant(true)
    }

    /// On every event, emit `false`.
    pub fn to_false(self) -> NodeInNetwork<'a, M, Stream<bool>> {
        self.constant(false)
    }

    /// On every event that is `true`, emit a unit event.
    pub fn on_true(self) -> NodeInNetwork<'a, M, Stream>
    where N1: NodeOf<bool> {
        self.new_node((Listen(self),), move |event, _, t0| {
            if *t0 {
                event.emit(&());
            }
        })
    }

    /// On every event that is `false`, emit a unit event.
    pub fn on_false(self) -> NodeInNetwork<'a, M, Stream>
    where N1: NodeOf<bool> {
        self.new_node((Listen(self),), move |event, _, t0| {
            if !t0 {
                event.emit(&());
            }
        })
    }

    /// On every event, emit its negation.
    pub fn not(self) -> NodeInNetwork<'a, M, Stream<bool>>
    where N1: NodeOf<bool> {
        self.new_node((Listen(self),), move |event, _, t0| {
            event.emit(&!t0);
        })
    }
}
