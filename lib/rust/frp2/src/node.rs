use crate::prelude::*;
use enso_generics::traits::*;

use crate::data::Data;
use crate::input::InputType;
use crate::input::Listen;
use crate::input::Sample;
use crate::network::with_runtime;
use crate::network::EventConsumer;
use crate::network::Network;
use crate::network::NodeData;
use crate::network::NodeId;
use crate::network::Runtime;
use crate::DefInfo;

// ====================
// === EventContext ===
// ====================

/// A wrapper for the runtime and current node data with explicit information about the event type.
/// It is mainly used to make the API for defining nodes easy and type-safe.
pub(crate) struct EventContext<'a, Output> {
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
    pub(crate) fn emit(self, value: &Output) {
        self.runtime.unchecked_emit(self.node, value);
    }
}



// ======================
// === NodeDefinition ===
// ======================



/// Abstraction allowing easy FRP node definition. It generates appropriate logic based on the input
/// types. The [`Inputs`] is a tuple containing any supported combination of [`Listen`], [`Sample`],
/// and [`ListenAndSample`]. Refer to their docs to learn more about their meaning.
pub(crate) trait NodeDefinition<Incl, Model, Output, F> {
    fn new_node_with_init<Kind>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Kind, Output>;
}

impl<Model> Network<Model> {
    #[inline(always)]
    fn _new_node_with_init<Incl, Kind, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        f: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, TypedNode<Kind, Output>>
    where
        Inputs: NodeDefinition<Incl, Model, Output, F>,
    {
        let node = Inputs::new_node_with_init(self, inps, f, |n| {
            init(n);
        });
        NodeInNetwork::new(self, node)
    }

    #[inline(always)]
    pub(crate) fn new_node_with_init<Kind, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        f: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, TypedNode<Kind, Output>>
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
    ) -> NodeInNetwork<Model, TypedNode<Kind, Output>>
    where
        Inputs: NodeDefinition<ModelIncluded, Model, Output, F>,
    {
        self._new_node_with_init::<ModelIncluded, Kind, Inputs, Output, F, _Out>(inps, f, init)
    }

    #[inline(always)]
    pub(crate) fn new_node<Kind, Inputs, Output, F>(
        &self,
        inps: Inputs,
        f: F,
    ) -> NodeInNetwork<Model, TypedNode<Kind, Output>>
    where
        Inputs: NodeDefinition<ModelNotIncluded, Model, Output, F>,
    {
        self.new_node_with_init(inps, f, |_| {})
    }

    #[inline(always)]
    pub(crate) fn new_node_with_model<Kind, Inputs, Output, F>(
        &self,
        inps: Inputs,
        f: F,
    ) -> NodeInNetwork<Model, TypedNode<Kind, Output>>
    where
        Inputs: NodeDefinition<ModelIncluded, Model, Output, F>,
    {
        self.new_node_with_model_with_init(inps, f, |_| {})
    }
}


pub(crate) struct ModelIncluded;
pub(crate) struct ModelNotIncluded;

pub(crate) struct ModelSkipped;

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
    ) -> TypedNode<Kind, Output> {
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
    ) -> TypedNode<Kind, Output> {
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
    ) -> TypedNode<Kind, Output> {
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
    ) -> TypedNode<Kind, Output> {
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

/// Any FRP node. This is a generalization for [`TypedNode`] with hidden `Kind` type.
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
pub struct TypedNode<Kind, Output> {
    pub(crate) _marker: PhantomData<*const (Kind, Output)>,
    pub(crate) id:      NodeId,
}

impl<Kind, Output> Node for TypedNode<Kind, Output> {
    type Output = Output;
    #[inline(always)]
    fn id(self) -> NodeId {
        self.id
    }
}

impl<Kind, Output> TypedNode<Kind, Output> {
    #[inline(never)]
    pub fn emit(&self, value: &Output)
    where Output: Data {
        with_runtime(|rt|
            // # Safety
            // The value is checked to have the correct [`Output`] type.
            unsafe { rt.unchecked_emit_borrow(self.id, value) });
    }
}

impl<Model> Network<Model> {
    /// Constructor of the node. The [`init`] function will be called on the newly created node.
    ///
    /// # Safety
    /// The type safety is not guaranteed. You have to ensure that the [`f`] function arguments and
    /// output type are correct.
    #[inline(always)]
    fn new_node_with_init_unchecked<Kind, Output>(
        &self,
        f: impl EventConsumer,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Kind, Output> {
        let _marker = PhantomData;
        let id = with_runtime(|rt| rt.new_node(self.id, DefInfo::unlabelled(), f, init));
        TypedNode { _marker, id }
    }
}



// =====================
// === NodeInNetwork ===
// =====================

/// A [`Node`] associated with a [`Network`]. This is used to provide a nice API for creation of new
/// nodes without the need for the user to specify in which network to create the node.
#[derive(Deref, DerefMut)]
pub struct NodeInNetwork<'t, Model, N> {
    #[deref]
    #[deref_mut]
    pub(crate) node:    N,
    pub(crate) network: &'t Network<Model>,
}

impl<'a, Model, N: Node> Node for NodeInNetwork<'a, Model, N> {
    type Output = N::Output;
    #[inline(always)]
    fn id(self) -> NodeId {
        self.node.id()
    }
}

impl<'t, Model, N: Node> Copy for NodeInNetwork<'t, Model, N> {}
impl<'t, Model, N: Node> Clone for NodeInNetwork<'t, Model, N> {
    fn clone(&self) -> Self {
        Self { node: self.node, network: self.network }
    }
}

impl<'t, Model, N> NodeInNetwork<'t, Model, N> {
    fn new(network: &'t Network<Model>, node: N) -> Self {
        Self { network, node }
    }
}
