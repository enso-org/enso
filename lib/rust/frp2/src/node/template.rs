use crate::prelude::*;
use enso_generics::traits::*;

use crate::node::input::*;

use crate::data::Data;
use crate::network::with_runtime;
use crate::network::Network;
use crate::network::NodeData;
use crate::network::Runtime;
use crate::node::class::NodeInNetwork;
use crate::node::class::TypedNode;
use crate::node::Node;
use crate::node::NodeWithDefaultOutput;


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



// ===================
// === Model Usage ===
// ===================

/// Marker type indicating that the node uses the model.
pub(crate) struct ModelUsed;

/// Marker type indicating that the node does not use the model.
pub(crate) struct ModelNotUsed;

/// A phantom type passed to node body closure if the model was marked not to be used.
pub(crate) struct ModelSkipped;

/// Resolved model based on the model chooser and the FRP network model type.
type ChosenModel<M, Model> = <M as ModelChooser<Model>>::ChosenModel;

/// A trait allowing choosing either real FRP Network model or a phantom one based on the provided
/// type-level configuration.
trait ModelChooser<Model> {
    type ChosenModel;
    type ClonedModel: 'static;
    fn clone_model(model: &Rc<OptRefCell<Model>>) -> Self::ClonedModel;
    fn with_model_borrow_mut<Out>(
        model: &Self::ClonedModel,
        f: impl FnOnce(&mut Self::ChosenModel) -> Out,
    ) -> Out;
}

impl<Model> ModelChooser<Model> for ModelNotUsed {
    type ChosenModel = ModelSkipped;
    type ClonedModel = ModelSkipped;
    #[inline(always)]
    fn clone_model(_model: &Rc<OptRefCell<Model>>) -> Self::ClonedModel {
        ModelSkipped
    }
    #[inline(always)]
    fn with_model_borrow_mut<Out>(
        _model: &Self::ClonedModel,
        f: impl FnOnce(&mut Self::ChosenModel) -> Out,
    ) -> Out {
        f(&mut ModelSkipped)
    }
}

impl<Model: 'static> ModelChooser<Model> for ModelUsed {
    type ChosenModel = Model;
    type ClonedModel = Rc<OptRefCell<Model>>;
    #[inline(always)]
    fn clone_model(model: &Rc<OptRefCell<Model>>) -> Self::ClonedModel {
        model.clone()
    }
    #[inline(always)]
    fn with_model_borrow_mut<Out>(
        model: &Self::ClonedModel,
        f: impl FnOnce(&mut Self::ChosenModel) -> Out,
    ) -> Out {
        f(&mut *model.borrow_mut())
    }
}



// ================
// === Template ===
// ================

/// Abstraction allowing easy FRP node definition. It generates appropriate logic based on the input
/// types. The [`Inputs`] is a tuple containing any supported combination of [`Listen`], [`Sample`],
/// and [`ListenAndSample`]. Refer to their docs to learn more about their meaning.
pub(crate) trait Template<M, Model, Output, F> {
    fn new_node_with_init<Type>(
        net: &Network<Model>,
        inputs: Self,
        body: F,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Type, Output>;
}

impl<Model> Network<Model> {
    #[inline(always)]
    fn _new_node_with_init<M, Type, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        body: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, TypedNode<Type, Output>>
    where
        Inputs: Template<M, Model, Output, F>,
    {
        let node = Inputs::new_node_with_init(self, inps, body, |n| void(init(n)));
        NodeInNetwork::new(self, node)
    }

    #[inline(always)]
    pub(crate) fn new_node_with_init<Type, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        body: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, TypedNode<Type, Output>>
    where
        Inputs: Template<ModelNotUsed, Model, Output, F>,
    {
        self._new_node_with_init::<ModelNotUsed, Type, Inputs, Output, F, _Out>(inps, body, init)
    }

    #[inline(always)]
    fn new_node_with_model_with_init<Type, Inputs, Output, F, _Out>(
        &self,
        inps: Inputs,
        body: F,
        init: impl FnOnce(&mut NodeData) -> _Out,
    ) -> NodeInNetwork<Model, TypedNode<Type, Output>>
    where
        Inputs: Template<ModelUsed, Model, Output, F>,
    {
        self._new_node_with_init::<ModelUsed, Type, Inputs, Output, F, _Out>(inps, body, init)
    }

    #[inline(always)]
    pub(crate) fn new_node<Type, Inputs, Output, F>(
        &self,
        inps: Inputs,
        body: F,
    ) -> NodeInNetwork<Model, TypedNode<Type, Output>>
    where
        Inputs: Template<ModelNotUsed, Model, Output, F>,
    {
        self.new_node_with_init(inps, body, |_| {})
    }

    #[inline(always)]
    pub(crate) fn new_node_with_model<Type, Inputs, Output, F>(
        &self,
        inps: Inputs,
        body: F,
    ) -> NodeInNetwork<Model, TypedNode<Type, Output>>
    where
        Inputs: Template<ModelUsed, Model, Output, F>,
    {
        self.new_node_with_model_with_init(inps, body, |_| {})
    }
}



impl<M, Model, Output, F> Template<M, Model, Output, F> for ()
where
    M: ModelChooser<Model>,
    F: Fn(EventContext<Output>, &mut ChosenModel<M, Model>) + 'static,
{
    #[inline(always)]
    fn new_node_with_init<Type>(
        net: &Network<Model>,
        _inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Type, Output> {
        let model = M::clone_model(&net.model);
        net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, _: &dyn Data| {
                M::with_model_borrow_mut(&model, |model| {
                    f(EventContext::unchecked_new(rt, node), model)
                })
            },
            init,
        )
    }
}

impl<M, Model, N0, Output, F> Template<M, Model, Output, F> for (Listen<N0>,)
where
    M: ModelChooser<Model>,
    N0: Node,
    F: 'static + Fn(EventContext<Output>, &mut ChosenModel<M, Model>, &N0::Output),
{
    #[inline(always)]
    fn new_node_with_init<Type>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Type, Output> {
        let model = M::clone_model(&net.model);
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, data: &dyn Data| {
                let data = unsafe { &*(data as *const dyn Data as *const N0::Output) };
                M::with_model_borrow_mut(&model, |model| {
                    f(EventContext::unchecked_new(rt, node), model, data);
                })
            },
            init,
        );
        with_runtime(|rt| inputs.field_iter(|input| rt.connect(*input, node.id)));
        node
    }
}

impl<M, Model, N0, N1, Output, F> Template<M, Model, Output, F> for (Listen<N0>, Sample<N1>)
where
    M: ModelChooser<Model>,
    N0: Node,
    N1: NodeWithDefaultOutput,
    F: 'static + Fn(EventContext<Output>, &mut ChosenModel<M, Model>, &N0::Output, &N1::Output),
{
    #[inline(always)]
    fn new_node_with_init<Type>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Type, Output> {
        let model = M::clone_model(&net.model);
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, data: &dyn Data| unsafe {
                let t0 = &*(data as *const dyn Data as *const N0::Output);
                rt.with_borrowed_node_output_coerced(inputs.1.node_id(), |t1| {
                    M::with_model_borrow_mut(&model, |model| {
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

impl<M, Model, N0, N1, N2, Output, F> Template<M, Model, Output, F>
    for (Listen<N0>, Sample<N1>, Sample<N2>)
where
    M: ModelChooser<Model>,
    N0: Node,
    N1: NodeWithDefaultOutput,
    N2: NodeWithDefaultOutput,
    F: 'static
        + Fn(EventContext<Output>, &mut ChosenModel<M, Model>, &N0::Output, &N1::Output, &N2::Output),
{
    #[inline(always)]
    fn new_node_with_init<Type>(
        net: &Network<Model>,
        inputs: Self,
        f: F,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Type, Output> {
        let model = M::clone_model(&net.model);
        let inputs = inputs.map_fields_into::<InputType>();
        let node = net.new_node_with_init_unchecked(
            move |rt: &Runtime, node: &NodeData, data: &dyn Data| unsafe {
                let t0 = &*(data as *const dyn Data as *const N0::Output);
                rt.with_borrowed_node_output_coerced(inputs.1.node_id(), |t1| {
                    rt.with_borrowed_node_output_coerced(inputs.2.node_id(), |t2| {
                        M::with_model_borrow_mut(&model, |model| {
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
