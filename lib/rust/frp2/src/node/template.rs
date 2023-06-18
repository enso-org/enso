//! FRP node template implementation. It defines the node boilerplate logic for a given set of input
//! types, allowing node definitions to be short and easy to read.

use crate::prelude::*;
use enso_generics::traits::*;

use crate::data::Data;
use crate::network::Network;
use crate::node::input;
use crate::node::input::Listen;
use crate::node::input::ListenAndSample;
use crate::node::input::Sample;
use crate::node::node::NodeInNetwork;
use crate::node::node::TypedNode;
use crate::node::Node;
use crate::node::NodeWithDefaultOutput;
use crate::runtime::with_runtime;
use crate::runtime::NodeData;
use crate::runtime::Runtime;

use shapely::replace2;


// TODO: Add explanations why unsafe code is actually safe. Should be part of this issue:
// https://github.com/enso-org/enso/issues/7043

// ====================
// === EventContext ===
// ====================

/// A wrapper for the runtime and current node data with explicit information about the event type.
/// It is mainly used to make the API for defining nodes easy and type-safe.
pub(crate) struct EventContext<'a, Output> {
    tp:      ZST<Output>,
    runtime: &'a Runtime,
    node:    &'a NodeData,
}

impl<'a, Output> EventContext<'a, Output> {
    /// Constructor. The `Output` type is not checked, so you need to be careful when using this
    /// function.
    #[inline(always)]
    #[allow(unsafe_code)]
    unsafe fn unchecked_new(runtime: &'a Runtime, node: &'a NodeData) -> Self {
        EventContext { tp: ZST(), runtime, node }
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

/// A trait allowing choosing either real FRP Network model or a phantom one ([`ModelSkipped`])
/// based on the provided type-level configuration.
trait ModelChooser<Model> {
    type ChosenModel;
    type ClonedModel: 'static;
    fn clone_model(model: &Rc<ZeroOverheadRefCell<Model>>) -> Self::ClonedModel;
    fn with_model_borrow_mut<Out>(
        model: &Self::ClonedModel,
        f: impl FnOnce(&mut Self::ChosenModel) -> Out,
    ) -> Out;
}

impl<Model> ModelChooser<Model> for ModelNotUsed {
    type ChosenModel = ModelSkipped;
    type ClonedModel = ModelSkipped;
    #[inline(always)]
    fn clone_model(_model: &Rc<ZeroOverheadRefCell<Model>>) -> Self::ClonedModel {
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
    type ClonedModel = Rc<ZeroOverheadRefCell<Model>>;
    #[inline(always)]
    fn clone_model(model: &Rc<ZeroOverheadRefCell<Model>>) -> Self::ClonedModel {
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
        let node = Inputs::new_node_with_init(self, inps, body, |n| init(n).void());
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

    #[allow(dead_code)]
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



// ======================
// === Template Impls ===
// ======================

/// Generate a template implementation for the given input types. Below we present an example
/// expansion of this macro.
///
/// ```text
/// >> gen_template_impl! { [L = N0 N1] [N2 N3] [N4 N5] }
///
/// impl<M, Model, L, N0, N1, N2, N3, N4, N5, Output, F>
/// Template<M, Model, Output, F> for
///     (Listen<N0>, Listen<N1>, ListenAndSample<N2>, ListenAndSample<N3>, Sample<N4>, Sample<N5>)
/// where
///         M: ModelChooser<Model>,
///         N0: Node<Output=L>, N1: Node<Output=L>,
///         N2: NodeWithDefaultOutput, N3: NodeWithDefaultOutput,
///         N4: NodeWithDefaultOutput, N5: NodeWithDefaultOutput,
///         F: 'static + Fn(EventContext<Output>, &mut ChosenModel<M, Model>,
///             &L, &N2::Output, &N3::Output, &N4::Output, &N5::Output)
/// {
/// ```
macro_rules! gen_template_impl {
    ([$($lty:tt => $($l:tt)*)?] [$($ls:tt)*] [$($s:tt)*]) => {
        #[allow(non_snake_case)]
        #[allow(unused_unsafe)]
        impl<M, Model, $($lty,)? $($($l,)*)? $($ls,)* $($s,)* Output, F>
        Template<M, Model, Output, F> for (
            $($(Listen<$l>,)*)?
            $(ListenAndSample<$ls>,)*
            $(Sample<$s>,)*
        )
        where
            M: ModelChooser<Model>,
            $($($l: Node<Output = $lty>,)*)?
            $($ls: NodeWithDefaultOutput,)*
            $($s: NodeWithDefaultOutput,)*
            F: 'static + Fn(
                EventContext<Output>,
                &mut ChosenModel<M, Model>,
                $(&$lty,)?
                $(&$ls::Output,)*
                $(&$s::Output,)*
            ),
        {
            #[inline(always)]
            #[allow(unsafe_code)]
            fn new_node_with_init<Type>(
                net: &Network<Model>,
                inputs: Self,
                f: F,
                init: impl FnOnce(&mut NodeData),
            ) -> TypedNode<Type, Output> {
                let model = M::clone_model(&net.model);
                let _inputs = inputs.map_fields_into::<input::Type>();
                let node = unsafe {
                    net.new_node_with_init_unchecked(
                        move |rt: &Runtime, node: &NodeData, _data: &dyn Data| {
                            $(
                                #[allow(trivial_casts)]
                                let $lty = &*(_data as *const dyn Data as *const $lty);
                            )?
                            $($(replace2!{($l) => let (_, _inputs) = _inputs.pop_first_field();})*)?
                            gen_template_impl_body! {
                                rt, model, node, f, _inputs,
                                [$($lty)?],
                                [$($ls)* $($s)*],
                                [$($ls)* $($s)*]
                            }
                        },
                        init,
                    )
                };
                with_runtime(|rt| _inputs.field_iter(|input| rt.connect(*input, node.id)));
                node
            }
        }
    };
}

macro_rules! gen_template_impl_body {
    ($rt:tt, $model:tt, $node:tt, $f:tt, $inputs:tt, $l:tt, $ls:tt, [$n:tt $($ns:tt)*]) => {
        let (input, _inputs) = $inputs.pop_first_field();
        $rt.with_borrowed_node_output_coerced(input.node_id(), |$n| {
            gen_template_impl_body!($rt, $model, $node, $f, _inputs, $l, $ls, [$($ns)*]);
        });
    };
    ($rt:tt, $model:tt, $node:tt, $f:tt, $inputs:tt, [$($lty:tt)?], [$($ls:tt)*], []) => {
        M::with_model_borrow_mut(&$model, |model| {
            $f(EventContext::unchecked_new($rt, $node), model, $($lty,)? $($ls,)*)
        })
    }
}



macro_rules! _range {
    (($($f:tt)*) $(($($args:tt)*))? [] $($ts:tt)*) => { $($f)* { $($($args)*)? [] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0] $($ts:tt)*) => { $($f)* { $($($args)*)? [0] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1] $($ts:tt)*) => { $($f)* { $($($args)*)? [1] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2] $($ts:tt)*) => { $($f)* { $($($args)*)? [2] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3] $($ts:tt)*) => { $($f)* { $($($args)*)? [3] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4] $($ts:tt)*) => { $($f)* { $($($args)*)? [4] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5] $($ts:tt)*) => { $($f)* { $($($args)*)? [5] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6] $($ts:tt)*) => { $($f)* { $($($args)*)? [6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7] $($ts:tt)*) => { $($f)* { $($($args)*)? [7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8] $($ts:tt)*) => { $($f)* { $($($args)*)? [8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9] $($ts:tt)*) => { $($f)* { $($($args)*)? [9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10] $($ts:tt)*) => { $($f)* { $($($args)*)? [10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [11] $($ts:tt)*) => { $($f)* { $($($args)*)? [11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [12] $($ts:tt)*) => { $($f)* { $($($args)*)? [12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [13] $($ts:tt)*) => { $($f)* { $($($args)*)? [13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [14] $($ts:tt)*) => { $($f)* { $($($args)*)? [14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [15] $($ts:tt)*) => { $($f)* { $($($args)*)? [15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [16] $($ts:tt)*) => { $($f)* { $($($args)*)? [16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=0] $($ts:tt)*) => { $($f)* { $($($args)*)? [0] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=1] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=2] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=3] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=4] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=5] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=6] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [0..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=1] $($ts:tt)*) => { $($f)* { $($($args)*)? [1] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=2] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=3] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=4] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=5] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=6] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [1..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=2] $($ts:tt)*) => { $($f)* { $($($args)*)? [2] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=3] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=4] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=5] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=6] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [2..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=3] $($ts:tt)*) => { $($f)* { $($($args)*)? [3] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=4] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=5] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=6] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [3..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [3 4 5 6 7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=4] $($ts:tt)*) => { $($f)* { $($($args)*)? [4] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=5] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=6] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [4..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [4 5 6 7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=5] $($ts:tt)*) => { $($f)* { $($($args)*)? [5] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=6] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [5..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [5 6 7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=6] $($ts:tt)*) => { $($f)* { $($($args)*)? [6] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [6..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [6 7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=7] $($ts:tt)*) => { $($f)* { $($($args)*)? [7] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [7..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [7 8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=8] $($ts:tt)*) => { $($f)* { $($($args)*)? [8] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [8..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [8 9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=9] $($ts:tt)*) => { $($f)* { $($($args)*)? [9] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [9 10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [9 10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [9 10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [9 10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [9 10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [9 10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [9..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [9 10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10..=10] $($ts:tt)*) => { $($f)* { $($($args)*)? [10] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [10 11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [10 11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [10 11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [10 11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [10 11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [10..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [10 11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [11..=11] $($ts:tt)*) => { $($f)* { $($($args)*)? [11] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [11..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [11 12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [11..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [11 12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [11..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [11 12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [11..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [11 12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [11..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [11 12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [12..=12] $($ts:tt)*) => { $($f)* { $($($args)*)? [12] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [12..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [12 13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [12..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [12 13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [12..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [12 13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [12..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [12 13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [13..=13] $($ts:tt)*) => { $($f)* { $($($args)*)? [13] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [13..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [13 14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [13..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [13 14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [13..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [13 14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [14..=14] $($ts:tt)*) => { $($f)* { $($($args)*)? [14] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [14..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [14 15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [14..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [14 15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [15..=15] $($ts:tt)*) => { $($f)* { $($($args)*)? [15] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [15..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [15 16] $($ts)* } };
    (($($f:tt)*) $(($($args:tt)*))? [16..=16] $($ts:tt)*) => { $($f)* { $($($args)*)? [16] $($ts)* } };
}

// range! { (range!)((range!)((foo!)(aa) [9..=10]) [4..=5]) [1..=3] }

/// For the input `[(f!)(args) [1 2 3 4]]` (The `(args)` part is optional) it generates:
/// ```text
/// f! { args [] }
/// f! { args [1] }
/// f! { args [1 2] }
/// f! { args [1 2 3] }
/// f! { args [1 2 3 4] }
/// ```
///
/// For the input length of X, it generates (X + 1) calls to `f!`.
macro_rules! with_all_init_sequences {
    ($f:tt $(($($args:tt)*))? [$($ns:tt)*]) => {
        with_all_init_sequences! { @ $f $(($($args)*))? [] [ $($ns)* ] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [$($ts:tt)*] [$s:tt $($ss:tt)*]) => {
        $($f)* { $($($args)*)? [$($ts)*] }
        with_all_init_sequences! { @ ($($f)*) $(($($args)*))? [$($ts)* $s] [$($ss)*] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [$($ts:tt)*] []) => {
        $($f)* { $($($args)*)? [$($ts)*] }
    };
}


/// Below we present an example expansion of this macro. The `(args)` part is optional:
///
/// ```text
/// >> all_splits2!((f!)(args) [1 2 3])
///
/// f! { args []      [1 2 3] }
/// f! { args [1]     [2 3]   }
/// f! { args [1 2]   [3]     }
/// f! { args [1 2 3] []      }
/// ```
///
/// For the input length X, it generates X calls to `f!`.
macro_rules! all_splits2 {
    (($($f:tt)*) $(($($args:tt)*))? [$($ts:tt)*]) => {
        all_splits2! { @ ($($f)*) $(($($args)*))? [] [$($ts)*] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [$($as:tt)*] [$b:tt $($bs:tt)*]) => {
        $($f)* { $($($args)*)? [$($as)*] [$b $($bs)*] }
        all_splits2! { @ ($($f)*) $(($($args)*))? [$($as)* $b] [$($bs)*] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [$a:tt $($as:tt)*] []) => {
        $($f)* { $($($args)*)? [$a $($as)*] [] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [] []) => {
        $($f)* { $($($args)*)? [] [] }
    };
}

/// Below we present an example expansion of this macro. The `(args)` part is optional:
///
/// ```text
/// >> all_splits3!((f!)(args) [1 2 3])
///
/// f! { args []      []      [1 2 3] }
/// f! { args []      [1]     [2 3]   }
/// f! { args []      [1 2]   [3]     }
/// f! { args []      [1 2 3] []      }
/// f! { args [1]     []      [2 3]   }
/// f! { args [1]     [2]     [3]     }
/// f! { args [1]     [2 3]   []      }
/// f! { args [1 2]   []      [3]     }
/// f! { args [1 2]   [3]     []      }
/// f! { args [1 2 3] []      []      }
/// ```
///
/// For the input length X, it generates (X + 1) * (X + 2) / 2 calls to `f!`.
macro_rules! _all_splits3 {
    (($($f:tt)*) $(($($args:tt)*))? [$($ts:tt)*]) => {
        all_splits3! { @ ($($f)*) $(($($args)*))? [] [] [$($ts)*] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [$($as:tt)*] [$($bs:tt)*] [$c:tt $($cs:tt)*]) => {
        $($f)* { $($($args)*)? [$($as)*] [$($bs)*] [$c $($cs)*] }
        all_splits3! { @ ($($f)*) $(($($args)*))? [$($as)*] [$($bs)* $c] [$($cs)*] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [$($as:tt)*] [$b:tt $($bs:tt)*] []) => {
        $($f)* { $($($args)*)? [$($as)*] [$b $($bs)*] [] }
        all_splits3! { @ ($($f)*) $(($($args)*))? [$($as)* $b] [] [$($bs)*] }
    };
    (@ ($($f:tt)*) $(($($args:tt)*))? [$($as:tt)*] [] []) => {
        $($f)* { $($($args)*)? [$($as)*] [] [] }
    };
}

macro_rules! gen_template_impls {
    ([$($as:tt)*] [$($bs:tt)*]) => { paste! {
        gen_template_impls! { @ [ $([<N $as>])* ] [ $([<N $bs>])* ] }
    }};
    (@ [] [$($bs:tt)*]) => { paste! {
        gen_template_impl! { [] [] [$($bs)*] }
    }};
    (@ [$($as:tt)*] [$($bs:tt)*]) => { paste! {
        gen_template_impl! { [] [$($as)*] [$($bs)*] }
        gen_template_impl! { [L => $($as)*] [] [$($bs)*] }
    }};
}

// For the input of X, it generates less than 2 * X * (X + 1) impls. So, for X = 16, it generates
// less than 512 impls.
with_all_init_sequences! { (all_splits2!)((gen_template_impls!)) [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] }
