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
    fn unchecked_new(runtime: &'a Runtime, node: &'a NodeData) -> Self {
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

/// For the input `[N0] [N2] [N2 N3]` it generates the following impl:
/// ```text
/// impl<M, Model, N0, N1, N2, N3, Output, F>
/// Template<M, Model, Output, F> for
/// (Listen<N0>, ListenAndSample<N1>, Sample<N2>, Sample<N3>) {...}
/// ```
///
/// The first bracket can contain zero or one value only. See docs of this crate to learn more.
macro_rules! gen_template_impl {
    ([$($listen:tt)?] [$($listen_and_sample:tt)*] [$($sample:tt)*]) => {
        #[allow(non_snake_case)]
        #[allow(unused_unsafe)]
        impl<M, Model, $($listen,)? $($listen_and_sample,)* $($sample,)* Output, F>
        Template<M, Model, Output, F> for (
            $(Listen<$listen>,)?
            $(ListenAndSample<$listen_and_sample>,)*
            $(Sample<$sample>,)*
        )
        where
            M: ModelChooser<Model>,
            $($listen: Node,)?
            $($listen_and_sample: NodeWithDefaultOutput,)*
            $($sample: NodeWithDefaultOutput,)*
            F: 'static + Fn(
                EventContext<Output>,
                &mut ChosenModel<M, Model>,
                $(&$listen::Output,)?
                $(&$listen_and_sample::Output,)?
                $(&$sample::Output,)*
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
                                let $listen = &*(_data as *const dyn Data as *const $listen::Output);
                                let (_, _inputs) = _inputs.pop_first_field();
                            )?
                            gen_template_impl_body! {
                                rt, model, node, f, _inputs,
                                [$($listen)? $($listen_and_sample)* $($sample)*],
                                [$($listen_and_sample)* $($sample)*]
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
    ($rt:tt, $model:tt, $node:tt, $f:tt, $inputs:tt, $t:tt, [$n:tt $($ns:tt)*]) => {
        let (input, _inputs) = $inputs.pop_first_field();
        $rt.with_borrowed_node_output_coerced(input.node_id(), |$n| {
            gen_template_impl_body!($rt, $model, $node, $f, _inputs, $t, [$($ns)*]);
        });
    };
    ($rt:tt, $model:tt, $node:tt, $f:tt, $inputs:tt, [$($ns:tt)*], []) => {
        M::with_model_borrow_mut(&$model, |model| {
            $f(EventContext::unchecked_new($rt, $node), model, $($ns,)*)
        })
    }
}

/// For the input `[f 1 2 3 4]` it generates:
/// ```text
/// f! {}
/// f! {1}
/// f! {1 2}
/// f! {1 2 3}
/// f! {1 2 3 4}
/// ```
///
/// For the input length of X, it generates (X + 1) calls to `f!`.
macro_rules! with_all_init_sequences {
    ($f:ident $($ns:literal)*) => {
        with_all_init_sequences! { @ $f [] [ $($ns)* ] }
    };
    (@ $f:ident [$($ts:tt)*] [$s:tt $($ss:tt)*]) => {
        $f! { $($ts)* }
        with_all_init_sequences! { @ $f [$($ts)* $s] [$($ss)*] }
    };
    (@ $f:ident  [$($ts:tt)*] []) => {
        $f! { $($ts)* }
    };
}

/// For the input `1 2 3 4` it generates:
/// ```text
/// gen_template_impl! { [N0] [ ]            [N1 N2 N3 N4] }
/// gen_template_impl! { []   [ ]            [N1 N2 N3 N4] }
/// gen_template_impl! { [N0] [N1]           [N2 N3 N4]    }
/// gen_template_impl! { []   [N1]           [N2 N3 N4]    }
/// gen_template_impl! { [N0] [N1 N2]        [N3 N4]       }
/// gen_template_impl! { []   [N1 N2]        [N3 N4]       }
/// gen_template_impl! { [N0] [N1 N2 N3]     [N4]          }
/// gen_template_impl! { []   [N1 N2 N3]     [N4]          }
/// gen_template_impl! { [N0] [N1 N2 N3 N4]  []            }
/// gen_template_impl! { []   [N1 N2 N3 N4]  []            }
/// ```
///
/// For the empty input, it generates:
/// ```text
/// gen_template_impl! { [N0] [] [] }
/// gen_template_impl! { []   [] [] }
/// ```
///
/// For the input length of X, it generates (X + 1) * 2 calls to `gen_template_impl!`.
macro_rules! with_all_divisions {
    ($($ns:literal)*) => { paste! {
        with_all_divisions! { @ [] [ $([<N $ns>])* ] }
    }};
    (@ [$($ts:tt)*] [$s:tt $($ss:tt)*]) => {
        gen_template_impl! { [N0] [$($ts)*] [$s $($ss)*] }
        gen_template_impl! { [] [$($ts)*] [$s $($ss)*] }
        with_all_divisions! { @ [$($ts)* $s] [$($ss)*] }
    };
    (@ $ts:tt []) => {
        gen_template_impl! { [N0] $ts [] }
        gen_template_impl! { [] $ts [] }
    };
}

// For the input of X, it generates (X + 1) * (X + 1) * 2 calls to `gen_template_impl!`.
// It means, that for the input of 16, it generates 578 calls to `gen_template_impl!`.
with_all_init_sequences![with_all_divisions 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15];
