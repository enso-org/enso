use crate::prelude::*;

use crate::data::Data;
use crate::network::Network;
use crate::node::template;
use crate::node::template::Template;
use crate::runtime::with_runtime;
use crate::runtime::EventConsumer;
use crate::runtime::NodeData;
use crate::runtime::NodeId;
use crate::DefInfo;



// ============
// === Node ===
// ============

/// Any FRP node. This is a generalization for [`TypedNode`] with hidden `Type`.
#[allow(missing_docs)]
pub trait Node: Copy {
    type Output: Data;
    fn id(self) -> NodeId;
}

/// A short version of typing `Node<Output = T>`.
pub trait NodeOf<T> = Node<Output = T>;

/// An alias for [`Node`] with default output type bounds.
pub trait NodeWithDefaultOutput = Node where <Self as Node>::Output: Default;

/// A generic node representation that is parametrized with the node kind and node output. All FRP
/// nodes use this struct under the hood.
#[derive(Derivative)]
#[derivative(Copy(bound = ""))]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[repr(transparent)]
pub struct TypedNode<Type, Output> {
    pub(crate) _marker: ZST<(Type, Output)>,
    pub(crate) id:      NodeId,
}

impl<Type, Output: Data> Node for TypedNode<Type, Output> {
    type Output = Output;
    #[inline(always)]
    fn id(self) -> NodeId {
        self.id
    }
}

impl<Type, Output> TypedNode<Type, Output> {
    /// # Safety
    /// The value is checked by the function type signature to have the correct [`Output`] type.
    #[inline(never)]
    #[allow(unsafe_code)]
    pub fn emit(&self, value: &Output)
    where Output: Data {
        with_runtime(|rt| unsafe { rt.unchecked_emit_borrow(self.id, value) });
    }
}

impl<Model> Network<Model> {
    /// Constructor of the node. The [`init`] function will be called on the newly created node.
    ///
    /// # Safety
    /// The type safety is not guaranteed. You have to ensure that the [`f`] function arguments and
    /// output type are correct.
    #[inline(always)]
    #[allow(unsafe_code)]
    pub(crate) unsafe fn new_node_with_init_unchecked<Type, Output>(
        &self,
        f: impl EventConsumer,
        init: impl FnOnce(&mut NodeData),
    ) -> TypedNode<Type, Output> {
        let id = with_runtime(|rt| rt.new_node(self.id, DefInfo::unlabelled(), f, init));
        TypedNode { _marker: ZST(), id }
    }
}



// =====================
// === NodeInNetwork ===
// =====================

/// A [`Node`] associated with a [`Network`]. This is used to provide a nice API for creation of new
/// nodes without the need for the user to specify in which network to create the node.
#[derive(Debug, Deref, DerefMut)]
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

impl<'a, Model, N> NodeInNetwork<'a, Model, N> {
    #[inline(always)]
    pub(crate) fn new(network: &'a Network<Model>, node: N) -> Self {
        Self { network, node }
    }

    #[inline(always)]
    pub(crate) fn new_node<Type, Inputs, Output, F>(
        &self,
        inputs: Inputs,
        body: F,
    ) -> NodeInNetwork<'a, Model, TypedNode<Type, Output>>
    where
        Inputs: Template<template::ModelNotUsed, Model, Output, F>,
    {
        self.network.new_node(inputs, body)
    }

    #[inline(always)]
    pub(crate) fn new_node_with_model<Type, Inputs, Output, F>(
        &self,
        inputs: Inputs,
        body: F,
    ) -> NodeInNetwork<'a, Model, TypedNode<Type, Output>>
    where
        Inputs: Template<template::ModelUsed, Model, Output, F>,
    {
        self.network.new_node_with_model(inputs, body)
    }
}
