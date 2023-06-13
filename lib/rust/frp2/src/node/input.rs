use crate::prelude::*;

use crate::network::NodeId;
use crate::node::Node;

#[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, PartialEq)]
#[repr(transparent)]
pub struct Listen<T>(pub(crate) T);

#[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, PartialEq)]
#[repr(transparent)]
pub struct Sample<T>(pub(crate) T);

#[derive(Clone, Copy, Debug, Deref, DerefMut, Eq, PartialEq)]
#[repr(transparent)]
pub struct ListenAndSample<T>(pub(crate) T);

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
