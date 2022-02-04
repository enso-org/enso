use crate::prelude::*;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use crate::node;
use crate::nodes;
use crate::stream::EventOutput;

pub struct FutureEvent<Out: node::Data> {
    node:  nodes::Future<Out>,
    value: Rc<RefCell<Option<Out>>>,
}

impl<Out: node::Data> FutureEvent<Out> {
    pub(crate) fn new(node: nodes::Future<Out>) -> Self {
        let value = node.upgrade().map(|n| n.value.clone_ref()).unwrap_or_default();
        Self { node, value }
    }

    pub fn expect(self, msg: &str) -> Out {
        self.value.borrow().as_ref().expect(msg).clone()
    }
}

impl<Out: node::Data> std::future::Future for FutureEvent<Out> {
    type Output = Option<Out>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let value = self.value.borrow();
        match &*value {
            Some(val) => Poll::Ready(Some(val.clone())),
            None =>
                if let Some(owned) = self.node.upgrade() {
                    owned.wakers.borrow_mut().push(cx.waker().clone());
                    Poll::Pending
                } else {
                    Poll::Ready(None)
                },
        }
    }
}
