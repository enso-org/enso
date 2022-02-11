//! A module containing helpers for FRP networks in asynchronous environments.

use crate::prelude::*;

use crate::extend;
use crate::node;
use crate::stream::EventOutput;
use crate::HasLabel;
use crate::Label;
use crate::Network;

use std::pin::Pin;
use std::task::Context;
use std::task::Poll;



// ===================
// === FutureEvent ===
// ===================

/// A structure returned from [`EventOutputExt::next_event`] method.
///
/// It's a future which is resolved once the given frp node emits an event. It also allows checking
/// if the value is provided - a useful tool when testing components with FRP API.
#[derive(Debug)]
pub struct FutureEvent<Out> {
    _network: Network,
    label:    Label,
    value:    Rc<RefCell<Option<Out>>>,
    wakers:   Rc<RefCell<SmallVec<[std::task::Waker; 1]>>>,
}

impl<Out> FutureEvent<Out> {
    fn new<T>(node: &T) -> Self
    where
        T: EventOutput<Output = Out> + HasLabel,
        Out: node::Data, {
        let label = node.label();
        let network = Network::new(format!("{label}.future_event"));
        let value: Rc<RefCell<Option<Out>>> = default();
        let wakers: Rc<RefCell<SmallVec<[std::task::Waker; 1]>>> = default();
        extend! { network
            let node = node.clone_ref();
            eval node ([value,wakers](event) {
                value.set(event.clone());
                for waker in wakers.take() {
                    waker.wake();
                }
            });
        }

        Self { _network: network, label, value, wakers }
    }

    /// Returns the received event.
    ///
    /// # Panics
    ///
    /// Panics if no event was emitted by the node since this structure creation.
    pub fn expect(self) -> Out {
        self.value.take().unwrap_or_else(|| ipanic!("Expected {self.label} event"))
    }
}

impl<Out: node::Data> std::future::Future for FutureEvent<Out> {
    type Output = Out;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let value = self.value.take();
        match value {
            Some(val) => Poll::Ready(val),
            None => {
                self.wakers.borrow_mut().push(cx.waker().clone());
                Poll::Pending
            }
        }
    }
}



// ======================
// === EventOutputExt ===
// ======================

/// A trait implemented automatically for every FRP node with helpers related to asynchronous
/// environment.
pub trait EventOutputExt: EventOutput {
    /// Returns the future which is resolved once this node emits a next event. The events emitted
    /// before calling this method are not considered.
    fn next_event(&self) -> FutureEvent<Self::Output>;
}

impl<T: EventOutput + HasLabel> EventOutputExt for T {
    fn next_event(&self) -> FutureEvent<Self::Output> {
        FutureEvent::new(self)
    }
}
