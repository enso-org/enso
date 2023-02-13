//! Module with the Enso Procol RPC handler.

use crate::prelude::*;

use crate::common::event::Event;
use crate::common::ongoing_calls::OngoingCalls;

use futures::channel::mpsc::UnboundedSender;
use json_rpc::Transport;
use json_rpc::TransportEvent;
use std::future::Future;



// ===================
// === Disposition ===
// ===================

/// Describes how the given server's message should be dealt with.
#[derive(Debug)]
pub enum Disposition<Id, Reply, Notification>
where
    Id: Debug,
    Reply: Debug,
    Notification: Debug, {
    /// Ignore the message.
    Ignore,
    /// Treat as a reply to an open request.
    HandleReply {
        /// Remote Call ID (correlation ID).
        id:    Id,
        /// The reply contents.
        reply: Reply,
    },
    /// Emit given event (usually error or a notification).
    EmitEvent {
        /// Event to be emitted.
        event: Event<Notification>,
    },
}

impl<Id, Reply, Notification> Disposition<Id, Reply, Notification>
where
    Id: Debug,
    Reply: Debug,
    Notification: Debug,
{
    /// Creates a notification event disposition.
    pub fn notify(notification: Notification) -> Self {
        Disposition::EmitEvent { event: Event::Notification(notification) }
    }

    /// Creates an error event disposition.
    pub fn error(error: impl Into<failure::Error> + Debug) -> Self {
        Disposition::EmitEvent { event: Event::Error(error.into()) }
    }
}



// ===================
// === HandlerData ===
// ===================

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct HandlerData<Id, Reply, Notification>
where
    Id: Eq + Hash + Debug,
    Notification: Debug,
    Reply: Debug, {
    #[derivative(Debug = "ignore")]
    transport:     Box<dyn Transport>,
    sender:        Option<UnboundedSender<Event<Notification>>>,
    ongoing_calls: OngoingCalls<Id, Reply>,
    #[derivative(Debug = "ignore")]
    processor:     Box<dyn FnMut(TransportEvent) -> Disposition<Id, Reply, Notification>>,
}

impl<Id, Reply, Notification> HandlerData<Id, Reply, Notification>
where
    Id: Copy + Debug + Display + Hash + Eq + Send + Sync + 'static,
    Notification: Debug,
    Reply: Debug,
{
    fn new<T, P>(transport: T, processor: P) -> HandlerData<Id, Reply, Notification>
    where
        T: Transport + 'static,
        P: FnMut(TransportEvent) -> Disposition<Id, Reply, Notification> + 'static, {
        HandlerData {
            transport:     Box::new(transport),
            sender:        None,
            ongoing_calls: OngoingCalls::new(),
            processor:     Box::new(processor),
        }
    }

    /// Emits event. Clients can consume them through `event_stream`.
    fn emit_event(&mut self, event: Event<Notification>) {
        if let Some(sender) = self.sender.as_ref() {
            // Error can happen if there is no listener. But we don't mind this.
            let _ = sender.unbounded_send(event);
        }
    }

    /// Feeds the reply to complete the corresponding open request.
    fn process_reply(&mut self, id: Id, reply: Reply) {
        info!("Processing reply to request {id}: {reply:?}");
        if let Err(error) = self.ongoing_calls.complete_request(id, reply) {
            self.emit_error(error);
        }
    }

    /// Helper that wraps error into an appropriate event value and emits it.
    fn emit_error(&mut self, error: impl Into<failure::Error> + Debug) {
        info!("Emitting error: {error:?}");
        let event = Event::Error(error.into());
        self.emit_event(event);
    }

    /// Handles incoming transport event. The `processor` is used to decide the further processing
    /// path.
    ///
    /// Main entry point for input data while running. Should be connected to the `Transport`s
    /// output event stream.
    pub fn process_event(&mut self, event: TransportEvent) {
        debug_span!("Processing incoming transport event").in_scope(|| {
            debug!("Transport event contents: {event:?}.");
            match event {
                TransportEvent::TextMessage(_) | TransportEvent::BinaryMessage(_) => {
                    let disposition = (self.processor)(event);
                    debug!("Disposition: {disposition:?}");
                    match disposition {
                        Disposition::HandleReply { id, reply } => self.process_reply(id, reply),
                        Disposition::EmitEvent { event } => self.emit_event(event),
                        Disposition::Ignore => {}
                    }
                }
                TransportEvent::Opened => {}
                TransportEvent::Closed => self.emit_event(Event::Closed),
            }
        });
    }

    pub fn make_request<F, R>(
        &mut self,
        message: &dyn IsRequest<Id = Id>,
        f: F,
    ) -> impl Future<Output = FallibleResult<R>>
    where
        F: FnOnce(Reply) -> FallibleResult<R>,
    {
        debug_span!("Making a new RPC call").in_scope(|| {
            let id = message.id();
            let ret = self.ongoing_calls.open_new_request(id, f);
            debug!("Sending message {message:?}");
            let sending_result = message.send(self.transport.as_mut());
            if sending_result.is_err() {
                // If we failed to send the request, it should be immediately removed.
                // This will result in the returned future immediately yielding error.
                self.ongoing_calls.remove_request(&id);
            }
            ret
        })
    }

    /// Creates a new stream with events from this handler.
    ///
    /// If such stream was already existing, it will be finished (and
    /// continuations should be able to process any remaining events).
    pub fn event_stream(&mut self) -> impl Stream<Item = Event<Notification>> {
        let (transmitter, receiver) = futures::channel::mpsc::unbounded();
        self.sender = Some(transmitter);
        receiver
    }

    /// See the `runner` on the `Client`.
    pub fn runner(this: &Rc<RefCell<Self>>) -> impl Future<Output = ()> {
        let event_receiver = this.borrow_mut().transport.establish_event_stream();
        let weak_this = Rc::downgrade(this);
        event_receiver.for_each(move |event: TransportEvent| {
            if let Some(state) = weak_this.upgrade() {
                state.borrow_mut().process_event(event);
            }
            futures::future::ready(())
        })
    }
}



// ===============
// === Handler ===
// ===============

/// Handler is a main provider of RPC protocol. Given a transport capable of transporting messages,
/// it manages whole communication with a peer. Works both with binary and text protocols.
///
/// It allows making request, where each request gets a unique `Id` and its future result is
/// represented using `Future`.
/// `Reply` represents peer's reply to a request.
/// `Notification` represents a notification received from a peer.
///
/// Notifications and internal messages are emitted using the `event_stream` stream.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Handler<Id, Reply, Notification: Debug>
where
    Id: Eq + Hash + Debug,
    Notification: Debug,
    Reply: Debug, {
    state: Rc<RefCell<HandlerData<Id, Reply, Notification>>>,
}

/// A value that can be used to represent a request to remote RPC server.
pub trait IsRequest: Debug {
    /// Request ID.
    type Id: Copy;

    /// Send the message to the peer using the provided transport.
    fn send(&self, transport: &mut dyn Transport) -> FallibleResult;

    /// Request ID, that will be used later to associate peer's response.
    fn id(&self) -> Self::Id;
}

impl<Id, Reply, Notification> Handler<Id, Reply, Notification>
where
    Id: Copy + Debug + Display + Hash + Eq + Send + Sync + 'static,
    Notification: Debug,
    Reply: Debug,
{
    /// Creates a new handler operating over given transport.
    ///
    /// `processor` must deal with decoding incoming transport events.
    pub fn new<T, P>(transport: T, processor: P) -> Self
    where
        T: Transport + 'static,
        P: FnMut(TransportEvent) -> Disposition<Id, Reply, Notification> + 'static, {
        let state = Rc::new(RefCell::new(HandlerData::new(transport, processor)));
        Handler { state }
    }

    /// Starts a new request described by a given message.
    ///
    /// The request shall be sent to the server immediately and then await the reply.
    /// Once the reply to this request arrives (or the call is abandoned for other reason, e.g. due
    /// to a disconnection) the returned Future shall complete.
    ///
    /// `f` is a function that shall be used to handle server's reply. It should try returning the
    /// desired request's result and must handle errors. (thus `FallibleResult`)
    ///
    /// We use here `&dyn IsRequest` rather them generic parameter `impl IsRequest` only to avoid
    /// lifetime issues caused by this Rust compiler bug:
    /// https://github.com/rust-lang/rust/issues/42940
    pub fn make_request<F, R>(
        &self,
        message: &dyn IsRequest<Id = Id>,
        f: F,
    ) -> impl Future<Output = FallibleResult<R>>
    where
        F: FnOnce(Reply) -> FallibleResult<R>,
    {
        self.state.borrow_mut().make_request(message, f)
    }

    /// See the `runner` on the `Client`.
    pub fn runner(&self) -> impl Future<Output = ()> {
        HandlerData::runner(&self.state)
    }

    /// Creates a new stream with events from this handler.
    ///
    /// If such stream was already existing, it will be finished (and continuations should be able
    /// to process any remaining events).
    pub fn event_stream(&self) -> impl Stream<Item = Event<Notification>> {
        self.state.borrow_mut().event_stream()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use json_rpc::test_util::transport::mock::MockTransport;

    #[test]
    fn test_closed_socked_event_passing() {
        let mut transport = MockTransport::new();
        let processor = |msg| panic!("Must never be called in this test, but got {msg:?}!");
        let handler = Handler::<i32, (), ()>::new(transport.clone_ref(), processor);
        let mut runner = handler.runner().boxed_local();
        let mut events = handler.event_stream().boxed_local();
        events.expect_pending();
        transport.mock_connection_closed();
        events.expect_pending();

        // Process events.
        runner.expect_pending();

        let event = events.expect_next();
        assert!(matches!(event, Event::Closed), "Event was: {event:?}");
        events.expect_pending();
    }
}
