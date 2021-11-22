//! Module providing `Handler` and related types used by its API.

use crate::prelude::*;

use crate::api;
use crate::api::Result;
use crate::ensogl::sleep;
use crate::ensogl::Duration;
use crate::error::HandlingError;
use crate::error::RpcError;
use crate::messages;
use crate::messages::Id;
use crate::transport::Transport;
use crate::transport::TransportEvent;

use futures::channel::mpsc::unbounded;
use futures::channel::mpsc::UnboundedSender;
use futures::channel::oneshot;
use futures::future;
use futures::FutureExt;
use futures::Stream;
use futures::StreamExt;
use serde::de::DeserializeOwned;
use std::future::Future;



// ====================
// === ReplyMessage ===
// ====================

/// Partially decoded reply message.
///
/// Known if `Error` or `Success` but returned value remains in JSON form.
pub type ReplyMessage = messages::Result<serde_json::Value>;

/// Converts remote message with JSON-serialized result into `Result<Ret>`.
pub fn decode_result<Ret: DeserializeOwned>(
    result: messages::Result<serde_json::Value>,
) -> Result<Ret> {
    match result {
        messages::Result::Success(ret) => Ok(serde_json::from_value::<Ret>(ret.result)?),
        messages::Result::Error { error } => Err(RpcError::RemoteError(error)),
    }
}



// ===================
// === IdGenerator ===
// ===================

/// Simple counter-based struct used to generate unique Id's.
///
/// The generated Ids are sequence 0, 1, 2, …
#[derive(Clone, Copy, Debug)]
pub struct IdGenerator {
    /// Next Id value to be returned.
    pub counter: i64,
}

impl IdGenerator {
    /// Obtain the new Id.
    pub fn generate(&mut self) -> Id {
        let id = self.counter;
        self.counter += 1;
        Id(id)
    }

    /// Create a new IdGenerator counting from 0.
    pub fn new() -> IdGenerator {
        IdGenerator::new_from(0)
    }

    /// Create a new IdGenerator that gives Ids beginning with given number.
    fn new_from(counter: i64) -> IdGenerator {
        IdGenerator { counter }
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}


// =============
// === Event ===
// =============

/// Event emitted by the `Handler<N>`.
#[derive(Debug)]
pub enum Event<N> {
    /// Transport has been closed.
    Closed,
    /// Error occurred.
    Error(HandlingError),
    /// Notification received.
    Notification(N),
}



// ===================
// === HandlerData ===
// ===================

/// Container that stores Sender's for ongoing calls. Each call identified by
/// id has its own sender. After reply is received, the call is removed
/// from this container.
pub type OngoingCalls = HashMap<Id, oneshot::Sender<ReplyMessage>>;



// ===============
// === Handler ===
// ===============

/// Handler is a main provider of RPC protocol. Given with a transport capable
/// of transporting text messages, it manages whole communication with a peer.
///
/// It allows making request, where method calls are described by values
/// implementing `RemoteMethodCall`. The response is returned as a `Future`.
///
/// Notifications and internal messages are emitted using the `events` stream.
///
/// `Notification` is a type for notifications. It should implement
/// `DeserializeOwned` and deserialize from JSON maps with `method` and `params`
/// fields.
pub use enso_shapely::shared;

shared! { Handler

/// Mutable state of the `Handler`.
#[derive(Debug)]
pub struct HandlerData<Notification> {
    /// Timeout for futures.
    timeout         : Duration,
    /// Ongoing calls.
    ongoing_calls   : OngoingCalls,
    /// Handle to send outgoing events.
    outgoing_events : Option<UnboundedSender<Event<Notification>>>,
    /// Provides identifiers for requests.
    id_generator    : IdGenerator,
    /// Transports text messages between this handler and the peer.
    transport       : Box<dyn Transport>,
}


impl<Notification> {
    /// Inserts a new entry for an ongoing request awaiting reply.
    pub fn insert_ongoing_request(&mut self, id:Id, sender:oneshot::Sender<ReplyMessage>) {
        self.ongoing_calls.insert(id,sender);
    }

    /// Removes the request from the map of ongoing requests, e.g. because it
    /// has been successfully completed or we know that we will not receive an
    /// answer anymore.
    ///
    /// Returns the channel handle for the request, it should be immediately
    /// after notified or dropped.
    pub fn remove_ongoing_request(&mut self, id:Id) -> Option<oneshot::Sender<ReplyMessage>> {
        self.ongoing_calls.remove(&id)
    }

    /// Removes all the ongoing requests. This will be recognized by the `Future`s
    /// as losing connection error.
    pub fn clear_ongoing_requests(&mut self) {
        self.ongoing_calls.clear()
    }

    /// Obtains an id for a new request to be made.
    pub fn generate_new_id(&mut self) -> Id {
        self.id_generator.generate()
    }

    /// Sends a text message to the peer.
    pub fn send_text_message(&mut self, text:&str) -> std::result::Result<(), failure::Error> {
        self.transport.send_text(text)
    }

    /// Creates a new stream with events from this handler.
    ///
    /// If such stream was already existing, it will be finished (and
    /// continuations should be able to process any remaining events).
    pub fn handler_event_stream(&mut self) -> impl Stream<Item = Event<Notification>> {
        let (transmitter,receiver) = unbounded();
        self.outgoing_events = Some(transmitter);
        receiver
    }

    /// Sends a handler event to the event stream.
    pub fn emit_event(&self, event:Event<Notification>) {
        if let Some(event_transmitter) = self.outgoing_events.as_ref() {
            channel::emit(event_transmitter,event)
        }
    }

    /// A `Duration` after which requests are timed out.
    pub fn timeout(&self) -> Duration {
        self.timeout
    }

    /// Set new timeout for future requests. Pending requests are not affected.
    pub fn set_timeout(&mut self, timeout:Duration) {
        self.timeout = timeout;
    }
}
} // shared!


// === Handler methods ===

impl<Notification> Handler<Notification> {
    /// Obtains stream of events from our transport layer.
    ///
    /// Calling this function invalidates (closes) any previous stream obtained
    /// from this function.
    ///
    /// This method is not public, as it is used internally by the `Handler` and
    /// calling it by a third-party can break this type semantics.
    fn transport_event_stream(&self) -> impl Stream<Item = TransportEvent> {
        let (event_transmitter, event_receiver) = unbounded();
        with(self.rc.borrow_mut(), |mut data| {
            data.transport.set_event_transmitter(event_transmitter);
        });
        event_receiver
    }

    /// Creates a new handler working on a given `Transport`.
    ///
    /// `Transport` must be functional (e.g. not in the process of opening).
    pub fn new(transport: impl Transport + 'static) -> Handler<Notification> {
        let data = HandlerData {
            timeout:         crate::constants::TIMEOUT,
            ongoing_calls:   default(),
            id_generator:    IdGenerator::new(),
            transport:       Box::new(transport),
            outgoing_events: None,
        };
        Handler { rc: Rc::new(RefCell::new(data)) }
    }

    /// Sends a request to the peer and returns a `Future` that shall yield a
    /// reply message. It is automatically decoded into the expected type.
    pub fn open_request<In: api::RemoteMethodCall>(
        &self,
        input: In,
    ) -> impl Future<Output = Result<In::Returned>> {
        let id = self.generate_new_id();
        let message = api::into_request_message(input, id);
        let serialized_message = serde_json::to_string(&message).unwrap();
        self.open_request_with_message(id, &serialized_message)
    }

    /// Sends a request to the peer and returns a `Future` that shall yield a reply message.
    ///
    /// This method exists to workaround a Rust compiler issue preventing using any kind of
    /// non-static generic types together with `impl trait` return syntax when the returned type may
    /// be required to be static. See: https://github.com/rust-lang/rust/issues/42940
    ///
    /// We avoid this by removing `api::RemoteMethodCall`-trait-bound type parameter (which would
    /// include a lifetime) and by passing JSON and method named obtained from it separately.
    /// This is suboptimal but still less evil than cloning all input arguments like before.
    ///
    /// FIXME: when possible unify with `open_request`
    pub fn open_request_with_json<Returned: DeserializeOwned>(
        &self,
        method_name: &str,
        input: &serde_json::Value,
    ) -> impl Future<Output = Result<Returned>> {
        let id = self.generate_new_id();
        let message = crate::messages::Message::new_request(id, method_name, input);
        let serialized_message = serde_json::to_string(&message).unwrap();
        self.open_request_with_message(id, &serialized_message)
    }

    /// Sends a request to the peer and returns a `Future` that shall yield a reply message.
    ///
    /// Helper common \code for `open_request` and `open_request_with_json`. See
    /// `open_request_with_json` docstring for more information.
    pub fn open_request_with_message<Returned: DeserializeOwned>(
        &self,
        id: Id,
        message_json: &str,
    ) -> impl Future<Output = Result<Returned>> {
        let (sender, receiver) = oneshot::channel::<ReplyMessage>();
        let ret = receiver.map(|result_or_cancel| {
            let result = result_or_cancel?;
            decode_result(result)
        });

        self.insert_ongoing_request(id, sender);
        if self.send_text_message(message_json).is_err() {
            // If message cannot be send, future ret must be cancelled.
            self.remove_ongoing_request(id);
        }

        let millis = self.timeout().as_millis();
        future::select(ret, sleep(self.timeout()).boxed_local()).map(move |either| match either {
            future::Either::Left((x, _)) => x,
            future::Either::Right((_, _)) => Err(RpcError::TimeoutError { millis }),
        })
    }

    /// Deal with `Response` message from the peer.
    ///
    /// It shall be either matched with an open request or yield an error.
    pub fn process_response(&self, message: messages::Response<serde_json::Value>) {
        if let Some(sender) = self.remove_ongoing_request(message.id) {
            // Disregard any error. We do not care if RPC caller already
            // dropped the future.
            sender.send(message.result).ok();
        } else {
            self.error_occurred(HandlingError::UnexpectedResponse(message));
        }
    }

    /// Deal with `Notification` message from the peer.
    ///
    /// If possible, emits a message with notification. In case of failure,
    /// emits relevant error.
    pub fn process_notification(&self, message: messages::Notification<serde_json::Value>)
    where Notification: DeserializeOwned {
        match serde_json::from_value(message.0) {
            Ok(notification) => {
                let event = Event::Notification(notification);
                self.emit_event(event);
            }
            Err(e) => {
                let err = HandlingError::InvalidNotification(e);
                self.error_occurred(err);
            }
        }
    }

    /// Deal with incoming text message from the peer.
    ///
    /// The message must conform either to the `Response` or to the
    /// `Notification` JSON-serialized format. Otherwise, an error is raised.
    pub fn process_incoming_message(&self, message: String)
    where Notification: DeserializeOwned {
        match messages::decode_incoming_message(&message) {
            Ok(messages::IncomingMessage::Response(response)) => self.process_response(response),
            Ok(messages::IncomingMessage::Notification(notification)) =>
                self.process_notification(notification),
            Err(err) => self.error_occurred(HandlingError::InvalidMessage(err)),
        }
    }

    /// With with a handling error. Uses `on_error` callback to notify the
    /// owner.
    pub fn error_occurred(&self, error: HandlingError) {
        self.emit_event(Event::Error(error))
    }

    /// Processes a single transport event.
    ///
    /// Each event either completes a requests or is translated into `Event`.
    pub fn process_event(&self, event: TransportEvent)
    where Notification: DeserializeOwned {
        match event {
            TransportEvent::TextMessage(msg) => self.process_incoming_message(msg),
            TransportEvent::BinaryMessage(data) =>
                self.error_occurred(HandlingError::UnexpectedBinaryMessage(data)),
            TransportEvent::Opened => {}
            TransportEvent::Closed => {
                // Dropping all ongoing calls will cancel their futures.
                self.clear_ongoing_requests();
                self.emit_event(Event::Closed);
            }
        }
    }

    /// Returns a `Future` that processes transport events incoming to this `Handler`.
    /// Subsequent call will invalidate a previous one (though old future should
    /// gracefully finish).
    ///
    /// A returned `Future` shall hold a weak handle to the data. Future will
    /// finish, when the `Transport`'s event stream finishes, e.g. due to
    /// dropping the `Transport` itself.
    ///
    /// It is expected that upon setting up the `Handler`, this future shall be
    /// passed to the main executor.
    pub fn runner(&mut self) -> impl Future<Output = ()>
    where Notification: DeserializeOwned + 'static {
        let event_receiver = self.transport_event_stream();
        let weak_data = Rc::downgrade(&self.rc);
        event_receiver.for_each(move |event: TransportEvent| {
            let data_opt = weak_data.clone().upgrade();
            let handler_opt = data_opt.map(|rc| Handler { rc });
            if let Some(handler) = handler_opt {
                handler.process_event(event)
            } else {
                // If the data is inaccessible, it is ok to just drop the event here.
            }
            futures::future::ready(())
        })
    }
}
