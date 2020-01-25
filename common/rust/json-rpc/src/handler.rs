//! Module providing `Handler` and related types used by its API.

use prelude::*;

use crate::api;
use crate::api::Result;
use crate::error::HandlingError;
use crate::error::RpcError;
use crate::messages;
use crate::messages::Id;
use crate::transport::Transport;
use crate::transport::TransportEvent;

use futures::FutureExt;
use futures::Stream;
use futures::channel::mpsc::unbounded;
use futures::channel::mpsc::UnboundedSender;
use futures::channel::oneshot;
use serde::de::DeserializeOwned;
use std::future::Future;
use std::sync::mpsc::TryRecvError;



// ====================
// === ReplyMessage ===
// ====================

/// Partially decoded reply message.
///
/// Known if `Error` or `Success` but returned value remains in JSON form.
pub type ReplyMessage = messages::Result<serde_json::Value>;

/// Converts remote message with JSON-serialized result into `Result<Ret>`.
pub fn decode_result<Ret:DeserializeOwned>
(result:messages::Result<serde_json::Value>) -> Result<Ret> {
    match result {
        messages::Result::Success(ret) =>
            Ok(serde_json::from_value::<Ret>(ret.result)?),
        messages::Result::Error(err) =>
            Err(RpcError::RemoteError(err))?,
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
    pub fn next(&mut self) -> Id {
        let id = self.counter;
        self.counter += 1;
        Id(id)
    }

    /// Create a new IdGenerator counting from 0.
    fn new() -> IdGenerator {
        IdGenerator::new_from(0)
    }

    /// Create a new IdGenerator that gives Ids beginning with given number.
    fn new_from(counter:i64) -> IdGenerator {
        IdGenerator { counter }
    }
}



// ====================
// === SharedBuffer ===
// ====================

/// The buffer shared between `Handler` and `Transport`.
///
/// The `Transport` callbacks store any input there. Then, `Handler` consumes it
/// when prompted with `tick` method.
#[derive(Debug)]
pub struct SharedBuffer {
    /// Incoming text messages.
    pub incoming: Vec<String>,

    /// Whether the transport was closed. This means that the current transport
    /// cannot be used anymore.
    pub closed: bool,
}

impl SharedBuffer {
    /// Create a new empty buffer.
    pub fn new() -> SharedBuffer {
        SharedBuffer {
            incoming : Vec::new(),
            closed   : false,
        }
    }

    /// Returns a new buffer with all the data moved from self.
    ///
    /// After the call incoming messages list in self is empty, however the
    /// status of `closed` flag is not changed.
    pub fn take(&mut self) -> SharedBuffer {
        let incoming = std::mem::replace(&mut self.incoming, Vec::new());
        let closed   = self.closed;
        SharedBuffer {incoming,closed}
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



// ===============
// === Handler ===
// ===============

/// Container that stores Sender's for ongoing calls. Each call identified by
/// id has its own sender. After reply is received, the call is removed
/// from this container.
pub type OngoingCalls = HashMap<Id,oneshot::Sender<ReplyMessage>>;

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
#[derive(Debug)]
pub struct Handler<Notification> {
    /// Contains handles to calls that were made but no response has came.
    pub ongoing_calls   : OngoingCalls,
    /// Provides identifiers for requests.
    pub id_generator    : IdGenerator,
    /// Transports text messages between this handler and the peer.
    pub transport       : Box<dyn Transport>,
    /// Allows receiving events from the `Transport`.
    pub incoming_events : std::sync::mpsc::Receiver<TransportEvent>,
    /// Handle to send outgoing events.
    pub outgoing_events : Option<UnboundedSender<Event<Notification>>>,
}

impl<Notification> Handler<Notification> {
    /// Creates a new handler working on a given `Transport`.
    ///
    /// `Transport` must be functional (e.g. not in the process of opening).
    pub fn new(transport:impl Transport + 'static) -> Handler<Notification> {
        let (event_tx, event_rx) = std::sync::mpsc::channel();
        let mut ret = Handler {
            ongoing_calls   : OngoingCalls::new(),
            id_generator    : IdGenerator::new(),
            transport       : Box::new(transport),
            incoming_events : event_rx,
            outgoing_events : None,
        };
        ret.transport.set_event_tx(event_tx);
        ret
    }

    /// Sends a request to the peer and returns a `Future` that shall yield a
    /// reply message. It is automatically decoded into the expected type.
    pub fn open_request<In:api::RemoteMethodCall>
    (&mut self, input:In) -> impl Future<Output = Result<In::Returned>> {
        let (sender, receiver) = oneshot::channel::<ReplyMessage>();
        let ret                = receiver.map(|result_or_cancel| {
            let result = result_or_cancel?;
            decode_result(result)
        });

        let id      = self.id_generator.next();
        let message = api::into_request_message(input,id);
        self.ongoing_calls.insert(message.payload.id, sender);

        let serialized_message = serde_json::to_string(&message).unwrap();
        if self.transport.send_text(serialized_message).is_err() {
            // If message cannot be send, future ret must be cancelled.
            self.ongoing_calls.remove(&id);
        }
        ret
    }

    /// Deal with `Response` message from the peer.
    ///
    /// It shall be either matched with an open request or yield an error.
    pub fn process_response
    (&mut self, message:messages::Response<serde_json::Value>) {
        if let Some(sender) = self.ongoing_calls.remove(&message.id) {
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
    pub fn process_notification
    (&mut self, message:messages::Notification<serde_json::Value>)
    where Notification: DeserializeOwned {
        match serde_json::from_value(message.0) {
            Ok(notification) => {
                let event = Event::Notification(notification);
                self.send_event(event);
            },
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
    pub fn process_incoming_message(&mut self, message:String)
    where Notification: DeserializeOwned {
        match messages::decode_incoming_message(message) {
            Ok(messages::IncomingMessage::Response(response)) =>
                self.process_response(response),
            Ok(messages::IncomingMessage::Notification(notification)) =>
                self.process_notification(notification),
            Err(err) =>
                self.error_occurred(HandlingError::InvalidMessage(err)),
        }
    }

    /// With with a handling error. Uses `on_error` callback to notify the
    /// owner.
    pub fn error_occurred(&mut self, error: HandlingError) {
        self.send_event(Event::Error(error))
    }

    /// Processes a single transport event.
    ///
    /// Each event either completes a requests or is translated into `Event`.
    pub fn process_event(&mut self, event:TransportEvent)
    where Notification: DeserializeOwned {
        match event {
            TransportEvent::TextMessage(msg) =>
                self.process_incoming_message(msg),
            TransportEvent::Closed => {
                // Dropping all ongoing calls will mark their futures as
                // cancelled.
                self.ongoing_calls.clear();
                self.send_event(Event::Closed);
            }
        }
    }

    /// Processes all incoming events. Returns as soon as there are no more
    /// messages pending.
    ///
    /// This will decode the incoming messages, providing input to the futures
    /// returned from RPC calls.
    /// Also this cancels any ongoing calls if the connection was lost.
    pub fn process_events(&mut self)
    where Notification: DeserializeOwned {
        loop {
            match self.incoming_events.try_recv() {
                Ok(event) => self.process_event(event),
                Err(TryRecvError::Disconnected) =>
                    panic!("transport dropped the event sender"),
                Err(TryRecvError::Empty) => break,
            }
        }
    }

    /// Sends a handler event to the event stream.
    pub fn send_event(&mut self, event:Event<Notification>) {
        if let Some(tx) = self.outgoing_events.as_mut() {
            match tx.unbounded_send(event) {
                Ok(()) => {},
                Err(e) =>
                    if e.is_full() {
                        // Impossible, as per `futures` library docs.
                        panic!("unbounded channel should never be full")
                    } else if e.is_disconnected() {
                        // It is ok for receiver to disconnect and ignore events.
                    } else {
                        // Never happens unless `futures` library changes API.
                        panic!("unknown unexpected error")
                    }
            }
        }
    }

    /// Creates a new stream with events from this handler.
    ///
    /// If such stream was already existing, it will be finished (and
    /// continuations should be able to process any remaining events).
    pub fn events(&mut self) -> impl Stream<Item = Event<Notification>> {
        let (tx,rx)          = unbounded();
        self.outgoing_events = Some(tx);
        rx
    }
}
