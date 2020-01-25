//! Module provides a `MockTransport` that implements `Transport`.
//!
//! It is meant to be used in tests.


use prelude::*;

use crate::transport::Transport;
use crate::transport::TransportEvent;

use std::collections::VecDeque;
use failure::Error;
use serde::de::DeserializeOwned;
use serde::Serialize;



// ====================
// === SendingError ===
// ====================

/// Errors emitted by the `MockTransport`.
#[derive(Clone,Copy,Debug,Fail)]
pub enum SendError {
    /// Cannot send message while the connection is closed.
    #[fail(display = "Cannot send message when socket is closed.")]
    TransportClosed,
}



// ========================
// === Transport Status ===
// ========================

/// Status of the `MockTransport`.
#[derive(Clone,Copy,Debug)]
pub enum Status {
    /// Transport is functional, can send messages.
    Open,
    /// Transport is not functional at the moment, cannot send messages.
    Closed
}



// ======================
// === Transport Data ===
// ======================

/// Mock transport shared data. Collects all the messages sent by the owner.
///
/// Allows mocking messages from the peer.
#[derive(Debug,Default)]
pub struct MockTransportData {
    /// Events sink.
    pub event_tx  : Option<std::sync::mpsc::Sender<TransportEvent>>,
    /// Messages sent by the user.
    pub sent_msgs : VecDeque<String>,
    /// Transport status.
    pub is_closed : bool,
}



// ======================
// === Mock Transport ===
// ======================

/// Shareable wrapper over `MockTransportData`.
#[derive(Clone,Debug,Default)]
pub struct MockTransport(Rc<RefCell<MockTransportData>>);

impl Transport for MockTransport {
    fn send_text(&mut self, text:String) -> Result<(), Error> {
        self.with_mut_data(|data| {
            if data.is_closed {
                Err(SendError::TransportClosed)?
            } else {
                data.sent_msgs.push_back(text.clone());
                Ok(())
            }
        })
    }

    fn set_event_tx(&mut self, tx:std::sync::mpsc::Sender<TransportEvent>) {
        self.with_mut_data(|data| {
            data.event_tx = Some(tx);
        })
    }
}

impl MockTransport {
    /// Create a new `MockTransport`.
    pub fn new() -> MockTransport {
        MockTransport::default()
    }

    /// Executes given function with access to borrowed mutable data reference.
    pub fn with_mut_data<R,F>(&mut self, f:F) -> R
    where F: FnOnce(&mut MockTransportData) -> R {
        let mut data = self.0.borrow_mut();
        f(&mut data)
    }

    /// Generates event that mocks receiving a text message from a peer.
    pub fn mock_peer_message_text<S:Into<String>>(&mut self, message:S) {
        let message = message.into();
        if let Some(ref tx) = self.0.borrow_mut().event_tx {
            let _ = tx.send(TransportEvent::TextMessage(message));
        }
    }

    /// Generates event that mocks receiving a text message from a peer with
    /// serialized JSON contents.
    pub fn mock_peer_message<T:Serialize>(&mut self, message:T) {
        let text = serde_json::to_string(&message);
        let text = text.expect("failed to serialize mock message");
        self.mock_peer_message_text(text)
    }

    /// Mocks event generated when peer closes the socket (or connection is lost
    /// for any other reason).
    pub fn mock_connection_closed(&mut self) {
        self.with_mut_data(|data| {
            if let Some(ref tx) = data.event_tx {
                data.is_closed = true;
                let _          = tx.send(TransportEvent::Closed);
            }
        })
    }

    /// Takes the message sent by the client and returns its texts.
    ///
    /// If the client did not sent any messages, panics.
    /// If the client sent multiple messages, the first one is returned.
    /// Further messages can be obtained by subsequent calls.
    pub fn expect_message_text(&mut self) -> String {
        self.with_mut_data(|data| {
            data.sent_msgs.pop_front().expect("client should have sent request")
        })
    }

    /// Similar to `expect_message_text` but deserializes the message into
    /// given type `T` from JSON.
    pub fn expect_message<T:DeserializeOwned>(&mut self) -> T {
        let text = self.expect_message_text();
        let res  = serde_json::from_str(&text);
        res.expect("failed to deserialize client's message")
    }
}
