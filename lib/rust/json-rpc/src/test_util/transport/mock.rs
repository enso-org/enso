//! Module provides a `MockTransport` that implements `Transport`.
//!
//! It is meant to be used in tests.

use crate::prelude::*;

use crate::transport::Transport;
use crate::transport::TransportEvent;

use failure::Error;
use futures::channel::mpsc::UnboundedSender;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::VecDeque;



// ====================
// === SendingError ===
// ====================

/// Errors emitted by the `MockTransport`.
#[derive(Clone, Copy, Debug, Fail)]
pub enum SendError {
    /// Cannot send message while the connection is closed.
    #[fail(display = "Cannot send message when socket is closed.")]
    TransportClosed,
}



// ========================
// === Transport Status ===
// ========================

/// Status of the `MockTransport`.
#[derive(Clone, Copy, Debug)]
pub enum Status {
    /// Transport is functional, can send messages.
    Open,
    /// Transport is not functional at the moment, cannot send messages.
    Closed,
}



// ======================
// === Transport Data ===
// ======================

/// Mock transport shared data. Collects all the messages sent by the owner.
///
/// Allows mocking messages from the peer.
#[derive(Debug, Default)]
pub struct MockTransportData {
    /// Events sink.
    pub event_transmitter: Option<UnboundedSender<TransportEvent>>,
    /// Text messages sent by the user.
    pub sent_text_msgs:    VecDeque<String>,
    /// Binary messages by the user.
    pub sent_binary_msgs:  VecDeque<Vec<u8>>,
    /// Transport status.
    pub is_closed:         bool,
}



// ======================
// === Mock Transport ===
// ======================

/// Shareable wrapper over `MockTransportData`.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct MockTransport(Rc<RefCell<MockTransportData>>);

impl Transport for MockTransport {
    fn send_text(&mut self, text: &str) -> Result<(), Error> {
        self.send_helper(move |data| data.sent_text_msgs.push_back(text.into()))
    }

    fn send_binary(&mut self, message: &[u8]) -> Result<(), Error> {
        self.send_helper(|data| data.sent_binary_msgs.push_back(message.into()))
    }

    fn set_event_transmitter(&mut self, transmitter: UnboundedSender<TransportEvent>) {
        self.with_mut_data(|data| {
            data.event_transmitter = Some(transmitter);
        })
    }
}

impl MockTransport {
    /// Create a new `MockTransport`.
    pub fn new() -> MockTransport {
        MockTransport::default()
    }

    /// Executes given function with access to borrowed mutable data reference.
    pub fn with_mut_data<R, F>(&mut self, f: F) -> R
    where F: FnOnce(&mut MockTransportData) -> R {
        let mut data = self.0.borrow_mut();
        f(&mut data)
    }

    /// Generates event that mocks receiving a text message from a peer.
    pub fn mock_peer_text_message<S: Into<String>>(&mut self, message: S) {
        let message = message.into();
        if let Some(ref mut transmitter) = self.0.borrow_mut().event_transmitter {
            let event = TransportEvent::TextMessage(message);
            channel::emit(transmitter, event);
        }
    }

    /// Generates event that mocks receiving a text message from a peer with
    /// serialized JSON contents.
    pub fn mock_peer_json_message<T: Serialize>(&mut self, message: T) {
        let text = serde_json::to_string(&message);
        let text = text.expect("failed to serialize mock message");
        self.mock_peer_text_message(text)
    }

    /// Generates event that mocks receiving a text message from a peer.
    pub fn mock_peer_binary_message(&mut self, data: &[u8]) {
        if let Some(ref mut transmitter) = self.0.borrow_mut().event_transmitter {
            let event = TransportEvent::BinaryMessage(Vec::from(data));
            channel::emit(transmitter, event);
        }
    }

    /// Mocks event generated when peer closes the socket (or connection is lost
    /// for any other reason).
    pub fn mock_connection_closed(&mut self) {
        self.with_mut_data(|data| {
            if let Some(ref mut transmitter) = data.event_transmitter {
                data.is_closed = true;
                channel::emit(transmitter, TransportEvent::Closed);
            }
        })
    }

    /// Takes the text message sent by the client and returns its contents.
    ///
    /// If the client has not sent any text messages, panics.
    /// If the client sent multiple text messages, the first one is returned.
    /// Further messages can be obtained by subsequent calls.
    pub fn expect_text_message(&mut self) -> String {
        self.with_mut_data(|data| {
            data.sent_text_msgs.pop_front().expect("client should have sent text message")
        })
    }

    /// Similar to `expect_message_text` but deserializes the message into
    /// given type `T` from JSON.
    pub fn expect_json_message<T: DeserializeOwned>(&mut self) -> T {
        let text = self.expect_text_message();
        let res = serde_json::from_str(&text);
        res.expect("failed to deserialize client's message")
    }

    /// Takes the binary message sent by the client and returns its contents.
    ///
    /// If the client has not sent any binary messages, panics.
    /// If the client sent multiple binary messages, the first one is returned.
    /// Further messages can be obtained by subsequent calls.
    pub fn expect_binary_message(&mut self) -> Vec<u8> {
        self.with_mut_data(|data| {
            data.sent_binary_msgs.pop_front().expect("client should have sent binary message")
        })
    }

    /// Executes a given function with a mutable reference to the transport data.
    /// The function should realize "sending" (well, mocked) the message through the transport.
    ///
    /// Fails if the transport is not open.
    pub fn send_helper<F>(&mut self, f: F) -> Result<(), Error>
    where F: FnOnce(&mut MockTransportData) {
        self.with_mut_data(|data| {
            if data.is_closed {
                Err(SendError::TransportClosed.into())
            } else {
                f(data);
                Ok(())
            }
        })
    }
}
