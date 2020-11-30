//! web_sys::WebSocket-based `Transport` implementation.

use crate::prelude::*;

use ensogl_system_web::closure::storage::OptionalFmMutClosure;
use ensogl_system_web::js_to_string;
use failure::Error;
use futures::channel::mpsc;
use json_rpc::Transport;
use json_rpc::TransportEvent;
use utils::channel;
use wasm_bindgen::JsCast;
use web_sys::BinaryType;
use web_sys::CloseEvent;
use web_sys::Event;
use web_sys::MessageEvent;



// ==============
// === Errors ===
// ==============

/// Errors that may happen when trying to establish WebSocket connection.
#[derive(Clone,Debug,Fail)]
pub enum ConnectingError {
    /// Failed to construct websocket. Usually this happens due to bad URL.
    #[fail(display = "Invalid websocket specification: {}.", _0)]
    ConstructionError(String),
    /// Failed to establish connection. Usually due to connectivity issues,
    /// wrong URL or server being down. Unfortunately, while the real error
    /// cause is usually logged down in js console, we have no reliable means of
    /// obtaining it programmatically. Reported error codes are utterly
    /// unreliable.
    #[fail(display = "Failed to establish connection.")]
    FailedToConnect,
}

/// Error that may occur when attempting to send the data over WebSocket
/// transport.
#[derive(Clone,Debug,Fail)]
pub enum SendingError {
    /// Calling `send` method has resulted in an JS exception.
    #[fail(display = "Failed to send message. Exception: {:?}.", _0)]
    FailedToSend(String),
    /// The socket was already closed, even before attempting sending a message.
    #[fail(display = "Failed to send message because socket state is {:?}.", _0)]
    NotOpen(State),
}

impl SendingError {
    /// Constructs from the error yielded by one of the JS's WebSocket sending functions.
    pub fn from_send_error(error:JsValue) -> SendingError {
        SendingError::FailedToSend(js_to_string(error))
    }
}



// =============
// === State ===
// =============

/// Describes the current state of WebSocket connection.
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum State {
    /// Socket has been created. The connection is not yet open.
    Connecting,
    /// The connection is open and ready to communicate.
    Open,
    /// The connection is in the process of closing.
    Closing,
    /// The connection is closed or couldn't be opened.
    Closed,
    /// Any other, unknown condition.
    Unknown(u16),
}

impl State {
    /// Returns current state of the given WebSocket.
    pub fn query_ws(ws:&web_sys::WebSocket) -> State {
        State::from_code(ws.ready_state())
    }

    /// Translates code returned by `WebSocket.readyState` into our enum.
    /// cf https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState
    pub fn from_code(code:u16) -> State {
        match code {
            web_sys::WebSocket::CONNECTING => State::Connecting,
            web_sys::WebSocket::OPEN       => State::Open,
            web_sys::WebSocket::CLOSING    => State::Closing,
            web_sys::WebSocket::CLOSED     => State::Closed,
            num                            => State::Unknown(num), // impossible
        }
    }
}



// =================
// === WebSocket ===
// =================

/// Wrapper over JS `WebSocket` object and callbacks to its signals.
#[derive(Debug)]
pub struct WebSocket {
    #[allow(missing_docs)]
    pub logger     : Logger,
    /// Handle to the JS `WebSocket` object.
    pub ws         : web_sys::WebSocket,
    /// Handle to a closure connected to `WebSocket.onmessage`.
    pub on_message : OptionalFmMutClosure<MessageEvent>,
    /// Handle to a closure connected to `WebSocket.onclose`.
    pub on_close   : OptionalFmMutClosure<CloseEvent>,
    /// Handle to a closure connected to `WebSocket.onopen`.
    pub on_open    : OptionalFmMutClosure<Event>,
    /// Handle to a closure connected to `WebSocket.onerror`.
    pub on_error   : OptionalFmMutClosure<Event>,
}

impl WebSocket {
    /// Wraps given WebSocket object.
    pub fn new
    (ws:web_sys::WebSocket, parent:impl AnyLogger, name:impl AsRef<str>) -> WebSocket {
        ws.set_binary_type(BinaryType::Arraybuffer);
        WebSocket {
            ws,
            logger     : Logger::sub(parent,name),
            on_message : default(),
            on_close   : default(),
            on_open    : default(),
            on_error   : default(),
        }
    }

    /// Establish connection with endpoint defined by the given URL and wrap it.
    /// Asynchronous, because it waits until connection is established.
    pub async fn new_opened(parent:Logger, url:impl Str) -> Result<WebSocket,ConnectingError> {
        let ws = web_sys::WebSocket::new(url.as_ref()).map_err(|e| {
            ConnectingError::ConstructionError(js_to_string(e))
        })?;
        let mut wst = WebSocket::new(ws,&parent,url.into());
        wst.wait_until_open().await?;
        Ok(wst)
    }

    /// Awaits until `open` signal has been emitted. Clears any callbacks on
    /// this `WebSocket`, if any has been set.
    async fn wait_until_open(&mut self) -> Result<(),ConnectingError> {
        // Connecting attempt shall either emit on_open or on_close.
        // We shall wait for whatever comes first.
        let (transmitter, mut receiver) = mpsc::unbounded::<Result<(),()>>();
        let transmitter_clone = transmitter.clone();
        self.set_on_close(move |_| {
            // Note [mwu] Ignore argument, `CloseEvent` here contains rubbish
            // anyway, nothing useful to pass to caller. Error code or reason
            // string should not be relied upon.
            utils::channel::emit(&transmitter_clone, Err(()));
        });
        self.set_on_open(move |_| {
            utils::channel::emit(&transmitter, Ok(()));
        });

        match receiver.next().await {
            Some(Ok(())) => {
                self.clear_callbacks();
                info!(self.logger, "Connection opened.");
                Ok(())
            }
            _ => Err(ConnectingError::FailedToConnect)
        }
    }

    /// Checks the current state of the connection.
    pub fn state(&self) -> State {
        State::query_ws(&self.ws)
    }

    /// Sets callback for the `close` event.
    pub fn set_on_close(&mut self, f:impl FnMut(CloseEvent) + 'static) {
        self.on_close.wrap(f);
        self.ws.set_onclose(self.on_close.js_ref());
    }

    /// Sets callback for the `error` event.
    pub fn set_on_error(&mut self, f:impl FnMut(Event) + 'static) {
        self.on_error.wrap(f);
        self.ws.set_onerror(self.on_error.js_ref());
    }

    /// Sets callback for the `message` event.
    pub fn set_on_message(&mut self, f:impl FnMut(MessageEvent) + 'static) {
        self.on_message.wrap(f);
        self.ws.set_onmessage(self.on_message.js_ref());
    }

    /// Sets callback for the `open` event.
    pub fn set_on_open(&mut self, f:impl FnMut(Event) + 'static) {
        self.on_open.wrap(f);
        self.ws.set_onopen(self.on_open.js_ref());
    }

    /// Clears all the available callbacks.
    pub fn clear_callbacks(&mut self) {
        self.on_close  .clear();
        self.on_error  .clear();
        self.on_message.clear();
        self.on_open   .clear();
        self.ws.set_onclose(None);
        self.ws.set_onerror(None);
        self.ws.set_onmessage(None);
        self.ws.set_onopen(None);
    }

    /// Executes a given function with a mutable reference to the socket.
    /// The function should attempt sending the message through the websocket.
    ///
    /// Fails if the socket is not opened or if the sending function failed.
    /// The error from `F` shall be translated into `SendingError`.
    pub fn send_with_open_socket<F,R>(&mut self, f:F) -> Result<R,Error>
    where F : FnOnce(&mut web_sys::WebSocket) -> Result<R,JsValue> {
        // Sending through the closed WebSocket can return Ok() with error only
        // appearing in the log. We explicitly check for this to get failure as
        // early as possible.
        //
        // If WebSocket closes after the check, caller will be able to handle it
        // when receiving `TransportEvent::Closed`.
        let state = self.state();
        if state != State::Open {
            Err(SendingError::NotOpen(state).into())
        } else {
            let result = f(&mut self.ws);
            result.map_err(|e| SendingError::from_send_error(e).into())
        }
    }
}

impl Transport for WebSocket {
    fn send_text(&mut self, message:&str) -> Result<(), Error> {
        info!(self.logger, "Sending text message of length {message.len()}");
        debug!(self.logger, "Message contents: {message}");
        self.send_with_open_socket(|ws| ws.send_with_str(message))
    }

    fn send_binary(&mut self, message:&[u8]) -> Result<(), Error> {
        info!(self.logger, "Sending binary message of length {message.len()}");
        debug!(self.logger,|| format!("Message contents: {:x?}", message));
        // TODO [mwu]
        //   Here we workaround issue from wasm-bindgen 0.2.58:
        //   https://github.com/rustwasm/wasm-bindgen/issues/2014
        //   The issue has been fixed in 0.2.59, however we can't upgrade, as we rely on fragile
        //   regexp-based machinery to process wasm-bindgen output.
        //   When fixed, we should pass `message` directly, without intermediate copy.
        let mut owned_copy = Vec::from(message);
        let mut_slice      = owned_copy.as_mut();
        self.send_with_open_socket(|ws| ws.send_with_u8_array(mut_slice))
    }

    fn set_event_transmitter(&mut self, transmitter:mpsc::UnboundedSender<TransportEvent>) {
        info!(self.logger,"Setting event transmitter.");
        let transmitter_copy = transmitter.clone();
        let logger_copy = self.logger.clone_ref();
        self.set_on_message(move |e| {
            let data = e.data();
            if let Some(text) = data.as_string() {
                debug!(logger_copy, "Received a text message: {text}");
                channel::emit(&transmitter_copy,TransportEvent::TextMessage(text));
            } else if let Ok(array_buffer) = data.dyn_into::<js_sys::ArrayBuffer>() {
                let array       = js_sys::Uint8Array::new(&array_buffer);
                let binary_data = array.to_vec();
                debug!(logger_copy,|| format!("Received a binary message: {:x?}", binary_data));
                let event = TransportEvent::BinaryMessage(binary_data);
                channel::emit(&transmitter_copy,event);
            } else {
                info!(logger_copy,"Received other kind of message: {js_to_string(e.data())}.");
            }
        });

        let transmitter_copy = transmitter.clone();
        let logger_copy = self.logger.clone_ref();
        self.set_on_close(move |_e| {
            info!(logger_copy,"Connection has been closed.");
            channel::emit(&transmitter_copy,TransportEvent::Closed);
        });

        let logger_copy = self.logger.clone_ref();
        self.set_on_open(move |_e| {
            info!(logger_copy,"Connection has been opened.");
            channel::emit(&transmitter, TransportEvent::Opened);
        });
    }
}
