//! web_sys::WebSocket-based `Transport` implementation.

use crate::prelude::*;
use enso_web::traits::*;

use enso_web::event::listener::Slot;
use failure::Error;
use futures::channel::mpsc;
use json_rpc::Transport;
use json_rpc::TransportEvent;
use wasm_bindgen::JsCast;
use web_sys::BinaryType;



// ==============
// === Errors ===
// ==============

/// Errors that may happen when trying to establish WebSocket connection.
#[derive(Clone, Debug, Fail)]
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

impl ConnectingError {
    /// Create a `ConstructionError` value from a JS value describing an error.
    pub fn construction_error(js_val: impl AsRef<enso_web::JsValue>) -> Self {
        let text = js_val.as_ref().print_to_string();
        ConnectingError::ConstructionError(text)
    }
}

/// Error that may occur when attempting to send the data over WebSocket
/// transport.
#[derive(Clone, Debug, Fail)]
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
    pub fn from_send_error(error: wasm_bindgen::JsValue) -> SendingError {
        SendingError::FailedToSend(error.print_to_string())
    }
}



// =============
// === State ===
// =============

/// Describes the current state of WebSocket connection.
#[derive(Clone, Copy, Debug, PartialEq)]
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
    pub fn query_ws(ws: &web_sys::WebSocket) -> State {
        State::from_code(ws.ready_state())
    }

    /// Translates code returned by `WebSocket.readyState` into our enum.
    /// cf https://developer.mozilla.org/en-US/docs/Web/API/WebSocket/readyState
    pub fn from_code(code: u16) -> State {
        match code {
            web_sys::WebSocket::CONNECTING => State::Connecting,
            web_sys::WebSocket::OPEN => State::Open,
            web_sys::WebSocket::CLOSING => State::Closing,
            web_sys::WebSocket::CLOSED => State::Closed,
            num => State::Unknown(num), // impossible
        }
    }
}



// =================
// === JS Events ===
// =================

/// Description of events that can be emitted by JS WebSocket.
pub mod event {
    use super::*;
    use enso_web::event::Type;

    /// Represents WebSocket.open event.
    #[derive(Clone, Copy, Debug)]
    pub enum Open {}
    impl Type for Open {
        type Interface = web_sys::Event;
        type Target = web_sys::WebSocket;
        const NAME: &'static str = "open";
    }

    /// Represents WebSocket.close event.
    #[derive(Clone, Copy, Debug)]
    pub enum Close {}
    impl Type for Close {
        type Interface = web_sys::CloseEvent;
        type Target = web_sys::WebSocket;
        const NAME: &'static str = "close";
    }

    /// Represents WebSocket.message event.
    #[derive(Clone, Copy, Debug)]
    pub enum Message {}
    impl Type for Message {
        type Interface = web_sys::MessageEvent;
        type Target = web_sys::WebSocket;
        const NAME: &'static str = "message";
    }

    /// Represents WebSocket.error event.
    #[derive(Clone, Copy, Debug)]
    pub enum Error {}
    impl Type for Error {
        type Interface = web_sys::Event;
        type Target = web_sys::WebSocket;
        const NAME: &'static str = "error";
    }
}



// =============
// === Model ===
// =============

/// An owning wrapper over JS `WebSocket` object and callbacks to its signals.
#[derive(Derivative)]
#[derivative(Debug)]
#[allow(missing_docs)]
struct Model {
    // === User-provided callbacks ===
    pub on_close:   Slot<event::Close>,
    pub on_message: Slot<event::Message>,
    pub on_open:    Slot<event::Open>,
    pub on_error:   Slot<event::Error>,

    // === Internal ===
    pub logger:            Logger,
    pub socket:            web_sys::WebSocket,
    /// Special callback on "close" event. As it must be invoked after `on_close`, care should be
    /// taken to keep it registered as an event listener *after* `on_close` registration.
    /// By default `Model` takes care of it by itself.
    pub on_close_internal: Slot<event::Close>,
    /// When enabled, the WS will try to automatically reconnect whenever connection is lost.
    pub auto_reconnect:    bool,
}

impl Model {
    /// Wraps given WebSocket object.
    pub fn new(socket: web_sys::WebSocket, logger: Logger) -> Model {
        socket.set_binary_type(BinaryType::Arraybuffer);
        Model {
            on_close: Slot::new(&socket, &logger),
            on_message: Slot::new(&socket, &logger),
            on_open: Slot::new(&socket, &logger),
            on_error: Slot::new(&socket, &logger),
            on_close_internal: Slot::new(&socket, &logger),
            auto_reconnect: true,
            logger,
            socket,
        }
    }

    /// Close the socket.
    pub fn close(&mut self, reason: &str) -> Result<(), wasm_bindgen::JsValue> {
        // If socket was manually requested to close, it should not try to reconnect then.
        self.auto_reconnect = false;
        let normal_closure = 1000;
        self.socket.close_with_code_and_reason(normal_closure, reason)?;
        self.clear_callbacks();
        Ok(())
    }

    /// Clear all the available callbacks.
    pub fn clear_callbacks(&mut self) {
        // We list explicitly all the fields, to get a compiler error when a new slot as added
        // but not handled here.
        #[allow(clippy::unneeded_field_pattern)]
        let Self {
            // Callback slots to be cleared.
            on_close,
            on_error,
            on_message,
            on_open,
            on_close_internal,
            // Explicitly ignored non-slot fields.
            auto_reconnect: _,
            logger: _,
            socket: _,
        } = self;
        // We don't care if removing actually removed anything.
        // If callbacks were not set, then they are clear from the start.
        on_close.clear_callback();
        on_error.clear_callback();
        on_message.clear_callback();
        on_open.clear_callback();
        on_close_internal.clear_callback()
    }

    /// Establish a new WS connection, using the same URL as the previous one.
    /// All callbacks will be transferred to the new connection.
    pub fn reconnect(&mut self) -> Result<(), wasm_bindgen::JsValue> {
        if !self.auto_reconnect {
            return Err(js_sys::Error::new("Reconnecting has been disabled").into());
        }

        let url = self.socket.url();
        info!(self.logger, "Reconnecting WS to {url}.");

        let new_ws = web_sys::WebSocket::new(&url)?;

        self.on_close.set_target(&new_ws);
        self.on_error.set_target(&new_ws);
        self.on_message.set_target(&new_ws);
        self.on_open.set_target(&new_ws);
        self.on_close_internal.set_target(&new_ws);
        self.socket = new_ws;

        Ok(())
    }
}

impl Drop for Model {
    fn drop(&mut self) {
        info!(self.logger, "Dropping WS model.");
        if let Err(e) = self.close("Rust Value has been dropped.") {
            error!(
                self.logger,
                "Error when closing socket due to being dropped: {e.print_to_string()}"
            )
        }
    }
}



// =================
// === WebSocket ===
// =================

/// Wrapper over JS `WebSocket` meant for general use.
#[derive(Clone, CloneRef, Debug)]
pub struct WebSocket {
    #[allow(missing_docs)]
    pub logger: Logger,
    model:      Rc<RefCell<Model>>,
}

impl WebSocket {
    /// Wrap given raw JS WebSocket object.
    pub fn new(ws: web_sys::WebSocket, parent: impl AnyLogger) -> WebSocket {
        let logger = Logger::new_sub(parent, ws.url());
        let model = Rc::new(RefCell::new(Model::new(ws, logger.clone())));
        WebSocket { logger, model }
    }

    /// Establish connection with endpoint defined by the given URL and wrap it.
    /// Asynchronous, because it waits until connection is established.
    pub async fn new_opened(
        parent: impl AnyLogger,
        url: &str,
    ) -> Result<WebSocket, ConnectingError> {
        let ws = web_sys::WebSocket::new(url).map_err(ConnectingError::construction_error)?;
        let mut wst = WebSocket::new(ws, &parent);
        wst.wait_until_open().await?;
        Ok(wst)
    }

    /// Generate a callback to be invoked when socket needs reconnecting.
    fn reconnect_trigger(&self) -> impl FnMut(web_sys::CloseEvent) {
        let model = Rc::downgrade(&self.model);
        let logger = self.logger.clone();
        move |_| {
            if let Some(model) = model.upgrade() {
                if let Err(e) = model.borrow_mut().reconnect() {
                    error!(logger, "Failed to reconnect: {e.print_to_string()}");
                }
            }
        }
    }

    /// Awaits until `open` signal has been emitted. Clears any callbacks on
    /// this `WebSocket`, if any has been set.
    async fn wait_until_open(&mut self) -> Result<(), ConnectingError> {
        // Connecting attempt shall either emit on_open or on_close.
        // We shall wait for whatever comes first.
        let (transmitter, mut receiver) = mpsc::unbounded::<Result<(), ()>>();
        let transmitter_clone = transmitter.clone();

        self.set_on_close(move |_| {
            // Note [mwu] Ignore argument, `CloseEvent` here contains rubbish
            // anyway, nothing useful to pass to caller. Error code or reason
            // string should not be relied upon.
            channel::emit(&transmitter_clone, Err(()));
        });
        self.set_on_open(move |_| {
            channel::emit(&transmitter, Ok(()));
        });

        match receiver.next().await {
            Some(Ok(())) => {
                self.model.borrow_mut().clear_callbacks();
                self.model.borrow_mut().on_close_internal.set_callback(self.reconnect_trigger());
                info!(self.logger, "Connection opened.");
                Ok(())
            }
            _ => Err(ConnectingError::FailedToConnect),
        }
    }

    /// Checks the current state of the connection.
    pub fn state(&self) -> State {
        State::query_ws(&self.model.borrow().socket)
    }

    fn with_borrow_mut_model<R>(&mut self, f: impl FnOnce(&mut Model) -> R) -> R {
        with(self.model.borrow_mut(), |mut model| f(model.deref_mut()))
    }

    /// Sets callback for the `close` event.
    pub fn set_on_close(&mut self, f: impl FnMut(web_sys::CloseEvent) + 'static) {
        self.with_borrow_mut_model(move |model| {
            model.on_close.set_callback(f);
            // Force internal callback to be after the user-defined one.
            model.on_close_internal.reattach();
        });
    }

    /// Sets callback for the `error` event.
    pub fn set_on_error(&mut self, f: impl FnMut(web_sys::Event) + 'static) {
        self.with_borrow_mut_model(move |model| model.on_error.set_callback(f))
    }

    /// Sets callback for the `message` event.
    pub fn set_on_message(&mut self, f: impl FnMut(web_sys::MessageEvent) + 'static) {
        self.with_borrow_mut_model(move |model| model.on_message.set_callback(f))
    }

    /// Sets callback for the `open` event.
    pub fn set_on_open(&mut self, f: impl FnMut(web_sys::Event) + 'static) {
        self.with_borrow_mut_model(move |model| model.on_open.set_callback(f))
    }

    /// Executes a given function with a mutable reference to the socket.
    /// The function should attempt sending the message through the websocket.
    ///
    /// Fails if the socket is not opened or if the sending function failed.
    /// The error from `F` shall be translated into `SendingError`.
    ///
    /// WARNING: `f` works under borrow_mut and must not give away control.
    fn send_with_open_socket<F, R>(&mut self, f: F) -> Result<R, Error>
    where F: FnOnce(&mut web_sys::WebSocket) -> Result<R, wasm_bindgen::JsValue> {
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
            let result = f(&mut self.model.borrow_mut().socket);
            result.map_err(|e| SendingError::from_send_error(e).into())
        }
    }
}

impl Transport for WebSocket {
    fn send_text(&mut self, message: &str) -> Result<(), Error> {
        info!(self.logger, "Sending text message of length {message.len()}.");
        debug!(self.logger, "Message contents: {message}");
        self.send_with_open_socket(|ws| ws.send_with_str(message))
    }

    fn send_binary(&mut self, message: &[u8]) -> Result<(), Error> {
        info!(self.logger, "Sending binary message of length {message.len()}.");
        debug!(self.logger, || format!("Message contents: {:x?}", message));
        // TODO [mwu]
        //   Here we workaround issue from wasm-bindgen 0.2.58:
        //   https://github.com/rustwasm/wasm-bindgen/issues/2014
        //   The issue has been fixed in 0.2.59, however we can't upgrade, as we rely on fragile
        //   regexp-based machinery to process wasm-bindgen output.
        //   When fixed, we should pass `message` directly, without intermediate copy.
        let mut owned_copy = Vec::from(message);
        let mut_slice = owned_copy.as_mut();
        self.send_with_open_socket(|ws| ws.send_with_u8_array(mut_slice))
    }

    fn set_event_transmitter(&mut self, transmitter: mpsc::UnboundedSender<TransportEvent>) {
        info!(self.logger, "Setting event transmitter.");
        let transmitter_copy = transmitter.clone();
        let logger_copy = self.logger.clone_ref();
        self.set_on_message(move |e| {
            let data = e.data();
            if let Some(text) = data.as_string() {
                debug!(logger_copy, "Received a text message: {text}");
                channel::emit(&transmitter_copy, TransportEvent::TextMessage(text));
            } else if let Ok(array_buffer) = data.dyn_into::<js_sys::ArrayBuffer>() {
                let array = js_sys::Uint8Array::new(&array_buffer);
                let binary_data = array.to_vec();
                debug!(logger_copy, || format!("Received a binary message: {:x?}", binary_data));
                let event = TransportEvent::BinaryMessage(binary_data);
                channel::emit(&transmitter_copy, event);
            } else {
                info!(logger_copy, "Received other kind of message: {e.data().print_to_string()}.");
            }
        });

        let transmitter_copy = transmitter.clone();
        let logger_copy = self.logger.clone_ref();
        self.set_on_close(move |_e| {
            info!(logger_copy, "Connection has been closed.");
            channel::emit(&transmitter_copy, TransportEvent::Closed);
        });

        let logger_copy = self.logger.clone_ref();
        self.set_on_open(move |_e| {
            info!(logger_copy, "Connection has been opened.");
            channel::emit(&transmitter, TransportEvent::Opened);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ensogl::system::web;
    use std::time::Duration;


    /// Provisional code allowing testing WS behavior and its events.
    /// Keeping it for future debug purposes.
    /// To run uncomment attribute line and invoke:
    /// `cargo watch -- wasm-pack test .\ide\ --chrome  --  websocket_test`
    //#[wasm_bindgen_test::wasm_bindgen_test]
    #[allow(dead_code)]
    async fn websocket_tests() {
        executor::web::test::setup_and_forget();
        let logger = DefaultTraceLogger::new("Test");
        info!(logger, "Started");

        // Create WebSocket
        let ws = WebSocket::new_opened(&logger, "ws://localhost:30445").await;
        let mut ws = ws.expect("Couldn't connect to WebSocket server.");
        info!(logger, "WebSocket opened: {ws:?}");

        // Log events
        let handler = ws.establish_event_stream().for_each(f!([logger](event) {
            info!(logger,"Socket emitted event: {event:?}");
            futures::future::ready(())
        }));

        // Spawn task to process events stream.
        executor::global::spawn(handler);

        // Close socket after some delay.
        web::sleep(Duration::from_secs(20)).await;
        info!(logger, "Finished");
    }
}
