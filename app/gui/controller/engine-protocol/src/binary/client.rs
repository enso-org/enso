//! Module defines LS binary protocol client `API` and its two implementation: `Client` and
//! `MockClient`.

use crate::prelude::*;

use crate::binary::message::ErrorPayload;
use crate::binary::message::FromServerPayloadOwned;
use crate::binary::message::MessageFromServerOwned;
use crate::binary::message::MessageToServerRef;
use crate::binary::message::ToServerPayload;
use crate::binary::message::VisualisationContext;
use crate::common::error::UnexpectedMessage;
use crate::handler::Disposition;
use crate::handler::Handler;
use crate::language_server::types::Path;
use crate::types::Sha3_224;

use json_rpc::Transport;
use json_rpc::TransportEvent;
use mockall::automock;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "Received a text message when expecting only the binary ones.")]
pub struct UnexpectedTextMessage;

/// Errors that can cause a remote call to fail.
pub type RpcError = json_rpc::error::RpcError<ErrorPayload>;


// ====================
// === Notification ===
// ====================

/// The notifications that binary protocol client may receive.
#[derive(Clone, Debug, PartialEq)]
pub enum Notification {
    /// A new data has been sent for a visualization.
    VisualizationUpdate {
        /// Identifies the specific visualization.
        context: VisualisationContext,
        /// Data to be passed to the visualization.
        data:    Vec<u8>,
    },
}

/// Events emitted by the LS binary protocol client.
pub type Event = crate::common::event::Event<Notification>;



// ===========
// === API ===
// ===========

/// The Engine Services Language Server Binary Protocol Client API.
#[automock]
pub trait API {
    /// Initializes the protocol. Must be called exactly once before making any other calls.
    fn init(&self, client_id: Uuid) -> StaticBoxFuture<FallibleResult>;

    /// Writes binary data to the file.
    fn write_file(&self, path: &Path, contents: &[u8]) -> StaticBoxFuture<FallibleResult>;

    /// Retrieves the file contents as a binary data.
    fn read_file(&self, path: &Path) -> StaticBoxFuture<FallibleResult<Vec<u8>>>;

    /// Writes a set of bytes to the specified file at the specified offset.
    fn write_bytes(
        &self,
        path: &Path,
        byte_offset: u64,
        overwrite: bool,
        bytes: &[u8],
    ) -> StaticBoxFuture<FallibleResult<Sha3_224>>;

    /// Asynchronous event stream with notification and errors.
    ///
    /// On a repeated call, previous stream is closed.
    fn event_stream(&self) -> StaticBoxStream<Event>;
}



// ==============
// === Client ===
// ==============

/// The client for Engine Services Language Server Binary Protocol.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct Client {
    handler: Handler<Uuid, FromServerPayloadOwned, Notification>,
    logger:  Logger,
}

impl Client {
    /// Helper function that fails if the received message represents a remote error.
    fn expect_success(result: FromServerPayloadOwned) -> FallibleResult {
        if let FromServerPayloadOwned::Success {} = result {
            Ok(())
        } else {
            Err(RpcError::MismatchedResponseType.into())
        }
    }

    /// Function that does early processing of the peer's message and decides how it shall be
    /// handled. Returns a function so that it may be passed to the `Handler`.
    fn processor(
    ) -> impl FnMut(TransportEvent) -> Disposition<Uuid, FromServerPayloadOwned, Notification> + 'static
    {
        move |event: TransportEvent| {
            let binary_data = match event {
                TransportEvent::BinaryMessage(data) => data,
                _ => return Disposition::error(UnexpectedTextMessage),
            };
            let message = match MessageFromServerOwned::deserialize(&binary_data) {
                Ok(message) => message,
                Err(e) => return Disposition::error(e),
            };
            debug!("Deserialized incoming binary message: {message:?}");
            let correlation_id = message.correlation_id;
            match message.0.payload {
                FromServerPayloadOwned::VisualizationUpdate { context, data } =>
                    Disposition::notify(Notification::VisualizationUpdate { data, context }),
                payload => {
                    if let Some(id) = correlation_id {
                        Disposition::HandleReply { id, reply: payload }
                    } else {
                        // Not a known notification and yet not a response to our request.
                        Disposition::error(UnexpectedMessage)
                    }
                }
            }
        }
    }

    /// Creates a new client from the given transport to the Language Server Data Endpoint.
    ///
    /// Before client is functional:
    /// * `runner` must be scheduled for execution;
    /// * `init` must be called or it needs to be wrapped into `Connection`.
    pub fn new(parent: impl AnyLogger, transport: impl Transport + 'static) -> Client {
        let logger = Logger::new_sub(parent, "binary-protocol-client");
        let processor = Self::processor();
        Client { logger: logger.clone_ref(), handler: Handler::new(transport, logger, processor) }
    }

    /// Starts a new request, described by the given payload.
    /// Function `f` serves to retrieve the request's result from the more general `Reply` type.
    pub fn make_request<F, R>(
        &self,
        payload: ToServerPayload,
        f: F,
    ) -> StaticBoxFuture<FallibleResult<R>>
    where
        F: FnOnce(FromServerPayloadOwned) -> FallibleResult<R>,
        R: 'static,
        F: 'static,
    {
        let message = MessageToServerRef::new(payload);
        let id = message.message_id;
        let completer = move |reply| {
            info!("Completing request {id} with a reply: {reply:?}");
            if let FromServerPayloadOwned::Error { code, message, data } = reply {
                let code = code as i64;
                let error = json_rpc::messages::Error { code, message, data };
                Err(RpcError::RemoteError(error).into())
            } else {
                f(reply)
            }
        };

        let fut = self.handler.make_request(&message, completer);
        Box::pin(fut)
    }

    /// A `runner`. Its execution must be scheduled for `Client` to be able to complete requests and
    /// emit events.
    pub fn runner(&self) -> impl Future<Output = ()> {
        self.handler.runner()
    }
}

impl API for Client {
    fn init(&self, client_id: Uuid) -> StaticBoxFuture<FallibleResult> {
        info!("Initializing binary connection as client with id {client_id}.");
        let payload = ToServerPayload::InitSession { client_id };
        self.make_request(payload, Self::expect_success)
    }

    fn write_file(&self, path: &Path, contents: &[u8]) -> StaticBoxFuture<FallibleResult> {
        info!("Writing file {} with {} bytes.", path, contents.len());
        let payload = ToServerPayload::WriteFile { path, contents };
        self.make_request(payload, Self::expect_success)
    }

    fn read_file(&self, path: &Path) -> StaticBoxFuture<FallibleResult<Vec<u8>>> {
        info!("Reading file {path}.");
        let payload = ToServerPayload::ReadFile { path };
        self.make_request(payload, move |result| {
            if let FromServerPayloadOwned::FileContentsReply { contents } = result {
                Ok(contents)
            } else {
                Err(RpcError::MismatchedResponseType.into())
            }
        })
    }

    fn write_bytes(
        &self,
        path: &Path,
        byte_offset: u64,
        overwrite: bool,
        bytes: &[u8],
    ) -> StaticBoxFuture<FallibleResult<Sha3_224>> {
        info!("Writing {} bytes to {path} at offset {byte_offset}", bytes.len());
        let payload = ToServerPayload::WriteBytes { path, byte_offset, overwrite, bytes };
        self.make_request(payload, move |result| {
            if let FromServerPayloadOwned::WriteBytesReply { checksum } = result {
                Ok(checksum.into())
            } else {
                Err(RpcError::MismatchedResponseType.into())
            }
        })
    }

    fn event_stream(&self) -> StaticBoxStream<Event> {
        self.handler.event_stream().boxed_local()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::binary::message::MessageFromServer;
    use crate::binary::message::MessageToServerOwned;
    use crate::binary::message::ToServerPayloadOwned;

    use futures::task::LocalSpawnExt;
    use json_rpc::test_util::transport::mock::MockTransport;



    // ===============
    // === Fixture ===
    // ===============

    struct ClientFixture {
        transport: MockTransport,
        client:    Client,
        executor:  futures::executor::LocalPool,
    }

    impl ClientFixture {
        fn new() -> ClientFixture {
            let logger = Logger::new("ClientFixture");
            let transport = MockTransport::new();
            let client = Client::new(&logger, transport.clone());
            let executor = futures::executor::LocalPool::new();
            executor.spawner().spawn_local(client.runner()).unwrap();
            ClientFixture { transport, client, executor }
        }
    }



    // ========================
    // === Testing Requests ===
    // ========================

    fn test_request<R>(
        make_request: impl Fn(&Client) -> StaticBoxFuture<FallibleResult<R>>,
        expected_result: R,
        expected_request: ToServerPayloadOwned,
        mock_reply: FromServerPayloadOwned,
    ) where
        R: Debug + PartialEq + Sized,
    {
        let mut fixture = ClientFixture::new();

        let mut fut = make_request(&fixture.client);

        let generated_message = fixture.transport.expect_binary_message();
        let generated_message = MessageToServerOwned::deserialize(&generated_message).unwrap();
        assert_eq!(generated_message.payload, expected_request);
        fut.expect_pending();

        let mut mock_reply = MessageFromServer::new(mock_reply);
        mock_reply.correlation_id = Some(generated_message.message_id);
        mock_reply.with_serialized(|data| fixture.transport.mock_peer_binary_message(data));
        fixture.executor.run_until_stalled();
        assert_eq!(fut.expect_ok(), expected_result);

        // Repeat request but now answer with error.
        let mut fut = make_request(&fixture.client);
        let generated_message = fixture.transport.expect_binary_message();
        let generated_message = MessageToServerOwned::deserialize(&generated_message).unwrap();

        let mock_error_code = 444;
        let mock_error_message = "This is error".to_string();
        let mut mock_reply = MessageFromServer::new(FromServerPayloadOwned::Error {
            code:    mock_error_code,
            message: mock_error_message,
            data:    None,
        });
        mock_reply.correlation_id = Some(generated_message.message_id);
        mock_reply.with_serialized(|data| fixture.transport.mock_peer_binary_message(data));
        fixture.executor.run_until_stalled();
        fut.expect_err();
    }

    #[test]
    fn test_init() {
        let client_id = Uuid::new_v4();
        test_request(
            |client| client.init(client_id),
            (),
            ToServerPayloadOwned::InitSession { client_id },
            FromServerPayloadOwned::Success {},
        );
    }

    #[test]
    fn test_write_file() {
        let root_id = Uuid::new_v4();
        let path = Path::new(root_id, &["Main.enso"]);
        let data = Vec::from("hello".as_bytes());
        test_request(
            |client| client.write_file(&path, &data),
            (),
            ToServerPayloadOwned::WriteFile { contents: data.clone(), path: path.clone() },
            FromServerPayloadOwned::Success {},
        );
    }

    #[test]
    fn test_read_file() {
        let root_id = Uuid::new_v4();
        let path = Path::new(root_id, &["Main.enso"]);
        let data = Vec::from("hello".as_bytes());
        test_request(
            |client| client.read_file(&path),
            data.clone(),
            ToServerPayloadOwned::ReadFile { path: path.clone() },
            FromServerPayloadOwned::FileContentsReply { contents: data },
        );
    }



    // =============================
    // === Testing Notifications ===
    // =============================

    #[test]
    fn test_visualization_update() {
        let mut fixture = ClientFixture::new();

        let mut event_fut = fixture.client.event_stream().into_future().boxed_local();
        fixture.executor.run_until_stalled();
        event_fut.expect_pending();

        let context = VisualisationContext {
            visualization_id: Uuid::new_v4(),
            expression_id:    Uuid::new_v4(),
            context_id:       Uuid::new_v4(),
        };
        let data = Vec::from("Hello".as_bytes());
        let message = MessageFromServer::new(FromServerPayloadOwned::VisualizationUpdate {
            data: data.clone(),
            context,
        });


        message.with_serialized(|data| fixture.transport.mock_peer_binary_message(data));
        fixture.executor.run_until_stalled();

        let expected_notification = Notification::VisualizationUpdate { context, data };
        let (event, tail) = event_fut.expect_ready();
        match event.expect("Expected some notification.") {
            Event::Notification(notification) => assert_eq!(notification, expected_notification),
            event => panic!("Expected notification event, got: {:?}", event),
        }
        tail.boxed_local().expect_pending();
    }
}
