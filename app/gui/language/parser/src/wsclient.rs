#![cfg(not(target_arch = "wasm32"))]

use crate::api;
use crate::api::Ast;
use crate::api::Error::*;
use crate::api::Metadata;
use crate::api::ParsedSourceFile;
use crate::prelude::*;

use websocket::stream::sync::TcpStream;
use websocket::ClientBuilder;
use websocket::Message;

use ast::id_map::JsonIdMap;
use ast::IdMap;
use serde::de::DeserializeOwned;
use std::fmt::Formatter;

type WsTcpClient = websocket::sync::Client<TcpStream>;



// ==========================
// == Constants & literals ==
// ==========================

pub const LOCALHOST: &str = "localhost";
pub const DEFAULT_PORT: i32 = 30615;
pub const DEFAULT_HOSTNAME: &str = LOCALHOST;

pub const HOSTNAME_VAR: &str = "ENSO_PARSER_HOSTNAME";
pub const PORT_VAR: &str = "ENSO_PARSER_PORT";



// ===========
// == Error ==
// ===========

pub type Result<T> = std::result::Result<T, Error>;

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "Failed to parse given address url: {}", _0)]
    WrongUrl(#[cause] websocket::client::ParseError),

    #[fail(display = "Connection error: {}", _0)]
    ConnectivityError(#[cause] websocket::WebSocketError),

    #[fail(display = "Peer has closed the connection")]
    PeerClosedConnection,

    #[fail(display = "Received non-text response: {:?}", _0)]
    NonTextResponse(websocket::OwnedMessage),

    #[fail(display = "JSON (de)serialization failed: {:?}", _0)]
    JsonSerializationError(#[cause] serde_json::error::Error),

    #[fail(display = "JSON deserialization failed: {:?}, JSON was: {}", _0, _1)]
    JsonDeserializationError(#[cause] serde_json::error::Error, String),
}

impl From<Error> for api::Error {
    fn from(e: Error) -> Self {
        api::interop_error(e)
    }
}
impl From<websocket::client::ParseError> for Error {
    fn from(error: websocket::client::ParseError) -> Self {
        Error::WrongUrl(error)
    }
}
impl From<websocket::WebSocketError> for Error {
    fn from(error: websocket::WebSocketError) -> Self {
        Error::ConnectivityError(error)
    }
}
impl From<serde_json::error::Error> for Error {
    fn from(error: serde_json::error::Error) -> Self {
        Error::JsonSerializationError(error)
    }
}



// ==============
// == Protocol ==
// ==============

/// All request supported by the Parser Service.
#[allow(clippy::enum_variant_names)]
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Request {
    ParseRequest { program: String, ids: JsonIdMap },
    ParseRequestWithMetadata { content: String },
    DocParserGenerateHtmlSource { program: String },
    DocParserGenerateHtmlFromDoc { code: String },
}

/// All responses that Parser Service might reply with.
#[derive(Debug, serde::Deserialize)]
pub enum Response<Metadata> {
    #[serde(bound(deserialize = "Metadata:Default+DeserializeOwned"))]
    Success {
        module: ParsedSourceFile<Metadata>,
    },
    Error {
        message: String,
    },
}

/// All responses that Doc Parser Service might reply with.
#[derive(Debug, serde::Deserialize)]
pub enum ResponseDoc {
    SuccessDoc { code: String },
    Error { message: String },
}



// ============
// == Config ==
// ============

/// Describes a WS endpoint.
#[derive(Debug, Clone)]
pub struct Config {
    pub host: String,
    pub port: i32,
}

impl Config {
    /// Formats URL String describing a WS endpoint.
    pub fn address_string(&self) -> String {
        format!("ws://{}:{}", self.host, self.port)
    }

    /// Obtains a default WS endpoint to use to connect to parser service
    /// using environment variables or, if they are not set, hardcoded
    /// defaults.
    pub fn from_env() -> Config {
        let host = env::env_var_or(HOSTNAME_VAR, DEFAULT_HOSTNAME);
        let port = env::parse_var_or(PORT_VAR, DEFAULT_PORT);
        Config { host, port }
    }
}



// ============
// == Client ==
// ============

/// Client to the Parser Service written in Scala.
///
/// Connects through WebSocket to the running service.
pub struct Client {
    connection: WsTcpClient,
}

mod internal {
    use super::*;
    impl Client {
        /// Serializes `Request` to JSON and sends to peer as a text message.
        pub fn send_request(&mut self, request: Request) -> Result<()> {
            let request_txt = serde_json::to_string(&request)?;
            let message = Message::text(request_txt);
            self.connection.send_message(&message)?;
            Ok(())
        }

        /// Obtains a text message from peer and deserializes it using JSON
        /// into a `Response`.
        ///
        /// Should be called exactly once after each `send_request` invocation.
        pub fn recv_response<M: Metadata>(&mut self) -> Result<Response<M>> {
            let response = self.connection.recv_message()?;
            match response {
                websocket::OwnedMessage::Text(text) =>
                    crate::from_json_str_without_recursion_limit(&text).map_err(Into::into),
                _ => Err(Error::NonTextResponse(response)),
            }
        }

        /// Obtains a text message from peer and deserializes it using JSON
        /// into a `ResponseDoc`.
        ///
        /// Should be called exactly once after each `send_request` invocation.
        pub fn recv_response_doc(&mut self) -> Result<ResponseDoc> {
            let response = self.connection.recv_message()?;
            match response {
                websocket::OwnedMessage::Text(code) => Ok(serde_json::from_str(&code)?),
                _ => Err(Error::NonTextResponse(response)),
            }
        }

        /// Sends given `Request` to peer and receives a `Response`.
        ///
        /// Both request and response are exchanged in JSON using text messages
        /// over WebSocket.
        pub fn rpc_call<M: Metadata>(&mut self, request: Request) -> Result<Response<M>> {
            self.send_request(request)?;
            self.recv_response()
        }

        /// Sends given `Request` to peer and receives a `ResponseDoc`.
        ///
        /// Both request and response are exchanged in JSON using text messages
        /// over WebSocket.
        pub fn rpc_call_doc(&mut self, request: Request) -> Result<ResponseDoc> {
            self.send_request(request)?;
            self.recv_response_doc()
        }
    }
}

impl Client {
    /// Creates a new `Client` connected to the already running parser service.
    pub fn from_conf(config: &Config) -> Result<Client> {
        let address = config.address_string();
        let mut builder = ClientBuilder::new(&address)?;
        let connection = builder.connect_insecure()?;
        Ok(Client { connection })
    }

    /// Creates a `Client` using configuration defined by environment or
    /// defaults if environment is not set.
    pub fn new() -> Result<Client> {
        // This parser is used only for native debugging, it is not used in production.
        // As such, we can use debug macros here.
        let config = Config::from_env();
        DEBUG!("Connecting to " config.address_string());
        let client = Client::from_conf(&config)?;
        DEBUG!("Established connection with {}" config.address_string());
        Ok(client)
    }

    /// Sends a request to parser service to parse Enso code.
    pub fn parse(&mut self, program: String, ids: IdMap) -> api::Result<Ast> {
        let ids = JsonIdMap::from_id_map(&ids, &program);
        let request = Request::ParseRequest { program, ids };
        let response = self.rpc_call::<serde_json::Value>(request)?;
        match response {
            Response::Success { module } => Ok(module.ast.into()),
            Response::Error { message } => Err(ParsingError(message)),
        }
    }

    /// Sends a request to parser service to parse code with metadata.
    pub fn parse_with_metadata<M: Metadata>(
        &mut self,
        program: String,
    ) -> api::Result<ParsedSourceFile<M>> {
        let request = Request::ParseRequestWithMetadata { content: program };
        let response = self.rpc_call(request)?;
        match response {
            Response::Success { module } => Ok(module),
            Response::Error { message } => Err(ParsingError(message)),
        }
    }

    /// Sends a request to parser service to generate HTML code from documented Enso code.
    pub fn generate_html_docs(&mut self, program: String) -> api::Result<String> {
        let request = Request::DocParserGenerateHtmlSource { program };
        let response_doc = self.rpc_call_doc(request)?;
        match response_doc {
            ResponseDoc::SuccessDoc { code } => Ok(code),
            ResponseDoc::Error { message } => Err(ParsingError(message)),
        }
    }

    /// Sends a request to parser service to generate HTML code from pure documentation code.
    pub fn generate_html_doc_pure(&mut self, code: String) -> api::Result<String> {
        let request = Request::DocParserGenerateHtmlFromDoc { code };
        let response_doc = self.rpc_call_doc(request)?;
        match response_doc {
            ResponseDoc::SuccessDoc { code } => Ok(code),
            ResponseDoc::Error { message } => Err(ParsingError(message)),
        }
    }
}

impl Debug for Client {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Websocket remote parser client>")
    }
}



// ===========
// == tests ==
// ===========

#[test]
fn wrong_url_reported() {
    let invalid_hostname = String::from("bgjhkb 7");
    let wrong_config = Config { host: invalid_hostname, port: 8080 };
    let client = Client::from_conf(&wrong_config);
    let got_wrong_url_error = matches::matches!(client, Err(Error::WrongUrl(_)));
    assert!(got_wrong_url_error, "expected WrongUrl error");
}
