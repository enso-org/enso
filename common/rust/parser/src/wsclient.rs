#![cfg(not(target_arch = "wasm32"))]

use failure::Fail;
use std::default::Default;
use websocket::{
    stream::sync::TcpStream, ClientBuilder, Message, OwnedMessage,
};

use crate::api;
use api::Error::*;
use Error::*;

type WsTcpClient = websocket::sync::Client<TcpStream>;

// ==========================
// == Constants & literals ==
// ==========================

pub const LOCALHOST:        &str = "localhost";
pub const DEFAULT_PORT:      i32 = 30615;
pub const DEFAULT_HOSTNAME: &str = LOCALHOST;

pub const HOSTNAME_VAR: &str = "ENSO_PARSER_HOSTNAME";
pub const PORT_VAR:     &str = "ENSO_PARSER_PORT";

// ===========
// == Error ==
// ===========

pub type Result<T> = std::result::Result<T, Error>;

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
}

impl From<Error> for api::Error {
    fn from(e: Error) -> Self {
        api::interop_error(e)
    }
}
impl From<websocket::client::ParseError> for Error {
    fn from(error: websocket::client::ParseError) -> Self {
        WrongUrl(error)
    }
}
impl From<websocket::WebSocketError> for Error {
    fn from(error: websocket::WebSocketError) -> Self {
        ConnectivityError(error)
    }
}
impl From<serde_json::error::Error> for Error {
    fn from(error: serde_json::error::Error) -> Self {
        JsonSerializationError(error)
    }
}

// ==============
// == Protocol ==
// ==============

/// All request supported by the Parser Service.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Request {
    ParseRequest { program: String },
}

/// All responses that Parser Service might reply with.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum Response {
    Success { ast:     String },
    Error   { message: String },
}

// ============
// == Config ==
// ============

/// Describes a WS endpoint.
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
        let host = env_var_or(HOSTNAME_VAR, DEFAULT_HOSTNAME);
        let port = env_var_or(PORT_VAR, Default::default())
            .parse()
            .unwrap_or(DEFAULT_PORT);
        Config { host, port }
    }
}

pub fn env_var_or(varname: &str, default_value: &str) -> String {
    std::env::var(varname).unwrap_or_else(|_| default_value.into())
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
            let message     = Message::text(request_txt);
            self.connection.send_message(&message)?;
            Ok(())
        }

        /// Obtains a text message from peer and deserializes it using JSON
        /// into a `Response`.
        ///
        /// Should be called exactly once after each `send_request` invocation.
        pub fn recv_response(&mut self) -> Result<Response> {
            let response = self.connection.recv_message()?;
            match response {
                OwnedMessage::Text(text) => Ok(serde_json::from_str(&text)?),
                _                        => Err(NonTextResponse(response)),
            }
        }

        /// Sends given `Request` to peer and receives a `Response`.
        ///
        /// Both request and response are exchanged in JSON using text messages
        /// over WebSocket.
        pub fn rpc_call(&mut self, request: Request) -> Result<Response> {
            self.send_request(request)?;
            self.recv_response()
        }
    }
}

impl Client {
    /// Creates a new `Client` connected to the already running parser service.
    pub fn from_conf(config: &Config) -> Result<Client> {
        let address     = config.address_string();
        let mut builder = ClientBuilder::new(&address)?;
        let connection  = builder.connect_insecure()?;
        Ok(Client { connection })
    }

    /// Creates a `Client` using configuration defined by environment or
    /// defaults if environment is not set.
    pub fn new() -> Result<Client> {
        let config = Config::from_env();
        println!("Connecting to {}", config.address_string());
        let client = Client::from_conf(&config)?;
        println!("Established connection with {}", config.address_string());
        Ok(client)
    }
}

impl api::IsParser for Client {
    fn parse(&mut self, program: String) -> api::Result<api::AST> {
        let request  = Request::ParseRequest { program };
        let response = self.rpc_call(request)?;
        match response {
            Response::Success { ast     } => Ok(ast),
            Response::Error   { message } => Err(ParsingError(message)),
        }
    }
}

// ===========
// == tests ==
// ===========


#[test]
fn wrong_url_reported() {
    let invalid_hostname    = String::from("bgjhkb 7");
    let wrong_config        = Config { host: invalid_hostname, port: 8080 };
    let client              = Client::from_conf(&wrong_config);
    let got_wrong_url_error = matches::matches!(client, Err(WrongUrl(_)));
    assert!(got_wrong_url_error, "expected WrongUrl error");
}
