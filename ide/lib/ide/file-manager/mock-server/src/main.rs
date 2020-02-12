//! WARNING: PROVISIONAL CODE [mwu]
//!
//! This is a provisional mock implementation for the File Manager Server.
//! Its purpose is to enable development and testing of the IDE code, until the
//! proper server is delivered by the cloud team.
//!
//! Only a few methods are provided, more can be added as needed, when needed.
//!
//! As such this implementation is low-effort: known to be incomplete and not
//! bothering itself with more complex error handling. Eventually it should be
//! removed altogether.

use enso_prelude::*;

use json_rpc::messages;
use serde::Serialize;
use serde::Deserialize;
use std::net::TcpListener;
use std::net::TcpStream;
use std::path::PathBuf;
use std::thread::spawn;
use tungstenite::accept_hdr;
use tungstenite::WebSocket;
use tungstenite::Message;
use tungstenite::handshake::server::Request;
use tungstenite::handshake::server::Response;



// ==========================
// == Constants & literals ==
// ==========================

/// Error code that server returns on a failed call.
const FAILED_CALL_ERROR_CODE:i64 = -32000;

/// Default port that server listens on.
pub const DEFAULT_PORT:i32  = 30616;

/// Environemnt variable that can override the port that server listens on.
pub const PORT_VAR:&str = "ENSO_FILE_MANAGER_PORT";



// ==================
// == Useful types ==
// ==================


/// Result used for implementing RPC methods.
type CallResult<T> = std::result::Result<T,failure::Error>;

/// Partially decoded Request - its `id` can be read but the call parameters
/// remain encoded in JSON.
type SomeRequest = messages::Message<messages::Request<serde_json::Value>>;



// ============
// == Handler ==
// ============

/// Handler for the established, websocket connection.
struct Handler {
    socket : WebSocket<TcpStream>,
}

impl Handler {
    /// Make the call and wrap the result (or error) into a reply message.
    fn realize_call<C: IsCall>(&self, call:&C) -> messages::Result<serde_json::Value> {
        match call.realize() {
            Ok(result) => messages::Result::new_success(serde_json::to_value(&result).unwrap()),
            Err(e)     => messages::Result::new_error_simple(FAILED_CALL_ERROR_CODE,e.to_string()),
        }
    }

    /// Recognize what kind of call has been requested and delegate it to appropriate
    /// handling code.
    fn handle_request(&self, request:SomeRequest) -> messages::Result<serde_json::Value> {
        let value = request.payload.call;
        match serde_json::from_value::<Call>(value) {
            Ok(call) => match call {
                Call::CopyFile(call) => self.realize_call(&call),
                Call::Exists  (call) => self.realize_call(&call),
                Call::List    (call) => self.realize_call(&call),
                Call::Read    (call) => self.realize_call(&call),
                Call::Touch   (call) => self.realize_call(&call),
                Call::Write   (call) => self.realize_call(&call),
            }
            Err(e) => {
                messages::Result::new_error_simple(FAILED_CALL_ERROR_CODE,e.to_string())
            }
        }
    }

    /// Takes JSON-encoded message and returns the response.
    fn handle_message(&self, request_text:String) -> serde_json::Value {
        let request = serde_json::from_str::<SomeRequest>(&request_text);
        match request {
            Ok(request) => {
                let id       = request.id;
                let result   = self.handle_request(request);
                let response = messages::Response{id,result};
                let reply    = messages::Message::new(response);
                serde_json::to_value(reply).unwrap()
            },
            Err(_e) => {
                // TODO: [mwu] here should go proper error handling that builds
                //       an error that has `id` set to `null`
                default()
            }
        }
    }

    /// Reads text messages and replies to them.
    /// If the connection is lost, silently returns.
    fn run(&mut self) {
        while let Ok(request_msg) = self.socket.read_message() {
            if request_msg.is_text() {
                let request_text = request_msg.to_string();
                println!("Got text: {}", request_text);
                let reply_json   = self.handle_message(request_text);
                let reply_text   = reply_json.to_string();
                println!("Replying with text: {}", reply_text);
                let reply_msg    = Message::text(reply_text);
                if self.socket.write_message(reply_msg).is_err() {
                    break;
                }
            }
            // ignore non-text messages.
        }
        println!("Finished handling connection");
    }
}



// ==========
// == Main ==
// ==========

fn main() {
    let port    = utils::env::parse_var_or(PORT_VAR, DEFAULT_PORT);
    let address = iformat!("127.0.0.1:{port}");
    let server  = TcpListener::bind(&address).unwrap();
    println!("Listening on {}", address);
    for stream in server.incoming() {
        spawn(move || {
            println!("Got a new connection");
            let callback = |_req: &Request, response: Response| {
                Ok(response)
            };
            let socket = accept_hdr(stream.unwrap(), callback).unwrap();
            let mut server = Handler { socket };
            server.run();
        });
    }
}



// =============
// == IsCall  ==
// =============

/// All methods supported by this server should implement the trait.
pub trait IsCall {
    /// Type of value returned on a successful call.
    type Result: Serialize;

    /// Perform the "actual" work for this call.
    fn realize(&self) -> CallResult<Self::Result>;
}



// ===========
// == Call  ==
// ===========

/// Possible call requests. Members should wrap structs implementing `IsCall`.
#[derive(Serialize,Deserialize,Debug,PartialEq,Clone)]
#[serde(tag="method", content="params", rename_all="camelCase")]
enum Call {
    CopyFile(CopyFile),
    Exists(Exists),
    List(List),
    Read(Read),
    Touch(Touch),
    Write(Write),
}



// ==============
// == Methods  ==
// ==============

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
struct CopyFile {from:PathBuf, to:PathBuf}
impl IsCall for CopyFile {
    type Result = ();
    fn realize(&self) -> CallResult<()> {
        Ok(std::fs::copy(&self.from,&self.to).map(|_| {})?)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
struct Exists {path:PathBuf}
impl IsCall for Exists {
    type Result = bool;
    fn realize(&self) -> CallResult<bool> {
        Ok(self.path.exists())
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
struct List {path:PathBuf}
impl IsCall for List {
    type Result = Vec<PathBuf>;
    fn realize(&self) -> CallResult<Self::Result> {
        let read_dirs = std::fs::read_dir(&self.path)?;
        let mut ret: Vec<PathBuf> = default();
        for rd in read_dirs {
            ret.push(rd?.path())
        }
        Ok(ret)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
struct Read {path:PathBuf}
impl IsCall for Read {
    type Result = String;
    fn realize(&self) -> CallResult<String> {
        Ok(std::fs::read_to_string(&self.path)?)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
struct Touch {path:PathBuf}
impl IsCall for Touch {
    type Result = ();
    fn realize(&self) -> CallResult<()> {
        let mut opts = std::fs::OpenOptions::new();
        opts.create(true).write(true);
        let _ = opts.open(&self.path)?;
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
struct Write {path:PathBuf, contents:String}
impl IsCall for Write {
    type Result = ();
    fn realize(&self) -> CallResult<()> {
        Ok(std::fs::write(&self.path,&self.contents)?)
    }
}
