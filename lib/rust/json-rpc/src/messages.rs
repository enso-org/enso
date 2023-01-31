//! This module provides data structures that follow JSON-RPC 2.0 scheme. Their
//! serialization and deserialization using serde_json shall is compatible with
//! JSON-RPC complaint peers.

use crate::prelude::*;

use serde::Deserialize;
use serde::Serialize;



// ===============
// === Message ===
// ===============

/// All JSON-RPC messages bear `jsonrpc` version number.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Deref)]
pub struct Message<T> {
    /// JSON-RPC Protocol version, should be 2.0.
    pub jsonrpc: Version,

    /// Payload, either a Request or Response or Notification in direct
    /// or serialized form.
    #[serde(flatten)]
    #[deref]
    pub payload: T,
}


// === Common Message Subtypes ===

/// A request message.
pub type RequestMessage<In> = Message<Request<MethodCall<In>>>;

/// A response message.
pub type ResponseMessage<Ret> = Message<Response<Ret>>;

/// A response message.
pub type NotificationMessage<Ret> = Message<Notification<MethodCall<Ret>>>;


// === `new` Functions ===

impl<T> Message<T> {
    /// Wraps given payload into a JSON-RPC 2.0 message.
    pub fn new(t: T) -> Message<T> {
        Message { jsonrpc: Version::V2, payload: t }
    }

    /// Construct a request message.
    pub fn new_request(id: Id, method: &str, params: T) -> RequestMessage<T> {
        let call = MethodCall { method: method.into(), params };
        let request = Request::new(id, call);
        Message::new(request)
    }

    /// Construct a successful response message.
    pub fn new_success(id: Id, result: T) -> ResponseMessage<T> {
        let result = Result::new_success(result);
        let response = Response { id, result };
        Message::new(response)
    }

    /// Construct a successful response message.
    pub fn new_error(
        id: Id,
        code: i64,
        message: String,
        data: Option<serde_json::Value>,
    ) -> ResponseMessage<T> {
        let result = Result::new_error(code, message, data);
        let response = Response { id, result };
        Message::new(response)
    }

    /// Construct a request message.
    pub fn new_notification(method: &'static str, params: T) -> NotificationMessage<T> {
        let call = MethodCall { method: method.into(), params };
        let notification = Notification(call);
        Message::new(notification)
    }
}



// ========================
// === Message Subparts ===
// ========================

/// An id identifying the call request.
///
/// Each request made by client should get a unique id (unique in a context of
/// the current session). Auto-incrementing integer is a common choice.
#[derive(Serialize, Deserialize)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash, Deref)]
#[derive(Display)]
pub struct Id(pub i64);

/// JSON-RPC protocol version. Only 2.0 is supported.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Version {
    /// JSON-RPC 2.0 specification. The supported version.
    #[serde(rename = "2.0")]
    V2,
}

/// A non-notification request.
///
/// `Call` must be a type, that upon JSON serialization provides `method` and
/// `params` fields, like `MethodCall`.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Deref)]
pub struct Request<Call> {
    /// An identifier for this request that will allow matching the response.
    pub id:   Id,
    #[serde(flatten)]
    #[deref]
    /// method and its params
    pub call: Call,
}

impl<M> Request<M> {
    /// Create a new request.
    pub fn new(id: Id, call: M) -> Request<M> {
        Request { id, call }
    }
}

/// A notification request.
///
/// `Call` must be a type, that upon JSON serialization provides `method` and
/// `params` fields, like `MethodCall`.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Notification<Call>(pub Call);

/// A response to a `Request`. Depending on `result` value it might be
/// successful or not.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Response<Res> {
    /// Identifier, matching the value given in `Request` when call was made.
    pub id:     Id,
    /// Call result.
    #[serde(flatten)]
    pub result: Result<Res>,
}

/// Result of the remote call — either a returned value or en error.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
#[serde(untagged)]
#[allow(missing_docs)]
pub enum Result<Res> {
    /// Returned value of a successful call.
    Success(Success<Res>),
    /// Error value from a called that failed on the remote side.
    Error { error: Error },
}

impl<Res> Result<Res> {
    /// Construct a successful remote call result value.
    pub fn new_success(result: Res) -> Result<Res> {
        Result::Success(Success { result })
    }

    /// Construct a failed remote call result value.
    pub fn new_error(code: i64, message: String, data: Option<serde_json::Value>) -> Result<Res> {
        Result::Error { error: Error { code, message, data } }
    }

    /// Construct a failed remote call result value that bears no optional data.
    pub fn new_error_simple(code: i64, message: String) -> Result<Res> {
        Self::new_error(code, message, None)
    }
}

/// Value yield by a successful remote call.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Success<Ret> {
    /// A value returned from a successful remote call.
    pub result: Ret,
}

/// Error raised on a failed remote call.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Error<Payload = serde_json::Value> {
    /// A number indicating what type of error occurred.
    pub code:    i64,
    /// A short description of the error.
    pub message: String,
    /// Optional value with additional information about the error.
    pub data:    Option<Payload>,
}

/// A message that can come from Server to Client — either a response or
/// notification.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
#[serde(untagged)]
pub enum IncomingMessage {
    /// A response to a call made by client.
    Response(Response<serde_json::Value>),
    /// A notification call (initiated by the server).
    Notification(Notification<serde_json::Value>),
}

/// Partially decodes incoming message.
///
/// This checks if has `jsonrpc` version string, and whether it is a
/// response or a notification.
pub fn decode_incoming_message(message: &str) -> serde_json::Result<IncomingMessage> {
    use serde_json::from_str;
    use serde_json::from_value;
    use serde_json::Value;
    let message = from_str::<Message<Value>>(message)?;
    from_value::<IncomingMessage>(message.payload)
}

/// Message from server to client.
///
/// `In` is any serializable (or already serialized) representation of the
/// method arguments passed in this call.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Deref)]
pub struct MethodCall<In> {
    /// Name of the method that is being called.
    pub method: String,
    /// Method arguments.
    #[deref]
    pub params: In,
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Map;
    use serde_json::Value;

    #[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
    struct MockRequest {
        number: i64,
    }
    impl MockRequest {
        const FIELD_COUNT: usize = 1;
        const FIELD_NAME: &'static str = "number";
    }

    mod protocol {
        // === Field Names ===
        pub const JSONRPC: &str = "jsonrpc";
        pub const METHOD: &str = "method";
        pub const PARAMS: &str = "params";
        pub const ID: &str = "id";

        // === Version strings ===
        pub const VERSION2_STRING: &str = "2.0";

        // === Other ===
        pub const FIELD_COUNT_IN_REQUEST: usize = 4;
        pub const FIELD_COUNT_IN_NOTIFICATION: usize = 3;
    }

    fn expect_field<'a, Obj: 'a>(obj: &'a Map<String, Value>, field_name: &str) -> &'a Value
    where &'a Obj: Into<&'a Value> {
        let missing_msg = format!("missing field {field_name}");
        obj.get(field_name).expect(&missing_msg)
    }

    #[test]
    fn test_request_serialization() {
        let id = Id(50);
        let method = "mockMethod";
        let number = 124;
        let params = MockRequest { number };
        let call = MethodCall { method: method.into(), params };
        let request = Request::new(id, call);
        let message = Message::new(request);

        let json = serde_json::to_value(message).expect("serialization error");
        let json = json.as_object().expect("expected an object");
        assert_eq!(json.len(), protocol::FIELD_COUNT_IN_REQUEST);
        let jsonrpc_field = expect_field(json, protocol::JSONRPC);
        assert_eq!(jsonrpc_field, protocol::VERSION2_STRING);
        assert_eq!(expect_field(json, protocol::ID), id.0);
        assert_eq!(expect_field(json, protocol::METHOD), method);
        let params_json = expect_field(json, protocol::PARAMS);
        let params_json = params_json.as_object().expect("params must be object");
        assert_eq!(params_json.len(), MockRequest::FIELD_COUNT);
        assert_eq!(expect_field(params_json, MockRequest::FIELD_NAME), number);
    }

    #[test]
    fn test_notification_serialization() {
        let method = "mockNotification";
        let number = 125;
        let params = MockRequest { number };
        let call = MethodCall { method: method.into(), params };
        let notification = Notification(call);
        let message = Message::new(notification);

        debug!("{}", serde_json::to_string(&message).unwrap());

        let json = serde_json::to_value(message).expect("serialization error");
        let json = json.as_object().expect("expected an object");
        assert_eq!(json.len(), protocol::FIELD_COUNT_IN_NOTIFICATION);
        let jsonrpc_field = expect_field(json, protocol::JSONRPC);
        assert_eq!(jsonrpc_field, protocol::VERSION2_STRING);
        assert_eq!(expect_field(json, protocol::METHOD), method);
        let params_json = expect_field(json, protocol::PARAMS);
        let params_json = params_json.as_object().expect("params must be object");
        assert_eq!(params_json.len(), MockRequest::FIELD_COUNT);
        assert_eq!(expect_field(params_json, MockRequest::FIELD_NAME), number);
    }


    #[test]
    fn test_response_deserialization() {
        let response = r#"{"jsonrpc":"2.0","id":0,"result":{"exists":true}}"#;
        let msg = serde_json::from_str(response).unwrap();
        if let IncomingMessage::Response(resp) = msg {
            assert_eq!(resp.id, Id(0));
            if let Result::Success(ret) = resp.result {
                let obj = ret.result.as_object().expect("expected object ret");
                assert_eq!(obj.len(), 1);
                let exists = obj.get("exists").unwrap().as_bool().unwrap();
                assert!(exists)
            } else {
                panic!("Expected a success result")
            }
        } else {
            panic!("Expected a response!");
        }
    }

    #[test]
    fn version_serialization_and_deserialization() {
        use protocol::VERSION2_STRING;
        use serde_json::from_str;
        let expected_json = Value::String(VERSION2_STRING.into());
        let expected_json_text = serde_json::to_string(&expected_json);
        let expected_json_text = expected_json_text.unwrap();
        let got_json_text = serde_json::to_string(&Version::V2).unwrap();
        assert_eq!(got_json_text, expected_json_text);

        let got_value = from_str::<Version>(&expected_json_text).unwrap();
        assert_eq!(got_value, Version::V2);
    }

    #[test]
    fn decode_incoming_error_message_text() {
        let text = r#"{"jsonrpc":"2.0","id":1,"error":{"code":1,"message":"Service error"}}"#;
        let decoding_result = decode_incoming_message(text);
        match decoding_result {
            Ok(IncomingMessage::Response(Response {
                result: Result::Error { error: Error { code, message, data } },
                ..
            })) => {
                assert_eq!(code, 1);
                assert_eq!(message, "Service error");
                assert!(data.is_none());
            }
            _ => panic!("Invalid decoding result of {text}: {decoding_result:?}"),
        }
    }
}
