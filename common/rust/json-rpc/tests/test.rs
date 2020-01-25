use prelude::*;

use futures::FutureExt;
use futures::Stream;
use json_rpc::*;
use json_rpc::api::RemoteMethodCall;
use json_rpc::api::Result;
use json_rpc::error::RpcError;
use json_rpc::error::HandlingError;
use json_rpc::messages::Id;
use json_rpc::messages::Message;
use json_rpc::messages::Version;
use json_rpc::test_util::transport::mock::MockTransport;
use serde::Deserialize;
use serde::Serialize;
use std::future::Future;
use std::pin::Pin;
use std::sync::mpsc::TryRecvError;
use utils::poll_future_output;
use utils::poll_stream_output;

type MockEvent = json_rpc::handler::Event<MockNotification>;



// =====================
// === Mock Protocol ===
// =====================


// === Remote Method ===

fn pow_impl(msg:MockRequestMessage) -> MockResponseMessage {
    let ret = MockResponse { result : msg.i * msg.i };
    Message::new_success(msg.id,ret)
}


// === Protocol Data ===

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct MockRequest {i:i64}

impl RemoteMethodCall for MockRequest {
    const NAME:&'static str = "pow";
    type Returned           = MockResponse;
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct MockResponse { result:i64 }

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
#[serde(tag = "method", content="params")]
pub enum MockNotification {
    Meow {text:String},
    Bark {text:String},
}


// === Helper Aliases ===

type MockRequestMessage = messages::RequestMessage<MockRequest>;

type MockResponseMessage = messages::ResponseMessage<MockResponse>;



// ===================
// === Mock Client ===
// ===================

pub struct Client {
    pub handler       : Handler<MockNotification>,
    pub events_stream : Pin<Box<dyn Stream<Item = MockEvent>>>,
}

impl Client {
    pub fn new(transport:impl Transport + 'static) -> Client {
        let mut handler   = Handler::new(transport);

        let events_stream = Box::pin(handler.events());
        Client {
            handler,
            events_stream,
        }
    }

    pub fn pow(&mut self, i:i64) -> impl Future<Output = Result<i64>> {
        let input = MockRequest { i };
        self.handler.open_request(input).map(|result| result.map(|r| r.result))
    }

    pub fn process_events(&mut self) {
        self.handler.process_events()
    }

    pub fn try_get_event(&mut self) -> Option<MockEvent> {
        poll_stream_output(&mut self.events_stream)
    }

    pub fn try_get_notification(&mut self) -> Option<MockNotification> {
        let event = self.try_get_event()?;
        if let MockEvent::Notification(n) = event {
            Some(n)
        } else {
            None
        }
    }

    pub fn expect_notification(&mut self) -> MockNotification {
        self.try_get_notification().expect("expected notification event")
    }

    pub fn expect_handling_error(&mut self) -> HandlingError {
        let event = self.try_get_event().expect("no events, while expected error event");
        if let json_rpc::handler::Event::Error(err) = event {
            err
        } else {
            panic!("expected error event, encountered: {:?}", event)
        }
    }
}



// ============
// === Test ===
// ============

fn setup() -> (MockTransport, Client) {
    let transport = MockTransport::new();
    let client = Client::new(transport.clone());
    (transport,client)
}

#[test]
fn test_success_call() {
    let (mut transport, mut client)   = setup();
    let     call_input                = 8;
    let mut fut                       = Box::pin(client.pow(8));
    let     expected_first_request_id = Id(0);

    // validate request sent
    let req_msg = transport.expect_message::<MockRequestMessage>();
    assert_eq!(req_msg.id, expected_first_request_id);
    assert_eq!(req_msg.method, MockRequest::NAME);
    assert_eq!(req_msg.i, call_input);
    assert_eq!(req_msg.jsonrpc, Version::V2);

    assert!(poll_future_output(&mut fut).is_none()); // no reply

    // let's reply
    let reply = pow_impl(req_msg);
    transport.mock_peer_message(reply);

    // before tick message should be in buffer and callbacks should not
    // complete
    assert!(poll_future_output(&mut fut).is_none()); // not ticked

    // now tick
    client.process_events();
    if let Err(TryRecvError::Empty) = client.handler.incoming_events.try_recv() {
        // ok
    } else {
        panic!("All messages from the buffer should be already processed");
    }
    let result = poll_future_output(&mut fut);
    let result = result.expect("result should be present");
    let result = result.expect("result should be a success");
    assert_eq!(result, 8*8);
}

#[test]
fn test_error_call() {
    let (mut transport, mut client) = setup();
    let mut fut                     = Box::pin(client.pow(8));
    assert!(poll_future_output(&mut fut).is_none()); // no reply

    // reply with error
    let req_msg           = transport.expect_message::<MockRequestMessage>();
    let error_code        = 5;
    let error_description = "wrong!";
    let error_data        = None;
    let error_msg: MockResponseMessage = Message::new_error(
        req_msg.id,
        error_code,
        error_description.into(),
        error_data.clone(),
    );
    transport.mock_peer_message(error_msg);

    // receive error
    client.process_events();
    let result = poll_future_output(&mut fut);
    let result = result.expect("result should be present");
    let result = result.expect_err("result should be a failure");
    if let RpcError::RemoteError(e) = result {
        assert_eq!(e.code, error_code);
        assert_eq!(e.data, error_data);
        assert_eq!(e.message, error_description);
    } else {
        panic!("Expected an error to be RemoteError");
    }
}

#[test]
fn test_garbage_reply_error() {
    let (mut transport, mut client) = setup();
    let mut fut                     = Box::pin(client.pow(8));
    assert!(poll_future_output(&mut fut).is_none()); // no reply
    transport.mock_peer_message_text("hello, nice to meet you");
    client.process_events();
    assert!(poll_future_output(&mut fut).is_none()); // no valid reply
    let internal_error = client.expect_handling_error();
    if let HandlingError::InvalidMessage(_) = internal_error {
    } else {
        panic!("Expected an error to be InvalidMessage");
    }
}

#[test]
fn test_disconnect_error() {
    let (mut transport, mut client) = setup();
    let mut fut                     = Box::pin(client.pow(8));
    assert!(poll_future_output(&mut fut).is_none()); // no reply
    transport.mock_connection_closed();
    assert!(poll_future_output(&mut fut).is_none()); // no reply
    client.process_events();
    let result = poll_future_output(&mut fut);
    let result = result.expect("result should be present");
    let result = result.expect_err("result should be a failure");
    if let RpcError::LostConnection = result {} else {
        panic!("Expected an error to be RemoteError");
    }
}

#[test]
fn test_sending_while_disconnected() {
    let (mut transport, mut client) = setup();
    transport.mock_connection_closed();
    let mut fut = Box::pin(client.pow(8));
    let result  = poll_future_output(&mut fut).unwrap();
    assert!(result.is_err())
}

fn test_notification(mock_notif:MockNotification) {
    let (mut transport, mut client) = setup();
    let message                     = Message::new(mock_notif.clone());
    assert!(client.try_get_notification().is_none());
    transport.mock_peer_message(message.clone());
    assert!(client.try_get_notification().is_none());
    client.process_events();
    let notification = client.try_get_notification();
    assert_eq!(notification.is_none(), false);
    assert_eq!(notification, Some(mock_notif));
}

#[test]
fn test_recognizing_notifications() {
    let meow_notification = MockNotification::Meow {text:"meow!".into()};
    test_notification(meow_notification);

    let bark_notification = MockNotification::Bark {text:"woof!".into()};
    test_notification(bark_notification);
}

#[test]
fn test_handling_invalid_notification() {
    let other_notification = r#"{
        "jsonrpc": "2.0",
        "method": "update",
        "params": [1,2,3,4,5]
    }"#;

    let (mut transport, mut client) = setup();
    assert!(client.try_get_notification().is_none());
    transport.mock_peer_message_text(other_notification);
    assert!(client.try_get_notification().is_none());
    client.process_events();
    let internal_error = client.expect_handling_error();
    if let HandlingError::InvalidNotification(_) = internal_error {}
    else {
        panic!("expected InvalidNotification error");
    }
}
