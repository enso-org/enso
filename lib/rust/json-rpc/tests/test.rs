// === Non-Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use json_rpc::prelude::*;
use json_rpc::*;

use enso_web::Duration;
use futures::task::LocalSpawnExt;
use futures::FutureExt;
use futures::Stream;
use json_rpc::api::RemoteMethodCall;
use json_rpc::api::Result;
use json_rpc::error::HandlingError;
use json_rpc::error::RpcError;
use json_rpc::messages::Id;
use json_rpc::messages::Message;
use json_rpc::messages::Version;
use json_rpc::test_util::transport::mock::MockTransport;
use serde::Deserialize;
use serde::Serialize;
use std::future::Future;
use std::pin::Pin;
use std::thread::sleep;



type MockEvent = json_rpc::handler::Event<MockNotification>;



// =====================
// === Mock Protocol ===
// =====================


// === Remote Method ===

fn pow_impl(msg: MockRequestMessage) -> MockResponseMessage {
    let ret = MockResponse { result: msg.i * msg.i };
    Message::new_success(msg.id, ret)
}


// === Protocol Data ===

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct MockRequest {
    i: i64,
}

impl RemoteMethodCall for MockRequest {
    const NAME: &'static str = "pow";
    type Returned = MockResponse;
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct MockResponse {
    result: i64,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
#[serde(tag = "method", content = "params")]
pub enum MockNotification {
    Meow { text: String },
    Bark { text: String },
}


// === Helper Aliases ===

type MockRequestMessage = messages::RequestMessage<MockRequest>;

type MockResponseMessage = messages::ResponseMessage<MockResponse>;



// ===================
// === Mock Client ===
// ===================

pub struct Client {
    pub handler:       Handler<MockNotification>,
    pub events_stream: Pin<Box<dyn Stream<Item = MockEvent>>>,
}

impl Client {
    pub fn new(transport: impl Transport + 'static) -> Client {
        let handler = Handler::new(transport);
        let events_stream = Box::pin(handler.handler_event_stream());
        Client { handler, events_stream }
    }

    pub fn pow(&mut self, i: i64) -> impl Future<Output = Result<i64>> {
        let input = MockRequest { i };
        self.handler.open_request(input).map(|result| result.map(|r| r.result))
    }

    pub fn events_processor(&mut self) -> impl Future<Output = ()> {
        self.handler.runner()
    }

    pub fn expect_no_notification_yet(&mut self) {
        self.events_stream.expect_pending()
    }

    pub fn expect_notification(&mut self) -> MockNotification {
        let event = self.events_stream.expect_next();
        if let MockEvent::Notification(notification) = event {
            notification
        } else {
            panic!("Expected a notification, got different kind of event: {:?}", event)
        }
    }

    pub fn expect_handling_error(&mut self) -> HandlingError {
        let event = self.events_stream.expect_next();
        if let json_rpc::handler::Event::Error(err) = event {
            err
        } else {
            panic!("Expected an error event, got different kind of event: {:?}", event)
        }
    }
}



// ============
// === Test ===
// ============

struct Fixture {
    transport: MockTransport,
    client:    Client,
    pool:      futures::executor::LocalPool,
}

impl Fixture {
    pub fn new() -> Fixture {
        let transport = MockTransport::new();
        let mut client = Client::new(transport.clone());
        let pool = futures::executor::LocalPool::new();
        let fut = client.events_processor();
        pool.spawner().spawn_local(fut).unwrap();
        Fixture { transport, client, pool }
    }
}

#[test]
fn test_success_call() {
    let mut fixture = Fixture::new();
    let call_input = 8;
    let mut fut = Box::pin(fixture.client.pow(8));
    let expected_first_request_id = Id(0);

    // validate request sent
    let req_msg = fixture.transport.expect_json_message::<MockRequestMessage>();
    assert_eq!(req_msg.id, expected_first_request_id);
    assert_eq!(req_msg.method, MockRequest::NAME);
    assert_eq!(req_msg.i, call_input);
    assert_eq!(req_msg.jsonrpc, Version::V2);

    fut.expect_pending(); // no reply

    // let's reply
    let reply = pow_impl(req_msg);
    fixture.transport.mock_peer_json_message(reply);

    // before yielding control message should be in buffer and futures should not complete
    fut.expect_pending(); // not ticked

    // yield control to executor
    fixture.pool.run_until_stalled();

    let result = fut.expect_ok();
    assert_eq!(result, 8 * 8);
}

#[test]
fn test_error_call() {
    let mut fixture = Fixture::new();
    let mut fut = Box::pin(fixture.client.pow(8));
    fut.expect_pending(); // no reply

    // reply with error
    let req_msg = fixture.transport.expect_json_message::<MockRequestMessage>();
    let error_code = 5;
    let error_description = "wrong!";
    let error_data = None;
    let error_msg: MockResponseMessage =
        Message::new_error(req_msg.id, error_code, error_description.into(), error_data.clone());
    fixture.transport.mock_peer_json_message(error_msg);

    // receive error
    fixture.pool.run_until_stalled();

    let result = fut.expect_err();
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
    let mut fixture = Fixture::new();
    let mut fut = Box::pin(fixture.client.pow(8));
    fut.expect_pending(); // no reply
    fixture.transport.mock_peer_text_message("hello, nice to meet you");

    fixture.pool.run_until_stalled();

    fut.expect_pending(); // no valid reply
    let internal_error = fixture.client.expect_handling_error();
    if let HandlingError::InvalidMessage(_) = internal_error {
    } else {
        panic!("Expected an error to be InvalidMessage");
    }
}

#[test]
fn test_timeout_error() {
    let mut fixture = Fixture::new();
    let mut fut = Box::pin(fixture.client.pow(8));

    fut.expect_pending(); // no reply
    fixture.pool.run_until_stalled();
    sleep(fixture.client.handler.timeout());
    sleep(Duration::from_secs(1)); // sleep a little longer than the timeout

    if let RpcError::TimeoutError { .. } = fut.expect_err() {
    } else {
        panic!("Expected an error to be TimeoutError");
    }
}

#[test]
fn test_disconnect_error() {
    let mut fixture = Fixture::new();
    let mut fut = Box::pin(fixture.client.pow(8));
    fut.expect_pending(); // no reply nor relevant event
    fixture.transport.mock_connection_closed();
    fut.expect_pending(); // closing event not yet processed

    fixture.pool.run_until_stalled();

    let result = fut.expect_err();
    if let RpcError::LostConnection = result {
    } else {
        panic!("Expected an error to be RemoteError");
    }
}

#[test]
fn test_sending_while_disconnected() {
    let mut fixture = Fixture::new();
    fixture.transport.mock_connection_closed();
    let mut fut = Box::pin(fixture.client.pow(8));
    fut.expect_err();
}

fn test_notification(mock_notif: MockNotification) {
    let mut fixture = Fixture::new();
    let message = Message::new(mock_notif.clone());
    fixture.client.events_stream.expect_pending();
    fixture.transport.mock_peer_json_message(message);
    fixture.client.events_stream.expect_pending();
    fixture.pool.run_until_stalled();

    let notification = fixture.client.expect_notification();
    assert_eq!(notification, mock_notif);
}

#[test]
fn test_recognizing_notifications() {
    let meow_notification = MockNotification::Meow { text: "meow!".into() };
    test_notification(meow_notification);

    let bark_notification = MockNotification::Bark { text: "woof!".into() };
    test_notification(bark_notification);
}

#[test]
fn test_handling_invalid_notification() {
    let other_notification = r#"{
        "jsonrpc": "2.0",
        "method": "update",
        "params": [1,2,3,4,5]
    }"#;

    let mut fixture = Fixture::new();
    fixture.client.expect_no_notification_yet();
    fixture.transport.mock_peer_text_message(other_notification);
    fixture.client.expect_no_notification_yet();

    fixture.pool.run_until_stalled();

    let internal_error = fixture.client.expect_handling_error();
    if let HandlingError::InvalidNotification(_) = internal_error {
    } else {
        panic!("expected InvalidNotification error");
    }
}
