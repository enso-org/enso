use super::*;

use futures::task::LocalSpawnExt;
use json_rpc::messages::Message;
use json_rpc::messages::RequestMessage;
use json_rpc::test_util::transport::mock::MockTransport;
use serde_json::json;
use serde_json::Value;
use std::future::Future;



// ===============
// === Fixture ===
// ===============

struct Fixture {
    transport: MockTransport,
    client:    Client,
    executor:  futures::executor::LocalPool,
}

fn setup_language_server() -> Fixture {
    let transport = MockTransport::new();
    let client = Client::new(transport.clone());
    let executor = futures::executor::LocalPool::new();
    executor.spawner().spawn_local(client.runner()).unwrap();
    Fixture { transport, client, executor }
}



// =============
// === Tests ===
// =============

#[test]
fn test_file_event_notification() {
    let mut fixture = setup_language_server();
    let mut events = Box::pin(fixture.client.events());
    events.expect_pending();

    let root_id = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
    let root_id = root_id.expect("Couldn't parse uuid.");
    let expected_event = FileEvent {
        path: Path { root_id, segments: vec!["Main.txt".into()] },
        kind: FileEventKind::Modified,
    };
    let notification_text = r#"{
            "jsonrpc": "2.0",
            "method": "file/event",
            "params": {
                "path" : {
                    "rootId"   : "00000000-0000-0000-0000-000000000000",
                    "segments" : ["Main.txt"]
                },
                "kind" : "Modified"
            }
        }"#;
    fixture.transport.mock_peer_text_message(notification_text);
    events.expect_pending();

    fixture.executor.run_until_stalled();

    if let Event::Notification(n) = events.expect_next() {
        assert_eq!(n, Notification::FileEvent(expected_event));
    } else {
        panic!("expected notification event");
    }
}

/// This function tests making a request using language server. It
/// * creates FM client and uses `make_request` to make a request,
/// * checks that request is made for `expected_method`,
/// * checks that request input is `expected_input`,
/// * mocks receiving a response from server with `result` and
/// * checks that FM-returned Future yields `expected_output`.
fn test_request<Fun, Fut, T>(
    make_request: Fun,
    expected_method: &str,
    expected_input: Value,
    result: Value,
    expected_output: T,
) where
    Fun: FnOnce(&mut Client) -> Fut,
    Fut: Future<Output = Result<T>>,
    T: Debug + PartialEq,
{
    let mut fixture = setup_language_server();
    let mut request_future = Box::pin(make_request(&mut fixture.client));

    let request = fixture.transport.expect_json_message::<RequestMessage<Value>>();
    assert_eq!(request.method, expected_method);
    assert_eq!(request.params, expected_input);

    let response = Message::new_success(request.id, result);
    fixture.transport.mock_peer_json_message(response);
    fixture.executor.run_until_stalled();
    assert_eq!(request_future.expect_ok(), expected_output);
}

#[test]
fn test_file_requests() {
    let root_id = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
    let root_id = root_id.expect("Couldn't parse uuid.");
    let main = Path { root_id, segments: vec!["Main.txt".into()] };
    let target = Path { root_id, segments: vec!["Target.txt".into()] };
    let path_main = json!({"path" : {
            "rootId"   : "00000000-0000-0000-0000-000000000000",
            "segments" : ["Main.txt"]
        }
    });
    let from_main_to_target = json!({
        "from" : {
            "rootId"   : "00000000-0000-0000-0000-000000000000",
            "segments" : ["Main.txt"]
        },
        "to" : {
            "rootId"   : "00000000-0000-0000-0000-000000000000",
            "segments" : ["Target.txt"]
        }
    });
    let file_exists_json = json!({"exists":true});
    let unit_json = json!(null);

    test_request(
        |client| client.copy_file(&main, &target),
        "file/copy",
        from_main_to_target.clone(),
        unit_json.clone(),
        (),
    );
    test_request(
        |client| client.delete_file(&main),
        "file/delete",
        path_main.clone(),
        unit_json.clone(),
        (),
    );
    test_request(
        |client| client.file_exists(&main),
        "file/exists",
        path_main.clone(),
        file_exists_json,
        response::FileExists { exists: true },
    );

    let list_response_json = json!({
        "paths" : [
            {
                "type" : "File",
                "name" : "foo.txt",
                "path" : {
                    "rootId"   : "00000000-0000-0000-0000-000000000000",
                    "segments" : []
                }
            },
            {
                "type" : "File",
                "name" : "bar.txt",
                "path" : {
                    "rootId"   : "00000000-0000-0000-0000-000000000000",
                    "segments" : []
                }
            }
        ]
    });
    let list_response_value = response::FileList {
        paths: vec![
            FileSystemObject::File {
                name: "foo.txt".into(),
                path: Path { root_id, segments: default() },
            },
            FileSystemObject::File {
                name: "bar.txt".into(),
                path: Path { root_id, segments: default() },
            },
        ],
    };
    test_request(
        |client| client.file_list(&main),
        "file/list",
        path_main.clone(),
        list_response_json,
        list_response_value,
    );
    test_request(
        |client| client.move_file(&main, &target),
        "file/move",
        from_main_to_target,
        unit_json.clone(),
        (),
    );

    let read_response_json = json!({"contents":"Hello world!"});
    let read_response = response::Read { contents: "Hello world!".into() };
    test_request(
        |client| client.read_file(&main),
        "file/read",
        path_main.clone(),
        read_response_json,
        read_response,
    );

    let parse_rfc3339 = |s| chrono::DateTime::parse_from_rfc3339(s).unwrap();
    let file_system_object = FileSystemObject::File {
        name: "test.txt".into(),
        path: Path { root_id, segments: default() },
    };
    let file_system_object_json = json!({
        "type" : "File",
        "name" : "test.txt",
        "path" : {
            "rootId"   : "00000000-0000-0000-0000-000000000000",
            "segments" : []
        }
    });
    let expected_attributes = response::FileInfo {
        attributes: FileAttributes {
            creation_time:      parse_rfc3339("2020-01-07T21:25:26Z"),
            last_access_time:   parse_rfc3339("2020-01-21T22:16:51.123994500+00:00"),
            last_modified_time: parse_rfc3339("2020-01-07T21:25:26Z"),
            kind:               file_system_object.clone(),
            byte_size:          125125,
        },
    };
    let sample_attributes_json = json!({ "attributes" : {
        "creationTime"     : "2020-01-07T21:25:26Z",
        "lastAccessTime"   : "2020-01-21T22:16:51.123994500+00:00",
        "lastModifiedTime" : "2020-01-07T21:25:26Z",
        "kind"             : file_system_object_json,
        "byteSize" : 125125
    }});
    test_request(
        |client| client.file_info(&main),
        "file/info",
        path_main,
        sample_attributes_json,
        expected_attributes,
    );
    let create_file_json = json!({ "object": file_system_object_json });
    test_request(
        |client| client.create_file(&file_system_object),
        "file/create",
        create_file_json,
        unit_json.clone(),
        (),
    );
    test_request(
        |client| client.write_file(&main, &"Hello world!".to_string()),
        "file/write",
        json!({
            "path" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : ["Main.txt"]
            },
            "contents" : "Hello world!"
        }),
        unit_json,
        (),
    );
}

#[test]
fn test_protocol_connection() {
    let init_protocol_connection_response = response::InitProtocolConnection {
        content_roots: vec![ContentRoot::Project { id: default() }],
    };
    test_request(
        |client| client.init_protocol_connection(&uuid::Uuid::default()),
        "session/initProtocolConnection",
        json!({
            "clientId" : "00000000-0000-0000-0000-000000000000"
        }),
        json!({
            "contentRoots" : [{
                "id"   : "00000000-0000-0000-0000-000000000000",
                "type" : "Project",
            }]
        }),
        init_protocol_connection_response,
    );
}

#[test]
fn test_acquire_capability() {
    let root_id = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
    let root_id = root_id.expect("Couldn't parse uuid.");
    let unit_json = json!(null);

    let path = Path { root_id, segments: default() };
    let method = "file/receivesTreeUpdates".to_string();
    let register_options = RegisterOptions::Path { path };
    test_request(
        |client| client.acquire_capability(&method, &register_options),
        "capability/acquire",
        json!({
            "method"          : "file/receivesTreeUpdates",
            "registerOptions" : {
                "path" : {
                    "rootId"   : "00000000-0000-0000-0000-000000000000",
                    "segments" : []
                }
            }
        }),
        unit_json,
        (),
    );
}


#[test]
fn test_computed_value_update() {
    use crate::language_server::Notification;
    use json_rpc::Event;

    let context_id = Uuid::parse_str("b36dea0b-b75a-40cf-aaad-5fcdf29a0573").unwrap();
    let id = Uuid::parse_str("d4b540c0-3ef5-487c-9453-df9d3efd351c").unwrap();
    let typename = "Number";
    let notification = json!({
        "jsonrpc" : "2.0",
        "method"  : "executionContext/expressionUpdates",
        "params"  :  {
            "contextId" : context_id,
            "updates"   : [{
                "expressionId"  : id,
                "type"          : typename,
                "methodPointer" : null,
                "profilingInfo" : [],
                "fromCache"     : true,
                "payload"       : ExpressionUpdatePayload::Value,
            }]
        }
    });

    let mut fixture = setup_language_server();
    let mut stream = fixture.client.events();
    stream.expect_pending();

    fixture.transport.mock_peer_json_message(notification);
    fixture.executor.run_until_stalled();

    let notification = stream.expect_next();
    match notification {
        Event::Notification(Notification::ExpressionUpdates(expression_updates)) => {
            assert_eq!(expression_updates.context_id, context_id);
            let update = &expression_updates.updates.first().unwrap();
            assert_eq!(update.expression_id, id);
            assert_eq!(update.typename.as_ref().map(|ty| ty.as_str()), Some(typename));
            assert!(update.method_pointer.is_none());
            assert!(update.from_cache);
            assert!(matches!(update.payload, ExpressionUpdatePayload::Value))
        }
        _ => panic!("Expected Notification::ExpressionUpdates"),
    }
}

#[test]
fn test_execution_context() {
    let root_id = uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000000");
    let root_id = root_id.expect("Couldn't parse uuid.");
    let main = Path { root_id, segments: vec!["Main.txt".into()] };
    let unit_json = json!(null);

    let context_id = uuid::Uuid::default();
    let method = "executionContext/canModify".to_string();
    let register_options = RegisterOptions::ExecutionContextId { context_id };
    let can_modify = CapabilityRegistration { method, register_options };
    let register_options = RegisterOptions::ExecutionContextId { context_id };
    let method = "executionContext/receivesUpdates".to_string();
    let receives_updates = CapabilityRegistration { method, register_options };
    let create_execution_context_response =
        response::CreateExecutionContext { context_id, can_modify, receives_updates };
    test_request(
        |client| client.create_execution_context(),
        "executionContext/create",
        json!({}),
        json!({
            "contextId" : "00000000-0000-0000-0000-000000000000",
            "canModify" : {
                "method"          : "executionContext/canModify",
                "registerOptions" : {
                    "contextId" : "00000000-0000-0000-0000-000000000000"
                }
            },
            "receivesUpdates" : {
                "method"          : "executionContext/receivesUpdates",
                "registerOptions" : {
                    "contextId" : "00000000-0000-0000-0000-000000000000"
                }
            }
        }),
        create_execution_context_response,
    );
    test_request(
        |client| client.destroy_execution_context(&context_id),
        "executionContext/destroy",
        json!({"contextId":"00000000-0000-0000-0000-000000000000"}),
        unit_json.clone(),
        (),
    );
    let expression_id = uuid::Uuid::default();
    let local_call = LocalCall { expression_id };
    let stack_item = StackItem::LocalCall(local_call);
    test_request(
        |client| client.push_to_execution_context(&context_id, &stack_item),
        "executionContext/push",
        json!({
            "contextId" : "00000000-0000-0000-0000-000000000000",
            "stackItem" : {
                "type"         : "LocalCall",
                "expressionId" : "00000000-0000-0000-0000-000000000000"
            }
        }),
        unit_json.clone(),
        (),
    );
    test_request(
        |client| client.pop_from_execution_context(&context_id),
        "executionContext/pop",
        json!({"contextId":"00000000-0000-0000-0000-000000000000"}),
        unit_json.clone(),
        (),
    );
    let visualisation_id = uuid::Uuid::default();
    let expression_id = uuid::Uuid::default();
    let expression = "1 + 1".to_string();
    let visualisation_module = "[Foo.Bar.Baz]".to_string();
    let visualisation_config = VisualisationConfiguration {
        execution_context_id: context_id,
        expression,
        visualisation_module,
    };
    test_request(
        |client| {
            client.attach_visualisation(&visualisation_id, &expression_id, &visualisation_config)
        },
        "executionContext/attachVisualisation",
        json!({
            "visualisationId"     : "00000000-0000-0000-0000-000000000000",
            "expressionId"        : "00000000-0000-0000-0000-000000000000",
            "visualisationConfig" : {
                "executionContextId"  : "00000000-0000-0000-0000-000000000000",
                "visualisationModule" : "[Foo.Bar.Baz]",
                "expression"          : "1 + 1"
            }
        }),
        unit_json.clone(),
        (),
    );
    test_request(
        |client| client.detach_visualisation(&context_id, &visualisation_id, &expression_id),
        "executionContext/detachVisualisation",
        json!({
            "contextId"       : "00000000-0000-0000-0000-000000000000",
            "visualisationId" : "00000000-0000-0000-0000-000000000000",
            "expressionId"    : "00000000-0000-0000-0000-000000000000"
        }),
        unit_json.clone(),
        (),
    );
    let expression = "1 + 1".to_string();
    let visualisation_module = "[Foo.Bar.Baz]".to_string();
    let visualisation_config = VisualisationConfiguration {
        execution_context_id: context_id,
        expression,
        visualisation_module,
    };
    test_request(
        |client| client.modify_visualisation(&visualisation_id, &visualisation_config),
        "executionContext/modifyVisualisation",
        json!({
            "visualisationId"     : "00000000-0000-0000-0000-000000000000",
            "visualisationConfig" : {
                "executionContextId"  : "00000000-0000-0000-0000-000000000000",
                "visualisationModule" : "[Foo.Bar.Baz]",
                "expression"          : "1 + 1"
            }
        }),
        unit_json.clone(),
        (),
    );
    let content = b"Hello World!";
    let current_version = Sha3_224::new(content);
    let content = String::from_utf8_lossy(content).to_string();
    let method = "text/canEdit".to_string();
    let register_options = RegisterOptions::Path { path: main.clone() };
    let write_capability = Some(CapabilityRegistration { method, register_options });
    let open_text_file_response = response::OpenTextFile {
        content,
        current_version: current_version.clone(),
        write_capability,
    };
    test_request(
        |client| client.open_text_file(&main),
        "text/openFile",
        json!({
            "path" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : ["Main.txt"]
            }
        }),
        json!({
            "writeCapability" : {
                "method"         : "text/canEdit",
                "registerOptions": {
                    "path" : {
                        "rootId"   : "00000000-0000-0000-0000-000000000000",
                        "segments" : ["Main.txt"]
                    }
                }
            },
            "content"        : "Hello World!",
            "currentVersion" : "716596afadfa17cd1cb35133829a02b03e4eed398ce029ce78a2161d"
        }),
        open_text_file_response,
    );
    let start = Position { line: 0, character: 5 };
    let end = Position { line: 0, character: 5 };
    let range = TextRange { start, end };
    let text = ",".to_string();
    let text_edit = TextEdit { range, text };
    let edits = vec![text_edit];
    let old_version = Sha3_224::new(b"Hello world!");
    let new_version = Sha3_224::new(b"Hello, world!");
    let path = main.clone();
    let edit = FileEdit { path, edits, old_version, new_version };
    test_request(
        |client| client.apply_text_file_edit(&edit),
        "text/applyEdit",
        json!({
            "edit" : {
                "path" : {
                    "rootId"   : "00000000-0000-0000-0000-000000000000",
                    "segments" : ["Main.txt"]
                },
                "edits" : [
                    {
                        "range" : {
                            "start" : {
                                "line"      : 0,
                                "character" : 5
                            },
                            "end" : {
                                "line"      : 0,
                                "character" : 5
                            }
                        },
                        "text" : ","
                    }
                ],
                "oldVersion" : "d3ee9b1ba1990fecfd794d2f30e0207aaa7be5d37d463073096d86f8",
                "newVersion" : "6a33e22f20f16642697e8bd549ff7b759252ad56c05a1b0acc31dc69"
            }
        }),
        unit_json.clone(),
        (),
    );
    test_request(
        |client| client.save_text_file(&main, &current_version),
        "text/save",
        json!({
            "path" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : ["Main.txt"]
            },
            "currentVersion" : "716596afadfa17cd1cb35133829a02b03e4eed398ce029ce78a2161d"
        }),
        unit_json.clone(),
        (),
    );
    test_request(
        |client| client.close_text_file(&main),
        "text/closeFile",
        json!({
            "path" : {
                "rootId"   : "00000000-0000-0000-0000-000000000000",
                "segments" : ["Main.txt"]
            }
        }),
        unit_json,
        (),
    );
}
