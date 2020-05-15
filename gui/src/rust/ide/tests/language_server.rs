//! Language Server integration tests.
//!
//! They are disabled by default, as there is no CI infrastructure to run them with Lanaguage
//! Server. To run tests manually, uncomment the `#[wasm_bindgen_test::wasm_bindgen_test(async)]`
//! attributes and use wasm-bindgen test.
//!
//! Note that running Lanugage Server is expected at `SERVER_ENDPOINT` (by default localhost:30616).
//! To run the language server manually run in the `enso` repository e.g.
//! ```
//! sbt "runner/run --server --root-id 6f7d58dd-8ee8-44cf-9ab7-9f0454033641 --path $HOME/ensotmp --rpc-port 30616"
//! ```

use ide::prelude::*;

use enso_protocol::language_server::*;
use enso_protocol::types::*;
use ide::transport::web::WebSocket;
use wasm_bindgen_test::wasm_bindgen_test_configure;

/// The endpoint at which the Language Server should be accepting WS connections.
const SERVER_ENDPOINT:&str = "ws://localhost:30616";

const PACKAGE_YAML:&str = r#"
maintainer: ''
license: ''
name: Test
version: ''
author: ''
"#;

const MAIN_CODE:&str = r#"
main =
    x = 6
    y = x.foo 5
    z = y + 5
    z

Number.foo = x ->
    y = this + 3
    z = y * x
    z



#### METADATA ####
[[{"index": {"value": 98}, "size": {"value": 5}}, "5fc0c11d-bd83-4ca3-b847-b8e362f7658c"],[{"index": {"value": 81}, "size": {"value": 8}}, "1cda3676-bd62-41f8-b6a1-a1e1b7c73d18"],[{"index": {"value": 42}, "size": {"value": 5}}, "899a11e5-4d2b-43dc-a867-2f2ef2d2ba62"],[{"index": {"value": 26}, "size": {"value": 7}}, "37f284d4-c593-4e65-a4be-4948fbd2adfb"],[{"index": {"value": 16}, "size": {"value": 1}}, "c553533e-a2b9-4305-9f12-b8fe7781f933"]]
[]"#;

const VISUALISATION_CODE:&str = r#"
    encode = x -> x.to_text

    incAndEncode = x -> here.encode x+1
"#;

wasm_bindgen_test_configure!(run_in_browser);

//#[wasm_bindgen_test::wasm_bindgen_test(async)]
#[allow(dead_code)]
async fn file_operations() {
    ensogl::system::web::set_stdout();

    let ws        = WebSocket::new_opened(SERVER_ENDPOINT).await;
    let ws        = ws.expect("Couldn't connect to WebSocket server.");
    let client    = Client::new(ws);
    let _executor = ide::setup_global_executor();

    executor::global::spawn(client.runner());

    let client_id = uuid::Uuid::new_v4();
    let session   = client.init_protocol_connection(&client_id).await;
    let session   = session.expect("Couldn't initialize session.");
    let root_id   = session.content_roots[0];

    let file      = Path{root_id,segments:vec!["src".into(),"Main.enso".into()]};
    let contents  = MAIN_CODE.to_string();
    let result    = client.write_file(&file,&contents).await;
    result.expect("Couldn't write main code file.");

    let visualisation_file = Path{root_id,segments:vec!["src".into(),"Visualisation.enso".into()]};
    let contents           = VISUALISATION_CODE.to_string();
    let response           = client.write_file(&visualisation_file,&contents).await;
    response.expect("Couldn't write visualisation file.");

    let package_file = Path{root_id,segments:vec!["package.yaml".into()]};
    let contents     = PACKAGE_YAML.to_string();
    let response     = client.write_file(&package_file,&contents).await;
    response.expect("Couldn't write yaml file.");

    let execution_context    = client.create_execution_context().await;
    let execution_context    = execution_context.expect("Couldn't create execution context.");
    let execution_context_id = match execution_context.can_modify.register_options {
        RegisterOptions::ExecutionContextId{context_id} => Some(context_id),
        _                                               => None
    }.expect("Couldn't get context ID.");

    let defined_on_type = "Main".to_string();
    let name            = "main".to_string();
    let method_pointer  = MethodPointer{file,defined_on_type,name};
    let positional_arguments_expressions = default();
    let this_argument_expression         = default();
    let explicit_call                    = ExplicitCall
        {method_pointer,positional_arguments_expressions,this_argument_expression};
    let stack_item = StackItem::ExplicitCall(explicit_call);
    let response   = client.push_to_execution_context(&execution_context_id,&stack_item).await;
    response.expect("Couldn't push execution context.");

    let response = client.pop_from_execution_context(&execution_context_id).await;
    response.expect("Couldn't pop execution context.");

    let visualisation_id     = uuid::Uuid::new_v4();
    let expression_id        = uuid::Uuid::parse_str("c553533e-a2b9-4305-9f12-b8fe7781f933");
    let expression_id        = expression_id.expect("Couldn't parse expression id.");
    let expression           = "x -> here.encode x".to_string();
    let visualisation_module = "Test.Visualisation".to_string();
    let visualisation_config = VisualisationConfiguration
    {execution_context_id,expression,visualisation_module};
    let response = client.attach_visualisation
        (&visualisation_id,&expression_id,&visualisation_config);
    response.await.expect("Couldn't attach visualisation.");

    let expression           = "x -> here.incAndEncode".to_string();
    let visualisation_module = "Test.Visualisation".to_string();
    let visualisation_config = VisualisationConfiguration
    {execution_context_id,expression,visualisation_module};
    let response = client.modify_visualisation(&visualisation_id,&visualisation_config).await;
    response.expect("Couldn't modify visualisation.");

    let response = client.detach_visualisation
        (&execution_context_id,&visualisation_id,&expression_id).await;
    response.expect("Couldn't detach visualisation.");

    let response = client.destroy_execution_context(&execution_context_id).await;
    response.expect("Couldn't destroy execution context.");

    let path      = Path{root_id, segments:vec!["foo".into()]};
    let name      = "text.txt".into();
    let object    = FileSystemObject::File {name,path};
    client.create_file(&object).await.expect("Couldn't create file.");

    let file_path = Path{root_id, segments:vec!["foo".into(),"text.txt".into()]};
    let contents  = "Hello world!".to_string();
    let result    = client.write_file(&file_path,&contents).await;
    result.expect("Couldn't write file.");

    let response = client.file_info(&file_path).await.expect("Couldn't get status.");
    assert_eq!(response.attributes.byte_size,12);
    assert_eq!(response.attributes.kind,object);

    let response = client.file_list(&Path{root_id,segments:vec!["foo".into()]}).await;
    let response = response.expect("Couldn't get file list");
    assert!(response.paths.iter().any(|file_system_object| object == *file_system_object));

    let read = client.read_file(&file_path).await.expect("Couldn't read contents.");
    assert_eq!(contents,read.contents);

    let new_path = Path{root_id,segments:vec!["foo".into(),"new_text.txt".into()]};
    client.copy_file(&file_path,&new_path).await.expect("Couldn't copy file");
    let read = client.read_file(&new_path).await.expect("Couldn't read contents.");
    assert_eq!(contents,read.contents);

    let move_path = Path{root_id,segments:vec!["foo".into(),"moved_text.txt".into()]};
    let file      = client.file_exists(&move_path).await;
    let file      = file.expect("Couldn't check if file exists.");
    if file.exists {
        client.delete_file(&move_path).await.expect("Couldn't delete file");
        let file = client.file_exists(&move_path).await;
        let file = file.expect("Couldn't check if file exists.");
        assert_eq!(file.exists,false);
    }

    client.move_file(&new_path,&move_path).await.expect("Couldn't move file");
    let read = client.read_file(&move_path).await.expect("Couldn't read contents");
    assert_eq!(contents,read.contents);

    let receives_tree_updates   = ReceivesTreeUpdates{path:move_path.clone()};
    let register_options        = RegisterOptions::ReceivesTreeUpdates(receives_tree_updates);
    let method                  = "text/canEdit".to_string();
    let capability_registration = CapabilityRegistration {method,register_options};
    let response = client.open_text_file(&move_path).await;
    let response = response.expect("Couldn't open text file.");
    assert_eq!(response.content, "Hello world!");
    assert_eq!(response.write_capability, Some(capability_registration));

    let start       = Position{line:0,character:5};
    let end         = Position{line:0,character:5};
    let range       = TextRange{start,end};
    let text        = ",".to_string();
    let text_edit   = TextEdit{range,text};
    let edits       = vec![text_edit];
    let old_version = Sha3_224::new(b"Hello world!");
    let new_version = Sha3_224::new(b"Hello, world!");
    let path        = move_path.clone();
    let edit        = FileEdit {path,edits,old_version,new_version:new_version.clone()};
    client.apply_text_file_edit(&edit).await.expect("Couldn't apply edit.");

    let future = client.save_text_file(&move_path,&new_version).await;
    future.expect("Couldn't save file.");

    client.close_text_file(&move_path).await.expect("Couldn't close text file.");

    let read = client.read_file(&move_path).await.expect("Couldn't read contents.");
    assert_eq!("Hello, world!".to_string(),read.contents);
}

//#[wasm_bindgen_test::wasm_bindgen_test(async)]
#[allow(dead_code)]
async fn file_events() {
    ensogl::system::web::set_stdout();
    let ws         = WebSocket::new_opened(SERVER_ENDPOINT).await;
    let ws         = ws.expect("Couldn't connect to WebSocket server.");
    let client     = Client::new(ws);
    let mut stream = client.events();

    let _executor = ide::setup_global_executor();

    executor::global::spawn(client.runner());

    let client_id = uuid::Uuid::default();
    let session   = client.init_protocol_connection(&client_id).await;
    let session   = session.expect("Couldn't initialize session.");
    let root_id   = session.content_roots[0];

    let path      = Path{root_id,segments:vec!["test.txt".into()]};
    let file      = client.file_exists(&path).await;
    let file      = file.expect("Couldn't check if file exists.");
    if file.exists {
        client.delete_file(&path).await.expect("Couldn't delete file");
        let file = client.file_exists(&path).await;
        let file = file.expect("Couldn't check if file exists.");
        assert_eq!(file.exists,false);
    }

    let path       = Path{root_id, segments:vec![]};
    let receives_tree_updates = ReceivesTreeUpdates{path};
    let options    = RegisterOptions::ReceivesTreeUpdates(receives_tree_updates);
    let capability = client.acquire_capability(&"receivesTreeUpdates".to_string(),&options).await;
    capability.expect("Couldn't acquire receivesTreeUpdates capability.");

    let path      = Path{root_id, segments:vec![]};
    let name      = "test.txt".into();
    let object    = FileSystemObject::File {name,path:path.clone()};
    client.create_file(&object).await.expect("Couldn't create file.");

    let path         = Path{root_id,segments:vec!["test.txt".into()]};
    let kind         = FileEventKind::Added;
    let event        = FileEvent {path,kind};
    let notification = Notification::FileEvent {event};

    let event = stream.next().await.expect("Couldn't get any notification.");
    if let Event::Notification(incoming_notification) = event {
        assert_eq!(incoming_notification,notification);
    } else {
        panic!("Incoming event isn't a notification.");
    }
}
