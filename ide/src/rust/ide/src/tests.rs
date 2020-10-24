use super::prelude::*;

use crate::controller::graph::NodeTrees;
use crate::transport::test_utils::TestWithMockedTransport;
use crate::ide::IdeInitializer;

use enso_protocol::project_manager;
use json_rpc::expect_call;
use json_rpc::test_util::transport::mock::MockTransport;
use serde_json::json;
use span_tree::node::InsertionPointType;
use span_tree::node;
use wasm_bindgen_test::wasm_bindgen_test_configure;
use wasm_bindgen_test::wasm_bindgen_test;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test(async)]
async fn failure_to_open_most_recent_project_is_reported() {
    let transport   = MockTransport::new();
    let mut fixture = TestWithMockedTransport::set_up(&transport);
    fixture.run_test(async move {
        let logger           = default();
        let client           = IdeInitializer::setup_project_manager(transport);
        let name             = crate::constants::DEFAULT_PROJECT_NAME;
        let project_metadata = IdeInitializer::get_most_recent_project_or_create_new
            (&logger,&client,name).await.expect("Couldn't get most recent or create new project.");
        let client  = Rc::new(client);
        let project = IdeInitializer::open_project(&logger,client,project_metadata);
        let project = project.await;
        project.expect_err("error should have been reported");
    });
    fixture.when_stalled_send_response(json!({
            "projects": [{
                "name"       : "Project",
                "id"         : "4b871393-eef2-4970-8765-4f3c1ea83d09",
                "lastOpened" : "2020-05-08T11:04:07.28738Z"
            }]
        }));
    fixture.when_stalled_send_error(1,"Service error");
}

#[wasm_bindgen_test(async)]
async fn get_project_or_create_new() {
    let logger      = default();
    let mock_client = project_manager::MockClient::default();

    let name             = project_manager::ProjectName::new("TestProject");
    let id               = uuid::Uuid::new_v4();
    let last_opened      = default();
    let expected_project = project_manager::ProjectMetadata{name,id,last_opened};
    let projects         = vec![expected_project.clone()];
    let project_lists    = project_manager::response::ProjectList{projects};
    let count            = None;
    expect_call!(mock_client.list_projects(count) => Ok(project_lists));

    let project = IdeInitializer::get_project_or_create_new(&logger, &mock_client, "TestProject");
    let project = project.await;
    assert_eq!(expected_project, project.expect("Couldn't get project."))
}

#[wasm_bindgen_test(async)]
async fn get_most_recent_project_or_create_new() {
    let logger      = default();
    let mock_client = project_manager::MockClient::default();

    let name             = project_manager::ProjectName::new("TestProject");
    let id               = uuid::Uuid::new_v4();
    let last_opened      = default();
    let expected_project = project_manager::ProjectMetadata{name,id,last_opened};
    let projects         = vec![expected_project.clone()];
    let project_lists    = project_manager::response::ProjectList{projects};
    let count            = Some(1);
    expect_call!(mock_client.list_projects(count) => Ok(project_lists));

    let project = IdeInitializer::get_most_recent_project_or_create_new
        (&logger,&mock_client,"TestProject");
    let project = project.await;
    assert_eq!(expected_project, project.expect("Couldn't get project."))
}

// x
#[wasm_bindgen_test]
fn span_tree_args() {
    use crate::test::mock::*;
    use span_tree::Node;

    let data    = Unified::new();
    let fixture = data.fixture_customize(|_,json_client| {
        // Additional completion request happens after picking completion.
        controller::searcher::test::expect_completion(json_client,&[1]);
    });
    let Fixture{graph,executed_graph,searcher,suggestion_db,..} = &fixture;
    let entry = suggestion_db.lookup(1).unwrap();
    searcher.pick_completion(entry.clone_ref()).unwrap();
    let id = searcher.commit_node().unwrap();

    let get_node   = || graph.node(id).unwrap();
    let get_inputs = || NodeTrees::new(&get_node().info,executed_graph).unwrap().inputs;
    let get_param  = |n| get_inputs().root_ref().leaf_iter().nth(n).and_then(|node| {
        node.argument_info()
    });
    let expected_this_param = model::suggestion_database::to_span_tree_param(&entry.arguments[0]);
    let expected_arg1_param = model::suggestion_database::to_span_tree_param(&entry.arguments[1]);


    // === Method notation, without prefix application ===
    assert_eq!(get_node().info.expression().repr(), "Base.foo");
    match get_inputs().root.children.as_slice() {
        // The tree here should have two nodes under root - one with given Ast and second for
        // an additional prefix application argument.
        [_,second] => {
            let Node{children,kind,..} = &second.node;
            let expected_kind = node::Kind::insertion_point()
                .with_kind(InsertionPointType::ExpectedArgument(0));
            assert!(children.is_empty());
            // assert_eq!(kind,&node::Kind::from(expected_kind));
            assert_eq!(kind.argument_info(),Some(expected_arg1_param.clone()));
        }
        _ => panic!("Expected only two children in the span tree's root"),
    };


    // === Method notation, with prefix application ===
    graph.set_expression(id,"Base.foo 50").unwrap();
    match get_inputs().root.children.as_slice() {
        // The tree here should have two nodes under root - one with given Ast and second for
        // an additional prefix application argument.
        [_,second] => {
            let Node{children,kind,..} = &second.node;
            assert!(children.is_empty());
            // assert_eq!(kind,&node::Kind::from(node::Kind::argument()));
            assert_eq!(kind.argument_info(),Some(expected_arg1_param.clone()));
        }
        _ => panic!("Expected only two children in the span tree's root"),
    };


    // === Function notation, without prefix application ===
    assert_eq!(entry.name,"foo");
    graph.set_expression(id,"foo").unwrap();
    assert_eq!(get_param(1).as_ref(),Some(&expected_this_param));
    assert_eq!(get_param(2).as_ref(),Some(&expected_arg1_param));
    assert_eq!(get_param(3).as_ref(),None);


    // === Function notation, with prefix application ===
    graph.set_expression(id,"foo Base").unwrap();
    assert_eq!(get_param(1).as_ref(),Some(&expected_this_param));
    assert_eq!(get_param(2).as_ref(),Some(&expected_arg1_param));
    assert_eq!(get_param(3).as_ref(),None);


    // === Changed function name, should not have known parameters ===
    graph.set_expression(id,"bar").unwrap();
    assert_eq!(get_param(1),None);
    assert_eq!(get_param(2),None);
    assert_eq!(get_param(3),None);

    graph.set_expression(id,"bar Base").unwrap();
    assert_eq!(get_param(1),Some(default()));
    assert_eq!(get_param(2),Some(span_tree::ArgumentInfo::this(None)));
    assert_eq!(get_param(3),Some(default())); // FIXME: is this correct?

    graph.set_expression(id,"Base.bar").unwrap();
    assert_eq!(get_param(1),Some(span_tree::ArgumentInfo::this(None)));
    assert_eq!(get_param(2),Some(default()));
    assert_eq!(get_param(3),None);

    // === Oversaturated call ===
    graph.set_expression(id,"foo Base 10 20 30").unwrap();
    assert_eq!(get_param(1).as_ref(),Some(&expected_this_param));
    assert_eq!(get_param(2).as_ref(),Some(&expected_arg1_param));
    assert_eq!(get_param(3).as_ref(),Some(&default()));
}
