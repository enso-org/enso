use super::prelude::*;

use crate::controller::graph::NodeTrees;
use crate::ide;
use crate::transport::test_utils::TestWithMockedTransport;

use engine_protocol::project_manager;
use engine_protocol::project_manager::ProjectName;
use json_rpc::test_util::transport::mock::MockTransport;
use serde_json::json;
use span_tree::node;
use span_tree::node::InsertionPointType;
use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;



wasm_bindgen_test_configure!(run_in_browser);



// =============================================
// === JSON Rpc Transport in IDE Initializer ===
// =============================================

#[wasm_bindgen_test]
fn failure_to_open_project_is_reported() {
    let transport = MockTransport::new();
    let mut fixture = TestWithMockedTransport::set_up(&transport);
    fixture.run_test(async move {
        let project_manager = Rc::new(project_manager::Client::new(transport));
        executor::global::spawn(project_manager.runner());
        let name = ProjectName::new_unchecked(crate::constants::DEFAULT_PROJECT_NAME.to_owned());
        let initializer = ide::initializer::WithProjectManager::new(project_manager, name);
        let result = initializer.initialize_project_model().await;
        result.expect_err("Error should have been reported.");
    });
    fixture.when_stalled_send_response(json!({
        "projects": [{
            "name"       : crate::constants::DEFAULT_PROJECT_NAME,
            "id"         : "4b871393-eef2-4970-8765-4f3c1ea83d09",
            "lastOpened" : "2020-05-08T11:04:07.28738Z"
        }]
    }));
    fixture.when_stalled_send_error(1, "Service error");
    // FIXME [mwu]
    //  For some reasons, the line below is needed for the test to succeed. This is unexpected, as
    //  fixture destructor calls `run_until_stalled` as well, and as it is supposed to perform all
    //  available work, there should be no difference between calling it once and multiple times.
    //  This looks like an error, and happens only for wasm targets.
    //  This did not happen on the 2019 nightly.
    //  Further investigation needed, tracked by https://github.com/enso-org/ide/issues/1575
    fixture.run_until_stalled();
}


// ====================================
// === SpanTree in Graph Controller ===
// ====================================

#[wasm_bindgen_test]
fn span_tree_args() {
    use crate::test::mock::*;
    use span_tree::Node;

    let data = Unified::new();
    let fixture = data.fixture_customize(|_, json_client, _| {
        // The searcher requests for completion when we clear the input.
        controller::searcher::test::expect_completion(json_client, &[1]);
        // Additional completion request happens after picking completion.
        controller::searcher::test::expect_completion(json_client, &[1]);
    });
    let Fixture { graph, executed_graph, searcher, suggestion_db, .. } = &fixture;
    let entry = suggestion_db.lookup(1).unwrap();

    searcher.set_input("".into()).unwrap();
    searcher
        .use_suggestion(controller::searcher::action::Suggestion::FromDatabase(entry.clone_ref()))
        .unwrap();

    let id = searcher.commit_node().unwrap();

    let get_node = || graph.node(id).unwrap();
    let get_inputs = || NodeTrees::new(&get_node().info, executed_graph).unwrap().inputs;
    let get_param = |n| {
        let inputs = get_inputs();
        let mut args = inputs.root_ref().leaf_iter().filter(|n| n.is_function_parameter());
        args.nth(n).and_then(|node| node.argument_info())
    };

    let expected_this_param =
        model::suggestion_database::entry::to_span_tree_param(&entry.arguments[0])
            .with_call_id(Some(id));
    let expected_arg1_param =
        model::suggestion_database::entry::to_span_tree_param(&entry.arguments[1])
            .with_call_id(Some(id));

    // === Method notation, without prefix application ===
    assert_eq!(get_node().info.expression().repr(), "Base.foo");
    match get_inputs().root.children.as_slice() {
        // The tree here should have two nodes under root - one with given Ast and second for
        // an additional prefix application argument.
        [_, second] => {
            let Node { children, kind, .. } = &second.node;
            let _expected_kind =
                node::Kind::insertion_point().with_kind(InsertionPointType::ExpectedArgument(0));
            assert!(children.is_empty());
            // assert_eq!(kind,&node::Kind::from(expected_kind));
            assert_eq!(kind.argument_info().as_ref(), Some(&expected_arg1_param));
        }
        _ => panic!("Expected only two children in the span tree's root"),
    };


    // === Method notation, with prefix application ===
    graph.set_expression(id, "Base.foo 50").unwrap();
    match get_inputs().root.children.as_slice() {
        // The tree here should have two nodes under root - one with given Ast and second for
        // an additional prefix application argument.
        [_, second] => {
            let Node { children, kind, .. } = &second.node;
            assert!(children.is_empty());
            assert_eq!(kind.argument_info().as_ref(), Some(&expected_arg1_param));
        }
        inputs =>
            panic!("Expected two children in the span tree's root but got {:?}", inputs.len()),
    };


    // === Function notation, without prefix application ===
    assert_eq!(entry.name, "foo");
    graph.set_expression(id, "foo").unwrap();
    assert_eq!(get_param(0).as_ref(), Some(&expected_this_param));
    assert_eq!(get_param(1).as_ref(), Some(&expected_arg1_param));
    assert_eq!(get_param(2).as_ref(), None);


    // === Function notation, with prefix application ===
    graph.set_expression(id, "foo Base").unwrap();
    assert_eq!(get_param(0).as_ref(), Some(&expected_this_param));
    assert_eq!(get_param(1).as_ref(), Some(&expected_arg1_param));
    assert_eq!(get_param(2).as_ref(), None);


    // === Changed function name, should not have known parameters ===
    graph.set_expression(id, "bar").unwrap();
    assert_eq!(get_param(0), None);
    assert_eq!(get_param(1), None);
    assert_eq!(get_param(2), None);

    graph.set_expression(id, "bar Base").unwrap();
    assert_eq!(get_param(0), Some(span_tree::ArgumentInfo::this(None, None)));
    assert_eq!(get_param(1), None);
    assert_eq!(get_param(2), None);

    graph.set_expression(id, "Base.bar").unwrap();
    assert_eq!(get_param(0), Some(span_tree::ArgumentInfo::this(None, None)));
    assert_eq!(get_param(1), Some(default()));
    assert_eq!(get_param(2), None);

    // === Oversaturated call ===
    graph.set_expression(id, "foo Base 10 20 30").unwrap();
    assert_eq!(get_param(0).as_ref(), Some(&expected_this_param));
    assert_eq!(get_param(1).as_ref(), Some(&expected_arg1_param));
    assert_eq!(get_param(2).as_ref(), Some(&default()));
    assert_eq!(get_param(3).as_ref(), Some(&default()));
    assert_eq!(get_param(4).as_ref(), None);
}
