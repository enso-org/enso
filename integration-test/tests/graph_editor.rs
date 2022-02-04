use enso_frp as frp;
use enso_gui::executor::test_utils::TestWithLocalPoolExecutor;
use enso_gui::initializer::setup_global_executor;
use enso_gui::prelude::*;
use enso_gui::Ide;
use ensogl::display;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;
use wasm_bindgen_test::wasm_bindgen_test;

struct DisplayObjectVisibleFuture {
    instance:     display::object::Instance,
    waker_set_up: bool,
}

impl Future for DisplayObjectVisibleFuture {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.instance.is_visible() {
            Poll::Ready(())
        } else if !self.waker_set_up {
            let waker = Cell::new(Some(cx.waker().clone()));
            self.instance.set_on_show(move |_, _| {
                if let Some(wk) = waker.take().take() {
                    wk.wake()
                }
            });
            self.waker_set_up = true;
            Poll::Pending
        } else {
            Poll::Pending
        }
    }
}

fn display_object_visible(object: &impl display::Object) -> DisplayObjectVisibleFuture {
    DisplayObjectVisibleFuture {
        instance:     object.display_object().clone_ref(),
        waker_set_up: false,
    }
}


wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
async fn create_new_project_and_add_nodes() {
    enso_web::forward_panic_hook_to_error();
    let _executor = setup_global_executor();
    let div = enso_web::create_div();
    div.set_id("root");
    enso_web::body().append_with_node_1(&div).expect("Failed to add root div element.");

    let initializer = enso_gui::ide::Initializer::new(default());
    let application = initializer.start().await.expect("Failed to initialize the application.");
    let network = frp::Network::new("Test");
    let project = application.presenter.view().project();
    let graph_editor = project.graph();
    frp::new_network! { network
        prompt_expected <- project.show_prompt.future();
        add_new_node_expected <- graph_editor.node_added.future();
    }

    let prompt_shown = prompt_expected.next_value();
    application
        .presenter
        .controller()
        .manage_projects()
        .expect("Should be able to manage projects")
        .create_new_project(None)
        .await;
    prompt_shown.await.expect("Prompt have not been shown");

    assert_eq!(graph_editor.model.nodes.all.len(), 2);
    let expect_node_added = add_new_node_expected.next_value();
    graph_editor.add_node();
    expect_node_added.expect("Expected node_added signal");
    assert_eq!(graph_editor.model.nodes.all.len(), 3);
}
