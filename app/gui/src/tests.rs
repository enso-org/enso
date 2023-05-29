use super::prelude::*;

use crate::controller::ide;
use crate::controller::ide::ManagingProjectAPI;
use crate::transport::test_utils::TestWithMockedTransport;

use engine_protocol::project_manager;
use json_rpc::test_util::transport::mock::MockTransport;
use serde_json::json;
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
        let ide_controller = ide::Desktop::new(project_manager).unwrap();
        let result = ide_controller.create_new_project(None, None).await;
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
