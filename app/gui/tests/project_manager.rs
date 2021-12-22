//! Project Manager tests.

#[cfg(test)]
mod tests {
    use enso_gui::prelude::*;

    use engine_protocol::project_manager::Client;
    use engine_protocol::project_manager::API;
    use enso_gui::transport::web::WebSocket;

    use engine_protocol::project_manager::MissingComponentAction::Install;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);


    //#[wasm_bindgen_test::wasm_bindgen_test(async)]
    #[allow(dead_code)]
    async fn project_life_cycle() {
        let logger = Logger::new("test");
        let ws = WebSocket::new_opened(logger, "ws://localhost:30535").await;
        let ws = ws.expect("Couldn't connect to WebSocket server.");
        let client = Client::new(ws);
        let _executor = enso_gui::initializer::setup_global_executor();

        executor::global::spawn(client.runner());

        let name = "TestProject".to_string();
        let creation = client.create_project(&name, &None, &None, &Install);
        let created = creation.await.expect("Couldn't create project.");
        let uuid = created.project_id;
        let _address = client.open_project(&uuid, &Install).await.expect("Couldn't open project.");
        client.close_project(&uuid).await.expect("Couldn't close project.");
        let list_response = client.list_projects(&None).await;
        let list_response = list_response.expect("Couldn't list recent projects.");
        assert!(list_response.projects.iter().any(|project| *project.name.deref() == name));

        let new_name = "NewTestProject".to_string();
        client.rename_project(&uuid, &new_name).await.expect("Couldn't rename project.");
        let list_response = client.list_projects(&None).await;
        let list_response = list_response.expect("Couldn't list recent projects.");
        assert!(!list_response.projects.iter().any(|project| *project.name.deref() == name));
        assert!(list_response.projects.iter().any(|project| *project.name.deref() == new_name));

        client.delete_project(&uuid).await.expect("Couldn't delete project.");
        let list_response = client.list_projects(&None).await;
        let list_response = list_response.expect("Couldn't list recent projects.");
        assert!(!list_response.projects.iter().any(|project| *project.name.deref() == name));
        // FIXME[dg]: project/listSample isn't implemented on the server-side yet.
        //client.list_samples(10).await.expect("Couldn't list samples.");
    }
}
