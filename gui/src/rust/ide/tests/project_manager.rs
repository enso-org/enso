//! Project Manager tests.

#[cfg(test)]
mod tests {
    use ide::prelude::*;

    use enso_protocol::project_manager::API;
    use enso_protocol::project_manager::Client;
    use ide::ide::*;
    use ide::transport::web::WebSocket;

    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);


    //#[wasm_bindgen_test::wasm_bindgen_test(async)]
    #[allow(dead_code)]
    async fn project_life_cycle() {
        let ws        = WebSocket::new_opened(default(),"ws://localhost:30535").await;
        let ws        = ws.expect("Couldn't connect to WebSocket server.");
        let client    = Client::new(ws);
        let _executor = setup_global_executor();

        executor::global::spawn(client.runner());

        let name     = "TestProject".to_string();
        let creation = client.create_project(&name).await.expect("Couldn't create project.");
        let uuid     = creation.project_id;
        let _address = client.open_project(&uuid).await.expect("Couldn't open project.");
        client.close_project(&uuid).await.expect("Couldn't close project.");
        client.delete_project(&uuid).await.expect("Couldn't delete project.");
        client.list_projects(&Some(10)).await.expect("Couldn't list recent projects.");
        // FIXME[dg]: project/listSample isn't implemented on the server-side yet.
        //client.list_samples(10).await.expect("Couldn't list samples.");
    }
}
