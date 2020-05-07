//! Project controller.
//!
//! Responsible for owning any remote connection clients, and providing controllers for specific
//! files and modules. Expected to live as long as the project remains open in the IDE.

use crate::prelude::*;

use enso_protocol::language_server;
use parser::Parser;



// ==========================
// === Project Controller ===
// ==========================

type ModulePath = controller::module::Path;

/// Project controller's state.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Handle {
    pub language_server_rpc : Rc<language_server::Connection>,
    pub module_registry     : Rc<model::module::registry::Registry>,
    pub parser              : Parser,
    pub logger              : Logger,
}

impl Handle {
    /// Create a new project controller.
    pub fn new(language_server_client:language_server::Connection) -> Self {
        Handle {
            module_registry     : default(),
            parser              : Parser::new_or_panic(),
            language_server_rpc : Rc::new(language_server_client),
            logger              : Logger::new("Project Controller"),
        }
    }

    /// Returns a text controller for a given file path.
    ///
    /// It supports both modules and plain text files.
    pub async fn text_controller
    (&self, path:language_server::Path) -> FallibleResult<controller::Text> {
        if is_path_to_module(&path) {
            trace!(self.logger,"Obtaining controller for module {path}");
            let module = self.module_controller(path).await?;
            Ok(controller::Text::new_for_module(module))
        } else {
            let ls = self.language_server_rpc.clone_ref();
            trace!(self.logger,"Obtaining controller for plain text {path}");
            Ok(controller::Text::new_for_plain_text(path,ls))
        }
    }

    /// Returns a module controller which have module opened from file.
    pub async fn module_controller
    (&self, path:ModulePath) -> FallibleResult<controller::Module> {
        trace!(self.logger,"Obtaining module controller for {path}");
        let model_loader = self.load_module(path.clone());
        let model        = self.module_registry.get_or_load(path.clone(),model_loader).await?;
        Ok(self.module_controller_with_model(path,model))
    }

    fn module_controller_with_model
    (&self, path:ModulePath, model:Rc<model::Module>)
    -> controller::Module {
        let ls     = self.language_server_rpc.clone_ref();
        let parser = self.parser.clone_ref();
        controller::Module::new(path,model,ls,parser)
    }

    async fn load_module(&self, path:ModulePath) -> FallibleResult<Rc<model::Module>> {
        let model  = Rc::<model::Module>::default();
        let module = self.module_controller_with_model(path,model.clone_ref());
        module.load_file().await.map(move |()| model)
    }
}

/// Checks if the given path looks like it is referring to module file.
fn is_path_to_module(path:&language_server::Path) -> bool {
    path.extension() == Some(constants::LANGUAGE_FILE_EXTENSION)
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::controller::text::FilePath;
    use crate::executor::test_utils::TestWithLocalPoolExecutor;

    use language_server::response;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;


    wasm_bindgen_test_configure!(run_in_browser);


    #[test]
    fn is_path_to_module_test() {
        let path = language_server::Path::new(default(), &["src","Main.enso"]);
        assert!(is_path_to_module(&path));

        let path = language_server::Path::new(default(), &["src","Main.txt"]);
        assert_eq!(is_path_to_module(&path), false);
    }

    #[wasm_bindgen_test]
    fn obtain_module_controller() {
        let mut test  = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let path         = ModulePath{root_id:default(),segments:vec!["TestLocation".into()]};
            let another_path = ModulePath{root_id:default(),segments:vec!["TestLocation2".into()]};

            let client = language_server::MockClient::default();
            let contents      = "2+2".to_string();
            client.set_file_read_result(path.clone(),Ok(response::Read{contents}));
            let contents      = "2 + 2".to_string();
            client.set_file_read_result(another_path.clone(),Ok(response::Read{contents}));
            let connection     = language_server::Connection::new_mock(client);
            let project        = controller::Project::new(connection);
            let module         = project.module_controller(path.clone()).await.unwrap();
            let same_module    = project.module_controller(path.clone()).await.unwrap();
            let another_module = project.module_controller(another_path.clone()).await.unwrap();

            assert_eq!(path,         *module.path);
            assert_eq!(another_path, *another_module.path);
            assert!(Rc::ptr_eq(&module.model, &same_module.model));
        });
    }

    #[wasm_bindgen_test]
    fn obtain_plain_text_controller() {
        TestWithLocalPoolExecutor::set_up().run_task(async move {
            let connection   = language_server::Connection::new_mock(default());
            let project_ctrl = controller::Project::new(connection);
            let root_id      = default();
            let path         = FilePath{root_id,segments:vec!["TestPath".into()]};
            let another_path = FilePath{root_id,segments:vec!["TestPath2".into()]};

            let text_ctrl    = project_ctrl.text_controller(path.clone()).await.unwrap();
            let another_ctrl = project_ctrl.text_controller(another_path.clone()).await.unwrap();

            let language_server = project_ctrl.language_server_rpc;

            assert!(Rc::ptr_eq(&language_server,&text_ctrl.language_server()));
            assert!(Rc::ptr_eq(&language_server,&another_ctrl.language_server()));
            assert_eq!(path        , *text_ctrl   .file_path().deref()  );
            assert_eq!(another_path, *another_ctrl.file_path().deref()  );
        });
    }

    #[wasm_bindgen_test]
    fn obtain_text_controller_for_module() {
        let mut test = TestWithLocalPoolExecutor::set_up();
        test.run_task(async move {
            let file_name    = format!("test.{}",constants::LANGUAGE_FILE_EXTENSION);
            let path         = ModulePath{root_id:default(),segments:vec![file_name]};
            let contents     = "2 + 2".to_string();

            let client       = language_server::MockClient::default();
            client.set_file_read_result(path.clone(), Ok(response::Read {contents}));
            let connection   = language_server::Connection::new_mock(client);
            let project_ctrl = controller::Project::new(connection);
            let text_ctrl    = project_ctrl.text_controller(path.clone()).await.unwrap();
            let content      = text_ctrl.read_content().await.unwrap();
            assert_eq!("2 + 2", content.as_str());
        });
    }
}
