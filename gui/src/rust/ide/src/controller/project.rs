//! Project controller.
//!
//! Responsible for owning any remote connection clients, and providing controllers for specific
//! files and modules. Expected to live as long as the project remains open in the IDE.

use crate::prelude::*;

use file_manager_client as fmc;
use json_rpc::Transport;
use parser::Parser;



// ==========================
// === Project Controller ===
// ==========================

type ModuleLocation = controller::module::Location;


/// Project controller's state.
#[derive(Debug)]
pub struct Handle {
    /// File Manager Client.
    pub file_manager: fmc::Handle,
    /// Cache of module controllers.
    pub module_registry: Rc<model::module::registry::Registry>,
    /// Parser handle.
    pub parser: Parser,
}

impl Handle {
    /// Create a new project controller.
    ///
    /// The remote connections should be already established.
    pub fn new(file_manager_transport:impl Transport + 'static) -> Self {
        Handle {
            file_manager    : fmc::Handle::new(file_manager_transport),
            module_registry : default(),
            parser          : Parser::new_or_panic(),
        }
    }

    /// Creates a new project controller. Schedules all necessary execution with
    /// the global executor.
    pub fn new_running(file_manager_transport:impl Transport + 'static) -> Self {
        let ret = Self::new(file_manager_transport);
        crate::executor::global::spawn(ret.file_manager.runner());
        ret
    }

    /// Returns a text controller for given file path.
    ///
    /// It may be a controller for both modules and plain text files.
    pub async fn text_controller(&self, path:fmc::Path) -> FallibleResult<controller::Text> {
        match ModuleLocation::from_path(&path) {
            Some(location) => {
                let module = self.module_controller(location).await?;
                Ok(controller::Text::new_for_module(module))
            },
            None => {
                let fm = self.file_manager.clone_ref();
                Ok(controller::Text::new_for_plain_text(path,fm))
            }
        }
    }

    /// Returns a module controller which have module opened from file.
    pub async fn module_controller
    (&self, location:ModuleLocation) -> FallibleResult<controller::Module> {
        let model_loader = self.load_module(location.clone());
        let model        = self.module_registry.get_or_load(location.clone(), model_loader).await?;
        Ok(self.module_controller_with_model(location,model))
    }

    fn module_controller_with_model
    (&self, location:ModuleLocation, model:Rc<model::Module>) -> controller::Module {
        let fm     = self.file_manager.clone_ref();
        let parser = self.parser.clone_ref();
        controller::Module::new(location, model, fm, parser)
    }

    async fn load_module(&self, location:ModuleLocation) -> FallibleResult<Rc<model::Module>> {
        let model  = Rc::<model::Module>::default();
        let module = self.module_controller_with_model(location, model.clone_ref());
        module.load_file().await.map(move |()| model)
    }
}



#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::transport::test_utils::TestWithMockedTransport;

    use file_manager_client::Path;
    use json_rpc::test_util::transport::mock::MockTransport;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;


    wasm_bindgen_test_configure!(run_in_browser);



    #[wasm_bindgen_test]
    fn obtain_module_controller() {
        let transport = MockTransport::new();
        let mut test  = TestWithMockedTransport::set_up(&transport);
        test.run_test(async move {
            let project     = controller::Project::new_running(transport);
            let location    = ModuleLocation::new("TestLocation");
            let another_loc = ModuleLocation::new("TestLocation2");

            let module         = project.module_controller(location.clone()).await.unwrap();
            let same_module    = project.module_controller(location.clone()).await.unwrap();
            let another_module = project.module_controller(another_loc.clone()).await.unwrap();

            assert_eq!(location,    module.location);
            assert_eq!(another_loc, another_module.location);
            assert!(Rc::ptr_eq(&module.model, &same_module.model));
        });

        test.when_stalled_send_response("2 + 2");
        test.when_stalled_send_response("3+3");
    }

    #[wasm_bindgen_test]
    fn obtain_plain_text_controller() {
        let transport       = MockTransport::new();
        TestWithLocalPoolExecutor::set_up().run_task(async move {
            let project_ctrl        = controller::Project::new_running(transport);
            let path                = Path::new("TestPath");
            let another_path        = Path::new("TestPath2");

            let text_ctrl    = project_ctrl.text_controller(path.clone()).await.unwrap();
            let another_ctrl = project_ctrl.text_controller(another_path.clone()).await.unwrap();

            assert!(project_ctrl.file_manager.identity_equals(&text_ctrl   .file_manager()));
            assert!(project_ctrl.file_manager.identity_equals(&another_ctrl.file_manager()));
            assert_eq!(path        , *text_ctrl   .file_path().deref()  );
            assert_eq!(another_path, *another_ctrl.file_path().deref()  );
        });
    }

    #[wasm_bindgen_test]
    fn obtain_text_controller_for_module() {
        let transport       = MockTransport::new();
        let mut test        = TestWithMockedTransport::set_up(&transport);
        test.run_test(async move {
            let project_ctrl = controller::Project::new_running(transport);
            let path         = ModuleLocation::new("test").to_path();
            let text_ctrl    = project_ctrl.text_controller(path.clone()).await.unwrap();
            let content      = text_ctrl.read_content().await.unwrap();
            assert_eq!("2 + 2", content.as_str());
        });
        test.when_stalled_send_response("2 + 2");
    }
}
