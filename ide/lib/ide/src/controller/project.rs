//! Project controller.
//!
//! Responsible for owning any remote connection clients, and providing controllers for specific
//! files and modules. Expected to live as long as the project remains open in the IDE.

use crate::prelude::*;

use crate::controller::FallibleResult;

use file_manager_client as fmc;
use json_rpc::Transport;
use parser::Parser;
use shapely::shared;
use weak_table::weak_value_hash_map::Entry::Occupied;
use weak_table::weak_value_hash_map::Entry::Vacant;



// ==========================
// === Project Controller ===
// ==========================

type ModuleLocation = controller::module::Location;

shared! { Handle

    /// Project controller's state.
    #[derive(Debug)]
    pub struct Controller {
        /// File Manager Client.
        file_manager: fmc::Handle,
        /// Cache of module controllers.
        module_cache: WeakValueHashMap<ModuleLocation,controller::module::WeakHandle>,
        /// Cache of text controllers.
        text_cache: WeakValueHashMap<fmc::Path,controller::text::WeakHandle>,
        /// Parser handle.
        parser: Parser,
        /// Id which will be given to next unsaved file.
        next_unsaved_id: usize,
    }

    impl {
        /// Create a new project controller.
        ///
        /// The remote connections should be already established.
        pub fn new(file_manager_transport:impl Transport + 'static) -> Self {
            Controller {
                file_manager    : fmc::Handle::new(file_manager_transport),
                module_cache    : default(),
                text_cache      : default(),
                parser          : Parser::new_or_panic(),
                next_unsaved_id : default(),
            }
        }

        /// Get the file manager handle used by this controller.
        pub fn file_manager(&self) -> fmc::Handle {
           self.file_manager.clone_ref()
        }
    }
}

impl Controller {
    /// Creates a new project controller. Schedules all necessary execution with
    /// the global executor.
    pub fn new_running(file_manager_transport:impl Transport + 'static) -> Self {
        let ret = Self::new(file_manager_transport);
        crate::executor::global::spawn(ret.file_manager.runner());
        ret
    }
}

impl Handle {
    /// Creates a new project controller. Schedules all necessary execution with
    /// the global executor.
    pub fn new_running(file_manager_transport:impl Transport + 'static) -> Self {
        let data = Controller::new_running(file_manager_transport);
        Self::new_from_data(data)
    }

    /// Returns a text controller for given file path.
    ///
    /// It may be a controller for both modules and plain text files.
    pub async fn get_text_controller(&self, path:fmc::Path)
    -> FallibleResult<controller::text::Handle> {
        let cached = self.with_borrowed(|data| data.text_cache.get(&path));
        match cached {
            Some(controller) => Ok(controller),
            None => {
                let loaded = self.create_text_controller(path.clone()).await?;
                //TODO[ao] Here we should make a better solution for case where we simultaneously
                // load one module twice.
                // This is duplicated with open_module, but it's not worth refactoring as it's
                // temporary solution.
                let cached = self.with_borrowed(|data|
                    match data.text_cache.entry(path) {
                        Occupied(entry) => entry.get().clone_ref(),
                        Vacant(entry)   => entry.insert(loaded)
                    }
                );
                Ok(cached)
            }
        }
    }

    /// Returns a module controller which have module opened from file.
    pub async fn get_module_controller(&self, loc:ModuleLocation)
    -> FallibleResult<controller::module::Handle> {
        let cached = self.with_borrowed(|data| data.module_cache.get(&loc));
        match cached {
            Some(controller) => Ok(controller),
            None => {
                let loaded = self.create_module_controller(loc.clone()).await?;
                //TODO[ao] Here we should make a better solution for case where we simultaneously
                // load one module twice.
                let cached = self.with_borrowed(|data|
                    match data.module_cache.entry(loc) {
                        Occupied(entry) => entry.get().clone_ref(),
                        Vacant(entry)   => entry.insert(loaded)
                    }
                );
                Ok(cached)
            },
        }
    }

    async fn create_text_controller(&self, path:fmc::Path)
    -> FallibleResult<controller::text::Handle> {
        match ModuleLocation::from_path(&path) {
            Some(location) => {
                let module = self.get_module_controller(location).await?;
                Ok(controller::text::Handle::new_for_module(module))
            },
            None => {
                let fm = self.file_manager();
                Ok(controller::text::Handle::new_for_plain_text(path, fm))
            }
        }
    }

    async fn create_module_controller(&self, location:ModuleLocation)
    -> FallibleResult<controller::module::Handle> {
        let (fm,parser) = self.with_borrowed(|d| (d.file_manager.clone_ref(),d.parser.clone_ref()));
        controller::module::Handle::new(location,fm,parser).await
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
            let project_ctrl = controller::project::Handle::new_running(transport);
            let location     = controller::module::Location("TestLocation".to_string());
            let another_loc  = controller::module::Location("TestLocation2".to_string());

            let module_ctrl         = project_ctrl.get_module_controller(location.clone()).await.unwrap();
            let same_module_ctrl    = project_ctrl.get_module_controller(location.clone()).await.unwrap();
            let another_module_ctrl = project_ctrl.get_module_controller(another_loc.clone()).await.unwrap();

            assert_eq!(location   , module_ctrl        .location());
            assert_eq!(another_loc, another_module_ctrl.location());
            assert!(module_ctrl.identity_equals(&same_module_ctrl));
        });

        test.when_stalled_send_response("2 + 2");
        test.when_stalled_send_response("3+3");
    }

    #[wasm_bindgen_test]
    fn obtain_plain_text_controller() {
        let transport       = MockTransport::new();
        TestWithLocalPoolExecutor::set_up().run_test(async move {
            let project_ctrl        = controller::project::Handle::new_running(transport);
            let file_manager_handle = project_ctrl.file_manager();
            let path                = Path("TestPath".to_string());
            let another_path        = Path("TestPath2".to_string());

            let text_ctrl        = project_ctrl.get_text_controller(path.clone()).await.unwrap();
            let same_text_ctrl   = project_ctrl.get_text_controller(path.clone()).await.unwrap();
            let another_txt_ctrl = project_ctrl.get_text_controller(another_path.clone()).await.unwrap();

            assert!(file_manager_handle.identity_equals(&text_ctrl       .file_manager()));
            assert!(file_manager_handle.identity_equals(&another_txt_ctrl.file_manager()));
            assert_eq!(path        , text_ctrl        .file_path()  );
            assert_eq!(another_path, another_txt_ctrl.file_path()  );
            assert!(text_ctrl.identity_equals(&same_text_ctrl));
        });
    }

    #[wasm_bindgen_test]
    fn obtain_text_controller_for_module() {
        let transport       = MockTransport::new();
        let mut test        = TestWithMockedTransport::set_up(&transport);
        test.run_test(async move {
            let project_ctrl = controller::project::Handle::new_running(transport);
            let path         = controller::module::Location("test".to_string()).to_path();
            let text_ctrl    = project_ctrl.get_text_controller(path.clone()).await.unwrap();
            let content      = text_ctrl.read_content().await.unwrap();
            assert_eq!("2 + 2", content.as_str());
        });
        test.when_stalled_send_response("2 + 2");
    }
}
