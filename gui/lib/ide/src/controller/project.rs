//! Project controller.
//!
//! Responsible for owning any remote connection clients, and providing controllers for specific
//! files and modules. Expected to live as long as the project remains open in the IDE.

use crate::prelude::*;

use json_rpc::Transport;
use weak_table::weak_value_hash_map::Entry::Occupied;
use weak_table::weak_value_hash_map::Entry::Vacant;
use file_manager_client as fmc;
use shapely::shared;

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
        text_cache: WeakValueHashMap<file_manager_client::Path,controller::text::WeakHandle>,
    }

    impl {
        /// Create a new project controller.
        ///
        /// The remote connections should be already established.
        pub fn new(file_manager_transport:impl Transport + 'static) -> Self {
            Controller {
                file_manager : fmc::Handle::new(file_manager_transport),
                module_cache : default(),
                text_cache   : default(),
            }
        }

        /// Returns a module controller for given module location.
        pub fn open_module(&mut self, loc:ModuleLocation) -> controller::module::Handle {
            match self.module_cache.entry(loc.clone()) {
                Occupied(entry) => entry.get().clone(),
                Vacant(entry)   => entry.insert(controller::module::Handle::new(loc)),
            }
        }

        /// Returns a text controller for given file path.
        pub fn open_text_file(&mut self, path:fmc::Path) -> controller::text::Handle {
            let fm = self.file_manager.clone();
            match self.text_cache.entry(path.clone()) {
                Occupied(entry) => entry.get().clone(),
                // TODO[ao] handle module files here.
                Vacant(entry) => {
                    let controller = controller::text::Handle::new_for_plain_test(path,fm);
                    entry.insert(controller)
                },
            }
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
}

#[cfg(test)]
mod test {
    use super::*;

    use file_manager_client::Path;
    use json_rpc::test_util::transport::mock::MockTransport;

    #[test]
    fn obtain_module_controller() {
        let transport        = MockTransport::new();
        let project_ctrl     = controller::project::Handle::new(transport);
        let location         = controller::module::Location("TestLocation".to_string());
        let another_location = controller::module::Location("TestLocation2".to_string());

        let module_ctrl         = project_ctrl.open_module(location.clone());
        let same_module_ctrl    = project_ctrl.open_module(location.clone());
        let another_module_ctrl = project_ctrl.open_module(another_location.clone());

        assert_eq!(location        , module_ctrl        .location());
        assert_eq!(another_location, another_module_ctrl.location());
        assert!(module_ctrl.identity_equals(&same_module_ctrl));
    }

    #[test]
    fn obtain_text_controller() {
        let transport           = MockTransport::new();
        let project_ctrl        = controller::project::Handle::new(transport);
        let file_manager_handle = project_ctrl.with_borrowed(|s| s.file_manager.clone());
        let path                = Path("TestPath".to_string());
        let another_path        = Path("TestPath2".to_string());

        let text_ctrl         = project_ctrl.open_text_file(path.clone());
        let same_text_ctrl    = project_ctrl.open_text_file(path.clone());
        let another_text_ctrl = project_ctrl.open_text_file(another_path.clone());

        assert!(file_manager_handle.identity_equals(&text_ctrl        .file_manager()));
        assert!(file_manager_handle.identity_equals(&another_text_ctrl.file_manager()));
        assert_eq!(path        , text_ctrl        .file_path()  );
        assert_eq!(another_path, another_text_ctrl.file_path()  );
        assert!(text_ctrl.identity_equals(&same_text_ctrl));
    }
}
