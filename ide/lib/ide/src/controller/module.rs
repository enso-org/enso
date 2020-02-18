//! Module Controller.
//!
//! The module controller keeps cached module state (module state is AST+Metadata or equivalent),
//! and uses it for synchronizing state for text and graph representations. It provides method
//! for registering text and graph changes. If for example text represntation will be changed, there
//! will be notifications for both text change and graph change.
//!
//! This module is still on WIP state, for now it contains stubs only.

use crate::prelude::*;

use shapely::shared;


/// Structure uniquely identifying module location in the project.
/// Mappable to filesystem path.
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub struct Location(pub String);

impl Location {
    /// Obtains path (within a project context) to the file with this module.
    pub fn to_path(&self) -> file_manager_client::Path {
        // TODO [mwu] Extremely provisional. When multiple files support is
        //            added, needs to be fixed, if not earlier.
        let Location(string) = self;
        let result = format!("./{}.{}", string, constants::LANGUAGE_FILE_EXTENSION);
        file_manager_client::Path::new(result)
    }
}

shared! { Handle
    /// State data of the module controller.
    #[derive(Debug)]
    pub struct State {
        /// This module's location.
        location : Location,
    }

    impl {
        /// Create new Module controller for given location.
        pub fn new(location:Location) -> Self {
            State {location:location}
        }

        /// Obtain clone of location.
        pub fn location(&self) -> Location {
            self.location.clone()
        }
    }
}

impl Handle {
    /// Receives a notification call when file with this module has been
    /// modified by a third-party tool (like non-IDE text editor).
    pub async fn file_externally_modified(&self) {
        // TODO: notify underlying text/graph controllers about the changes
        todo!()
    }
}
