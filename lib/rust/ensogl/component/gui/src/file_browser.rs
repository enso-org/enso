//! In this module the File Browser component will be implemented in the future. Currently it
//! contains only an API description.

pub mod model;

use crate::prelude::*;

use crate::file_browser::model::*;

use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_core::display::Scene;
use std::path::PathBuf;


// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_content      (AnyFolderContent),
        move_focus_left  (),
        move_focus_right (),
        move_focus_by    (isize),

        copy_focused       (),
        cut_focused        (),
        paste_into_focused (),
    }

    Output {
        entry_selected (PathBuf),
        entry_chosen   (PathBuf),

        copy       (PathBuf),
        cut        (PathBuf),
        paste_into (PathBuf),
    }
}



// ===================
// === FileBrowser ===
// ===================

/// A file browser component. It allows to browse the content of a folder and it's subfolders and
/// emits an event when an entry is chosen.
#[derive(Clone, CloneRef, Debug)]
pub struct FileBrowser {
    logger:         Logger,
    frp:            Frp,
    display_object: display::object::Instance,
}

impl Deref for FileBrowser {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}


impl FileBrowser {
    /// Constructore
    pub fn new() -> Self {
        let logger = Logger::new("FileBrowser");
        let frp = Frp::new();
        let display_object = display::object::Instance::new(&logger);
        Self { logger, frp, display_object }
    }
}

impl Default for FileBrowser {
    fn default() -> Self {
        Self::new()
    }
}

impl display::Object for FileBrowser {
    fn display_object(&self) -> &display::object::Instance<Scene> {
        &self.display_object
    }
}
