//! Application top-level structure definition. Handles views, keyboard shortcuts and more.

pub mod command;
pub mod shortcut;
pub mod view;

pub use view::View;

use crate::prelude::*;

use crate::display;
use crate::display::world::World;


// ===================
// === Application ===
// ===================

/// A top level structure for an application. It combines a view, keyboard shortcut manager, and is
/// intended to also manage layout of visible panes.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct Application {
    pub logger    : Logger,
    pub display   : World,
    pub commands  : command::Registry,
    pub shortcuts : shortcut::Registry,
    pub views     : view::Registry,
}

impl Application {
    /// Constructor.
    pub fn new(dom:&web_sys::HtmlElement) -> Self {
        let logger    = Logger::new("Application");
        let display   = World::new(dom);
        let commands  = command::Registry::create(&logger);
        let shortcuts = shortcut::Registry::new(&logger, &commands);
        let views     = view::Registry::create(&logger,&display,&commands,&shortcuts);
        Self {logger,display,commands,shortcuts,views}
    }
}

impl display::Object for Application {
    fn display_object(&self) -> &display::object::Instance {
        self.display.display_object()
    }
}
