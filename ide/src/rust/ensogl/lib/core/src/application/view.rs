//! Definition of `View`, a visual component of an application.

use crate::prelude::*;

use crate::display::world::World;
use super::command;
use super::shortcut;
use super::Application;



// ============
// === View ===
// ============

/// A visual component of an application.
pub trait View : command::Provider + shortcut::DefaultShortcutProvider {
    /// Constructor.
    fn new(app:&Application) -> Self;
}



// ==================
// === Definition ===
// ==================

/// View definition. Keeps shortcut handles for each registered view.
#[derive(Debug)]
pub struct Definition {
    shortcut_handles : Vec<shortcut::Handle>
}



// ================
// === Registry ===
// ================

/// View registry. Please note that all view definitions should be registered here as soon as
/// possible in order to enable their default shortcuts and spread the information about their API.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct Registry {
    pub logger            : Logger,
    pub display           : World,
    pub command_registry  : command::Registry,
    pub shortcut_registry : shortcut::Registry,
    pub definitions       : Rc<RefCell<HashMap<String,Definition>>>,
}

impl Registry {
    /// Constructor.
    pub fn create
    ( logger            : impl AnyLogger
    , display           : &World
    , command_registry  : &command::Registry
    , shortcut_registry : &shortcut::Registry
    ) -> Self {
        let logger            = Logger::sub(logger,"view_registry");
        let display           = display.clone_ref();
        let command_registry  = command_registry.clone_ref();
        let shortcut_registry = shortcut_registry.clone_ref();
        let definitions       = default();
        Self {logger,display,command_registry,shortcut_registry,definitions}
    }

    /// View registration.
    pub fn register<V:View>(&self) {
        let label            = V::label().into();
        let shortcut_handles = V::default_shortcuts().into_iter().map(|shortcut| {
            self.shortcut_registry.add(shortcut)
        }).collect();
        let definition = Definition {shortcut_handles};
        self.definitions.borrow_mut().insert(label,definition);
        self.command_registry.register::<V>();
    }

    /// New view constructor.
    pub fn new_view<V:View>(&self, app:&Application) -> V {
        let label          = V::label();
        let was_registered = self.definitions.borrow().get(label).is_some();
        if !was_registered {
            warning!(&self.logger,
                "The view '{label}' was created but never registered, performing automatic \
                registration. You should always register available views as soon as possible to \
                enable their default shortcuts and spread the information about their API.");
            self.register::<V>();
        }
        let view = V::new(app);
        self.command_registry.register_instance(&view);
        view
    }
}
