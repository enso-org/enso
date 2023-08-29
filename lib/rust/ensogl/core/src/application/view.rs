//! Definition of `View`, a visual component of an application.

use crate::prelude::*;

use super::command;
use super::shortcut;
use super::Application;


// ==============
// === Export ===
// ==============

pub use command::View;



// ================
// === Registry ===
// ================

/// View registry. Please note that all view definitions should be registered here as soon as
/// possible in order to enable their default shortcuts and spread the information about their API.
#[derive(Debug, Clone, CloneRef)]
#[allow(missing_docs)]
pub struct Registry {
    pub command_registry:  command::Registry,
    pub shortcut_registry: shortcut::Registry,
    pub definitions:       Rc<RefCell<HashSet<String>>>,
}

impl Registry {
    /// Constructor.
    pub fn create(
        command_registry: &command::Registry,
        shortcut_registry: &shortcut::Registry,
    ) -> Self {
        let command_registry = command_registry.clone_ref();
        let shortcut_registry = shortcut_registry.clone_ref();
        let definitions = default();
        Self { command_registry, shortcut_registry, definitions }
    }

    /// View registration.
    ///
    /// Registers any keyboard shortcuts provided by the [View], and registers the View for use
    /// with commands. Should be called for every View that might potentially be instantiated at
    /// any point in the future, so that the keyboard shortcuts overview has full information from
    /// the outset.
    pub fn register<V: View>(&self) {
        let label = V::label().into();
        for shortcut in V::global_shortcuts() {
            self.shortcut_registry.add(shortcut)
        }
        self.definitions.borrow_mut().insert(label);
        self.command_registry.register::<V>();
    }

    /// New view constructor.
    pub fn new_view<V: View>(&self, app: &Application) -> V {
        let label = V::label();
        let was_registered = self.definitions.borrow().get(label).is_some();
        if !was_registered {
            // FIXME[WD]: The registration should be performed automatically by using before-main
            //     entry points.
            self.register::<V>();
        }
        let view = V::new(app);
        let id = self.command_registry.register_instance(&view);
        let focused_shortcuts = V::focused_shortcuts();
        if !focused_shortcuts.is_empty() {
            let network = V::network(&view);
            let registry = app.shortcuts.instance_bound_child_in_network(
                id,
                &view,
                &app.display.default_scene,
                network,
            );
            for shortcut in focused_shortcuts {
                registry.add(shortcut)
            }
        }
        view
    }
}
