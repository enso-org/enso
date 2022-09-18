//! A module containing a controller for the breadcrumbs panel of the component browser.

use crate::controller::searcher::component;
use crate::prelude::*;



// ===================
// === Breadcrumbs ===
// ===================

/// Breadcrumbs controller.
///
/// This object is responsible for interactions between the breadcrumbs and the searcher controller.
///
/// TODO: The actual implementation would be finished in
///   [Breadcrumbs Panel integration task](https://www.pivotaltracker.com/story/show/182675703).
#[derive(Debug, Clone, CloneRef)]
pub struct Breadcrumbs {
    currently_selected: Rc<Cell<Option<component::Id>>>,
}

impl Breadcrumbs {
    /// Constructor.
    pub fn new() -> Self {
        Self { currently_selected: default() }
    }

    /// Push the new breadcrumb to the breadcrumbs panel.
    pub fn push(&self, id: component::Id) {
        self.currently_selected.set(Some(id));
    }

    /// Returns true if the currently selected breadcrumb is the root one.
    pub fn is_top_module(&self) -> bool {
        self.currently_selected.get().is_none()
    }

    /// Returns a currently selected breadcrumb id.
    pub fn currently_selected(&self) -> Option<component::Id> {
        self.currently_selected.get()
    }
}
