//! A module containing a controller for the breadcrumbs panel of the component browser.

use crate::prelude::*;

use crate::controller::searcher::component;



// ===================
// === Breadcrumbs ===
// ===================

/// A controller that keeps the path of entered modules in the Searcher and provides the
/// functionality of the breadcrumbs panel.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct Breadcrumbs {
    list:     Rc<RefCell<Vec<component::Id>>>,
    selected: Rc<Cell<usize>>,
}

impl Breadcrumbs {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Push the new breadcrumb to the breadcrumbs panel.
    pub fn push(&self, id: component::Id) {
        DEBUG!("Pushing breadcrumb: {id:?}.");
        let selected = self.selected.get();
        let mut borrowed = self.list.borrow_mut();
        if selected != borrowed.len() {
            borrowed.truncate(selected);
        }
        borrowed.push(id);
        self.select(borrowed.len());
    }

    pub fn select(&self, id: usize) {
        DEBUG!("Select: {id}");
        self.selected.set(id);
    }

    /// Returns true if the currently selected breadcrumb is the root one.
    pub fn is_top_module(&self) -> bool {
        self.selected.get() == 0
    }

    /// Returns a currently selected breadcrumb id.
    pub fn selected(&self) -> Option<component::Id> {
        if self.is_top_module() {
            None
        } else {
            let index = self.selected.get();
            self.list.borrow().get(index - 1).cloned()
        }
    }
}
