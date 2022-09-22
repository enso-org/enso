//! A module containing a controller for the breadcrumbs panel of the component browser.

use crate::prelude::*;

use crate::controller::searcher::component;
use double_representation::module;
use model::suggestion_database::entry::QualifiedName;



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

    /// TODO
    pub fn clear(&self) {
        self.list.borrow_mut().clear();
        self.selected.set(0);
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



// ===============
// === Builder ===
// ===============

#[derive(Debug, Clone)]
pub struct BreadcrumbEntry {
    displayed_name: ImString,
    component_id:   component::Id,
    qualified_name: QualifiedName,
}

pub struct Builder<'a> {
    database: &'a model::SuggestionDatabase,
    module:   component::Id,
}

impl<'a> Builder<'a> {
    pub fn for_module(database: &'a model::SuggestionDatabase, module: &component::Id) -> Self {
        Self { database, module: *module }
    }

    pub fn build(self, components: &component::List) -> Option<Vec<BreadcrumbEntry>> {
        let mut result = Vec::new();
        let module_name = components.module_qualified_name(self.module)?;
        let entry = self.lookup(&module_name)?;
        result.push(entry.clone_ref());
        result.extend(self.collect_parents(entry));
        let project_name = module_name.project_name.clone();
        let main_module = module::QualifiedName::new_main(project_name.clone());
        if let Some(entry) = self.lookup(&main_module) {
            result.push(BreadcrumbEntry {
                displayed_name: String::from(project_name.project).into(),
                ..entry
            });
        }
        Some(result.reversed())
    }

    fn lookup(&self, name: &module::QualifiedName) -> Option<BreadcrumbEntry> {
        let (component_id, entry) = self.database.lookup_by_qualified_name(name.into_iter())?;
        let displayed_name = entry.name.into();
        let qualified_name = entry.qualified_name();
        Some(BreadcrumbEntry { displayed_name, component_id, qualified_name })
    }

    fn collect_parents(&self, entry: BreadcrumbEntry) -> Vec<BreadcrumbEntry> {
        let mut result = Vec::new();
        let mut entry = self.database.lookup_by_qualified_name(entry.qualified_name.into_iter());
        while let Some(parent) = entry.qualified_name.parent_module() {
            if let Some(parent_entry) = self.database.lookup_by_qualified_name(parent) {
                result.push(parent_entry.clone_ref());
                entry = parent_entry;
            } else {
                break;
            }
        }
        result
    }
}
