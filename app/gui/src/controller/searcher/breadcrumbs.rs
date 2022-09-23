//! A module containing a controller for the breadcrumbs panel of the component browser.

use crate::prelude::*;

use crate::controller::searcher::component;
use double_representation::module;
use model::suggestion_database::entry::QualifiedName;
use model::suggestion_database::Entry;


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

impl BreadcrumbEntry {
    pub fn name(&self) -> ImString {
        self.displayed_name.clone_ref()
    }

    pub fn id(&self) -> component::Id {
        self.component_id
    }

    pub fn qualified_name(&self) -> &QualifiedName {
        &self.qualified_name
    }
}

impl From<(component::Id, Rc<Entry>)> for BreadcrumbEntry {
    fn from((component_id, entry): (component::Id, Rc<Entry>)) -> Self {
        let qualified_name = entry.qualified_name();
        let displayed_name = ImString::new(&entry.name);
        BreadcrumbEntry { displayed_name, component_id, qualified_name }
    }
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
        let entry = BreadcrumbEntry::from(self.lookup(&module_name)?);
        result.push(entry);
        result.extend(self.collect_parents(&module_name));
        let project_name = module_name.project_name.clone();
        let main_module = module::QualifiedName::new_main(project_name.clone());
        if let Some(entry) = self.lookup(&main_module) {
            result.push(BreadcrumbEntry {
                displayed_name: String::from(project_name.project).into(),
                ..entry.into()
            });
        }
        Some(result.reversed())
    }

    fn lookup(&self, name: &module::QualifiedName) -> Option<(component::Id, Rc<Entry>)> {
        self.database.lookup_by_qualified_name(name.into_iter())
    }

    fn collect_parents(&self, name: &module::QualifiedName) -> Vec<BreadcrumbEntry> {
        let mut result = Vec::new();
        let (_, mut current) = self.lookup(name).expect("Entry should be present in the database.");
        while let Some(parent) = current.parent_module() {
            if let Some((component_id, parent_entry)) = self.lookup(&parent) {
                result.push((component_id, parent_entry.clone_ref()).into());
                current = parent_entry;
            } else {
                break;
            }
        }
        result
    }
}
