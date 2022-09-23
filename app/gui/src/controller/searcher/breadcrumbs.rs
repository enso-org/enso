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
/// functionality of the breadcrumbs panel. It is used to store the state of the breadcrumbs
/// panel, but it does not provide any view-related functionality. The integration between the
/// controller and the view is done by the [searcher presenter](crate::presenter::searcher).
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

    pub fn set_content<'a>(&self, breadcrumbs: impl Iterator<Item = &'a BreadcrumbEntry>) {
        let selected = self.selected.get();
        let mut borrowed = self.list.borrow_mut();
        if selected != borrowed.len() {
            borrowed.truncate(selected);
        }
        let ids = breadcrumbs.map(|entry| entry.id());
        borrowed.extend(ids);
        self.select(borrowed.len());
    }

    /// Mark the entry with the given index as selected.
    pub fn select(&self, id: usize) {
        self.selected.set(id);
    }

    /// Returns true if the currently selected breadcrumb is the first one.
    pub fn is_top_module(&self) -> bool {
        self.selected.get() == 0
    }

    /// Returns a currently selected breadcrumb id. Returns [`None`] if the top level breadcrumb
    /// is selected.
    pub fn selected(&self) -> Option<component::Id> {
        if self.is_top_module() {
            None
        } else {
            let index = self.selected.get();
            self.list.borrow().get(index - 1).cloned()
        }
    }
}



// =======================
// === BreadcrumbEntry ===
// =======================

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



// ===============
// === Builder ===
// ===============

/// A builder for the breadcrumbs list. It is used to include all parent modules when pushing the
/// new breadcrumb to the panel.
pub struct Builder<'a> {
    database:   &'a model::SuggestionDatabase,
    components: component::List,
}

impl<'a> Builder<'a> {
    /// Constructor.
    pub fn new(database: &'a model::SuggestionDatabase, components: component::List) -> Self {
        Self { database, components }
    }

    /// Build a list of breadcrumbs for a specified module. The list will contain:
    /// 1. The main module of the project.
    /// 2. All parent modules of the [`module`].
    /// 3. The [`module`] itself.
    ///
    /// Returns [`None`] if the [`module`] is not found in the database or in the components list.
    pub fn build(self, module: &component::Id) -> Option<Vec<BreadcrumbEntry>> {
        let mut result = Vec::new();
        let module_name = self.components.module_qualified_name(*module)?;
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

    /// Collect all parent modules of the given module.
    ///
    /// Panics if the module is not found in the database.
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
