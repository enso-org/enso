//! A module containing a controller for the breadcrumbs panel of the component browser.

use crate::prelude::*;

use crate::model::suggestion_database;

use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use model::suggestion_database::Entry;



// ===================
// === Breadcrumbs ===
// ===================

/// A controller that keeps the path of entered modules in the Searcher and provides the
/// functionality of the breadcrumbs panel. The integration between the
/// controller and the view is done by the [searcher
/// presenter](crate::presenter::component_browser_searcher).
#[derive(Debug, Clone, CloneRef, Default)]
pub struct Breadcrumbs {
    list:     Rc<RefCell<Vec<BreadcrumbEntry>>>,
    selected: Rc<Cell<usize>>,
}

impl Breadcrumbs {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Set the list of breadcrumbs to be displayed in the breadcrumbs panel.
    pub fn set_content(&self, breadcrumbs: impl Iterator<Item = BreadcrumbEntry>) {
        let mut borrowed = self.list.borrow_mut();
        *borrowed = breadcrumbs.collect();
        self.select(borrowed.len());
    }

    /// A list of breadcrumbs' text labels to be displayed in the panel.
    pub fn names(&self) -> Vec<ImString> {
        self.list.borrow().iter().map(|entry| entry.name()).collect()
    }

    /// The last (right-most) breadcrumb in the list.
    pub fn last(&self) -> Option<suggestion_database::entry::Id> {
        self.list.borrow().last().map(BreadcrumbEntry::id)
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
    pub fn selected(&self) -> Option<suggestion_database::entry::Id> {
        if self.is_top_module() {
            None
        } else {
            let index = self.selected.get();
            self.list.borrow().get(index - 1).map(BreadcrumbEntry::id)
        }
    }
}



// =======================
// === BreadcrumbEntry ===
// =======================

/// A single entry in the breadcrumbs panel.
#[derive(Debug, Clone)]
pub struct BreadcrumbEntry {
    displayed_name: ImString,
    component_id:   suggestion_database::entry::Id,
    qualified_name: QualifiedName,
}

impl BreadcrumbEntry {
    /// A displayed label of the entry.
    pub fn name(&self) -> ImString {
        self.displayed_name.clone_ref()
    }

    /// A component id of the entry.
    pub fn id(&self) -> suggestion_database::entry::Id {
        self.component_id
    }

    /// A qualified name of the entry.
    pub fn qualified_name(&self) -> &QualifiedName {
        &self.qualified_name
    }
}

impl From<(suggestion_database::entry::Id, Rc<Entry>)> for BreadcrumbEntry {
    fn from((component_id, entry): (suggestion_database::entry::Id, Rc<Entry>)) -> Self {
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
#[derive(Debug)]
pub struct Builder<'a> {
    database: &'a model::SuggestionDatabase,
}

impl<'a> Builder<'a> {
    /// Constructor.
    pub fn new(database: &'a model::SuggestionDatabase) -> Self {
        Self { database }
    }

    /// Build a list of breadcrumbs for a specified module. The list will contain:
    /// 1. The main module of the project.
    /// 2. All parent modules of the [`module`].
    /// 3. The [`module`] itself.
    ///
    /// Returns an empty vector if the [`module`] is not found in the database or in the
    /// components list.
    pub fn build(
        self,
        module: suggestion_database::entry::Id,
    ) -> impl Iterator<Item = BreadcrumbEntry> {
        self.database
            .lookup(module)
            .map(|db_entry| {
                let project_name = db_entry.defined_in.project();
                let main_module_name = QualifiedName::new_main(project_name.clone_ref());
                let main_module = self.lookup(&main_module_name);
                let main_module_entry =
                    main_module.map(|entry: (suggestion_database::entry::Id, Rc<Entry>)| {
                        BreadcrumbEntry {
                            displayed_name: project_name.project.clone_ref(),
                            ..entry.into()
                        }
                    });
                let parents = self.collect_parents(&db_entry.defined_in);
                let entry = BreadcrumbEntry::from((module, db_entry));
                iter::once(entry).chain(parents).chain(main_module_entry).rev()
            })
            .into_iter()
            .flatten()
    }

    fn lookup<'b>(
        &self,
        name: impl Into<QualifiedNameRef<'b>>,
    ) -> Option<(suggestion_database::entry::Id, Rc<Entry>)> {
        self.database.lookup_by_qualified_name(name).ok()
    }

    /// Collect all parent modules of the given module.
    ///
    /// Panics if the module is not found in the database.
    fn collect_parents(&self, name: &QualifiedName) -> Vec<BreadcrumbEntry> {
        let parents = name.parents();
        let database_entries = parents.filter_map(|name| self.lookup(name));
        // Note: it would be nice to avoid allocation here, but we need to reverse the
        // iterator later, so returning `impl Iterator` is not an option. We can only reverse
        // `DoubleEndedIterator`.
        database_entries.map(BreadcrumbEntry::from).collect()
    }
}
