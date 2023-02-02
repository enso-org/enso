//! The crate contains all structures for representing suggestions and their database.

#![recursion_limit = "512"]
// === Features ===
#![feature(arc_unwrap_or_clone)]
#![feature(async_closure)]
#![feature(associated_type_bounds)]
#![feature(cell_update)]
#![feature(drain_filter)]
#![feature(exact_size_is_empty)]
#![feature(iter_order_by)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(result_option_inspect)]
#![feature(map_try_insert)]
#![feature(assert_matches)]
#![feature(hash_drain_filter)]
#![feature(unwrap_infallible)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_prelude::*;

use crate::documentation_ir::EntryDocumentation;
use crate::entry::Kind;
use crate::entry::ModuleSpan;

use double_representation::module::MethodId;
use double_representation::name::QualifiedName;
use double_representation::name::QualifiedNameRef;
use engine_protocol::language_server;
use engine_protocol::language_server::SuggestionId;
use enso_data_structures::hash_map_tree::HashMapTree;
use enso_notification as notification;
use flo_stream::Subscriber;
use language_server::types::SuggestionDatabaseUpdatesEvent;
use language_server::types::SuggestionsDatabaseVersion;


// ==============
// === Export ===
// ==============

pub mod documentation_ir;
pub mod entry;
pub mod example;
pub mod mock;

pub use engine_protocol;
pub use entry::Entry;
pub use example::Example;



/// Common types that should be visible across the whole IDE crate.
pub mod prelude {
    pub use ast::prelude::*;
    pub use enso_prelude::*;
}


// ============================
// === QualifiedNameToIdMap ===
// ============================

/// A map from [`entry::QualifiedName`]s to [`entry::Id`]s. The methods of the type provide
/// semantics of a map, while the internal representation is based on a [`HashMapTree`].
///
/// The internal representation conserves memory when storing many paths sharing a common prefix of
/// segments.
#[derive(Clone, Debug, Default)]
struct QualifiedNameToIdMap {
    tree: HashMapTree<ImString, Option<entry::Id>>,
}

impl QualifiedNameToIdMap {
    /// Gets the [`entry::Id`] at `path` or [`None`] if not found.
    pub fn get<P, I>(&self, path: P) -> Option<entry::Id>
    where
        P: IntoIterator<Item = I>,
        I: Into<ImString>, {
        self.tree.get(path).and_then(|v| *v)
    }

    /// Sets the `id` at `path`. Emits a warning if an `id` was set at this `path` before the
    /// operation.
    pub fn set_and_warn_if_existed(&mut self, path: &QualifiedName, id: entry::Id) {
        let value = Some(id);
        let old_value = self.replace_value_and_traverse_back_pruning_empty_subtrees(path, value);
        if old_value.is_some() {
            warn!("An existing suggestion entry id at {path} was overwritten with {id}.");
        }
    }

    /// Removes the [`entry::Id`] stored at `path`. Emits a warning if there was no [`entry::Id`]
    /// stored at `path` before the operation.
    pub fn remove_and_warn_if_did_not_exist(&mut self, path: &QualifiedName) {
        let old_value = self.replace_value_and_traverse_back_pruning_empty_subtrees(path, None);
        if old_value.is_none() {
            let msg = format!(
                "Could not remove a suggestion entry id at {path} because it does not exist."
            );
            warn!("{msg}");
        }
    }

    /// Sets the value at `path` to `value` and returns the replaced value (returns `None` if there
    /// was no node at `path`). Then visits nodes on the `path` in reverse order and removes every
    /// visited empty leaf node from its parent.
    ///
    /// A node is defined as empty when it contains a `None` value. A node is a leaf when its
    /// [`is_leaf`] method returns `true`.
    ///
    /// The function is optimized to not create new empty nodes if they would be deleted by the
    /// function before it returns.
    fn replace_value_and_traverse_back_pruning_empty_subtrees<P, I>(
        &mut self,
        path: P,
        value: Option<entry::Id>,
    ) -> Option<entry::Id>
    where
        P: IntoIterator<Item = I>,
        I: Into<ImString>,
    {
        let mut swapped_value = value;
        swap_value_and_traverse_back_pruning_empty_subtrees(
            &mut self.tree,
            path,
            &mut swapped_value,
        );
        swapped_value
    }
}



// ======================
// === HierarchyIndex ===
// ======================

/// A map from "parent" to "children" entries. It could be called a "self-type index", as it
/// primarily stores self-type relations between entries. But it also stores Type-Module relations.
///
/// We store a list of "children" entry ids for each key in the map. "Children" are either
/// Constructors and Methods of a specific type, or Types defined in the module. We populate this
/// index when constructing the suggestion database and keep it up-to-date when applying updates.
///
/// This index is needed to render documentation pages when the Module's documentation page includes
/// information about every Type and Method defined there, and the documentation page of the Type
/// displays all Constructors and Methods defined for this type.
#[derive(Clone, Debug, Default)]
struct HierarchyIndex {
    inner:   HashMap<entry::Id, HashSet<entry::Id>>,
    /// Orphans are entries that are not yet in the index, but are referenced by other entries.
    /// For example, if we have a Type entry, but we haven't yet received its Module entry, we
    /// store the Type entry as an orphan of Module entry. When we receive the Module entry, we
    /// add all orphans to the index. This way we do not rely on the order of updates from the
    /// Engine.
    ///
    /// Key is the parent entry name, value is a set of orphan entries.
    orphans: HashMap<QualifiedName, HashSet<entry::Id>>,
}

impl HierarchyIndex {
    /// Add a new entry to the index. If the entry already exists, do nothing.
    ///
    /// If the entry has [`Entry::self_type`], we use it as a key in the index. If the entry is a
    /// [`Kind::Type`], we use its parent module as a key.
    pub fn add(
        &mut self,
        id: entry::Id,
        entry: &Entry,
        qualified_name_to_id_map: &QualifiedNameToIdMap,
    ) {
        if let Some(self_type) = &entry.self_type {
            if let Some(self_type_id) = qualified_name_to_id_map.get(self_type) {
                self.inner.entry(self_type_id).or_default().insert(id);
            } else {
                self.orphans.entry(self_type.clone()).or_default().insert(id);
            }
        }
        if entry.kind == Kind::Type {
            if let Some(parent_module) = entry.parent_module() {
                if let Some(parent_id) = qualified_name_to_id_map.get(&parent_module) {
                    self.inner.entry(parent_id).or_default().insert(id);
                } else {
                    let parent_module_name = parent_module.to_owned();
                    self.orphans.entry(parent_module_name).or_default().insert(id);
                }
            } else {
                let entry_name = &entry.name;
                warn!("Could not find a parent module for type {entry_name}.");
            }
        }
        if let Some(orphans) = self.orphans.remove(&entry.qualified_name()) {
            self.inner.entry(id).or_default().extend(orphans);
        }
    }

    /// Remove the entry from the index, as it is removed from the database.
    pub fn remove(&mut self, id: entry::Id) {
        self.inner.remove(&id);
        self.remove_from_parent(id);
    }

    /// Remove the entry from "children" collections, but leave it as a parent. It is needed when
    /// we want to modify some entries. The modification doesn't change the children (so we preserve
    /// the key), but it can change the parent of the altered entry, so we remove it from any
    /// parent.
    pub fn remove_from_parent(&mut self, id: entry::Id) {
        for (_, children) in self.inner.iter_mut() {
            children.remove(&id);
        }
    }

    /// Get all "children" of the entry with the given id. Returns a set of Methods and Constructors
    /// of the Type entry, or a set of Types defined in the Module entry. Returns [`None`] for
    /// other entries.
    pub fn get(&self, id: &entry::Id) -> Option<&HashSet<entry::Id>> {
        self.inner.get(id)
    }

    /// Whether the index is empty.
    #[cfg(test)]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// The length of the index.
    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}


// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Eq, Fail, PartialEq)]
#[fail(display = "The suggestion with id {} has not been found in the database.", _0)]
pub struct NoSuchEntry(pub SuggestionId);



// ====================
// === Notification ===
// ====================

/// Notification about change in a suggestion database,
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Notification {
    /// The database has been updated.
    Updated,
}



// ================
// === Database ===
// ================

/// The Suggestion Database
///
/// This is database of possible suggestions in Searcher. To achieve best performance, some
/// often-called Language Server methods returns the list of keys of this database instead of the
/// whole entries. Additionally the suggestions contains information about functions and their
/// argument names and types.
#[derive(Clone, Debug, Default)]
pub struct SuggestionDatabase {
    entries:                  RefCell<HashMap<entry::Id, Rc<Entry>>>,
    qualified_name_to_id_map: RefCell<QualifiedNameToIdMap>,
    hierarchy_index:          RefCell<HierarchyIndex>,
    examples:                 RefCell<Vec<Rc<Example>>>,
    version:                  Cell<SuggestionsDatabaseVersion>,
    notifications:            notification::Publisher<Notification>,
}

impl SuggestionDatabase {
    /// Create a database with no entries.
    pub fn new_empty() -> Self {
        default()
    }

    /// Create a database filled with entries provided by the given iterator.
    pub fn new_from_entries<'a>(
        entries: impl IntoIterator<Item = (&'a SuggestionId, &'a Entry)>,
    ) -> Self {
        let ret = Self::new_empty();
        let entries = entries.into_iter().map(|(id, entry)| (*id, Rc::new(entry.clone())));
        ret.entries.borrow_mut().extend(entries);
        ret
    }

    /// Create a new database which will take its initial content from the Language Server.
    pub async fn create_synchronized(
        language_server: &language_server::Connection,
    ) -> FallibleResult<Self> {
        let response = language_server.client.get_suggestions_database().await?;
        Ok(Self::from_ls_response(response))
    }

    /// Create a new database model from response received from the Language Server.
    fn from_ls_response(response: language_server::response::GetSuggestionDatabase) -> Self {
        let mut entries = HashMap::new();
        let mut qualified_name_to_id_map = QualifiedNameToIdMap::default();
        let mut hierarchy_index = HierarchyIndex::default();
        for ls_entry in response.entries {
            let id = ls_entry.id;
            let entry = Entry::from_ls_entry(ls_entry.suggestion);
            qualified_name_to_id_map.set_and_warn_if_existed(&entry.qualified_name(), id);
            entries.insert(id, Rc::new(entry));
        }
        for (id, entry) in &entries {
            hierarchy_index.add(*id, entry, &qualified_name_to_id_map);
        }
        //TODO[ao]: This is a temporary solution. Eventually, we should gather examples from the
        //          available modules documentation. (https://github.com/enso-org/ide/issues/1011)
        let examples = example::EXAMPLES.iter().cloned().map(Rc::new).collect_vec();
        Self {
            entries:                  RefCell::new(entries),
            qualified_name_to_id_map: RefCell::new(qualified_name_to_id_map),
            hierarchy_index:          RefCell::new(hierarchy_index),
            examples:                 RefCell::new(examples),
            version:                  Cell::new(response.current_version),
            notifications:            default(),
        }
    }

    /// Subscribe for notifications about changes in the database.
    pub fn subscribe(&self) -> Subscriber<Notification> {
        self.notifications.subscribe()
    }

    /// Get suggestion entry by id.
    pub fn lookup(&self, id: entry::Id) -> Result<Rc<Entry>, NoSuchEntry> {
        self.entries.borrow().get(&id).cloned().ok_or(NoSuchEntry(id))
    }

    /// Apply the update event to the database.
    pub fn apply_update_event(&self, event: SuggestionDatabaseUpdatesEvent) {
        for update in event.updates {
            let mut entries = self.entries.borrow_mut();
            let mut qn_to_id_map = self.qualified_name_to_id_map.borrow_mut();
            let mut hierarchy_index = self.hierarchy_index.borrow_mut();
            match update {
                entry::Update::Add { id, suggestion } => match (*suggestion).try_into() {
                    Ok(entry) => {
                        qn_to_id_map.set_and_warn_if_existed(&Entry::qualified_name(&entry), id);
                        hierarchy_index.add(id, &entry, &qn_to_id_map);
                        entries.insert(id, Rc::new(entry));
                    }
                    Err(err) => {
                        error!("Discarding update for {id}: {err}")
                    }
                },
                entry::Update::Remove { id } => {
                    let removed = entries.remove(&id);
                    match removed {
                        Some(entry) => {
                            qn_to_id_map.remove_and_warn_if_did_not_exist(&entry.qualified_name());
                            hierarchy_index.remove(id);
                        }

                        None => {
                            let msg = format!(
                                "Received a suggestion database 'Remove' event for a nonexistent \
                                entry id: {id}."
                            );
                            error!("{msg}");
                        }
                    }
                }
                entry::Update::Modify { id, modification, .. } => {
                    if let Some(old_entry) = entries.get_mut(&id) {
                        let entry = Rc::make_mut(old_entry);
                        qn_to_id_map.remove_and_warn_if_did_not_exist(&entry.qualified_name());
                        hierarchy_index.remove_from_parent(id);
                        let errors = entry.apply_modifications(*modification);
                        hierarchy_index.add(id, entry, &qn_to_id_map);
                        qn_to_id_map.set_and_warn_if_existed(&entry.qualified_name(), id);
                        for error in errors {
                            error!("Error when applying update for entry {id}: {error:?}");
                        }
                    } else {
                        error!("Received Modify event for nonexistent id: {id}");
                    }
                }
            };
        }
        self.version.set(event.current_version);
        self.notifications.notify(Notification::Updated);
    }

    /// Look up given id in the suggestion database and if it is a known method obtain a pointer to
    /// it.
    pub fn lookup_method_ptr(
        &self,
        id: SuggestionId,
    ) -> FallibleResult<language_server::MethodPointer> {
        let entry = self.lookup(id)?;
        language_server::MethodPointer::try_from(entry.as_ref())
    }

    /// Search the database for an entry of method identified by given id.
    pub fn lookup_method(&self, id: MethodId) -> Option<Rc<Entry>> {
        self.entries.borrow().values().cloned().find(|entry| entry.method_id().contains(&id))
    }

    /// Search the database for an entry at `fully_qualified_name`. The parameter is expected to be
    /// composed of segments separated by the [`ACCESS`] character.
    pub fn lookup_by_qualified_name_str(&self, qualified_name_str: &str) -> Option<Rc<Entry>> {
        let qualified_name = QualifiedName::from_text(qualified_name_str).ok()?;
        let (_, entry) = self.lookup_by_qualified_name(&qualified_name)?;
        Some(entry)
    }

    /// Search the database for an entry at `name` consisting fully qualified name segments, e.g.
    /// [`model::QualifiedName`].
    pub fn lookup_by_qualified_name<'a>(
        &self,
        name: impl Into<QualifiedNameRef<'a>>,
    ) -> Option<(SuggestionId, Rc<Entry>)> {
        let name = name.into();
        let segments = name.segments().map(CloneRef::clone_ref);
        let id = self.qualified_name_to_id_map.borrow().get(segments);
        id.and_then(|id| Some((id, self.lookup(id).ok()?)))
    }

    /// Search the database for entries with given name and visible at given location in module.
    pub fn lookup_at(&self, name: impl Str, location: &ModuleSpan) -> Vec<Rc<Entry>> {
        self.entries
            .borrow()
            .values()
            .filter(|entry| entry.matches_name(name.as_ref()) && entry.is_visible_at(location))
            .cloned()
            .collect()
    }

    /// Search the database for Local or Function entries with given name and visible at given
    /// location in module.
    pub fn lookup_locals_at(&self, name: impl Str, location: &ModuleSpan) -> Vec<Rc<Entry>> {
        self.entries
            .borrow()
            .values()
            .cloned()
            .filter(|entry| {
                let is_local = entry.kind == Kind::Function || entry.kind == Kind::Local;
                is_local && entry.matches_name(name.as_ref()) && entry.is_visible_at(location)
            })
            .collect()
    }

    /// Search the database for Method entry with given name and defined for given module.
    pub fn lookup_module_method(
        &self,
        name: impl Str,
        module: &QualifiedName,
    ) -> Option<Rc<Entry>> {
        self.entries.borrow().values().cloned().find(|entry| {
            let is_method = entry.kind == Kind::Method;
            let is_defined_for_module = entry.has_self_type(module);
            is_method && is_defined_for_module && entry.matches_name(name.as_ref())
        })
    }

    /// Lookup hierarchy index for given id. See [`HierarchyIndex`] for more information.
    pub fn lookup_hierarchy(&self, id: entry::Id) -> Result<HashSet<entry::Id>, NoSuchEntry> {
        let hierarchy = self.hierarchy_index.borrow();
        let children = hierarchy.get(&id).cloned().ok_or(NoSuchEntry(id))?;
        Ok(children)
    }

    /// Lookup documentation of the given entry.
    pub fn documentation_for_entry(&self, id: entry::Id) -> EntryDocumentation {
        match EntryDocumentation::new(self, &id) {
            Ok(docs) => docs,
            Err(err) => {
                error!("Error when generating documentation for entry {id}: {}", err);
                default()
            }
        }
    }

    /// An iterator over all examples gathered from suggestions.
    ///
    /// If the database was modified during iteration, the iterator does not panic, but may return
    /// unpredictable result (a mix of old and new values).
    pub fn iterate_examples(&self) -> impl Iterator<Item = Rc<Example>> + '_ {
        let indices = 0..self.examples.borrow().len();
        indices.filter_map(move |i| self.examples.borrow().get(i).cloned())
    }

    /// Get vector of all ids of available entries.
    pub fn keys(&self) -> Vec<entry::Id> {
        self.entries.borrow().keys().cloned().collect()
    }

    /// Put the entry to the database. Using this function likely breaks the synchronization between
    /// Language Server and IDE, and should be used only in tests.
    pub fn put_entry(&self, id: entry::Id, entry: Entry) {
        let mut qn_to_id_map = self.qualified_name_to_id_map.borrow_mut();
        qn_to_id_map.set_and_warn_if_existed(&entry.qualified_name(), id);
        self.hierarchy_index.borrow_mut().add(id, &entry, &qn_to_id_map);
        self.entries.borrow_mut().insert(id, Rc::new(entry));
    }
}

impl From<language_server::response::GetSuggestionDatabase> for SuggestionDatabase {
    fn from(database: language_server::response::GetSuggestionDatabase) -> Self {
        Self::from_ls_response(database)
    }
}



// ===============
// === Helpers ===
// ===============


// === QualifiedNameToIdMap helpers ===

/// Swaps the value at `path` in `node` with `value` (sets `value` to `None` if there was no node
/// at `path`). Then visits nodes on the `path` in reverse order and removes every visited empty
/// leaf node from its parent.
///
/// In this function, a node is defined as empty when it contains a `None` value. A node is a leaf
/// when its [`is_leaf`] method returns `true`.
///
/// The function is optimized to not create new empty nodes if they would be deleted by the
/// function before it returns.
///
/// This function is a helper of the
/// [`QualifiedNameToIdMap::replace_value_and_traverse_back_pruning_empty_subtrees`] method. It
/// performs the same operation but the replaced value is swapped with `value` instead of being
/// returned, and the `path` iterator is mutable. This allows the function to call itself
/// recursively.
fn swap_value_and_traverse_back_pruning_empty_subtrees<P, I>(
    node: &mut HashMapTree<ImString, Option<entry::Id>>,
    path: P,
    value: &mut Option<entry::Id>,
) where
    P: IntoIterator<Item = I>,
    I: Into<ImString>,
{
    use std::collections::hash_map::Entry;
    let mut path = path.into_iter();
    match path.next() {
        None => std::mem::swap(&mut node.value, value),
        Some(key) => match node.branches.entry(key.into()) {
            Entry::Occupied(mut entry) => {
                let node = entry.get_mut();
                swap_value_and_traverse_back_pruning_empty_subtrees(node, path, value);
                if node.value.is_none() && node.is_leaf() {
                    entry.remove_entry();
                }
            }
            Entry::Vacant(entry) =>
                if let Some(v) = value.take() {
                    entry.insert(default()).set(path, Some(v));
                },
        },
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub mod test {
    use super::*;

    use enso_executor::test_utils::TestWithLocalPoolExecutor;

    use double_representation::name::NamePath;
    use engine_protocol::language_server::FieldUpdate;
    use engine_protocol::language_server::SuggestionEntry;
    use engine_protocol::language_server::SuggestionsDatabaseEntry;
    use engine_protocol::language_server::SuggestionsDatabaseModification;
    use futures::stream::StreamExt;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    const GIBBERISH_MODULE_NAME: &str = "local.Gibberish.Модул\u{200f}ь!\0@&$)(*!)\t";

    #[test]
    fn initialize_database() {
        // Empty db
        let response = language_server::response::GetSuggestionDatabase {
            entries:         vec![],
            current_version: 123,
        };
        let db = SuggestionDatabase::from_ls_response(response);
        assert!(db.entries.borrow().is_empty());
        assert!(db.hierarchy_index.borrow().is_empty());
        assert_eq!(db.version.get(), 123);

        // Non-empty db
        let entry = SuggestionEntry::Constructor {
            name:                   "TextAtom".to_string(),
            module:                 "test.TestProject.TestModule".to_string(),
            arguments:              vec![],
            return_type:            "test.TestProject.TestModule.TestAtom".to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
            reexport:               None,
        };
        let db_entry = SuggestionsDatabaseEntry { id: 12, suggestion: entry };
        let response = language_server::response::GetSuggestionDatabase {
            entries:         vec![db_entry],
            current_version: 456,
        };
        let db = SuggestionDatabase::from_ls_response(response);
        assert_eq!(db.entries.borrow().len(), 1);
        assert!(db.hierarchy_index.borrow().is_empty());
        assert_eq!(*db.lookup(12).unwrap().name, "TextAtom".to_string());
        assert_eq!(db.version.get(), 456);
    }

    #[test]
    fn applying_update() {
        let mut fixture = TestWithLocalPoolExecutor::set_up();
        let db = mock::standard_db_mock();
        let replaced_entry = QualifiedName::from_text("Standard.Base.Number").unwrap();
        let (replaced_id, _) = db.lookup_by_qualified_name(&replaced_entry).unwrap();
        let new_entry = SuggestionEntry::Constructor {
            name:                   "NewEntry".to_owned(),
            module:                 "test.TestProject.TestModule".to_owned(),
            arguments:              vec![],
            return_type:            "test.TestProject.TestModule.TestAtom".to_owned(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
            reexport:               None,
        };
        let new_entry_modification = SuggestionsDatabaseModification {
            module: Some(FieldUpdate::set("test.TestProject.TestModule2".to_owned())),
            ..default()
        };

        let mut notifications = db.subscribe().boxed_local();
        notifications.expect_pending();

        // Remove
        let remove_update = entry::Update::Remove { id: replaced_id };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![remove_update],
            current_version: 2,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        assert_eq!(db.lookup(replaced_id), Err(NoSuchEntry(replaced_id)));
        assert_eq!(db.version.get(), 2);

        // Add
        let add_update =
            entry::Update::Add { id: replaced_id, suggestion: Box::new(new_entry) };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![add_update],
            current_version: 3,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(replaced_id).unwrap().name, "NewEntry");
        assert_eq!(db.version.get(), 3);

        // Modify
        let modify_update = entry::Update::Modify {
            id:           replaced_id,
            external_id:  None,
            modification: Box::new(new_entry_modification),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![modify_update],
            current_version: 4,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(
            db.lookup(replaced_id).unwrap().defined_in.to_string(),
            "test.TestProject.TestModule2"
        );
        assert_eq!(db.version.get(), 4);
    }

    /// Looks up an entry at `fully_qualified_name` in the `db` and verifies the name of the
    /// retrieved entry.
    fn lookup_and_verify_result_name(db: &SuggestionDatabase, fully_qualified_name: &str) {
        let lookup = db.lookup_by_qualified_name_str(fully_qualified_name);
        assert_eq!(lookup.unwrap().qualified_name().to_string(), fully_qualified_name);
    }

    /// Looks up an entry at `fully_qualified_name` in the `db` and verifies that the lookup result
    /// is [`None`].
    fn lookup_and_verify_empty_result(db: &SuggestionDatabase, fully_qualified_name: &str) {
        let lookup = db.lookup_by_qualified_name_str(fully_qualified_name);
        assert_eq!(lookup, None);
    }

    fn db_entry(id: SuggestionId, suggestion: SuggestionEntry) -> SuggestionsDatabaseEntry {
        SuggestionsDatabaseEntry { id, suggestion }
    }

    /// Initializes a [`SuggestionDatabase`] with a few sample entries of varying [`entry::Kind`]
    /// and tests the results of the [`SuggestionDatabase::lookup_by_fully_qualified_name`] method
    /// when called on that database.
    #[test]
    fn lookup_by_fully_qualified_name_in_db_created_from_ls_response() {
        // Initialize a suggestion database with sample entries.
        let entry0 = SuggestionEntry::Type {
            name:                   "TestType".to_string(),
            module:                 "test.TestProject.TestModule".to_string(),
            params:                 vec![],
            parent_type:            Some("Any".to_string()),
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let entry1 = SuggestionEntry::Constructor {
            name:                   "TestAtom".to_string(),
            module:                 "test.TestProject.TestModule".to_string(),
            arguments:              vec![],
            return_type:            "test.TestProject.TestModule.TestType".to_string(),
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let entry2 = SuggestionEntry::Method {
            name:                   "create_process".to_string(),
            module:                 "Standard.Builtins.Main".to_string(),
            self_type:              "Standard.Builtins.Main.System".to_string(),
            arguments:              vec![],
            return_type:            "Standard.Builtins.Main.System_Process_Result".to_string(),
            is_static:              false,
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let entry3 = SuggestionEntry::Module {
            module:                 "local.Unnamed_6.Main".to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            reexport:               None,
        };
        let entry4 = SuggestionEntry::Local {
            module:                 "local.Unnamed_6.Main".to_string(),
            name:                   "operator1".to_string(),
            return_type:            "Standard.Base.Data.Vector.Vector".to_string(),
            external_id:            None,
            scope:                  (default()..=default()).into(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
        };
        let entry5 = SuggestionEntry::Function {
            module:                 "NewProject.NewModule".to_string(),
            name:                   "testFunction1".to_string(),
            arguments:              vec![],
            return_type:            "Standard.Base.Data.Vector.Vector".to_string(),
            scope:                  (default()..=default()).into(),
            external_id:            None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
        };
        let ls_response = language_server::response::GetSuggestionDatabase {
            entries:         vec![
                db_entry(0, entry0),
                db_entry(1, entry1),
                db_entry(2, entry2),
                db_entry(3, entry3),
                db_entry(4, entry4),
                db_entry(5, entry5),
            ],
            current_version: 1,
        };
        let db = SuggestionDatabase::from_ls_response(ls_response);

        // Check that the entries used to initialize the database can be found using the
        // `lookup_by_fully_qualified_name` method.
        lookup_and_verify_result_name(&db, "test.TestProject.TestModule.TestType");
        lookup_and_verify_result_name(&db, "test.TestProject.TestModule.TestType.TestAtom");
        lookup_and_verify_result_name(&db, "Standard.Builtins.System.create_process");
        lookup_and_verify_result_name(&db, "local.Unnamed_6");
        lookup_and_verify_result_name(&db, "local.Unnamed_6.operator1");
        lookup_and_verify_result_name(&db, "NewProject.NewModule.testFunction1");

        // Check that looking up names not added to the database does not return entries.
        lookup_and_verify_empty_result(&db, "test.TestProject.TestModule");
        lookup_and_verify_empty_result(&db, "Standard.Builtins.create_process");
        lookup_and_verify_empty_result(&db, "local.NoSuchEntry");
    }

    // Check that the suggestion database doesn't panic when quering invalid qualified names.
    #[test]
    fn lookup_by_fully_qualified_name_with_invalid_names() {
        let db = SuggestionDatabase::new_empty();
        let _ = db.lookup_by_qualified_name_str("");
        let _ = db.lookup_by_qualified_name_str(".");
        let _ = db.lookup_by_qualified_name_str("..");
        let _ = db.lookup_by_qualified_name_str("Empty.Entry.");
        let _ = db.lookup_by_qualified_name_str("Empty..Entry");
        let _ = db.lookup_by_qualified_name_str(GIBBERISH_MODULE_NAME);
    }

    // Initialize suggestion database with some invalid entries and check if it doesn't panics.
    #[test]
    fn initialize_database_with_invalid_entries() {
        // Prepare some nonsense inputs from the Engine.
        let entry_with_empty_name = SuggestionEntry::Constructor {
            name:                   "".to_string(),
            module:                 "Empty.Entry".to_string(),
            arguments:              vec![],
            return_type:            "".to_string(),
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let empty_entry = SuggestionEntry::Local {
            module:                 "".to_string(),
            name:                   "".to_string(),
            return_type:            "".to_string(),
            external_id:            None,
            scope:                  (default()..=default()).into(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
        };
        let gibberish_entry = SuggestionEntry::Module {
            module:                 GIBBERISH_MODULE_NAME.to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            reexport:               None,
        };

        let ls_response = language_server::response::GetSuggestionDatabase {
            entries:         vec![
                db_entry(1, entry_with_empty_name),
                db_entry(2, empty_entry),
                db_entry(3, gibberish_entry),
            ],
            current_version: 1,
        };
        let _ = SuggestionDatabase::from_ls_response(ls_response);
    }

    /// Apply a [`SuggestionDatabaseUpdatesEvent`] to a [`SuggestionDatabase`] initialized with
    /// sample data, then test the results of calling the
    /// [`SuggestionDatabase::lookup_by_fully_qualified_name`] method on that database.
    #[test]
    fn lookup_by_fully_qualified_name_after_db_update() {
        let db = mock::standard_db_mock();
        let entry_with_doc_change =
            QualifiedName::from_text("local.Project.Submodule.TestType").unwrap();
        let entry_with_mod_change =
            QualifiedName::from_text("local.Project.Submodule.module_method").unwrap();
        let entry_removed = QualifiedName::from_text("Standard.Base.Number").unwrap();
        let (entry_with_doc_change_id, _) =
            db.lookup_by_qualified_name(&entry_with_doc_change).unwrap();
        let (entry_with_mod_change_id, _) =
            db.lookup_by_qualified_name(&entry_with_mod_change).unwrap();
        let (entry_removed_id, _) = db.lookup_by_qualified_name(&entry_removed).unwrap();
        let entry_added_id = 123;
        // Modify the database contents by applying an update event.
        let entry_doc_change = Box::new(SuggestionsDatabaseModification {
            arguments:              vec![],
            module:                 None,
            self_type:              None,
            return_type:            None,
            documentation:          Some(FieldUpdate::set("New doc".to_string())),
            documentation_sections: None,
            scope:                  None,
            reexport:               None,
        });
        let entry_mod_change = Box::new(SuggestionsDatabaseModification {
            arguments:              vec![],
            module:                 Some(FieldUpdate::set(
                "new_project.NewProject.NewModule".to_string(),
            )),
            self_type:              None,
            return_type:            None,
            documentation:          None,
            documentation_sections: None,
            scope:                  None,
            reexport:               None,
        });
        let new_entry = SuggestionEntry::Module {
            module:                 "local.Unnamed_6.Main".to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            reexport:               None,
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![
                entry::Update::Modify {
                    id:           entry_with_doc_change_id,
                    external_id:  None,
                    modification: entry_doc_change,
                },
                entry::Update::Modify {
                    id:           entry_with_mod_change_id,
                    external_id:  None,
                    modification: entry_mod_change,
                },
                entry::Update::Remove { id: entry_removed_id },
                entry::Update::Add { id: entry_added_id, suggestion: Box::new(new_entry) },
            ],
            current_version: 2,
        };
        db.apply_update_event(update);

        // Check the results of `lookup_by_fully_qualified_name` after the update.
        lookup_and_verify_empty_result(&db, &entry_removed.to_string());
        lookup_and_verify_result_name(&db, &entry_with_doc_change.to_string());
        lookup_and_verify_result_name(&db, &entry_with_mod_change.to_string());
        lookup_and_verify_empty_result(&db, "Standard.Builtins.System.create_process");
        lookup_and_verify_result_name(&db, "local.Unnamed_6");
    }

    /// Initialize a [`SuggestionDatabase`] with a sample entry, then apply an update removing that
    /// entry and another update adding a different entry at the same [`entry::Id`]. Test that the
    /// [`SuggestionDatabase::lookup_by_fully_qualified_name`] method returns correct results after
    /// this scenario is finished.
    #[test]
    fn lookup_by_fully_qualified_name_after_db_update_reuses_id() {
        // Initialize a suggestion database with a sample entry.
        let entry1 = SuggestionEntry::Constructor {
            name:                   "TextAtom".to_string(),
            module:                 "TestProject.TestModule".to_string(),
            arguments:              vec![],
            return_type:            "TestAtom".to_string(),
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let id = 1;
        let response = language_server::response::GetSuggestionDatabase {
            entries:         vec![db_entry(id, entry1)],
            current_version: 1,
        };
        let db = SuggestionDatabase::from_ls_response(response);

        // Apply a DB update removing the entry.
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Remove { id }],
            current_version: 2,
        };
        db.apply_update_event(update);

        // Apply a DB update adding a different entry at the same `id`.
        let entry2 = SuggestionEntry::Module {
            module:                 "local.Unnamed_6.Main".to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            reexport:               None,
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Add { id, suggestion: Box::new(entry2) }],
            current_version: 3,
        };
        db.apply_update_event(update);

        // Check that the first entry is not visible in the DB and the second one is visible.
        lookup_and_verify_empty_result(&db, "TestProject.TestModule.TextAtom");
        lookup_and_verify_result_name(&db, "local.Unnamed_6");
    }

    /// Test the [`QualifiedNameToIdMap::replace_value_and_traverse_back_pruning_empty_subtrees`]
    /// method when called repeatedly with different [`entry::Id`] values. The test is done with a
    /// simple path (one segment) and on a more complex path (two segments).
    #[test]
    fn replace_value_and_traverse_back_pruning_empty_subtrees() {
        let paths = vec!["A", "A.B"];
        for path in paths {
            let qualified_name: NamePath = path.split('.').map(ImString::new).collect();
            let qn_to_id_map: RefCell<QualifiedNameToIdMap> = default();
            let expected_result = RefCell::new(None);
            let replace_and_verify_result = |value| {
                let mut map = qn_to_id_map.borrow_mut();
                let result = map
                    .replace_value_and_traverse_back_pruning_empty_subtrees(&qualified_name, value);
                assert_eq!(result, *expected_result.borrow());
                *expected_result.borrow_mut() = value;
            };
            assert_eq!(qn_to_id_map.borrow().get(&qualified_name), None);
            replace_and_verify_result(None);
            replace_and_verify_result(Some(1));
            replace_and_verify_result(Some(2));
            assert_eq!(qn_to_id_map.borrow().get(&qualified_name), Some(2));
            replace_and_verify_result(None);
            assert_eq!(qn_to_id_map.borrow().get(&qualified_name), None);
            replace_and_verify_result(None);
            assert_eq!(qn_to_id_map.borrow().get(&qualified_name), None);
        }
    }

    /// Test the [`QualifiedNameToIdMap::replace_value_and_traverse_back_pruning_empty_subtrees`]
    /// method on paths sharing a common prefix. Verify that replacing an id at a path does not
    /// interfere with id values stored at other paths, even if the paths share a common prefix.
    #[test]
    fn replace_value_and_traverse_back_pruning_empty_subtrees_for_overlapping_paths() {
        let mut map: QualifiedNameToIdMap = default();
        let paths = &["A.B", "A.B.C", "A", "A.X.Y", "A.X"];
        let values = &[1, 2, 3, 4, 5].map(Some);
        for (path, value) in paths.iter().zip(values) {
            let path: NamePath = path.split('.').map(ImString::new).collect();
            assert_eq!(map.get(&path), None);
            let result = map.replace_value_and_traverse_back_pruning_empty_subtrees(&path, *value);
            assert_eq!(result, None);
            assert_eq!(map.get(&path), *value);
        }
        for (path, value) in paths.iter().zip(values) {
            let path: NamePath = path.split('.').map(ImString::new).collect();
            assert_eq!(map.get(&path), *value);
            let result = map.replace_value_and_traverse_back_pruning_empty_subtrees(&path, None);
            assert_eq!(result, *value);
            assert_eq!(map.get(&path), None);
        }
    }


    // === Hierarchy Index tests ===

    fn lookup_id_by_name(db: &SuggestionDatabase, name: &str) -> Option<entry::Id> {
        let name = QualifiedName::from_text(name).ok()?;
        let (id, _) = db.lookup_by_qualified_name(&name)?;
        Some(id)
    }

    /// Verify that the entry with the given `name` has `expected` children. Children are defined as
    /// methods or constructors of the Type or Types defined in the module.
    fn verify_hierarchy_index(db: &SuggestionDatabase, name: &str, expected: &[&str]) {
        let id = lookup_id_by_name(db, name);
        let id = id.unwrap_or_else(|| panic!("No entry with name {}", name));
        let hierarchy_index = db.hierarchy_index.borrow();
        let actual_ids = hierarchy_index.get(&id);
        let actual_ids = actual_ids.unwrap_or_else(|| panic!("No entry for id {}", id));
        let id_from_name = |name: &&str| {
            lookup_id_by_name(db, name).unwrap_or_else(|| panic!("No entry with name {}", name))
        };
        let expected_ids: HashSet<_> = expected.iter().map(id_from_name).collect();
        let name_from_id = |id: &entry::Id| {
            db.lookup(*id).map(|e| e.name.clone()).unwrap_or_else(|_| "<not found>".to_string())
        };
        let actual = actual_ids.iter().map(name_from_id).collect::<Vec<_>>();
        assert_eq!(actual_ids, &expected_ids, "Actual {:?} != expected {:?}", actual, expected);
    }

    /// Test that hierarchy index is populated when the database is created from the language server
    /// response.
    #[test]
    fn hierarchy_index_is_populated_after_construction() {
        // Initialize a suggestion database with sample entries.
        let entry0 = SuggestionEntry::Type {
            name:                   "TestType".to_string(),
            module:                 "test.TestProject.TestModule".to_string(),
            params:                 vec![],
            parent_type:            Some("Any".to_string()),
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let entry1 = SuggestionEntry::Constructor {
            name:                   "Empty".to_string(),
            module:                 "test.TestProject.TestModule".to_string(),
            arguments:              vec![],
            return_type:            "test.TestProject.TestModule.TestType".to_string(),
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let entry2 = SuggestionEntry::Method {
            name:                   "test_method".to_string(),
            module:                 "test.TestProject.TestModule".to_string(),
            self_type:              "test.TestProject.TestModule.TestType".to_string(),
            arguments:              vec![],
            return_type:            "Standard.Builtins.Main.System_Process_Result".to_string(),
            is_static:              false,
            reexport:               None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            external_id:            None,
        };
        let entry3 = SuggestionEntry::Module {
            module:                 "test.TestProject.TestModule".to_string(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
            reexport:               None,
        };
        let entry4 = SuggestionEntry::Local {
            module:                 "test.TestProject.TestModule".to_string(),
            name:                   "operator1".to_string(),
            return_type:            "Standard.Base.Data.Vector.Vector".to_string(),
            external_id:            None,
            scope:                  (default()..=default()).into(),
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
        };
        let entry5 = SuggestionEntry::Function {
            module:                 "test.TestProject.TestModule".to_string(),
            name:                   "testFunction1".to_string(),
            arguments:              vec![],
            return_type:            "Standard.Base.Data.Vector.Vector".to_string(),
            scope:                  (default()..=default()).into(),
            external_id:            None,
            documentation:          None,
            documentation_html:     None,
            documentation_sections: default(),
        };
        let ls_response = language_server::response::GetSuggestionDatabase {
            entries:         vec![
                db_entry(0, entry0),
                db_entry(1, entry1),
                db_entry(2, entry2),
                db_entry(3, entry3),
                db_entry(4, entry4),
                db_entry(5, entry5),
            ],
            current_version: 1,
        };
        let db = SuggestionDatabase::from_ls_response(ls_response);

        // Verify that the hierarchy index is populated.
        let hierarchy_index = db.hierarchy_index.borrow();
        assert_eq!(hierarchy_index.len(), 2);
        verify_hierarchy_index(&db, "test.TestProject.TestModule.TestType", &[
            "test.TestProject.TestModule.TestType.test_method",
            "test.TestProject.TestModule.TestType.Empty",
        ]);
        verify_hierarchy_index(&db, "test.TestProject.TestModule", &[
            "test.TestProject.TestModule.TestType",
        ]);
    }

    /// Verify that the hierarchy index is populated for `standard_db_mock`. Next test cases use
    /// standard db mock for initial database state, so we need to make sure it is populated as
    /// expected.
    #[test]
    fn hierarchy_index_of_standard_db_mock() {
        let db = mock::standard_db_mock();
        assert_eq!(db.hierarchy_index.borrow().len(), 4);
        verify_hierarchy_index(&db, "Standard.Base", &[
            "Standard.Base.Maybe",
            "Standard.Base.Number",
            "Standard.Base.Boolean",
        ]);
        verify_hierarchy_index(&db, "Standard.Base.Maybe", &[
            "Standard.Base.Maybe.Some",
            "Standard.Base.Maybe.None",
            "Standard.Base.Maybe.is_some",
        ]);
        verify_hierarchy_index(&db, "local.Project.Submodule", &[
            "local.Project.Submodule.TestType",
            "local.Project.Submodule.module_method",
        ]);
        verify_hierarchy_index(&db, "local.Project.Submodule.TestType", &[
            "local.Project.Submodule.TestType.static_method",
        ]);
    }

    /// Verify that the hierarchy index is correctly updated when adding, removing and modifying
    /// entries.
    #[test]
    fn hierarchy_index_is_updated_after_db_update() {
        let db = mock::standard_db_mock();
        let added_id = 30;


        // === Add new method ===

        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Add {
                id:         added_id,
                suggestion: Box::new(SuggestionEntry::Method {
                    name:                   "new_method".to_string(),
                    module:                 "local.Project.Submodule".to_string(),
                    self_type:              "local.Project.Submodule.TestType".to_string(),
                    arguments:              vec![],
                    return_type:            "Some.Type".to_string(),
                    is_static:              false,
                    reexport:               None,
                    documentation:          None,
                    documentation_html:     None,
                    documentation_sections: default(),
                    external_id:            None,
                }),
            }],
            current_version: 1,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 4);
        verify_hierarchy_index(&db, "local.Project.Submodule.TestType", &[
            "local.Project.Submodule.TestType.static_method",
            "local.Project.Submodule.TestType.new_method",
        ]);


        // === Change the self type of the added method ===

        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Modify {
                id:           added_id,
                external_id:  None,
                modification: Box::new(SuggestionsDatabaseModification {
                    arguments:              vec![],
                    module:                 None,
                    self_type:              Some(FieldUpdate::set(
                        "Standard.Base.Maybe".to_string(),
                    )),
                    return_type:            None,
                    documentation:          None,
                    documentation_sections: None,
                    scope:                  None,
                    reexport:               None,
                }),
            }],
            current_version: 2,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 4);
        verify_hierarchy_index(&db, "local.Project.Submodule.TestType", &[
            "local.Project.Submodule.TestType.static_method",
        ]);
        verify_hierarchy_index(&db, "Standard.Base.Maybe", &[
            "Standard.Base.Maybe.Some",
            "Standard.Base.Maybe.None",
            "Standard.Base.Maybe.is_some",
            "Standard.Base.Maybe.new_method",
        ]);


        // === Remove the added method ===

        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Remove { id: added_id }],
            current_version: 3,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 4);
        verify_hierarchy_index(&db, "Standard.Base.Maybe", &[
            "Standard.Base.Maybe.Some",
            "Standard.Base.Maybe.None",
            "Standard.Base.Maybe.is_some",
        ]);
    }

    /// Additional verification that hierarchy index is updated when we modify the parent module of
    /// Type.
    #[test]
    fn hierarchy_index_is_updated_after_module_modifications() {
        let db = mock::standard_db_mock();
        let modify_id_1 = lookup_id_by_name(&db, "Standard.Base.Number").unwrap();
        let modify_id_2 = lookup_id_by_name(&db, "Standard.Base.Boolean").unwrap();
        let maybe_id = lookup_id_by_name(&db, "Standard.Base.Maybe").unwrap();
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![
                // Add new module.
                entry::Update::Add {
                    id:         20,
                    suggestion: Box::new(SuggestionEntry::Module {
                        module:                 "Standard.NewModule".to_string(),
                        documentation:          None,
                        documentation_html:     None,
                        reexport:               None,
                        documentation_sections: vec![],
                    }),
                },
                // Move Number and Boolean to the new module.
                entry::Update::Modify {
                    id:           modify_id_1,
                    external_id:  None,
                    modification: Box::new(SuggestionsDatabaseModification {
                        arguments:              vec![],
                        module:                 Some(FieldUpdate::set(
                            "Standard.NewModule".to_string(),
                        )),
                        self_type:              None,
                        return_type:            None,
                        documentation:          None,
                        documentation_sections: None,
                        scope:                  None,
                        reexport:               None,
                    }),
                },
                entry::Update::Modify {
                    id:           modify_id_2,
                    external_id:  None,
                    modification: Box::new(SuggestionsDatabaseModification {
                        arguments:              vec![],
                        module:                 Some(FieldUpdate::set(
                            "Standard.NewModule".to_string(),
                        )),
                        self_type:              None,
                        return_type:            None,
                        documentation:          None,
                        documentation_sections: None,
                        scope:                  None,
                        reexport:               None,
                    }),
                },
                // Modify the definition of Maybe. It should not affect the hierarchy index.
                entry::Update::Modify {
                    id:           maybe_id,
                    external_id:  None,
                    modification: Box::new(SuggestionsDatabaseModification {
                        arguments:              vec![], // remove arguments
                        module:                 None,
                        self_type:              None,
                        return_type:            None,
                        documentation:          None,
                        documentation_sections: None,
                        scope:                  None,
                        reexport:               None,
                    }),
                },
            ],
            current_version: 1,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 5);
        verify_hierarchy_index(&db, "Standard.Base", &["Standard.Base.Maybe"]);
        verify_hierarchy_index(&db, "Standard.Base.Maybe", &[
            "Standard.Base.Maybe.Some",
            "Standard.Base.Maybe.None",
            "Standard.Base.Maybe.is_some",
        ]);
        verify_hierarchy_index(&db, "Standard.NewModule", &[
            "Standard.NewModule.Boolean",
            "Standard.NewModule.Number",
        ]);
    }

    /// Test that the hierarchy index does not depend on the order of received updates. For example,
    /// we can receive a method entry before receiving a self type entry.
    #[test]
    fn hierarchy_index_does_not_depend_on_order_of_updates() {
        // === Add a new method, then add a self type for it ===

        let db = mock::standard_db_mock();
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Add {
                id:         20,
                suggestion: Box::new(SuggestionEntry::Method {
                    external_id:            None,
                    name:                   "new_method".to_string(),
                    module:                 "Standard.Base".to_string(),
                    arguments:              vec![],
                    self_type:              "Standard.Base.NewType".to_string(),
                    return_type:            "Standard.Base.Maybe".to_string(),
                    is_static:              false,
                    reexport:               None,
                    documentation:          None,
                    documentation_html:     None,
                    documentation_sections: vec![],
                }),
            }],
            current_version: 1,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 4);
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Add {
                id:         21,
                suggestion: Box::new(SuggestionEntry::Type {
                    external_id:            None,
                    name:                   "NewType".to_string(),
                    module:                 "Standard.Base".to_string(),
                    params:                 vec![],
                    parent_type:            None,
                    reexport:               None,
                    documentation:          None,
                    documentation_html:     None,
                    documentation_sections: vec![],
                }),
            }],
            current_version: 2,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 5);
        let new_type = lookup_id_by_name(&db, "Standard.Base.NewType").unwrap();
        let new_method = lookup_id_by_name(&db, "Standard.Base.NewType.new_method").unwrap();
        assert_eq!(db.lookup_hierarchy(new_type).unwrap(), HashSet::from([new_method]));


        // === Add a new type, then add a parent module for it ===

        let db = mock::standard_db_mock();
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Add {
                id:         20,
                suggestion: Box::new(SuggestionEntry::Type {
                    external_id:            None,
                    name:                   "NewType".to_string(),
                    module:                 "Standard.NewModule".to_string(),
                    params:                 vec![],
                    reexport:               None,
                    documentation:          None,
                    documentation_html:     None,
                    documentation_sections: vec![],
                    parent_type:            None,
                }),
            }],
            current_version: 1,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 4);
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![entry::Update::Add {
                id:         21,
                suggestion: Box::new(SuggestionEntry::Module {
                    module:                 "Standard.NewModule".to_string(),
                    reexport:               None,
                    documentation:          None,
                    documentation_html:     None,
                    documentation_sections: vec![],
                }),
            }],
            current_version: 2,
        };
        db.apply_update_event(update);
        assert_eq!(db.hierarchy_index.borrow().len(), 5);
        let new_module = lookup_id_by_name(&db, "Standard.NewModule").unwrap();
        let new_type = lookup_id_by_name(&db, "Standard.NewModule.NewType").unwrap();
        assert_eq!(db.lookup_hierarchy(new_module).unwrap(), HashSet::from([new_type]));
    }
}
