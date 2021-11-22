//! The module contains all structures for representing suggestions and their database.
pub mod entry;
pub mod example;

use crate::prelude::*;

use crate::model::module::MethodId;
use crate::model::suggestion_database::entry::Kind;
use crate::notification;

use double_representation::module::QualifiedName;
use engine_protocol::language_server;
use engine_protocol::language_server::SuggestionId;
use enso_text::Location;
use flo_stream::Subscriber;
use language_server::types::SuggestionDatabaseUpdatesEvent;
use language_server::types::SuggestionsDatabaseVersion;

pub use entry::Entry;
pub use example::Example;



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
#[derive(Clone, Copy, Debug, PartialEq)]
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
#[derive(Clone, Debug)]
pub struct SuggestionDatabase {
    logger:        Logger,
    entries:       RefCell<HashMap<entry::Id, Rc<Entry>>>,
    examples:      RefCell<Vec<Rc<Example>>>,
    version:       Cell<SuggestionsDatabaseVersion>,
    notifications: notification::Publisher<Notification>,
}

impl SuggestionDatabase {
    /// Create a database with no entries.
    pub fn new_empty(logger: impl AnyLogger) -> Self {
        let logger = Logger::new_sub(logger, "SuggestionDatabase");
        let entries = default();
        let examples = default();
        let version = default();
        let notifications = default();
        Self { logger, entries, examples, version, notifications }
    }

    /// Create a database filled with entries provided by the given iterator.
    pub fn new_from_entries<'a>(
        logger: impl AnyLogger,
        entries: impl IntoIterator<Item = (&'a SuggestionId, &'a Entry)>,
    ) -> Self {
        let ret = Self::new_empty(logger);
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
        let logger = Logger::new("SuggestionDatabase");
        let mut entries = HashMap::new();
        for ls_entry in response.entries {
            let id = ls_entry.id;
            match Entry::from_ls_entry(ls_entry.suggestion) {
                Ok(entry) => {
                    entries.insert(id, Rc::new(entry));
                }
                Err(err) => {
                    error!(logger, "Discarded invalid entry {id}: {err}");
                }
            }
        }
        //TODO[ao]: This is a temporary solution. Eventually, we should gather examples from the
        //          available modules documentation. (https://github.com/enso-org/ide/issues/1011)
        let examples = example::EXAMPLES.iter().cloned().map(Rc::new).collect_vec();
        Self {
            logger,
            entries: RefCell::new(entries),
            examples: RefCell::new(examples),
            version: Cell::new(response.current_version),
            notifications: default(),
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
            match update {
                entry::Update::Add { id, suggestion } => match suggestion.try_into() {
                    Ok(entry) => {
                        entries.insert(id, Rc::new(entry));
                    }
                    Err(err) => {
                        error!(self.logger, "Discarding update for {id}: {err}")
                    }
                },
                entry::Update::Remove { id } => {
                    let removed = entries.remove(&id);
                    if removed.is_none() {
                        error!(self.logger, "Received Remove event for nonexistent id: {id}");
                    }
                }
                entry::Update::Modify { id, modification, .. } => {
                    if let Some(old_entry) = entries.get_mut(&id) {
                        let entry = Rc::make_mut(old_entry);
                        let errors = entry.apply_modifications(*modification);
                        for error in errors {
                            error!(
                                self.logger,
                                "Error when applying update for entry {id}: {error:?}"
                            );
                        }
                    } else {
                        error!(self.logger, "Received Modify event for nonexistent id: {id}");
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

    /// Search the database for entries with given name and visible at given location in module.
    pub fn lookup_by_name_and_location(
        &self,
        name: impl Str,
        module: &QualifiedName,
        location: Location,
    ) -> Vec<Rc<Entry>> {
        self.entries
            .borrow()
            .values()
            .filter(|entry| {
                entry.matches_name(name.as_ref()) && entry.is_visible_at(module, location)
            })
            .cloned()
            .collect()
    }

    /// Search the database for Local or Function entries with given name and visible at given
    /// location in module.
    pub fn lookup_locals_by_name_and_location(
        &self,
        name: impl Str,
        module: &QualifiedName,
        location: Location,
    ) -> Vec<Rc<Entry>> {
        self.entries
            .borrow()
            .values()
            .cloned()
            .filter(|entry| {
                let is_local = entry.kind == Kind::Function || entry.kind == Kind::Local;
                is_local
                    && entry.matches_name(name.as_ref())
                    && entry.is_visible_at(module, location)
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

    /// An iterator over all examples gathered from suggestions.
    ///
    /// If the database was modified during iteration, the iterator does not panic, but may return
    /// unpredictable result (a mix of old and new values).
    pub fn iterate_examples(&self) -> impl Iterator<Item = Rc<Example>> + '_ {
        let indices = 0..self.examples.borrow().len();
        indices.filter_map(move |i| self.examples.borrow().get(i).cloned())
    }

    /// Put the entry to the database. Using this function likely breaks the synchronization between
    /// Language Server and IDE, and should be used only in tests.
    #[cfg(test)]
    pub fn put_entry(&self, id: entry::Id, entry: Entry) {
        self.entries.borrow_mut().insert(id, Rc::new(entry));
    }
}

impl From<language_server::response::GetSuggestionDatabase> for SuggestionDatabase {
    fn from(database: language_server::response::GetSuggestionDatabase) -> Self {
        Self::from_ls_response(database)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::model::suggestion_database::entry::Scope;

    use engine_protocol::language_server::FieldUpdate;
    use engine_protocol::language_server::Position;
    use engine_protocol::language_server::SuggestionArgumentUpdate;
    use engine_protocol::language_server::SuggestionEntryArgument;
    use engine_protocol::language_server::SuggestionEntryScope;
    use engine_protocol::language_server::SuggestionsDatabaseEntry;
    use engine_protocol::language_server::SuggestionsDatabaseModification;
    use enso_text::traits::*;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);



    #[test]
    fn initialize_database() {
        // Empty db
        let response = language_server::response::GetSuggestionDatabase {
            entries:         vec![],
            current_version: 123,
        };
        let db = SuggestionDatabase::from_ls_response(response);
        assert!(db.entries.borrow().is_empty());
        assert_eq!(db.version.get(), 123);

        // Non-empty db
        let entry = language_server::types::SuggestionEntry::Atom {
            name:               "TextAtom".to_string(),
            module:             "TestProject.TestModule".to_string(),
            arguments:          vec![],
            return_type:        "TestAtom".to_string(),
            documentation:      None,
            documentation_html: None,
            external_id:        None,
        };
        let db_entry = SuggestionsDatabaseEntry { id: 12, suggestion: entry };
        let response = language_server::response::GetSuggestionDatabase {
            entries:         vec![db_entry],
            current_version: 456,
        };
        let db = SuggestionDatabase::from_ls_response(response);
        assert_eq!(db.entries.borrow().len(), 1);
        assert_eq!(*db.lookup(12).unwrap().name, "TextAtom".to_string());
        assert_eq!(db.version.get(), 456);
    }

    //TODO[ao] this test should be split between various cases of applying modification to single
    //  entry and here only for testing whole database.
    #[test]
    fn applying_update() {
        let mut fixture = TestWithLocalPoolExecutor::set_up();
        let entry1 = language_server::types::SuggestionEntry::Atom {
            name:               "Entry1".to_owned(),
            module:             "TestProject.TestModule".to_owned(),
            arguments:          vec![],
            return_type:        "TestAtom".to_owned(),
            documentation:      None,
            documentation_html: None,
            external_id:        None,
        };
        let entry2 = language_server::types::SuggestionEntry::Atom {
            name:               "Entry2".to_owned(),
            module:             "TestProject.TestModule".to_owned(),
            arguments:          vec![],
            return_type:        "TestAtom".to_owned(),
            documentation:      None,
            documentation_html: None,
            external_id:        None,
        };
        let new_entry2 = language_server::types::SuggestionEntry::Atom {
            name:               "NewEntry2".to_owned(),
            module:             "TestProject.TestModule".to_owned(),
            arguments:          vec![],
            return_type:        "TestAtom".to_owned(),
            documentation:      None,
            documentation_html: None,
            external_id:        None,
        };
        let arg1 = SuggestionEntryArgument {
            name:          "Argument1".to_owned(),
            repr_type:     "Number".to_owned(),
            is_suspended:  false,
            has_default:   false,
            default_value: None,
        };
        let arg2 = SuggestionEntryArgument {
            name:          "Argument2".to_owned(),
            repr_type:     "TestAtom".to_owned(),
            is_suspended:  true,
            has_default:   false,
            default_value: None,
        };
        let arg3 = SuggestionEntryArgument {
            name:          "Argument3".to_owned(),
            repr_type:     "Number".to_owned(),
            is_suspended:  false,
            has_default:   true,
            default_value: Some("13".to_owned()),
        };
        let entry3 = language_server::types::SuggestionEntry::Function {
            external_id: None,
            name:        "entry3".to_string(),
            module:      "TestProject.TestModule".to_string(),
            arguments:   vec![arg1, arg2, arg3],
            return_type: "".to_string(),
            scope:       SuggestionEntryScope {
                start: Position { line: 1, character: 2 },
                end:   Position { line: 2, character: 4 },
            },
        };

        let db_entry1 = SuggestionsDatabaseEntry { id: 1, suggestion: entry1 };
        let db_entry2 = SuggestionsDatabaseEntry { id: 2, suggestion: entry2 };
        let db_entry3 = SuggestionsDatabaseEntry { id: 3, suggestion: entry3 };
        let initial_response = language_server::response::GetSuggestionDatabase {
            entries:         vec![db_entry1, db_entry2, db_entry3],
            current_version: 1,
        };
        let db = SuggestionDatabase::from_ls_response(initial_response);
        let mut notifications = db.subscribe().boxed_local();
        notifications.expect_pending();

        // Remove
        let remove_update = entry::Update::Remove { id: 2 };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![remove_update],
            current_version: 2,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        assert_eq!(db.lookup(2), Err(NoSuchEntry(2)));
        assert_eq!(db.version.get(), 2);

        // Add
        let add_update = entry::Update::Add { id: 2, suggestion: new_entry2 };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![add_update],
            current_version: 3,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(2).unwrap().name, "NewEntry2");
        assert_eq!(db.version.get(), 3);

        // Empty modify
        let modify_update = entry::Update::Modify {
            id:           1,
            external_id:  None,
            modification: Box::new(SuggestionsDatabaseModification {
                arguments:          vec![],
                module:             None,
                self_type:          None,
                return_type:        None,
                documentation:      None,
                documentation_html: None,
                scope:              None,
            }),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![modify_update],
            current_version: 4,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(1).unwrap().arguments, vec![]);
        assert_eq!(db.lookup(1).unwrap().return_type, "TestAtom");
        assert_eq!(db.lookup(1).unwrap().documentation_html, None);
        assert!(matches!(db.lookup(1).unwrap().scope, Scope::Everywhere));
        assert_eq!(db.version.get(), 4);

        // Modify with some invalid fields
        let modify_update = entry::Update::Modify {
            id:           1,
            external_id:  None,
            modification: Box::new(SuggestionsDatabaseModification {
                // Invalid: the entry does not have any arguments.
                arguments:          vec![SuggestionArgumentUpdate::Remove { index: 0 }],
                // Valid.
                return_type:        Some(FieldUpdate::set("TestAtom2".to_owned())),
                // Valid.
                documentation:      Some(FieldUpdate::set("Blah blah".to_owned())),
                // Valid.
                documentation_html: Some(FieldUpdate::set("<p>Blah blah</p>".to_owned())),
                // Invalid: atoms does not have any scope.
                scope:              Some(FieldUpdate::set(SuggestionEntryScope {
                    start: Position { line: 4, character: 10 },
                    end:   Position { line: 8, character: 12 },
                })),
                module:             None,
                self_type:          None,
            }),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![modify_update],
            current_version: 5,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(1).unwrap().arguments, vec![]);
        assert_eq!(db.lookup(1).unwrap().return_type, "TestAtom2");
        assert_eq!(db.lookup(1).unwrap().documentation_html, Some("<p>Blah blah</p>".to_owned()));
        assert!(matches!(db.lookup(1).unwrap().scope, Scope::Everywhere));
        assert_eq!(db.version.get(), 5);

        // Modify Argument and Scope
        let modify_update = entry::Update::Modify {
            id:           3,
            external_id:  None,
            modification: Box::new(SuggestionsDatabaseModification {
                arguments:          vec![SuggestionArgumentUpdate::Modify {
                    index:         2,
                    name:          Some(FieldUpdate::set("NewArg".to_owned())),
                    repr_type:     Some(FieldUpdate::set("TestAtom".to_owned())),
                    is_suspended:  Some(FieldUpdate::set(true)),
                    has_default:   Some(FieldUpdate::set(false)),
                    default_value: Some(FieldUpdate::remove()),
                }],
                return_type:        None,
                documentation:      None,
                documentation_html: None,
                scope:              Some(FieldUpdate::set(SuggestionEntryScope {
                    start: Position { line: 1, character: 5 },
                    end:   Position { line: 3, character: 0 },
                })),
                self_type:          None,
                module:             None,
            }),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![modify_update],
            current_version: 6,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(3).unwrap().arguments.len(), 3);
        assert_eq!(db.lookup(3).unwrap().arguments[2].name, "NewArg");
        assert_eq!(db.lookup(3).unwrap().arguments[2].repr_type, "TestAtom");
        assert!(db.lookup(3).unwrap().arguments[2].is_suspended);
        assert_eq!(db.lookup(3).unwrap().arguments[2].default_value, None);
        let range = Location { line: 1.line(), column: 5.column() }..=Location {
            line:   3.line(),
            column: 0.column(),
        };
        assert_eq!(db.lookup(3).unwrap().scope, Scope::InModule { range });
        assert_eq!(db.version.get(), 6);

        // Add Argument
        let new_argument = SuggestionEntryArgument {
            name:          "NewArg2".to_string(),
            repr_type:     "Number".to_string(),
            is_suspended:  false,
            has_default:   false,
            default_value: None,
        };
        let add_arg_update = entry::Update::Modify {
            id:           3,
            external_id:  None,
            modification: Box::new(SuggestionsDatabaseModification {
                arguments:          vec![SuggestionArgumentUpdate::Add {
                    index:    2,
                    argument: new_argument,
                }],
                return_type:        None,
                documentation:      None,
                documentation_html: None,
                scope:              None,
                self_type:          None,
                module:             None,
            }),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![add_arg_update],
            current_version: 7,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(3).unwrap().arguments.len(), 4);
        assert_eq!(db.lookup(3).unwrap().arguments[2].name, "NewArg2");
        assert_eq!(db.version.get(), 7);

        // Remove Argument
        let remove_arg_update = entry::Update::Modify {
            id:           3,
            external_id:  None,
            modification: Box::new(SuggestionsDatabaseModification {
                arguments:          vec![SuggestionArgumentUpdate::Remove { index: 2 }],
                return_type:        None,
                documentation:      None,
                documentation_html: None,
                scope:              None,
                self_type:          None,
                module:             None,
            }),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates:         vec![remove_arg_update],
            current_version: 8,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(), Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(3).unwrap().arguments.len(), 3);
        assert_eq!(db.lookup(3).unwrap().arguments[2].name, "NewArg");
        assert_eq!(db.version.get(), 8);
    }
}
