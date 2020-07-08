//! The module contains all structures for representing suggestions and their database.
//!
use crate::prelude::*;

use enso_protocol::language_server;
use language_server::types::SuggestionsDatabaseVersion;
use language_server::types::SuggestionDatabaseUpdateEvent;

pub use language_server::types::SuggestionEntry as Entry;
pub use language_server::types::SuggestionEntryId as EntryId;
pub use language_server::types::SuggestionsDatabaseUpdate as Update;



// ================
// === Database ===
// ================

/// The Suggestion Database
///
/// This is database of possible suggestions in Searcher. To achieve best performance, some
/// often-called Language Server methods returns the list of keys of this database instead of the
/// whole entries. Additionally the suggestions contains information about functions and their
/// argument names and types.
#[derive(Clone,Debug,Default)]
pub struct SuggestionDatabase {
    entries : RefCell<HashMap<EntryId,Rc<Entry>>>,
    version : Cell<SuggestionsDatabaseVersion>,
}

impl SuggestionDatabase {
    /// Create a new database which will take its initial content from the Language Server.
    pub async fn create_synchronized
    (language_server:&language_server::Connection) -> FallibleResult<Self> {
        let response = language_server.client.get_suggestions_database().await?;
        Ok(Self::new_from_ls_response(response))
    }

    /// Create a new database model from response received from the Language Server.
    fn new_from_ls_response(response:language_server::response::GetSuggestionDatabase) -> Self {
        let mut entries = HashMap::new();
        for entry in response.entries {
            entries.insert(entry.id, Rc::new(entry.suggestion));
        }
        Self {
            entries : RefCell::new(entries),
            version : Cell::new(response.current_version),
        }
    }

    /// Get suggestion entry by id.
    pub fn get(&self, id:EntryId) -> Option<Rc<Entry>> {
        self.entries.borrow().get(&id).cloned()
    }

    /// Apply the update event to the database.
    pub fn apply_update_event(&self, event:SuggestionDatabaseUpdateEvent) {
        for update in event.updates {
            match update {
                Update::Add    {id,entry} => self.entries.borrow_mut().insert(id,Rc::new(entry)),
                Update::Remove {id}       => self.entries.borrow_mut().remove(&id),
            };
        }
        self.version.set(event.current_version);
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;
    use enso_protocol::language_server::SuggestionsDatabaseEntry;

    #[test]
    fn initialize_database() {
        // Empty db
        let response = language_server::response::GetSuggestionDatabase {
            entries         : vec![],
            current_version : 123
        };
        let db = SuggestionDatabase::new_from_ls_response(response);
        assert!(db.entries.borrow().is_empty());
        assert_eq!(db.version.get()    , 123);

        // Non-empty db
        let entry = Entry::SuggestionEntryAtom {
            name          : "TextAtom".to_string(),
            module        : "TestModule".to_string(),
            arguments     : vec![],
            return_type   : "TestAtom".to_string(),
            documentation : None
        };
        let db_entry = SuggestionsDatabaseEntry {id:12, suggestion:entry};
        let response = language_server::response::GetSuggestionDatabase {
            entries         : vec![db_entry],
            current_version : 456
        };
        let db = SuggestionDatabase::new_from_ls_response(response);
        assert_eq!(db.entries.borrow().len(), 1);
        assert_eq!(*db.get(12).unwrap().name(), "TextAtom".to_string());
        assert_eq!(db.version.get(), 456);
    }

    #[test]
    fn applying_update() {
        let entry1 = Entry::SuggestionEntryAtom {
            name          : "Entry1".to_string(),
            module        : "TestModule".to_string(),
            arguments     : vec![],
            return_type   : "TestAtom".to_string(),
            documentation : None
        };
        let entry2 = Entry::SuggestionEntryAtom {
            name          : "Entry2".to_string(),
            module        : "TestModule".to_string(),
            arguments     : vec![],
            return_type   : "TestAtom".to_string(),
            documentation : None
        };
        let new_entry2 = Entry::SuggestionEntryAtom {
            name          : "NewEntry2".to_string(),
            module        : "TestModule".to_string(),
            arguments     : vec![],
            return_type   : "TestAtom".to_string(),
            documentation : None
        };

        let db_entry1        = SuggestionsDatabaseEntry {id:1, suggestion:entry1};
        let db_entry2        = SuggestionsDatabaseEntry {id:2, suggestion:entry2};
        let initial_response = language_server::response::GetSuggestionDatabase {
            entries         : vec![db_entry1,db_entry2],
            current_version : 1,
        };
        let db = SuggestionDatabase::new_from_ls_response(initial_response);

        // Remove
        let remove_update = Update::Remove {id:2};
        let update        = SuggestionDatabaseUpdateEvent {
            updates         : vec![remove_update],
            current_version : 2
        };
        db.apply_update_event(update);
        assert_eq!(db.get(2),        None);
        assert_eq!(db.version.get(), 2   );

        // Add
        let add_update = Update::Add {id:2, entry:new_entry2};
        let update     = SuggestionDatabaseUpdateEvent {
            updates         : vec![add_update],
            current_version : 3,
        };
        db.apply_update_event(update);
        assert_eq!(db.get(2).unwrap().name(), "NewEntry2");
        assert_eq!(db.version.get(),          3          );
    }
}
