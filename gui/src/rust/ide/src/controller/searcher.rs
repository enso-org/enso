//! This module contains all structures related to Searcher Controller.

use crate::prelude::*;

use crate::notification;

use data::text::TextLocation;
use enso_protocol::language_server;
use flo_stream::Subscriber;



// =======================
// === Suggestion List ===
// =======================

/// A single suggestion on the Searcher suggestion list.
#[derive(Clone,CloneRef,Debug,Eq,PartialEq)]
pub enum Suggestion {
    /// Suggestion for input completion: possible functions, arguments, etc.
    Completion(Rc<model::suggestion_database::Entry>)
    // In future, other suggestion types will be added (like suggestions of actions, etc.).
}

/// List of suggestions available in Searcher.
#[derive(Clone,CloneRef,Debug)]
pub enum Suggestions {
    /// The suggestion list is still loading from the Language Server.
    Loading,
    /// The suggestion list is loaded.
    #[allow(missing_docs)]
    Loaded {
        list : Rc<Vec<Suggestion>>
    },
    /// Loading suggestion list resulted in error.
    Error(Rc<failure::Error>)
}

impl Suggestions {
    /// Check if suggestion list is still loading.
    pub fn is_loading(&self) -> bool {
        match self {
            Self::Loading => true,
            _             => false,
        }
    }

    /// Check if retrieving suggestion list was unsuccessful
    pub fn is_error(&self) -> bool {
        match self {
            Self::Error(_) => true,
            _              => false,
        }
    }

    /// Get the list of suggestions. Returns None if still loading or error was returned.
    pub fn list(&self) -> Option<&Vec<Suggestion>> {
        match self {
            Self::Loaded {list} => Some(list),
            _                   => None,
        }
    }
}

impl Default for Suggestions {
    fn default() -> Self {
        Self::Loading
    }
}



// =====================
// === Notifications ===
// =====================

/// The notification emitted by Searcher Controller
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
pub enum Notification {
    /// A new Suggestion list is available.
    NewSuggestionList
}



// ===========================
// === Searcher Controller ===
// ===========================

/// A controller state. Currently it caches the currently kept suggestions list and the current
/// searcher input.
#[derive(Clone,Debug,Default)]
struct Data {
    current_input : String,
    current_list  : Suggestions,
}

/// Searcher Controller.
///
/// This is an object providing all required functionalities for Searcher View: mainly it is the
/// suggestion list to display depending on the searcher input, and actions of picking one or
/// accepting the Searcher input (pressing "Enter").
#[derive(Clone,CloneRef,Debug)]
pub struct Searcher {
    logger          : Logger,
    data            : Rc<RefCell<Data>>,
    notifier        : notification::Publisher<Notification>,
    module          : Rc<model::module::QualifiedName>,
    position        : Immutable<TextLocation>,
    database        : Rc<model::SuggestionDatabase>,
    language_server : Rc<language_server::Connection>,
}

impl Searcher {
    /// Create new Searcher Controller.
    pub fn new
    ( parent   : impl AnyLogger
    , project  : &model::Project
    , module   : model::module::Path
    , position : TextLocation
    ) -> Self {
        let this = Self {
            position        : Immutable(position),
            logger          : Logger::sub(parent,"Searcher Controller"),
            data            : default(),
            notifier        : default(),
            module          : Rc::new(project.qualified_module_name(&module)),
            database        : project.suggestion_db.clone_ref(),
            language_server : project.language_server_rpc.clone_ref(),
        };
        this.reload_list();
        this
    }

    /// Subscribe to controller's notifications.
    pub fn subscribe(&self) -> Subscriber<Notification> {
        self.notifier.subscribe()
    }

    /// Get the current suggestion list.
    pub fn suggestions(&self) -> Suggestions {
        self.data.borrow().current_list.clone_ref()
    }

    /// Set the Searcher Input.
    ///
    /// This function should be called each time user modifies Searcher input in view. It may result
    /// in a new suggestion list (the aprriopriate notification will be emitted).
    pub fn set_input(&self, new_input:String) {
        self.data.borrow_mut().current_input = new_input;
        //TODO[ao] here goes refreshing suggestion list after input change.
    }

    /// Reload Suggestion List.
    ///
    /// The current list will be set as "Loading" and Language Server will be requested for a new
    /// list - once it be retrieved, the new list will be set and notification will be emitted.
    fn reload_list(&self) {
        let module      = self.module.as_ref();
        let self_type   = None;
        let return_type = None;
        let tags        = None;
        let position    = self.position.deref().into();
        let request     = self.language_server.completion(module,&position,&self_type,&return_type,&tags);
        let data        = self.data.clone_ref();
        let database    = self.database.clone_ref();
        let logger      = self.logger.clone_ref();
        let notifier    = self.notifier.clone_ref();

        self.data.borrow_mut().current_list = Suggestions::Loading;
        executor::global::spawn(async move {
            info!(logger,"Requesting new suggestion list.");
            let ls_response = request.await;
            info!(logger,"Received suggestions from Language Server.");
            let new_list = match ls_response {
                Ok(list) => {
                    let entry_ids   = list.results.into_iter();
                    let entries     = entry_ids.filter_map(|id| {
                        let entry = database.get(id);
                        if entry.is_none() {
                            error!(logger,"Missing entry {id} in Suggestion Database.");
                        }
                        entry
                    });
                    let suggestions = entries.map(Suggestion::Completion);
                    Suggestions::Loaded {list:Rc::new(suggestions.collect())}
                },
                Err(error) => Suggestions::Error(Rc::new(error.into()))
            };
            data.borrow_mut().current_list = new_list;
            notifier.publish(Notification::NewSuggestionList).await;
        });
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::executor::test_utils::TestWithLocalPoolExecutor;
    use crate::model::module::Path;

    use json_rpc::expect_call;
    use utils::test::traits::*;

    #[test]
    fn reloading_list() {
        let mut test    = TestWithLocalPoolExecutor::set_up();
        let client      = language_server::MockClient::default();
        let module_path = Path::from_mock_module_name("Test");

        let completion_response = language_server::response::Completion {
            results: vec![1,5,9],
            current_version: default(),
        };
        expect_call!(client.completion(
            module      = "Test.Test".to_string(),
            position    = TextLocation::at_document_begin().into(),
            self_type   = None,
            return_type = None,
            tag         = None
        ) => Ok(completion_response));

        let searcher = Searcher {
            logger          : default(),
            data            : default(),
            notifier        : default(),
            module          : Rc::new(module_path.qualified_module_name("Test")),
            position        : Immutable(TextLocation::at_document_begin()),
            database        : default(),
            language_server : language_server::Connection::new_mock_rc(client),
        };
        let entry1 = model::suggestion_database::Entry {
            name          : "TestFunction1".to_string(),
            kind          : model::suggestion_database::EntryKind::Function,
            module        : "Test.Test".to_string(),
            arguments     : vec![],
            return_type   : "Number".to_string(),
            documentation : default(),
            self_type     : None
        };
        let entry2 = model::suggestion_database::Entry {
            name : "TestFunction2".to_string(),
            ..entry1.clone()
        };
        searcher.database.put_entry(1,entry1);
        let entry1 = searcher.database.get(1).unwrap();
        searcher.database.put_entry(9,entry2);
        let entry2 = searcher.database.get(9).unwrap();

        let mut subscriber = searcher.subscribe();

        searcher.reload_list();
        assert!(searcher.suggestions().is_loading());
        test.run_until_stalled();
        let expected_list = vec![Suggestion::Completion(entry1),Suggestion::Completion(entry2)];
        assert_eq!(searcher.suggestions().list(), Some(&expected_list));
        let notification = subscriber.next().boxed_local().expect_ready();
        assert_eq!(notification, Some(Notification::NewSuggestionList));
    }
}
