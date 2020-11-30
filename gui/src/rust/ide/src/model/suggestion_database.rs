//! The module contains all structures for representing suggestions and their database.

use crate::prelude::*;

use crate::constants::keywords;
use crate::double_representation::module::QualifiedName;
use crate::model::module::MethodId;
use crate::notification;

use data::text::TextLocation;
use enso_protocol::language_server;
use enso_protocol::language_server::SuggestionId;
use enso_protocol::language_server::FieldUpdate;
use flo_stream::Subscriber;
use language_server::types::FieldAction;
use language_server::types::SuggestionsDatabaseVersion;
use language_server::types::SuggestionDatabaseUpdatesEvent;


pub use language_server::types::SuggestionEntryArgument as Argument;
pub use language_server::types::SuggestionId as EntryId;
pub use language_server::types::SuggestionsDatabaseUpdate as Update;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Debug,Clone,Copy,Eq,Fail,PartialEq)]
#[fail(display = "The suggestion with id {} has not been found in the database.", _0)]
pub struct NoSuchEntry(pub SuggestionId);

#[allow(missing_docs)]
#[derive(Debug,Fail,Clone)]
#[fail(display = "Entry named {} does not represent a method.", _0)]
pub struct NotAMethod(pub String);

#[allow(missing_docs)]
#[derive(Debug,Fail,Clone)]
#[fail(display = "Argument index {} is invalid for suggestion entry named {}.",index,name)]
pub struct InvalidArgumentIndex {
    pub name  : String,
    pub index : usize
}

#[allow(missing_docs)]
#[derive(Copy,Clone,Debug,Fail)]
#[fail(display = "Invalid update for field {}.",_0)]
pub struct InvalidFieldUpdate(pub &'static str);

#[allow(missing_docs)]
#[derive(Debug,Fail,Clone)]
#[fail(display = "Entry named {} is described as method but does not have a `this` parameter.", _0)]
pub struct MissingThisOnMethod(pub String);



// =============
// === Entry ===
// =============

/// A type of suggestion entry.
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
#[allow(missing_docs)]
pub enum EntryKind {
    Atom,Function,Local,Method
}

/// Describes the visibility range of some entry (i.e. identifier available as suggestion).
///
/// Methods are visible "Everywhere", as they are imported on a module level, so they are not
/// specific to any particular span in the module file.
/// However local variables and local function have limited visibility.
#[derive(Clone,Debug,Eq,PartialEq)]
pub enum Scope {
    /// The entry is visible in the whole module where it was defined. It can be also brought to
    /// other modules by import declarations.
    Everywhere,
    /// Local symbol that is visible only in a particular section of the module where it has been
    /// defined.
    #[allow(missing_docs)]
    InModule {range:RangeInclusive<TextLocation>}
}



/// The Suggestion Database Entry.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct Entry {
    /// A name of suggested object.
    pub name : String,
    /// A type of suggestion.
    pub kind : EntryKind,
    /// A module where the suggested object is defined, represented as vector of segments.
    pub module : QualifiedName,
    /// Argument lists of suggested object (atom or function). If the object does not take any
    /// arguments, the list is empty.
    pub arguments : Vec<Argument>,
    /// A type returned by the suggested object.
    pub return_type : String,
    /// A documentation associated with object.
    pub documentation : Option<String>,
    /// A type of the "self" argument. This field is `None` for non-method suggestions.
    pub self_type : Option<String>,
    /// A scope where this suggestion is visible.
    pub scope : Scope,
}

impl Entry {
    /// Check if this entry has self type same as the given identifier.
    pub fn has_self_type(&self, self_type:impl AsRef<str>) -> bool {
        let self_type = self_type.as_ref();
        self.self_type.as_ref().contains_if(|my_self_type| *my_self_type == self_type)
    }

    /// Returns the code which should be inserted to Searcher input when suggestion is picked.
    pub fn code_to_insert(&self, current_module:Option<&QualifiedName>) -> String {
        let module_name = self.module.name();
        if self.has_self_type(&module_name) {
            let module_var = if current_module.contains(&&self.module) {keywords::HERE}
                else {module_name};
            format!("{}.{}",module_var,self.name)
        } else {
            self.name.clone()
        }
    }

    /// Returns the code which should be inserted to Searcher input when suggestion is picked,
    /// omitting module name.
    pub fn code_to_insert_skip_module(&self) -> String {
        self.name.clone()
    }

    /// Return the Method Id of suggested method.
    ///
    /// Returns none, if this is not suggestion for a method.
    pub fn method_id(&self) -> Option<MethodId> {
        if self.kind != EntryKind::Method {
            None
        } else if let Some(self_type) = &self.self_type {
            Some(MethodId {
                module          : self.module.clone(),
                defined_on_type : self_type.clone(),
                name            : self.name.clone(),
            })
        } else {
            None
        }
    }

    /// Checks if entry is visible at given location in a specific module.
    pub fn is_visible_at(&self, module:&QualifiedName, location:TextLocation) -> bool {
        match &self.scope {
            Scope::Everywhere         => true,
            Scope::InModule   {range} => self.module == *module && range.contains(&location),
        }
    }

    /// Checks if entry name matches the given name. The matching is case-insensitive.
    pub fn matches_name(&self, name:impl Str) -> bool {
        self.name.to_lowercase() == name.as_ref().to_lowercase()
    }

    /// Generate information about invoking this entity for span tree context.
    pub fn invocation_info(&self) -> span_tree::generate::context::CalledMethodInfo {
        self.into()
    }
}


// === Handling LanguageServer Types ===

impl Entry {
    /// Create entry from the structure deserialized from the Language Server responses.
    pub fn from_ls_entry(entry:language_server::types::SuggestionEntry)
    -> FallibleResult<Self> {
        use language_server::types::SuggestionEntry::*;
        let this = match entry {
            Atom {name,module,arguments,return_type,documentation,..} => Self {
                name,arguments,return_type,documentation,
                module        : module.try_into()?,
                self_type     : None,
                kind          : EntryKind::Atom,
                scope         : Scope::Everywhere,
            },
            Method {name,module,arguments,self_type,return_type,documentation,..} => Self {
                name,arguments,return_type,documentation,
                module        : module.try_into()?,
                self_type     : Some(self_type),
                kind          : EntryKind::Method,
                scope         : Scope::Everywhere,
            },
            Function {name,module,arguments,return_type,scope,..} => Self {
                name,arguments,return_type,
                module        : module.try_into()?,
                self_type     : None,
                documentation : default(),
                kind          : EntryKind::Function,
                scope         : Scope::InModule {range:scope.into()},
            },
            Local {name,module,return_type,scope,..} => Self {
                name,return_type,
                arguments     : default(),
                module        : module.try_into()?,
                self_type     : None,
                documentation : default(),
                kind          : EntryKind::Local,
                scope         : Scope::InModule {range:scope.into()},
            },
        };
        Ok(this)
    }

    fn apply_modifications
    ( &mut self
    , arguments     : Vec<language_server::types::SuggestionArgumentUpdate>
    , return_type   : Option<FieldUpdate<String>>
    , documentation : Option<FieldUpdate<String>>
    , scope         : Option<FieldUpdate<language_server::types::SuggestionEntryScope>>
    ) -> Vec<failure::Error> {
        let other_update_results =
            [ Entry::apply_field_update    ("return_type"  , &mut self.return_type  , return_type  )
            , Entry::apply_opt_field_update("documentation", &mut self.documentation, documentation)
            , self.apply_scope_update(scope)
            ];
        let other_update_results = SmallVec::from_buf(other_update_results).into_iter();
        let other_update_errors  = other_update_results.filter_map(|res| res.err());
        let arg_update_errors    = arguments.into_iter().flat_map(|arg| self.apply_arg_update(arg));
        arg_update_errors.chain(other_update_errors).collect_vec()
    }

    fn apply_arg_update(&mut self, update:language_server::types::SuggestionArgumentUpdate)
    -> Vec<failure::Error> {
        use language_server::types::SuggestionArgumentUpdate as Update;
        let error = |index| {
            let name = self.name.clone();
            vec![failure::Error::from(InvalidArgumentIndex {name,index})]
        };
        match update {
            Update::Add {index,..} if index > self.arguments.len() => {
                error(index)
            }
            Update::Remove {index} | Update::Modify {index,..} if index >= self.arguments.len() => {
                error(index)
            }
            Update::Add {index,argument} => {
                self.arguments.insert(index,argument);
                vec![]
            }
            Update::Remove {index} => {
                self.arguments.remove(index);
                vec![]
            }
            Update::Modify {index,name,repr_type,is_suspended,has_default,default_value} => {
                let arg     = &mut self.arguments[index];
                type E = Entry;
                let results =
                    [E::apply_field_update    ("name"         ,&mut arg.name         ,name)
                    ,E::apply_field_update    ("repr_type"    ,&mut arg.repr_type    ,repr_type)
                    ,E::apply_field_update    ("is_suspended" ,&mut arg.is_suspended ,is_suspended)
                    ,E::apply_field_update    ("has_default"  ,&mut arg.has_default  ,has_default)
                    ,E::apply_opt_field_update("default_value",&mut arg.default_value,default_value)
                    ];
                SmallVec::from_buf(results).into_iter().filter_map(|res| res.err()).collect_vec()
            }
        }
    }

    fn apply_scope_update
    (&mut self, update:Option<FieldUpdate<language_server::types::SuggestionEntryScope>>)
    -> FallibleResult {
        if let Some(update) = update {
            let err = || Err(failure::Error::from(InvalidFieldUpdate("scope")));
            match &mut self.scope {
                Scope::Everywhere       => { return err() },
                Scope::InModule {range} => {
                    if let Some(value) = update.value { *range = value.into() }
                    else                              { return err()          }
                }
            }
        }
        Ok(())
    }

    fn apply_field_update<T:Default>
    (field_name:&'static str, field:&mut T, update:Option<FieldUpdate<T>>) -> FallibleResult {
        let err = InvalidFieldUpdate(field_name);
        if let Some(update) = update {
            match update.tag {
                FieldAction::Set    => { *field = update.value.ok_or(err)? },
                FieldAction::Remove => { *field = default()                }
            }
        }
        Ok(())
    }

    fn apply_opt_field_update<T>
    (field_name:&'static str, field:&mut Option<T>, update:Option<FieldUpdate<T>>)
    -> FallibleResult {
        let err = InvalidFieldUpdate(field_name);
        if let Some(update) = update {
            match update.tag {
                FieldAction::Set    => { *field = Some(update.value.ok_or(err)?) },
                FieldAction::Remove => { *field = None                           }
            }
        }
        Ok(())
    }
}

impl TryFrom<language_server::types::SuggestionEntry> for Entry {
    type Error = failure::Error;
    fn try_from(entry:language_server::types::SuggestionEntry) -> FallibleResult<Self> {
        Self::from_ls_entry(entry)
    }
}

impl TryFrom<&Entry> for language_server::MethodPointer {
    type Error = failure::Error;
    fn try_from(entry:&Entry) -> FallibleResult<Self> {
        (entry.kind==EntryKind::Method).ok_or_else(|| NotAMethod(entry.name.clone()))?;
        let missing_this_err = || MissingThisOnMethod(entry.name.clone());
        let defined_on_type  = entry.self_type.clone().ok_or_else(missing_this_err)?;
        Ok(language_server::MethodPointer {
            defined_on_type,
            module : entry.module.to_string(),
            name   : entry.name.clone(),
        })
    }
}

impl TryFrom<Entry> for language_server::MethodPointer {
    type Error = failure::Error;
    fn try_from(entry:Entry) -> FallibleResult<Self> {
        language_server::MethodPointer::try_from(&entry)
    }
}

impl From<&Entry> for span_tree::generate::context::CalledMethodInfo {
    fn from(entry:&Entry) -> span_tree::generate::context::CalledMethodInfo {
        let parameters = entry.arguments.iter().map(to_span_tree_param).collect();
        span_tree::generate::context::CalledMethodInfo {parameters}
    }
}

// === SpanTree helpers ===

/// Converts the information about function parameter from suggestion database into the form used
/// by the span tree nodes.
pub fn to_span_tree_param(param_info:&Argument) -> span_tree::ArgumentInfo {
    span_tree::ArgumentInfo {
        // TODO [mwu] Check if database actually do must always have both of these filled.
        name : Some(param_info.name.clone()),
        tp   : Some(param_info.repr_type.clone()),
    }
}



// ====================
// === Notification ===
// ====================

/// Notification about change in a suggestion database,
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum Notification {
    /// The database has been updated.
    Updated
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
#[derive(Clone,Debug)]
pub struct SuggestionDatabase {
    logger        : Logger,
    entries       : RefCell<HashMap<EntryId,Rc<Entry>>>,
    version       : Cell<SuggestionsDatabaseVersion>,
    notifications : notification::Publisher<Notification>,
}

impl SuggestionDatabase {
    /// Create a database with no entries.
    pub fn new_empty(logger:impl AnyLogger) -> Self {
        let logger        = Logger::sub(logger,"SuggestionDatabase");
        let entries       = default();
        let version       = default();
        let notifications = default();
        Self {logger,entries,version,notifications}
    }

    /// Create a database filled with entries provided by the given iterator.
    pub fn new_from_entries<'a>
    (logger:impl AnyLogger, entries:impl IntoIterator<Item=(&'a SuggestionId,&'a Entry)>) -> Self {
        let ret     = Self::new_empty(logger);
        let entries = entries.into_iter().map(|(id,entry)| (*id,Rc::new(entry.clone())));
        ret.entries.borrow_mut().extend(entries);
        ret
    }

    /// Create a new database which will take its initial content from the Language Server.
    pub async fn create_synchronized
    (language_server:&language_server::Connection) -> FallibleResult<Self> {
        let response = language_server.client.get_suggestions_database().await?;
        Ok(Self::from_ls_response(response))
    }

    /// Create a new database model from response received from the Language Server.
    fn from_ls_response(response:language_server::response::GetSuggestionDatabase) -> Self {
        let logger      = Logger::new("SuggestionDatabase");
        let mut entries = HashMap::new();
        for ls_entry in response.entries {
            let id = ls_entry.id;
            match Entry::from_ls_entry(ls_entry.suggestion) {
                Ok(entry) => { entries.insert(id, Rc::new(entry)); },
                Err(err)  => { error!(logger,"Discarded invalid entry {id}: {err}"); },
            }
        }
        Self {
            logger,
            entries       : RefCell::new(entries),
            version       : Cell::new(response.current_version),
            notifications : default()
        }
    }

    /// Subscribe for notifications about changes in the database.
    pub fn subscribe(&self) -> Subscriber<Notification> {
        self.notifications.subscribe()
    }

    /// Get suggestion entry by id.
    pub fn lookup(&self, id:EntryId) -> Result<Rc<Entry>,NoSuchEntry> {
        self.entries.borrow().get(&id).cloned().ok_or(NoSuchEntry(id))
    }

    /// Apply the update event to the database.
    pub fn apply_update_event(&self, event:SuggestionDatabaseUpdatesEvent) {
        for update in event.updates {
            let mut entries = self.entries.borrow_mut();
            match update {
                Update::Add {id,suggestion} => match suggestion.try_into() {
                    Ok(entry) => { entries.insert(id,Rc::new(entry));                       },
                    Err(err)  => { error!(self.logger, "Discarding update for {id}: {err}") },
                },
                Update::Remove {id} => {
                    let removed = entries.remove(&id);
                    if removed.is_none() {
                        error!(self.logger, "Received Remove event for nonexistent id: {id}");
                    }
                },
                Update::Modify {id,arguments,return_type,documentation,scope,..} => {
                    if let Some(old_entry) = entries.get_mut(&id) {
                        let entry  = Rc::make_mut(old_entry);
                        let errors = entry.apply_modifications
                            (arguments,return_type,documentation,scope);
                        for error in errors {
                            error!(self.logger
                                ,"Error when applying update for entry {id}: {error:?}");
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
    pub fn lookup_method_ptr
    (&self, id:SuggestionId) -> FallibleResult<language_server::MethodPointer> {
        let entry = self.lookup(id)?;
        Ok(language_server::MethodPointer::try_from(entry.as_ref())?)
    }

    /// Search the database for an entry of method identified by given id.
    pub fn lookup_method(&self, id:MethodId) -> Option<Rc<Entry>> {
        self.entries.borrow().values().cloned().find(|entry| entry.method_id().contains(&id))
    }

    /// Search the database for entries with given name and visible at given location in module.
    pub fn lookup_by_name_and_location
    (&self, name:impl Str, module:&QualifiedName, location:TextLocation) -> Vec<Rc<Entry>> {
        self.entries.borrow().values().filter(|entry| {
            entry.matches_name(name.as_ref()) && entry.is_visible_at(module,location)
        }).cloned().collect()
    }

    /// Search the database for Local or Function entries with given name and visible at given
    /// location in module.
    pub fn lookup_locals_by_name_and_location
    (&self, name:impl Str, module:&QualifiedName, location:TextLocation) -> Vec<Rc<Entry>> {
        self.entries.borrow().values().cloned().filter(|entry| {
            let is_local = entry.kind == EntryKind::Function || entry.kind == EntryKind::Local;
            is_local && entry.matches_name(name.as_ref()) && entry.is_visible_at(module,location)
        }).collect()
    }

    /// Search the database for Method entry with given name and defined for given module.
    pub fn lookup_module_method
    (&self, name:impl Str, module:&QualifiedName) -> Option<Rc<Entry>> {
        self.entries.borrow().values().cloned().find(|entry| {
            let is_method             = entry.kind == EntryKind::Method;
            let is_defined_for_module = entry.has_self_type(module.name());
            is_method && is_defined_for_module && entry.matches_name(name.as_ref())
        })
    }

    /// Put the entry to the database. Using this function likely break the synchronization between
    /// Language Server and IDE, and should be used only in tests.
    #[cfg(test)]
    pub fn put_entry(&self, id:EntryId, entry:Entry) {
        self.entries.borrow_mut().insert(id,Rc::new(entry));
    }
}

impl From<language_server::response::GetSuggestionDatabase> for SuggestionDatabase {
    fn from(database:language_server::response::GetSuggestionDatabase) -> Self {
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

    use enso_protocol::language_server::SuggestionsDatabaseEntry;
    use enso_protocol::language_server::SuggestionArgumentUpdate;
    use enso_protocol::language_server::SuggestionEntryScope;
    use enso_protocol::language_server::Position;
    use enso_protocol::language_server::SuggestionEntryArgument;
    use utils::test::stream::StreamTestExt;
    use wasm_bindgen_test::wasm_bindgen_test_configure;
    use enso_data::text::TextLocation;



    wasm_bindgen_test_configure!(run_in_browser);

    #[test]
    fn code_from_entry() {
        let module         = QualifiedName::from_text("Project.Main").unwrap();
        let another_module = QualifiedName::from_text("Project.AnotherModule").unwrap();
        let atom_entry = Entry {
            name          : "Atom".to_string(),
            kind          : EntryKind::Atom,
            module        : module.clone(),
            arguments     : vec![],
            return_type   : "Number".to_string(),
            documentation : None,
            self_type     : None,
            scope         : Scope::Everywhere,
        };
        let method_entry = Entry {
            name      : "method".to_string(),
            kind      : EntryKind::Method,
            self_type : Some("Number".to_string()),
            ..atom_entry.clone()
        };
        let module_method_entry = Entry {
            name      : "moduleMethod".to_string(),
            self_type : Some("Main".to_string()),
            ..method_entry.clone()
        };

        let current_module = None;
        assert_eq!(atom_entry.code_to_insert(current_module)         , "Atom");
        assert_eq!(method_entry.code_to_insert(current_module)       , "method");
        assert_eq!(module_method_entry.code_to_insert(current_module), "Main.moduleMethod");

        let current_module = Some(&module);
        assert_eq!(atom_entry.code_to_insert(current_module)         , "Atom");
        assert_eq!(method_entry.code_to_insert(current_module)       , "method");
        assert_eq!(module_method_entry.code_to_insert(current_module), "here.moduleMethod");

        let current_module = Some(&another_module);
        assert_eq!(atom_entry.code_to_insert(current_module)         , "Atom");
        assert_eq!(method_entry.code_to_insert(current_module)       , "method");
        assert_eq!(module_method_entry.code_to_insert(current_module), "Main.moduleMethod");
    }

    #[test]
    fn method_id_from_entry() {
        let non_method = Entry {
            name          : "function".to_string(),
            kind          : EntryKind::Function,
            module        : "Test.Test".to_string().try_into().unwrap(),
            arguments     : vec![],
            return_type   : "Number".to_string(),
            documentation : None,
            self_type     : None,
            scope         : Scope::Everywhere,
        };
        let method = Entry {
            name      : "method".to_string(),
            kind      : EntryKind::Method,
            self_type : Some("Number".to_string()),
            ..non_method.clone()
        };
        let expected = MethodId {
            module          : "Test.Test".to_string().try_into().unwrap(),
            defined_on_type : "Number".to_string(),
            name            : "method".to_string()
        };
        assert_eq!(non_method.method_id() , None);
        assert_eq!(method.method_id()     , Some(expected));
    }

    #[test]
    fn initialize_database() {
        // Empty db
        let response = language_server::response::GetSuggestionDatabase {
            entries         : vec![],
            current_version : 123
        };
        let db = SuggestionDatabase::from_ls_response(response);
        assert!(db.entries.borrow().is_empty());
        assert_eq!(db.version.get()    , 123);

        // Non-empty db
        let entry = language_server::types::SuggestionEntry::Atom {
            name          : "TextAtom".to_string(),
            module        : "TestProject.TestModule".to_string(),
            arguments     : vec![],
            return_type   : "TestAtom".to_string(),
            documentation : None,
            external_id   : None,
        };
        let db_entry = SuggestionsDatabaseEntry {id:12, suggestion:entry};
        let response = language_server::response::GetSuggestionDatabase {
            entries         : vec![db_entry],
            current_version : 456
        };
        let db = SuggestionDatabase::from_ls_response(response);
        assert_eq!(db.entries.borrow().len(), 1);
        assert_eq!(*db.lookup(12).unwrap().name, "TextAtom".to_string());
        assert_eq!(db.version.get(), 456);
    }

    #[test]
    fn applying_update() {
        let mut fixture = TestWithLocalPoolExecutor::set_up();
        let entry1 = language_server::types::SuggestionEntry::Atom {
            name          : "Entry1".to_owned(),
            module        : "TestProject.TestModule".to_owned(),
            arguments     : vec![],
            return_type   : "TestAtom".to_owned(),
            documentation : None,
            external_id   : None,
        };
        let entry2 = language_server::types::SuggestionEntry::Atom {
            name          : "Entry2".to_owned(),
            module        : "TestProject.TestModule".to_owned(),
            arguments     : vec![],
            return_type   : "TestAtom".to_owned(),
            documentation : None,
            external_id   : None,
        };
        let new_entry2 = language_server::types::SuggestionEntry::Atom {
            name          : "NewEntry2".to_owned(),
            module        : "TestProject.TestModule".to_owned(),
            arguments     : vec![],
            return_type   : "TestAtom".to_owned(),
            documentation : None,
            external_id   : None,
        };
        let arg1 = SuggestionEntryArgument {
            name          : "Argument1".to_owned(),
            repr_type     : "Number".to_owned(),
            is_suspended  : false,
            has_default   : false,
            default_value : None
        };
        let arg2 = SuggestionEntryArgument {
            name          : "Argument2".to_owned(),
            repr_type     : "TestAtom".to_owned(),
            is_suspended  : true,
            has_default   : false,
            default_value : None
        };
        let arg3 = SuggestionEntryArgument {
            name          : "Argument3".to_owned(),
            repr_type     : "Number".to_owned(),
            is_suspended  : false,
            has_default   : true,
            default_value : Some("13".to_owned())
        };
        let entry3 = language_server::types::SuggestionEntry::Function {
            external_id : None,
            name        : "entry3".to_string(),
            module      : "TestProject.TestModule".to_string(),
            arguments   : vec![arg1,arg2,arg3],
            return_type : "".to_string(),
            scope       : SuggestionEntryScope {
                start : Position { line:1, character:2 },
                end   : Position { line:2, character:4 }
            }
        };

        let db_entry1        = SuggestionsDatabaseEntry {id:1, suggestion:entry1};
        let db_entry2        = SuggestionsDatabaseEntry {id:2, suggestion:entry2};
        let db_entry3        = SuggestionsDatabaseEntry {id:3, suggestion:entry3};
        let initial_response = language_server::response::GetSuggestionDatabase {
            entries         : vec![db_entry1,db_entry2,db_entry3],
            current_version : 1,
        };
        let db            = SuggestionDatabase::from_ls_response(initial_response);
        let mut notifications = db.subscribe().boxed_local();
        notifications.expect_pending();

        // Remove
        let remove_update = Update::Remove {id:2};
        let update        = SuggestionDatabaseUpdatesEvent {
            updates         : vec![remove_update],
            current_version : 2
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(),Notification::Updated);
        assert_eq!(db.lookup(2), Err(NoSuchEntry(2)));
        assert_eq!(db.version.get(), 2);

        // Add
        let add_update = Update::Add {id:2, suggestion:new_entry2};
        let update     = SuggestionDatabaseUpdatesEvent {
            updates         : vec![add_update],
            current_version : 3,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(),Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(2).unwrap().name, "NewEntry2");
        assert_eq!(db.version.get(), 3);

        // Empty modify
        let modify_update = Update::Modify {
            id            : 1,
            external_id   : None,
            arguments     : vec![],
            return_type   : None,
            documentation : None,
            scope         : None
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates         : vec![modify_update],
            current_version : 4,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(),Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(1).unwrap().arguments    , vec![]);
        assert_eq!(db.lookup(1).unwrap().return_type  , "TestAtom");
        assert_eq!(db.lookup(1).unwrap().documentation, None);
        assert!(matches!(db.lookup(1).unwrap().scope, Scope::Everywhere));
        assert_eq!(db.version.get(), 4);

        // Modify with some invalid fields
        let modify_update = Update::Modify {
            id          : 1,
            external_id : None,
            // Invalid: the entry does not have any arguments.
            arguments:vec![SuggestionArgumentUpdate::Remove {index:0}],
            // Valid.
            return_type:Some(FieldUpdate::set("TestAtom2".to_owned())),
            // Valid.
            documentation:Some(FieldUpdate::set("Blah blah".to_owned())),
            // Invalid: atoms does not have any scope.
            scope:Some(FieldUpdate::set(SuggestionEntryScope {
                start : Position {line:4, character:10},
                end   : Position {line:8, character:12}
            })),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates         : vec![modify_update],
            current_version : 5,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(),Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(1).unwrap().arguments    , vec![]);
        assert_eq!(db.lookup(1).unwrap().return_type  , "TestAtom2");
        assert_eq!(db.lookup(1).unwrap().documentation, Some("Blah blah".to_owned()));
        assert!(matches!(db.lookup(1).unwrap().scope, Scope::Everywhere));
        assert_eq!(db.version.get(), 5);

        // Modify Argument and Scope
        let modify_update = Update::Modify {
            id            : 3,
            external_id   : None,
            arguments     : vec![SuggestionArgumentUpdate::Modify {
                index         : 2,
                name          : Some(FieldUpdate::set("NewArg".to_owned())),
                repr_type     : Some(FieldUpdate::set("TestAtom".to_owned())),
                is_suspended  : Some(FieldUpdate::set(true)),
                has_default   : Some(FieldUpdate::set(false)),
                default_value : Some(FieldUpdate::remove()),
            }],
            return_type   : None,
            documentation : None,
            scope         : Some(FieldUpdate::set(SuggestionEntryScope {
                start: Position { line: 1, character: 5 },
                end: Position { line: 3, character: 0 }
            })),
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates         : vec![modify_update],
            current_version : 6,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(),Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(3).unwrap().arguments.len(), 3);
        assert_eq!(db.lookup(3).unwrap().arguments[2].name, "NewArg");
        assert_eq!(db.lookup(3).unwrap().arguments[2].repr_type, "TestAtom");
        assert!   (db.lookup(3).unwrap().arguments[2].is_suspended);
        assert_eq!(db.lookup(3).unwrap().arguments[2].default_value, None);
        let range = TextLocation {line:1, column:5}..=TextLocation {line:3, column:0};
        assert_eq!(db.lookup(3).unwrap().scope, Scope::InModule{range});
        assert_eq!(db.version.get(), 6);

        // Add Argument
        let new_argument = SuggestionEntryArgument {
            name          : "NewArg2".to_string(),
            repr_type     : "Number".to_string(),
            is_suspended  : false,
            has_default   : false,
            default_value : None
        };
        let add_arg_update = Update::Modify {
            id            : 3,
            external_id   : None,
            arguments     : vec![SuggestionArgumentUpdate::Add {index:2, argument:new_argument}],
            return_type   : None,
            documentation : None,
            scope         : None,
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates         : vec![add_arg_update],
            current_version : 7,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(),Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(3).unwrap().arguments.len(), 4);
        assert_eq!(db.lookup(3).unwrap().arguments[2].name, "NewArg2");
        assert_eq!(db.version.get(), 7);

        // Remove Argument
        let remove_arg_update = Update::Modify {
            id            : 3,
            external_id   : None,
            arguments     : vec![SuggestionArgumentUpdate::Remove {index:2}],
            return_type   : None,
            documentation : None,
            scope         : None,
        };
        let update = SuggestionDatabaseUpdatesEvent {
            updates         : vec![remove_arg_update],
            current_version : 8,
        };
        db.apply_update_event(update);
        fixture.run_until_stalled();
        assert_eq!(notifications.expect_next(),Notification::Updated);
        notifications.expect_pending();
        assert_eq!(db.lookup(3).unwrap().arguments.len(), 3);
        assert_eq!(db.lookup(3).unwrap().arguments[2].name, "NewArg");
        assert_eq!(db.version.get(), 8);
    }
}
