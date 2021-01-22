//! Module with the Suggestion Database Entry and all structures related to.

use crate::prelude::*;

use crate::constants::keywords;
use crate::double_representation::module;
use crate::double_representation::tp;
use crate::model::module::MethodId;

use data::text::TextLocation;
use enso_protocol::language_server;
use enso_protocol::language_server::FieldUpdate;
use language_server::types::FieldAction;

pub use language_server::types::SuggestionEntryArgument as Argument;
pub use language_server::types::SuggestionId as Id;
pub use language_server::types::SuggestionsDatabaseUpdate as Update;



// ==============
// === Errors ===
// ==============

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
#[fail(display = "Entry named {} does not represent a method.", _0)]
pub struct NotAMethod(pub String);

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
pub enum Kind {
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
    pub kind : Kind,
    /// A module where the suggested object is defined, represented as vector of segments.
    pub module : module::QualifiedName,
    /// Argument lists of suggested object (atom or function). If the object does not take any
    /// arguments, the list is empty.
    pub arguments : Vec<Argument>,
    /// A type returned by the suggested object.
    pub return_type : String,
    /// A documentation associated with object.
    pub documentation : Option<String>,
    /// A type of the "self" argument. This field is `None` for non-method suggestions.
    pub self_type : Option<tp::QualifiedName>,
    /// A scope where this suggestion is visible.
    pub scope : Scope,
}

impl Entry {
    /// Check if this entry has self type same as the given identifier.
    pub fn has_self_type<TypeName>(&self, self_type:&TypeName) -> bool
    where TypeName : PartialEq<tp::QualifiedName> {
        self.self_type.contains(self_type)
    }

    /// Returns the code which should be inserted to Searcher input when suggestion is picked.
    pub fn code_to_insert(&self, current_module:Option<&module::QualifiedName>) -> String {
        if self.has_self_type(&self.module) {
            let module_var = if current_module.contains(&&self.module) {keywords::HERE.to_owned()}
                else {self.module.name().clone().into()};
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
        if self.kind != Kind::Method {
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
    pub fn is_visible_at(&self, module:&module::QualifiedName, location:TextLocation) -> bool {
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
                kind          : Kind::Atom,
                scope         : Scope::Everywhere,
            },
            Method {name,module,arguments,self_type,return_type,documentation,..} => Self {
                name,arguments,return_type,documentation,
                module        : module.try_into()?,
                self_type     : Some(self_type.try_into()?),
                kind          : Kind::Method,
                scope         : Scope::Everywhere,
            },
            Function {name,module,arguments,return_type,scope,..} => Self {
                name,arguments,return_type,
                module        : module.try_into()?,
                self_type     : None,
                documentation : default(),
                kind          : Kind::Function,
                scope         : Scope::InModule {range:scope.into()},
            },
            Local {name,module,return_type,scope,..} => Self {
                name,return_type,
                arguments     : default(),
                module        : module.try_into()?,
                self_type     : None,
                documentation : default(),
                kind          : Kind::Local,
                scope         : Scope::InModule {range:scope.into()},
            },
        };
        Ok(this)
    }

    /// Apply modification to the entry.
    pub fn apply_modifications
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
                Scope::Everywhere       => { err() },
                Scope::InModule {range} => {
                    if let Some(value) = update.value {
                        *range = value.into();
                        Ok(())
                    } else { err() }
                }
            }
        } else {
            Ok(())
        }
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
        (entry.kind== Kind::Method).ok_or_else(|| NotAMethod(entry.name.clone()))?;
        let missing_this_err = || MissingThisOnMethod(entry.name.clone());
        let defined_on_type  = entry.self_type.clone().ok_or_else(missing_this_err)?;
        Ok(language_server::MethodPointer {
            defined_on_type : defined_on_type.into(),
            module          : entry.module.to_string(),
            name            : entry.name.clone(),
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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn code_from_entry() {
        let module         = module::QualifiedName::from_text("Project.Main").unwrap();
        let another_module = module::QualifiedName::from_text("Project.AnotherModule").unwrap();
        let atom_entry = Entry {
            name          : "Atom".to_string(),
            kind          : Kind::Atom,
            module        : module.clone(),
            arguments     : vec![],
            return_type   : "Number".to_string(),
            documentation : None,
            self_type     : None,
            scope         : Scope::Everywhere,
        };
        let method_entry = Entry {
            name      : "method".to_string(),
            kind      : Kind::Method,
            self_type : Some("Base.Main.Number".to_string().try_into().unwrap()),
            ..atom_entry.clone()
        };
        let module_method_entry = Entry {
            name      : "moduleMethod".to_string(),
            self_type : Some(module.clone().into()),
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
            kind          : Kind::Function,
            module        : "Test.Test".to_string().try_into().unwrap(),
            arguments     : vec![],
            return_type   : "Number".to_string(),
            documentation : None,
            self_type     : None,
            scope         : Scope::Everywhere,
        };
        let method = Entry {
            name      : "method".to_string(),
            kind      : Kind::Method,
            self_type : Some("Base.Main.Number".to_string().try_into().unwrap()),
            ..non_method.clone()
        };
        let expected = MethodId {
            module          : "Test.Test".to_string().try_into().unwrap(),
            defined_on_type : "Base.Main.Number".to_string().try_into().unwrap(),
            name            : "method".to_string()
        };
        assert_eq!(non_method.method_id() , None);
        assert_eq!(method.method_id()     , Some(expected));
    }
}
