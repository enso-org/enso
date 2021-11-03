//! Module with the Suggestion Database Entry and all structures related to.

use crate::prelude::*;

use crate::constants::keywords;
use crate::double_representation::module;
use crate::double_representation::tp;
use crate::model::module::MethodId;

use data::text::TextLocation;
use enso_protocol::language_server;
use enso_protocol::language_server::FieldUpdate;
use enso_protocol::language_server::SuggestionsDatabaseModification;
use language_server::types::FieldAction;
use std::collections::BTreeSet;

pub use language_server::types::SuggestionEntryArgument as Argument;
pub use language_server::types::SuggestionId as Id;
pub use language_server::types::SuggestionsDatabaseUpdate as Update;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "Argument index {} is invalid for suggestion entry named {}.", index, name)]
pub struct InvalidArgumentIndex {
    pub name:  String,
    pub index: usize,
}

#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Fail)]
#[fail(display = "Invalid update for field {}.", _0)]
pub struct InvalidFieldUpdate(pub &'static str);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "Entry named {} does not represent a method.", _0)]
pub struct NotAMethod(pub String);

#[allow(missing_docs)]
#[derive(Debug, Fail, Clone)]
#[fail(display = "Entry named {} is described as method but does not have a `this` parameter.", _0)]
pub struct MissingThisOnMethod(pub String);



// =============
// === Entry ===
// =============

/// A type of suggestion entry.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum Kind {
    Atom,
    Function,
    Local,
    Method,
    Module,
}

/// Describes the visibility range of some entry (i.e. identifier available as suggestion).
///
/// Methods are visible "Everywhere", as they are imported on a module level, so they are not
/// specific to any particular span in the module file.
/// However local variables and local function have limited visibility.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Scope {
    /// The entry is visible in the whole module where it was defined. It can be also brought to
    /// other modules by import declarations.
    Everywhere,
    /// Local symbol that is visible only in a particular section of the module where it has been
    /// defined.
    #[allow(missing_docs)]
    InModule { range: RangeInclusive<TextLocation> },
}

/// Represents code snippet and the imports needed for it to work.
/// Typically is module-specific, as different modules may require different imports.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CodeToInsert {
    /// Code to be inserted.
    pub code:    String,
    /// Modules that need to be imported when inserting this code.
    pub imports: BTreeSet<module::QualifiedName>,
}

/// The Suggestion Database Entry.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entry {
    /// A type of suggestion.
    pub kind:               Kind,
    /// A module where the suggested object is defined, represented as vector of segments.
    pub module:             module::QualifiedName,
    /// A name of suggested object.
    pub name:               String,
    /// Argument lists of suggested object (atom or function). If the object does not take any
    /// arguments, the list is empty.
    pub arguments:          Vec<Argument>,
    /// A type returned by the suggested object.
    pub return_type:        String,
    /// A HTML documentation associated with object.
    pub documentation_html: Option<String>,
    /// A type of the "self" argument. This field is `None` for non-method suggestions.
    pub self_type:          Option<tp::QualifiedName>,
    /// A scope where this suggestion is visible.
    pub scope:              Scope,
}

impl Entry {
    /// Check if this entry has self type same as the given identifier.
    pub fn has_self_type<TypeName>(&self, self_type: &TypeName) -> bool
    where TypeName: PartialEq<tp::QualifiedName> {
        self.self_type.contains(self_type)
    }

    /// Describes code insertion to be done when Searcher input suggestion is picked.
    pub fn code_to_insert(
        &self,
        current_module: Option<&module::QualifiedName>,
        generate_this: bool,
    ) -> CodeToInsert {
        let is_local_entry = current_module.contains(&&self.module);
        let mut imports = BTreeSet::new();

        // Entry import should be skipped when:
        // * it is a regular (i.e. non extension) method, as it will bee found dynamically through
        //   the `this` argument;
        // * it is an entry defined in the current module, so it is already visible.
        let should_skip_import = self.is_regular_method() || is_local_entry;
        if !should_skip_import {
            imports.insert(self.module.clone());
        }

        let this_expr = if generate_this {
            // TODO [mwu] Currently we support `this` generation for module atoms only.
            //            This should be extended to any atom that is known to be nullary.
            //            Tracked by https://github.com/enso-org/ide/issues/1299
            if self.is_regular_module_method() {
                if is_local_entry {
                    // No additional import for `here`.
                    Some(keywords::HERE.to_owned())
                } else {
                    // If we are inserting an additional `this` argument, the used name must be
                    // visible.
                    imports.insert(self.module.clone());
                    let mut module = self.module.clone();
                    module.remove_main_module_segment();
                    Some(module.name().into())
                }
            } else {
                // Only nullary atoms can be generated by inserting their name.
                None
            }
        } else {
            // No "this" expression unless we have been requested to add one.
            None
        };

        let code = match this_expr {
            Some(this_expr) =>
                format!("{}{}{}", this_expr, ast::opr::predefined::ACCESS, self.name),
            None => self.name.clone(),
        };

        CodeToInsert { code, imports }
    }

    /// Check if this is a non-extension module method.
    // TODO [mwu] Currently we have no means to recognize module extension methods. It will be
    //            needed for implementing https://github.com/enso-org/ide/issues/1299
    //            Then likely separate methods `is_module_method` and `is_regular_method` should
    //            be introduced.
    pub fn is_regular_module_method(&self) -> bool {
        self.has_self_type(&self.module)
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
        } else {
            self.self_type.as_ref().map(|self_type| MethodId {
                module:          self.module.clone(),
                defined_on_type: self_type.clone(),
                name:            self.name.clone(),
            })
        }
    }

    /// Checks if entry is visible at given location in a specific module.
    pub fn is_visible_at(&self, module: &module::QualifiedName, location: TextLocation) -> bool {
        match &self.scope {
            Scope::Everywhere => true,
            Scope::InModule { range } => self.module == *module && range.contains(&location),
        }
    }

    /// Checks if entry name matches the given name. The matching is case-insensitive.
    pub fn matches_name(&self, name: impl Str) -> bool {
        self.name.to_lowercase() == name.as_ref().to_lowercase()
    }

    /// Generate information about invoking this entity for span tree context.
    pub fn invocation_info(&self) -> span_tree::generate::context::CalledMethodInfo {
        self.into()
    }

    /// Check if this is a regular method (i.e. non-extension method).
    pub fn is_regular_method(&self) -> bool {
        let is_method = self.kind == Kind::Method;
        let not_extension = self.self_type.contains_if(|this| this.in_module(&self.module))
            || self.self_type.contains(&self.module);
        is_method && not_extension
    }

    /// Get the full qualified name of the entry.
    pub fn qualified_name(&self) -> tp::QualifiedName {
        tp::QualifiedName::new_module_member(self.module.clone(), self.name.clone())
    }
}


// === Handling LanguageServer Types ===

impl Entry {
    /// Create entry from the structure deserialized from the Language Server responses.
    pub fn from_ls_entry(entry: language_server::types::SuggestionEntry) -> FallibleResult<Self> {
        use language_server::types::SuggestionEntry::*;
        let this = match entry {
            #[allow(unused)]
            Atom {
                name,
                module,
                arguments,
                return_type,
                documentation,
                documentation_html,
                ..
            } => Self {
                name,
                arguments,
                return_type,
                documentation_html,
                module: module.try_into()?,
                self_type: None,
                kind: Kind::Atom,
                scope: Scope::Everywhere,
            },
            #[allow(unused)]
            Method {
                name,
                module,
                arguments,
                self_type,
                return_type,
                documentation,
                documentation_html,
                ..
            } => Self {
                name,
                arguments,
                return_type,
                documentation_html,
                module: module.try_into()?,
                self_type: Some(self_type.try_into()?),
                kind: Kind::Method,
                scope: Scope::Everywhere,
            },
            Function { name, module, arguments, return_type, scope, .. } => Self {
                name,
                arguments,
                return_type,
                module: module.try_into()?,
                self_type: None,
                documentation_html: default(),
                kind: Kind::Function,
                scope: Scope::InModule { range: scope.into() },
            },
            Local { name, module, return_type, scope, .. } => Self {
                name,
                return_type,
                arguments: default(),
                module: module.try_into()?,
                self_type: None,
                documentation_html: default(),
                kind: Kind::Local,
                scope: Scope::InModule { range: scope.into() },
            },
            Module { module, documentation_html, .. } => {
                let module_name: module::QualifiedName = module.clone().try_into()?;
                Self {
                    documentation_html,
                    name: module_name.id().name().into(),
                    arguments: default(),
                    module: module_name,
                    self_type: None,
                    kind: Kind::Module,
                    scope: Scope::Everywhere,
                    return_type: module,
                }
            }
        };
        Ok(this)
    }

    /// Apply modification to the entry.
    pub fn apply_modifications(
        &mut self,
        modification: SuggestionsDatabaseModification,
    ) -> Vec<failure::Error> {
        let m = modification;
        let module = m.module.map(|f| f.try_map(module::QualifiedName::from_text)).transpose();
        let self_type = m.self_type.map(|f| f.try_map(tp::QualifiedName::from_text)).transpose();
        let other_update_results = [
            Entry::apply_field_update("return_type", &mut self.return_type, m.return_type),
            Entry::apply_opt_field_update(
                "documentation_html",
                &mut self.documentation_html,
                m.documentation_html,
            ),
            module.and_then(|m| Entry::apply_field_update("module", &mut self.module, m)),
            self_type
                .and_then(|s| Entry::apply_opt_field_update("self_type", &mut self.self_type, s)),
            self.apply_scope_update(m.scope),
        ];
        let other_update_results = SmallVec::from_buf(other_update_results).into_iter();
        let other_update_errors = other_update_results.filter_map(|res| res.err());
        let arg_update_errors = m.arguments.into_iter().flat_map(|arg| self.apply_arg_update(arg));
        arg_update_errors.chain(other_update_errors).collect_vec()
    }

    fn apply_arg_update(
        &mut self,
        update: language_server::types::SuggestionArgumentUpdate,
    ) -> Vec<failure::Error> {
        use language_server::types::SuggestionArgumentUpdate as Update;
        let error = |index| {
            let name = self.name.clone();
            vec![failure::Error::from(InvalidArgumentIndex { name, index })]
        };
        match update {
            Update::Add { index, .. } if index > self.arguments.len() => error(index),
            Update::Remove { index } | Update::Modify { index, .. }
                if index >= self.arguments.len() =>
                error(index),
            Update::Add { index, argument } => {
                self.arguments.insert(index, argument);
                vec![]
            }
            Update::Remove { index } => {
                self.arguments.remove(index);
                vec![]
            }
            Update::Modify { index, name, repr_type, is_suspended, has_default, default_value } => {
                let arg = &mut self.arguments[index];
                type E = Entry;
                let results = [
                    E::apply_field_update("name", &mut arg.name, name),
                    E::apply_field_update("repr_type", &mut arg.repr_type, repr_type),
                    E::apply_field_update("is_suspended", &mut arg.is_suspended, is_suspended),
                    E::apply_field_update("has_default", &mut arg.has_default, has_default),
                    E::apply_opt_field_update(
                        "default_value",
                        &mut arg.default_value,
                        default_value,
                    ),
                ];
                SmallVec::from_buf(results).into_iter().filter_map(|res| res.err()).collect_vec()
            }
        }
    }

    fn apply_scope_update(
        &mut self,
        update: Option<FieldUpdate<language_server::types::SuggestionEntryScope>>,
    ) -> FallibleResult {
        if let Some(update) = update {
            let err = || Err(failure::Error::from(InvalidFieldUpdate("scope")));
            match &mut self.scope {
                Scope::Everywhere => err(),
                Scope::InModule { range } =>
                    if let Some(value) = update.value {
                        *range = value.into();
                        Ok(())
                    } else {
                        err()
                    },
            }
        } else {
            Ok(())
        }
    }

    fn apply_field_update<T>(
        field_name: &'static str,
        field: &mut T,
        update: Option<FieldUpdate<T>>,
    ) -> FallibleResult {
        let err = InvalidFieldUpdate(field_name);
        if let Some(update) = update {
            match update.tag {
                FieldAction::Set => {
                    *field = update.value.ok_or(err)?;
                    Ok(())
                }
                FieldAction::Remove => Err(err.into()),
            }
        } else {
            Ok(())
        }
    }

    fn apply_opt_field_update<T>(
        field_name: &'static str,
        field: &mut Option<T>,
        update: Option<FieldUpdate<T>>,
    ) -> FallibleResult {
        let err = InvalidFieldUpdate(field_name);
        if let Some(update) = update {
            match update.tag {
                FieldAction::Set => *field = Some(update.value.ok_or(err)?),
                FieldAction::Remove => *field = None,
            }
        }
        Ok(())
    }
}

impl TryFrom<language_server::types::SuggestionEntry> for Entry {
    type Error = failure::Error;
    fn try_from(entry: language_server::types::SuggestionEntry) -> FallibleResult<Self> {
        Self::from_ls_entry(entry)
    }
}

impl TryFrom<&Entry> for language_server::MethodPointer {
    type Error = failure::Error;
    fn try_from(entry: &Entry) -> FallibleResult<Self> {
        (entry.kind == Kind::Method).ok_or_else(|| NotAMethod(entry.name.clone()))?;
        let missing_this_err = || MissingThisOnMethod(entry.name.clone());
        let defined_on_type = entry.self_type.clone().ok_or_else(missing_this_err)?;
        Ok(language_server::MethodPointer {
            defined_on_type: defined_on_type.into(),
            module:          entry.module.to_string(),
            name:            entry.name.clone(),
        })
    }
}

impl TryFrom<Entry> for language_server::MethodPointer {
    type Error = failure::Error;
    fn try_from(entry: Entry) -> FallibleResult<Self> {
        language_server::MethodPointer::try_from(&entry)
    }
}

impl From<&Entry> for span_tree::generate::context::CalledMethodInfo {
    fn from(entry: &Entry) -> span_tree::generate::context::CalledMethodInfo {
        let parameters = entry.arguments.iter().map(to_span_tree_param).collect();
        span_tree::generate::context::CalledMethodInfo { parameters }
    }
}

// === SpanTree helpers ===

/// Converts the information about function parameter from suggestion database into the form used
/// by the span tree nodes.
pub fn to_span_tree_param(param_info: &Argument) -> span_tree::ArgumentInfo {
    span_tree::ArgumentInfo {
        // TODO [mwu] Check if database actually do must always have both of these filled.
        name: Some(param_info.name.clone()),
        tp:   Some(param_info.repr_type.clone()),
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;


    fn expect(
        entry: &Entry,
        current_module: Option<&module::QualifiedName>,
        generate_this: bool,
        expected_code: &str,
        expected_imports: &[&module::QualifiedName],
    ) {
        let CodeToInsert { code, imports } = entry.code_to_insert(current_module, generate_this);
        assert_eq!(code, expected_code);
        assert_eq!(imports.iter().collect_vec().as_slice(), expected_imports);
    }

    #[test]
    fn code_from_entry() {
        let main_module = module::QualifiedName::from_text("local.Project.Main").unwrap();
        let another_module =
            module::QualifiedName::from_text("local.Project.Another_Module").unwrap();
        let atom = Entry {
            name:               "Atom".to_owned(),
            kind:               Kind::Atom,
            module:             main_module.clone(),
            arguments:          vec![],
            return_type:        "Number".to_owned(),
            documentation_html: None,
            self_type:          None,
            scope:              Scope::Everywhere,
        };
        let method = Entry {
            name: "method".to_string(),
            kind: Kind::Method,
            self_type: Some("std.Base.Main.Number".to_string().try_into().unwrap()),
            ..atom.clone()
        };
        let module_method = Entry {
            name: "module_method".to_owned(),
            self_type: Some(main_module.clone().into()),
            ..method.clone()
        };
        let another_module_method = Entry {
            module: another_module.clone(),
            self_type: Some(another_module.clone().into()),
            ..module_method.clone()
        };
        let module_extension = Entry {
            module: another_module.clone(),
            name: "module_extension".to_string(),
            ..module_method.clone()
        };
        let atom_extension = Entry {
            module: another_module.clone(),
            name: "atom_extension".to_string(),
            self_type: Some(atom.qualified_name()),
            ..module_method.clone()
        };

        expect(&atom, None, true, "Atom", &[&main_module]);
        expect(&atom, None, false, "Atom", &[&main_module]);
        expect(&atom, Some(&main_module), true, "Atom", &[]);
        expect(&atom, Some(&main_module), false, "Atom", &[]);
        expect(&atom, Some(&another_module), true, "Atom", &[&main_module]);
        expect(&atom, Some(&another_module), false, "Atom", &[&main_module]);
        expect(&atom, Some(&another_module), true, "Atom", &[&main_module]);
        expect(&atom, Some(&another_module), false, "Atom", &[&main_module]);

        expect(&method, None, true, "method", &[&main_module]);
        expect(&method, None, false, "method", &[&main_module]);
        expect(&method, Some(&main_module), true, "method", &[]);
        expect(&method, Some(&main_module), false, "method", &[]);
        expect(&method, Some(&another_module), true, "method", &[&main_module]);
        expect(&method, Some(&another_module), false, "method", &[&main_module]);

        expect(&module_method, None, true, "Project.module_method", &[&main_module]);
        expect(&module_method, None, false, "module_method", &[]);
        expect(&module_method, Some(&main_module), true, "here.module_method", &[]);
        expect(&module_method, Some(&main_module), false, "module_method", &[]);
        expect(&module_method, Some(&another_module), true, "Project.module_method", &[
            &main_module,
        ]);
        expect(&module_method, Some(&another_module), false, "module_method", &[]);

        expect(&another_module_method, None, true, "Another_Module.module_method", &[
            &another_module,
        ]);
        expect(&another_module_method, None, false, "module_method", &[]);
        expect(
            &another_module_method,
            Some(&main_module),
            true,
            "Another_Module.module_method",
            &[&another_module],
        );
        expect(&another_module_method, Some(&main_module), false, "module_method", &[]);
        expect(&another_module_method, Some(&another_module), true, "here.module_method", &[]);
        expect(&another_module_method, Some(&another_module), false, "module_method", &[]);

        // TODO [mwu] Extensions on nullary atoms should also be able to generate this.
        //            The tests below will need to be adjusted, once that behavior is fixed.
        //            See https://github.com/enso-org/ide/issues/1299
        expect(&module_extension, None, true, "module_extension", &[&another_module]);
        expect(&module_extension, None, false, "module_extension", &[&another_module]);
        expect(&module_extension, Some(&main_module), true, "module_extension", &[&another_module]);
        expect(&module_extension, Some(&main_module), false, "module_extension", &[
            &another_module,
        ]);
        expect(&module_extension, Some(&another_module), true, "module_extension", &[]);
        expect(&module_extension, Some(&another_module), false, "module_extension", &[]);

        expect(&atom_extension, None, true, "atom_extension", &[&another_module]);
        expect(&atom_extension, None, false, "atom_extension", &[&another_module]);
        expect(&atom_extension, Some(&main_module), true, "atom_extension", &[&another_module]);
        expect(&atom_extension, Some(&main_module), false, "atom_extension", &[&another_module]);
        expect(&atom_extension, Some(&another_module), true, "atom_extension", &[]);
        expect(&atom_extension, Some(&another_module), false, "atom_extension", &[]);
    }

    #[test]
    fn method_id_from_entry() {
        let non_method = Entry {
            name:               "function".to_string(),
            kind:               Kind::Function,
            module:             "Test.Test".to_string().try_into().unwrap(),
            arguments:          vec![],
            return_type:        "Number".to_string(),
            documentation_html: None,
            self_type:          None,
            scope:              Scope::Everywhere,
        };
        let method = Entry {
            name: "method".to_string(),
            kind: Kind::Method,
            self_type: Some("Base.Main.Number".to_string().try_into().unwrap()),
            ..non_method.clone()
        };
        let expected = MethodId {
            module:          "Test.Test".to_string().try_into().unwrap(),
            defined_on_type: "Base.Main.Number".to_string().try_into().unwrap(),
            name:            "method".to_string(),
        };
        assert_eq!(non_method.method_id(), None);
        assert_eq!(method.method_id(), Some(expected));
    }
}
