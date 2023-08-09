//! A module with utilities for building [`SuggestionDatabase`] mocks in tests. The most important
//! is [`mock_suggestion_database`] macro.

use crate::prelude::*;

use crate::entry;
use crate::entry::Argument;
use crate::Entry;
use crate::SuggestionDatabase;

use double_representation::name::QualifiedName;
use enso_text::Location;


// ==============
// === Export ===
// ==============

pub use enso_doc_parser;



// =================
// === Constants ===
// =================

/// Default type of the argument.
pub const DEFAULT_TYPE: &str = "Standard.Base.Any";



// ===============
// === Builder ===
// ===============

/// Suggestion Database Mock builder.
///
/// This builder is contextual; after adding a new (sub) module we are in the context of this
/// module, and each entity will be added to this module. To go back a one level up in module
/// hierarchy use [`leave`] method. Similarly for types.
///
/// # Panics
///
/// This structure is meant to be used in tests only, therefore it does not restrain from panicking
/// on invalid operations (e.g. when trying to add an entry without any module in context).
///
/// # Example
///
/// ```
/// use enso_suggestion_database::entry::IconName;
/// use enso_suggestion_database::mock::Builder;
///
/// let mut builder = Builder::new();
/// builder.add_and_enter_module("local.Project", |e| e);
/// builder.add_and_enter_type("Type", vec![], |e| e.with_icon(IconName::from_tag_body("an_icon")));
/// builder.add_constructor("Constructor", vec![], |e| e);
/// builder.leave();
/// builder.add_method("module_method", vec![], "local.Project.Type", true, |e| e);
/// builder.leave();
/// let db = builder.result;
///
/// assert!(db.lookup_by_qualified_name_str("local.Project.Type.Constructor").is_ok());
/// assert!(db.lookup_by_qualified_name_str("local.Project.module_method").is_ok());
/// ```
#[derive(Debug, Default)]
pub struct Builder {
    next_id:    entry::Id,
    /// The built database.
    pub result: SuggestionDatabase,
    in_module:  Option<QualifiedName>,
    in_type:    Option<QualifiedName>,
}


impl Builder {
    /// Create new builder.
    pub fn new() -> Self {
        default()
    }

    /// Create a builder from an already existing database. The builder will start with the next
    /// available entry id.
    pub fn from_existing_db(db: SuggestionDatabase) -> Self {
        let next_id = db.entries.borrow().keys().cloned().max().unwrap_or_default() + 1;
        Self { next_id, result: db, in_module: None, in_type: None }
    }

    fn add_entry(&mut self, entry: Entry, modifier: impl FnOnce(Entry) -> Entry) {
        let id = self.next_id;
        self.next_id += 1;
        self.result.put_entry(id, modifier(entry));
    }

    /// Add a new module and set it as a new context, so the next entries will be added inside this
    /// module now.
    ///
    /// If the context is empty, the `segment` parameter should be a valid qualified name (with
    /// project namespace and name), otherwise this function will panic. If we are already in the
    /// module, the `segment` shall be just a name of the new sub-module.
    pub fn add_and_enter_module<S>(&mut self, segment: S, modifier: impl FnOnce(Entry) -> Entry)
    where S: Into<ImString> + TryInto<QualifiedName, Error: Debug> {
        self.enter_module(segment);
        let module_path = self.in_module.as_ref().unwrap();
        self.add_entry(Entry::new_module(module_path.clone()), modifier);
    }

    /// Add a new type and set it as new context, so the next entries will be added inside this type
    /// now.
    pub fn add_and_enter_type(
        &mut self,
        type_name: impl Into<String>,
        arguments: Vec<Argument>,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let type_name = type_name.into();
        let defined_in = self.in_module.as_ref().expect("Cannot add type when not in module.");
        self.in_type = Some(defined_in.clone().new_child(&type_name));
        self.add_entry(
            Entry::new_type(defined_in.clone(), type_name).with_arguments(arguments),
            modifier,
        )
    }

    /// Enter a module and set it as a new context, so the next entries will be added inside this
    /// module now.
    ///
    /// If the context is empty, the `segment` parameter should be a valid qualified name (with
    /// project namespace and name), otherwise this function will panic. If we are already in the
    /// module, the `segment` shall be just a name of the new sub-module.
    pub fn enter_module<S>(&mut self, segment: S)
    where S: Into<ImString> + TryInto<QualifiedName, Error: Debug> {
        if let Some(path) = &mut self.in_module {
            path.push_segment(segment.into());
        } else {
            let initial_path = segment.try_into().unwrap();
            self.in_module = Some(initial_path);
        }
    }

    /// Leave the current type or module.
    ///
    /// If the context was inside type, we just leave it to containing module. If it was a module,
    /// we're going one level in module hierarchy up. If there is no parent module, the context
    /// is set to empty and we're expected to call [`add_and_enter_module`] with fully qualified
    /// name, or consume the [`result`].
    pub fn leave(&mut self) {
        if self.in_type.take().is_none() {
            self.in_module = self.in_module.as_ref().and_then(|m| m.parent()).map(|m| m.to_owned())
        }
    }

    /// Add a new method.
    ///
    /// The method self type will be taken from the context.
    pub fn add_method(
        &mut self,
        name: impl Into<ImString>,
        arguments: Vec<Argument>,
        return_type: impl TryInto<QualifiedName, Error: Debug>,
        is_static: bool,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let in_module = self.in_module.as_ref().expect("Cannot add method without context.");
        let on_type = self.in_type.as_ref().unwrap_or(in_module);
        let return_type = return_type.try_into().expect("Invalid return type.");
        self.add_entry(
            Entry::new_method(in_module.clone(), on_type.clone(), name, return_type, is_static)
                .with_arguments(arguments),
            modifier,
        );
    }

    /// Add a new constructor.
    ///
    /// Context should be inside a type, otherwise this function will panic.
    pub fn add_constructor(
        &mut self,
        name: impl Into<ImString>,
        arguments: Vec<Argument>,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let on_type = self.in_type.as_ref().expect("Cannot add constructor when not in type.");
        self.add_entry(
            Entry::new_constructor(on_type.clone(), name).with_arguments(arguments),
            modifier,
        );
    }

    /// Add a new function.
    ///
    /// Context should be inside a module, otherwise this function will panic.
    pub fn add_function(
        &mut self,
        name: impl Into<ImString>,
        arguments: Vec<Argument>,
        return_type: impl TryInto<QualifiedName, Error: Debug>,
        scope: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let in_module = self.in_module.as_ref().expect("Cannot add function without context.");
        let return_type = return_type.try_into().expect("Invalid return type.");
        let function = Entry::new_function(in_module.clone(), name, return_type, scope);
        let entry = function.with_arguments(arguments);
        self.add_entry(entry, modifier);
    }

    /// Add a new local.
    ///
    /// Context should be inside a module, otherwise this function will panic.
    pub fn add_local(
        &mut self,
        name: impl Into<ImString>,
        return_type: impl TryInto<QualifiedName, Error: Debug>,
        scope: RangeInclusive<Location<enso_text::Utf16CodeUnit>>,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let in_module = self.in_module.as_ref().expect("Cannot add local without context.");
        let return_type = return_type.try_into().expect("Invalid return type.");
        self.add_entry(Entry::new_local(in_module.clone(), name, return_type, scope), modifier);
    }
}



// ================================
// === mock_suggestion_database ===
// ================================

/// Mock a single [`Argument`] basing on the method/type/constructor argument syntax in
/// [`mock_suggestion_database`] macro. See it's documentation for details.
#[macro_export]
macro_rules! mock_suggestion_database_entry_argument {
    ($name:ident) => {
        Argument {
            name: stringify!($name).to_owned(),
            repr_type: DEFAULT_TYPE.to_owned(),
            is_suspended: false,
            has_default: false,
            default_value: None,
            tag_values:    Vec::new(),
        }
    };
    ($name:ident: $($path:ident).*) => {
        Argument {
            name: stringify!($name).to_owned(),
            repr_type: stringify!($($path).*).to_owned(),
            is_suspended: false,
            has_default: false,
            default_value: None,
            tag_values:    Vec::new(),
        }
    }
}

/// Mock a vector of [`Argument`] basing on the method/type/constructor argument list syntax in
/// [`mock_suggestion_database`] macro. See it's documentation for details.
#[macro_export]
macro_rules! mock_suggestion_database_entry_arguments {
    ($(($($arg_name:ident $(:$($arg_type_path:ident).*)?),*))?) => {
        vec![$($(mock_suggestion_database_entry_argument!{$arg_name $(:$($arg_type_path).*)?}),*)?]
    }
}

/// Mock recursively a list of entries.
///
/// This is a helper for [`mock_suggestion_database`] macro. See it's documentation for details.
#[macro_export]
macro_rules! mock_suggestion_database_entries {
    ([$builder:ident] $(#[$($attr_setter:tt)*])* mod $name:ident { $($content:tt)* } $($rest:tt)*) => {
        $builder.add_and_enter_module(stringify!{$name}, |e| e$(.$($attr_setter)*)*);
        mock_suggestion_database_entries! { [$builder] $($content)* };
        $builder.leave();
        mock_suggestion_database_entries! { [$builder] $($rest)* };
    };
    ([$builder:ident] $(#[$($attr_setter:tt)*])* type $name:ident $(($($args:tt)*))? { $($content:tt)* } $($rest:tt)*) => {
        let args = mock_suggestion_database_entry_arguments! {$(($($args)*))?};
        $builder.add_and_enter_type(stringify!{$name}, args, |e| e$(.$($attr_setter)*)*);
        mock_suggestion_database_entries! { [$builder] $($content)* };
        $builder.leave();
        mock_suggestion_database_entries! { [$builder] $($rest)* };
    };
    ([$builder:ident] $(#[$($attr_setter:tt)*])* $name:ident $(($($args:tt)*))?; $($rest:tt)*) => {
        let args = mock_suggestion_database_entry_arguments! {$(($($args)*))?};
        $builder.add_constructor(stringify!{$name}, args, |e| e$(.$($attr_setter)*)*);
        mock_suggestion_database_entries! { [$builder] $($rest)* };
    };
    ([$builder:ident] $(#[$($attr_setter:tt)*])* fn $name:ident $(($($args:tt)*))? -> $($return_type_path:ident).*; $($rest:tt)*) => {
        let args = mock_suggestion_database_entry_arguments! {$(($($args)*))?};
        $builder.add_method(stringify!{$name}, args, stringify!{$($return_type_path).*}, false, |e| e$(.$($attr_setter)*)*);
        mock_suggestion_database_entries! { [$builder] $($rest)* };
    };
    ([$builder:ident] $(#[$($attr_setter:tt)*])* static fn $name:ident $(($($args:tt)*))? -> $($return_type_path:ident).*; $($rest:tt)*) => {
        let args = mock_suggestion_database_entry_arguments! {$(($($args)*))?};
        $builder.add_method(stringify!{$name}, args, stringify!{$($return_type_path).*}, true, |e| e$(.$($attr_setter)*)*);
        mock_suggestion_database_entries! { [$builder] $($rest)* };
    };
    ([$builder:ident]) => {}
}

/// Mock suggestion database macro.
///
/// This macro takes a declaration of suggestion database entries in a kind of pseudo code:
/// ```
/// use enso_suggestion_database::entry;
/// use enso_suggestion_database::mock_suggestion_database;
///
/// let db = mock_suggestion_database! {
///    Standard.Base {
///        type Maybe {
///            Some (a);
///            None;
///
///            fn is_some() -> Standard.Base.Boolean;
///        }
///    }
///    local.Project {
///        mod Submodule {
///            type TestType (a: Standard.Base.Number, b: Standard.Base.Maybe) {
///                static fn static_method(x) -> Standard.Base.Number;
///            }
///
///            #[with_icon(entry::IconName::from_tag_body("TestIcon"))]
///            static fn module_method() -> local.Project.Submodule.TestType;
///        }
///    }
/// };
/// ```
///
/// ## Statements
///
/// There are five possible statements:
/// * In the root scope the only valid statements are _project declarations_ - a project name (with
///   a namespace) followed by a block containing statements of the main project module content.
/// * _Module declarations_ starts with `mod` keyword followed by a name and block with module
///   content.
/// * _Type declarations_ startis with `type` keyword followed by a name, optional argument list,
///   and the block with type content (its constructors and methods).
/// * _Method declarations_ starts with `fn` (or `static fn` if the function is static) followed by
///   a name, argument list and return type.
/// * _Constructor declarations_ starts right with the constructor name, followed by optional
///   argument list.
///
/// ## Arguments and return types.
///
/// If the argument has no type declared, it defaults to the [`DEFAULT_TYPE`] contant. **There is no
/// name resulotion mechanism there; every type in argument list or return type must
/// be fully qualified**.
///
/// ## Attributes
///
/// Each statement may be preceded by one or more attributes looking like in Rust. The attribute
/// content is just a call to the [`Entry`] modifier - a method of [`Entry`] taking and returning
/// modified `self`.
///
/// See also [`doc_section!`] macro for defining documentation sections for entries.
#[macro_export]
macro_rules! mock_suggestion_database {
    ($($(#[$($attr_setter:tt)*])* $ns:ident.$project:ident { $($content:tt)* })*) => {
        {
            #[allow(unused_imports)]
            use $crate::mock::Builder;
            #[allow(unused_imports)]
            use $crate::mock::DEFAULT_TYPE;
            #[allow(unused_imports)]
            use $crate::mock_suggestion_database_entries;
            #[allow(unused_imports)]
            use $crate::mock_suggestion_database_entry_arguments;
            #[allow(unused_imports)]
            use $crate::mock_suggestion_database_entry_argument;
            #[allow(unused_imports)]
            use $crate::entry::Argument;

            let mut builder = Builder::new();
            $(
                builder.add_and_enter_module(stringify!{$ns.$project}, |e| e$(.$($attr_setter)*)*);
                mock_suggestion_database_entries! { [builder] $($content)* };
                builder.leave();
            )*
            builder.result
        }
    }
}

/// This is a helper for [`mock_suggestion_database`] macro. See it's documentation for details.
#[macro_export]
macro_rules! doc_section_mark {
    (!) => {
        $crate::mock::enso_doc_parser::Mark::Important
    };
    (>) => {
        $crate::mock::enso_doc_parser::Mark::Example
    };
    (?) => {
        $crate::mock::enso_doc_parser::Mark::Info
    };
}

/// A helper macro for mocking entry's documentation.
///
/// ### [`DocSection::Paragrah`]
/// ```
/// # use enso_suggestion_database::doc_section;
/// doc_section!("Some text.");
/// ```
///
/// ### [`DocSection::Tag`]
/// ```
/// # use enso_suggestion_database::doc_section;
/// doc_section!(@ Deprecated, "Tag body.");
/// ```
///
/// ### [`DocSection::Keyed`]
/// ```
/// # use enso_suggestion_database::doc_section;
/// doc_section!("Key" => "Value");
/// ```
///
/// ### [`DocSection::Marked`]
/// ```
/// # use enso_suggestion_database::doc_section;
/// doc_section!(! "Marked as important");
/// doc_section!(! "Optional header", "Marked as important");
/// doc_section!(? "Marked as info");
/// doc_section!(? "Optional header", "Marked as info");
/// doc_section!(> "Marked as example");
/// doc_section!(> "Optional header", "Marked as example");
/// ```
#[macro_export]
macro_rules! doc_section {
    (@ $tag:ident, $body:expr) => {
        $crate::mock::enso_doc_parser::DocSection::Tag {
            tag:  $crate::mock::enso_doc_parser::Tag::$tag,
            body: $body.into(),
        }
    };
    ($mark:tt $body:expr) => {
        $crate::mock::enso_doc_parser::DocSection::Marked {
            mark:   $crate::doc_section_mark!($mark),
            header: None,
            body:   $body.into(),
        }
    };
    ($mark:tt $header:expr, $body:expr) => {
        $crate::mock::enso_doc_parser::DocSection::Marked {
            mark:   $crate::doc_section_mark!($mark),
            header: Some($header.into()),
            body:   $body.into(),
        }
    };
    ($paragraph:expr) => {
        $crate::mock::enso_doc_parser::DocSection::Paragraph { body: $paragraph.into() }
    };
    ($key:expr => $body:expr) => {
        $crate::mock::enso_doc_parser::DocSection::Keyed { key: $key.into(), body: $body.into() }
    };
}



// ========================
// === standard_db_mock ===
// ========================

/// An basic [`SuggestionDatabase`] mock containing various types of entries: modules, types
/// module methods, static methods etc. used as a default mock for tests.
pub fn standard_db_mock() -> SuggestionDatabase {
    mock_suggestion_database! {
        Standard.Base {
            type Number {}
            type Boolean {}
            type Maybe {
                Some (a);
                None;

                fn is_some() -> Standard.Base.Boolean;
            }
        }
        local.Project {
            static fn main_module_method() -> Standard.Base.Any;
            mod Submodule {
                type TestType (a: Standard.Base.Number, b: Standard.Base.Maybe) {
                    static fn static_method(x) -> Standard.Base.Number;
                }

                #[with_icon(entry::IconName::from_tag_body("TestIcon"))]
                static fn module_method() -> local.Project.Submodule.TestType;
            }
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mocking_suggestion_database() {
        let db = standard_db_mock();

        let standard_base = db.lookup_by_qualified_name_str("Standard.Base").unwrap();
        assert_eq!(standard_base.kind, entry::Kind::Module);
        assert_eq!(standard_base.name, "Main");

        let number = db.lookup_by_qualified_name_str("Standard.Base.Number").unwrap();
        assert_eq!(number.kind, entry::Kind::Type);
        assert_eq!(number.defined_in.to_string(), "Standard.Base");
        assert_eq!(number.arguments.len(), 0);

        let some = db.lookup_by_qualified_name_str("Standard.Base.Maybe.Some").unwrap();
        assert_eq!(some.kind, entry::Kind::Constructor);
        assert_eq!(some.defined_in.to_string(), "Standard.Base");
        assert_eq!(some.self_type.as_ref().unwrap().to_string(), "Standard.Base.Maybe");
        assert_eq!(some.arguments.len(), 1);
        assert_eq!(some.arguments[0].name, "a");
        assert_eq!(some.arguments[0].repr_type, DEFAULT_TYPE);

        let is_some = db.lookup_by_qualified_name_str("Standard.Base.Maybe.is_some").unwrap();
        assert_eq!(is_some.kind, entry::Kind::Method);
        assert_eq!(is_some.defined_in.to_string(), "Standard.Base");
        assert_eq!(is_some.self_type.as_ref().unwrap().to_string(), "Standard.Base.Maybe");
        assert_eq!(is_some.return_type.to_string(), "Standard.Base.Boolean");
        assert_eq!(is_some.arguments.len(), 0);
        assert!(!is_some.is_static);

        let test_type =
            db.lookup_by_qualified_name_str("local.Project.Submodule.TestType").unwrap();
        assert_eq!(test_type.kind, entry::Kind::Type);
        assert_eq!(test_type.defined_in.to_string(), "local.Project.Submodule");
        assert_eq!(test_type.arguments.len(), 2);
        assert_eq!(test_type.arguments[0].name, "a");
        assert_eq!(test_type.arguments[0].repr_type, "Standard.Base.Number");
        assert_eq!(test_type.arguments[1].name, "b");
        assert_eq!(test_type.arguments[1].repr_type, "Standard.Base.Maybe");

        let static_method = db
            .lookup_by_qualified_name_str("local.Project.Submodule.TestType.static_method")
            .unwrap();
        assert_eq!(static_method.kind, entry::Kind::Method);
        assert_eq!(static_method.defined_in.to_string(), "local.Project.Submodule");
        assert_eq!(
            static_method.self_type.as_ref().unwrap().to_string(),
            "local.Project.Submodule.TestType"
        );
        assert_eq!(static_method.return_type.to_string(), "Standard.Base.Number");
        assert_eq!(static_method.arguments.len(), 1);
        assert_eq!(static_method.arguments[0].name, "x");
        assert_eq!(static_method.arguments[0].repr_type, DEFAULT_TYPE);
        assert!(static_method.is_static);

        let module_method =
            db.lookup_by_qualified_name_str("local.Project.Submodule.module_method").unwrap();
        assert_eq!(module_method.kind, entry::Kind::Method);
        assert_eq!(module_method.defined_in.to_string(), "local.Project.Submodule");
        assert_eq!(
            module_method.self_type.as_ref().unwrap().to_string(),
            "local.Project.Submodule"
        );
        assert_eq!(module_method.return_type.to_string(), "local.Project.Submodule.TestType");
        assert_eq!(module_method.arguments.len(), 0);
        assert!(module_method.is_static);
    }
}
