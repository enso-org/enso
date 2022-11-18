use crate::prelude::*;

use crate::model::suggestion_database::entry;
use crate::model::suggestion_database::entry::Argument;
use crate::model::suggestion_database::Entry;
use crate::model::SuggestionDatabase;

use double_representation::name::QualifiedName;



pub const DEFAULT_TYPE: &str = "Standard.Base.Any";

#[derive(Debug)]
pub struct Builder {
    next_id:    entry::Id,
    pub result: SuggestionDatabase,
    in_module:  Option<QualifiedName>,
    in_type:    Option<QualifiedName>,
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}

impl Builder {
    pub fn new() -> Self {
        Self {
            next_id:   0,
            result:    SuggestionDatabase::new_empty(),
            in_module: None,
            in_type:   None,
        }
    }

    fn add_entry(&mut self, entry: Entry, modifier: impl FnOnce(Entry) -> Entry) {
        let id = self.next_id;
        self.next_id += 1;
        self.result.put_entry(id, modifier(entry));
    }

    pub fn add_and_enter_module<S>(&mut self, segment: S, modifier: impl FnOnce(Entry) -> Entry)
    where S: Into<ImString> + TryInto<QualifiedName, Error: Debug> {
        let module_path = if let Some(path) = &mut self.in_module {
            path.push_segment(segment.into());
            path.clone()
        } else {
            let initial_path = segment.try_into().unwrap();
            self.in_module = Some(initial_path.clone());
            initial_path
        };
        self.add_entry(Entry::new_module(module_path), modifier);
    }

    pub fn add_and_enter_type(
        &mut self,
        type_name: impl Into<String>,
        arguments: Vec<Argument>,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let type_name = type_name.into();
        let defined_in = self.in_module.as_ref().expect("Cannot add type when not in module");
        self.in_type = Some(defined_in.clone().new_child(&type_name));
        self.add_entry(
            Entry::new_type(defined_in.clone(), type_name).with_arguments(arguments),
            modifier,
        )
    }

    pub fn leave(&mut self) {
        if self.in_type.take().is_none() {
            self.in_module = self.in_module.as_ref().and_then(|m| m.parent()).map(|m| m.to_owned())
        }
    }

    pub fn add_method(
        &mut self,
        name: impl Into<String>,
        arguments: Vec<Argument>,
        return_type: impl TryInto<QualifiedName, Error: Debug>,
        is_static: bool,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let in_module = self.in_module.as_ref().expect("Cannot add method without context");
        let on_type = self.in_type.as_ref().unwrap_or(in_module);
        let return_type = return_type.try_into().expect("Invalid return type");
        self.add_entry(
            Entry::new_method(in_module.clone(), on_type.clone(), name, return_type, is_static)
                .with_arguments(arguments),
            modifier,
        );
    }

    pub fn add_constructor(
        &mut self,
        name: impl Into<String>,
        arguments: Vec<Argument>,
        modifier: impl FnOnce(Entry) -> Entry,
    ) {
        let on_type = self.in_type.as_ref().expect("Cannot add constructor when not in type");
        self.add_entry(
            Entry::new_constructor(on_type.clone(), name).with_arguments(arguments),
            modifier,
        );
    }
}

#[macro_export]
macro_rules! mock_suggestion_database_entry_argument {
    ($name:ident) => {
        Argument {
            name: stringify!($name).to_owned(),
            repr_type: DEFAULT_TYPE.to_owned(),
            is_suspended: false,
            has_default: false,
            default_value: None,
        }
    };
    ($name:ident: $($path:ident).*) => {
        Argument {
            name: stringify!($name).to_owned(),
            repr_type: stringify!($($path).*).to_owned(),
            is_suspended: false,
            has_default: false,
            default_value: None,
        }
    }
}

#[macro_export]
macro_rules! mock_suggestion_database_entry_arguments {
    ($(($($arg_name:ident $(:$($arg_type_path:ident).*)?),*))?) => {
        vec![$($(mock_suggestion_database_entry_argument!{$arg_name $(:$($arg_type_path).*)?}),*)?]
    }
}

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

#[macro_export]
macro_rules! mock_suggestion_database {
    ($($(#[$($attr_setter:tt)*])* $ns:ident.$project:ident { $($content:tt)* })*) => {
        {
            #[allow(unused_imports)]
            use $crate::model::suggestion_database::mock::Builder;
            #[allow(unused_imports)]
            use $crate::model::suggestion_database::mock::DEFAULT_TYPE;
            #[allow(unused_imports)]
            use $crate::mock_suggestion_database_entries;
            #[allow(unused_imports)]
            use $crate::mock_suggestion_database_entry_arguments;
            #[allow(unused_imports)]
            use $crate::mock_suggestion_database_entry_argument;
            #[allow(unused_imports)]
            use $crate::model::suggestion_database::entry::Argument;

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
            mod Submodule {
                #[with_documentation("Some test type")]
                type TestType (a: Standard.Base.Number, b: Standard.Base.Maybe) {
                    static fn static_method(x) -> Standard.Base.Number;
                }

                #[with_icon(entry::IconName::from_snake_case("TestIcon"))]
                static fn module_method() -> local.Project.Submodule.TestType;
            }
        }
    }
}

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
