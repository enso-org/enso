use double_representation::name::QualifiedName;
use engine_protocol::language_server::SuggestionEntryArgument;
use crate::prelude::*;

use crate::model::suggestion_database::{entry, Entry};
use crate::model::SuggestionDatabase;

#[derive(Debug)]
pub struct Builder {
    next_id:    entry::Id,
    pub result: SuggestionDatabase,
    context:    Option<QualifiedName>,
}

impl Builder {
    fn new() -> Self {
        Self {
            next_id: 0,
            result: SuggestionDatabase::new_empty(),
            context: None,
        }
    }

    fn add_entry(&mut self, entry: Entry) {
        let id = self.next_id;
        self.next_id += 1;
        self.result.put_entry(self.next_id, entry);
    }

    pub fn add_and_enter_module<S>(&mut self, segment: S)
        where S: Into<ImString> + TryInto<QualifiedName> {
        let module_path = if let Some(path) = &mut self.context {
            path.push(segment.into());
            path.clone()
        } else {
            let initial_path = segment.try_into().unwrap();
            self.context = Some(initial_path.clone());
            initial_path
        };
        self.add_entry(Entry::new_module(module_path));
    }

    pub fn add_and_enter_type(
        &mut self,
        type_name: impl Into<String>,
        arguments: Vec<SuggestionEntryArgument>,
    ) {
        let type_name = type_name.into();
        let defined_in = self.context.take().expect("Cannot add type without module");
        self.context = Some(defined_in.clone().new_child(&type_name));
        self.add_entry(Entry::new_type(defined_in, type_name).with_arguments(arguments))
    }

    pub fn leave(&mut self) {
        self.context = self.context.and_then(|ctx| ctx.parent()).map(|ctx| ctx.to_owned())
    }

    pub fn add_method(
        &mut self,
        name: impl Into<String>,
        arguments: Vec<SuggestionEntryArgument>,
        return_type: impl TryInto<QualifiedName>,
        is_static: bool,
    ) {
        let on_type = self.context.as_ref().expect("Cannot add method without context");
        let return_type = return_type.try_into().expect("Invalid return type");
        self.add_entry(
            Entry::new_nonextension_method(on_type.clone(), name, return_type, is_static)
                .with_arguments(arguments),
        );
    }

    pub fn add_constructor(
        &mut self,
        name: impl Into<String>,
        arguments: Vec<SuggestionEntryArgument>,
    ) {
        let on_type = self.context.as_ref().expect("Cannot add constructor without context");
        self.add_entry(Entry::new_constructor(on_type.clone(), name).with_arguments(arguments));
    }
}

#[macro_export]
macro_rules! mock_suggestion_database_entry_argument {
        ($name:ident) => {
            SuggestionEntryArgument {
                name: stringify!($name),
                repr_type: "Any",
                is_suspended: false,
                has_default: false,
                default_value: false,
            }
        };
        ($name:ident: $($path:ident).*) => {
            SuggestionEntryArgument {
                name: stringify!($name),
                repr_type: stringify!($($path).*),
                is_suspended: false,
                has_default: false,
                default_value: false,
            }
        }
    }

#[macro_export]
macro_rules! mock_suggestion_database_entry_arguments {
        ($(($($arg_name:ident $(:$($arg_type_path:ident).*)?),*))?) => {
            vec![$($($crate::mock_suggestion_database_entry_argument!{$arg_name $(:$($arg_type_path).*)?}),*)?]
        }
    }

#[macro_export]
macro_rules! mock_suggestion_database_entries {
        ([$builder:ident] mod $name:ident { $($content:tt)* } $($rest:tt)*) => {
            $builder.add_and_enter_module(stringify!{$name});
            $crate::mock_database_entries! { [$builder] $($content)* };
            $builder.leave;
            $crate::mock_database_entries! { [$builder] $($rest)* };
        };
        ([$builder:ident] type $name:ident $(($($args:tt)*))? { $($content:tt)* } $($rest:tt)*) => {
            let args = $crate::mock_suggestion_database_entry_arguments! {$(($($args)*))?};
            $builder.add_and_enter_type(stringify!{$name}, args);
            $crate::mock_database_entries! { [$builder] $($content)* };
            $builder.leave;
            $crate::mock_database_entries! { [$builder] $($rest)* };
        };
        ([$builder:ident] $name:ident $(($($args:tt)*))?; $($rest:tt)*) => {
            let args = $crate::mock_suggestion_database_entry_arguments! {$(($($args)*))?};
            $builder.add_constructor(stringify!{$name}, args);
            $crate::mock_database_entries! { [$builder] $($rest)* };
        };
        ([$builder:ident] fn $name:ident $(($($args:tt)*))? -> $($return_type_path:ident).*; $($rest:tt)*) => {
            let args = $crate::mock_suggestion_database_entry_arguments! {$(($($args)*))?};
            $builder.add_method(stringify!{$name}, args, stringify!{$($return_type_path).*}, false);
            $crate::mock_database_entries! { [$builder] $($rest)* };
        };
        ([$builder:ident] static fn $name:ident $(($($args:tt)*))? -> $($return_type_path:ident).*; $($rest:tt)*) => {
            let args = $crate::mock_suggestion_database_entry_arguments! {$(($($args)*))?};
            $builder.add_method(stringify!{$name}, args, stringify!{$($return_type_path).*}, true);
            $crate::mock_database_entries! { [$builder] $($rest)* };
        };
        ([$builder:ident]) => {}
    }

#[macro_export]
macro_rules! mock_suggestion_database {
        ($($ns:ident.$project:ident { $($content:tt)* })*) => {
            {
                let mut builder = $crate::model::suggestion_database::mock::MockBuilder::new();
                $(
                    builder.add_and_enter_module(stringify!{$ns$(.$project)?});
                    $crate::mock_database_entries! { [builder] $($content)* };
                    builder.leave;
                )*
                builder.result
            }
        }
    }

mod tests {
    use super::*;

    #[test]
    fn mocking_suggestion_database() {
        let db = mock_suggestion_database! {
            Standard.Base {
                type Number;
                type Boolean;
                type Maybe {
                    Some (a);
                    None;

                    fn is_some() -> Boolean;
                }
            }
            local.Project {
                mod Submodule {
                    type TestType (a: Standard.Base.Number, b: Standard.Base.Maybe) {
                        static fn static_method(x) -> Standard.Base.Number;
                    }

                    fn module_method() -> local.Project.Submodule.TestType;
                }
            }
        };

        a
    }
}