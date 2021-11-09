//! Built-in verbosity level definitions and a set of utilities to define custom levels. Please note
//! that the verbosity level mechanism is completely user-extensible and this implementation can be
//! completely redefined by the user.

use crate::entry::level;
use crate::entry::Entry;
use crate::prelude::*;
use crate::processor::formatter;



// ==============
// === Macros ===
// ==============

/// Utility for defining verbosity levels. Each verbosity level is defined as a separate structure.
/// Moreover, it will also define a module `filter_from` containing similar structures, which will
/// be useful for compile time filtering. The meaning of `filter_from::Warning` is meant to be
/// "keep every message with priority higher or equal to warning".
///
/// For example, for the given input `define_levels!(Trace,Debug,Info,Warning,Error);`, the
/// following output will be generated:
///
/// ```ignore
///     #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///     pub struct Trace;
///     #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///     pub struct Debug;
///     #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///     pub struct Info;
///     #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///     pub struct Warning;
///     #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///     pub struct Error;
///     pub mod filter_from {
///         #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///         pub struct Trace;
///         #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///         pub struct Debug;
///         #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///         pub struct Info;
///         #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///         pub struct Warning;
///         #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
///         pub struct Error;
///     }
/// ```
#[macro_export]
macro_rules! define_levels {
    ($($name:ident),*) => {
        $(
            /// Log level.
            #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
            pub struct $name;
        )*

        /// Allows compile-time filtering of all entries from (more important) than the selected
        /// level. For example, `filter_from::Warning` will keep warnings and errors only.
        pub mod filter_from {
            $(
                /// Filtering log level.
                #[derive(Clone,Copy,Debug,Default,PartialEq,Eq,Hash)]
                pub struct $name;
            )*
        }
    };
}


/// Group levels defined with `define_levels` (possibly several, possibly merging user-defined
/// types) into a common structure for logger parametrization. Also, defines a generic formatter
/// impl for the type, which redirects calls to more specific instances.
///
/// For example, for `define_levels_group!(DefaultLevels {Trace,Debug,Info,Warning,Error});`, the
/// following code will be generated:
///
/// ```ignore
/// #[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
/// pub enum DefaultLevels { Trace,Debug,Info,Warning,Error }
/// impl From<Trace>   for DefaultLevels { fn from(_: Trace)   -> Self { Self::Trace } }
/// impl From<Debug>   for DefaultLevels { fn from(_: Debug)   -> Self { Self::Debug } }
/// impl From<Info>    for DefaultLevels { fn from(_: Info)    -> Self { Self::Info } }
/// impl From<Warning> for DefaultLevels { fn from(_: Warning) -> Self { Self::Warning } }
/// impl From<Error>   for DefaultLevels { fn from(_: Error)   -> Self { Self::Error } }
///
/// impl<T> formatter::GenericDefinition<DefaultLevels> for T
///     where T : formatter::Definition<level::Trace>,
///           T : formatter::Definition<level::Debug>,
///           T : formatter::Definition<level::Info>,
///           T : formatter::Definition<level::Warning>,
///           T : formatter::Definition<level::Error> {
///     fn generic_format(entry:&Entry<DefaultLevels>) -> Option<Self::Output> {
///         match entry.level {
///             DefaultLevels::Trace   => formatter::format::<T, level::Trace>(&entry.gen_entry),
///             DefaultLevels::Debug   => formatter::format::<T, level::Debug>(&entry.gen_entry),
///             DefaultLevels::Info    => formatter::format::<T, level::Info>(&entry.gen_entry),
///             DefaultLevels::Warning => formatter::format::<T, level::Warning>(&entry.gen_entry),
///             DefaultLevels::Error   => formatter::format::<T, level::Error>(&entry.gen_entry)
///         }
///     }
/// }
/// ```
#[macro_export]
macro_rules! define_levels_group {
    ($group_name:ident { $($name:ident),* $(,)?} ) => {
        /// Possible verbosity levels enum.
        #[allow(missing_docs)]
        #[derive(Clone,Copy,Debug,PartialEq,Eq,Hash)]
        pub enum $group_name {
            $($name),*
        }

        $(
            impl From<$name> for $group_name {
                fn from(_:$name) -> Self {
                    Self::$name
                }
            }
        )*

        impl<T> formatter::GenericDefinition<DefaultLevels> for T
        where $(T : formatter::Definition<level::$name>),* {
            fn generic_format(entry:&Entry<DefaultLevels>) -> Option<Self::Output> {
                match entry.level {
                    $(
                        DefaultLevels::$name =>
                            formatter::format::<T,level::$name> (&entry.gen_entry)
                    ),*
                }
            }
        }
    };
}



// =======================
// === Built-in Levels ===
// =======================

define_levels!(Trace, Debug, Info, Warning, Error);
define_levels_group!(DefaultLevels { Trace, Debug, Info, Warning, Error });



// =====================
// === DefaultFilter ===
// =====================

/// Default compile-time logger filtering. Keeps all logs.
pub type DefaultFilter = filter_from::Trace;
