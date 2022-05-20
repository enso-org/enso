//! Implementation of application argument reader capable of reading arguments passed by a WASM
//! variable.

use crate::prelude::*;



// =================
// === ArgReader ===
// =================

/// Marker trait used to disambiguate overlapping impls of [`ArgReader`].
#[marker]
pub trait ArgMarker {}

/// Trait used to convert provided string arguments to the desired type.
#[allow(missing_docs)]
pub trait ArgReader: Sized {
    fn read_arg(str: String) -> Option<Self>;
}


// === Default ===

/// Helper trait used to disambiguate overlapping impls of [`ArgReader`].
#[allow(missing_docs)]
pub trait ArgReaderFromString: Sized {
    fn read_arg_from_string(str: String) -> Option<Self>;
}

impl<T> ArgReaderFromString for T
where String: TryInto<T>
{
    fn read_arg_from_string(str: String) -> Option<Self> {
        str.try_into().ok()
    }
}

impl<T> ArgReaderFromString for T {
    default fn read_arg_from_string(_: String) -> Option<Self> {
        unreachable!()
    }
}

impl<T> ArgMarker for T where T: TryFrom<String> {}
impl<T> ArgReader for T
where T: ArgMarker
{
    default fn read_arg(str: String) -> Option<Self> {
        ArgReaderFromString::read_arg_from_string(str)
    }
}


// === Specializations ===

impl ArgMarker for bool {}
impl ArgReader for bool {
    fn read_arg(str: String) -> Option<Self> {
        match &str[..] {
            "true" => Some(true),
            "false" => Some(false),
            "ok" => Some(true),
            "fail" => Some(false),
            "enabled" => Some(true),
            "disabled" => Some(false),
            "yes" => Some(true),
            "no" => Some(false),
            _ => None,
        }
    }
}



// =================
// === read_args ===
// =================

/// Defines an application argument reader. As a result, a new lazy-initialized static variable
/// `ARGS` will be created, and it will read the arguments on its first access (in case you want to
/// force argument read, use the `init` function).
///
/// For example, given the following definition:
/// ```ignore
/// read_args! {
///     js::global.config {
///         entry      : String,
///         project    : String,
///         dark_theme : bool,
///     }
/// }
/// ```
///
/// The following structs will be generated (some functions omitted for clarity):
///
/// ```ignore
/// #[derive(Clone,Debug,Default)]
/// pub struct Args {
///     pub entry      : Option<String>,
///     pub project    : Option<String>,
///     pub dark_theme : Option<bool>,
/// }
///
/// lazy_static! {
///     pub static ref ARGS : Args = Args::new();
/// }
/// ```
///
/// The header `js::global.config` means that the JavaScript space will be queried for variable
/// `global.config`, which will be queried for every field of the generated structure. In case the
/// JavaScript variable will not contain the key, it will be left as None. For each available key,
/// the [`ArgReader`] trait will be used to read it back to Rust types. The [`ArgReader`] is a thin
/// wrapper over the [`Into`] trait with some additional conversions (e.g. for [`bool`]). In case
/// the conversion will fail, a warning will be raised.
#[macro_export]
macro_rules! read_args {
    ([$($($path:tt)*).*] { $($(#[$($attr:tt)+])* $field:ident : $field_type:ty),* $(,)? }) => {
        mod _READ_ARGS {
            use super::*;
            use $crate::prelude::*;
            use $crate::system::web::traits::*;

            /// Reflection mechanism containing string representation of option names.
            #[derive(Clone,Copy,Debug,Default)]
            pub struct ArgNames;
            impl ArgNames {
                $(
                    /// Name of the field.
                    pub fn $field(&self) -> &'static str {
                        stringify!{$field}
                    }
                )*
            }

            /// The structure containing application configs.
            #[derive(Clone,Debug,Default)]
            #[allow(missing_docs)]
            pub struct Args {
                $(
                    $(#[$($attr)*])*
                    pub $field : Option<$field_type>
                ),*
            }

            impl Args {
                /// Constructor.
                fn new() -> Self {
                    let logger = Logger::new(stringify!{Args});
                    let path   = vec![$($($path)*),*];
                    match ensogl::system::web::Reflect::get_nested_object
                    (&ensogl::system::web::window,&path).ok() {
                        None => {
                            let path = path.join(".");
                            error!(&logger,"The config path '{path}' is invalid.");
                            default()
                        }
                        Some(cfg) => {
                            let keys      = ensogl::system::web::Object::keys_vec(&cfg);
                            let mut keys  = keys.into_iter().collect::<HashSet<String>>();
                            $(
                                let name   = stringify!{$field};
                                let tp     = stringify!{$field_type};
                                let $field = ensogl::system::web::Reflect::
                                    get_nested_object_printed_as_string(&cfg,&[name]).ok();
                                let $field = $field.map
                                    ($crate::application::args::ArgReader::read_arg);
                                if $field == Some(None) {
                                    warning!(&logger,"Failed to convert the argument '{name}' \
                                                      value to the '{tp}' type.");
                                }
                                let $field = $field.flatten();
                                keys.remove(name);
                            )*
                            for key in keys {
                                warning!(&logger,"Unknown config option provided '{key}'.");
                            }
                            Self {$($field),*}
                        }
                    }
                }

                /// This is a dummy function which initializes the arg reading process. This
                /// function does nothing, however, in order to call it, the user would need to
                /// access a field in the lazy static variable `ARGS`, which would trigger argument
                /// parsing process.
                pub fn init(&self) {}

                /// Reflection mechanism to get string representation of argument names.
                pub fn names(&self) -> ArgNames { ArgNames }
            }

            lazy_static! {
                /// Application arguments initialized in a lazy way (on first read).
                pub static ref ARGS : Args = Args::new();
            }
        }
        pub use _READ_ARGS::*;
    };
}
