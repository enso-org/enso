//! Implementation of application argument reader capable of reading arguments passed by a WASM
//! variable.

use crate::prelude::*;



// =============================
// === Ident Case Conversion ===
// =============================

/// Convert snake to camel case name.
pub fn snake_case_to_camel_case(name: &str) -> String {
    let mut output = String::new();
    let mut capitalize_next = false;
    for c in name.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            output.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            output.push(c);
        }
    }
    output
}



// ====================
// === OptArgReader ===
// ====================

auto trait NotOption {}
impl<T> !NotOption for Option<T> {}

/// Argument parser that checks if the desired parameter is of an [`Option`] type. If it is not,
/// the call is redirected to [`ArgReader`].
#[allow(missing_docs)]
pub trait OptArgReader: Sized {
    fn read_arg(input: &Option<String>) -> Result<Self, anyhow::Error>;
}

impl<T: ArgReader> OptArgReader for Option<T> {
    fn read_arg(input: &Option<String>) -> Result<Self, anyhow::Error> {
        match input.as_ref() {
            None => Ok(None),
            Some(str) => ArgReader::read_arg(str.as_ref()).map(Some),
        }
    }
}

impl<T: ArgReader + NotOption> OptArgReader for T {
    fn read_arg(input: &Option<String>) -> Result<Self, anyhow::Error> {
        match input.as_ref() {
            None => Err(anyhow!("Missing required argument.")),
            Some(str) => ArgReader::read_arg(str.as_ref()),
        }
    }
}



// =================
// === ArgReader ===
// =================

/// Marker trait used to disambiguate overlapping impls of [`ArgReader`].
#[marker]
pub trait ArgMarker {}

/// Trait used to convert provided string arguments to the desired type.
#[allow(missing_docs)]
pub trait ArgReader: Sized {
    fn read_arg(str: &str) -> Result<Self, anyhow::Error>;
}

// === Default ===

/// Helper trait used to disambiguate overlapping impls of [`ArgReader`].
#[allow(missing_docs)]
pub trait ArgReaderFromString: Sized {
    fn read_arg_from_string(str: &str) -> Result<Self, anyhow::Error>;
}

impl<T> ArgReaderFromString for T
where for<'t> &'t str: TryInto<T>
{
    fn read_arg_from_string(input: &str) -> Result<Self, anyhow::Error> {
        input.try_into().map_err(|_| anyhow!("Cannot convert '{input}' to argument value."))
    }
}

impl<T> ArgReaderFromString for T {
    default fn read_arg_from_string(input: &str) -> Result<Self, anyhow::Error> {
        Err(anyhow!("Cannot convert '{input}' to argument value."))
    }
}

impl<T> ArgMarker for T where T: for<'t> TryFrom<&'t str> {}
impl<T> ArgReader for T
where T: ArgMarker
{
    default fn read_arg(str: &str) -> Result<Self, anyhow::Error> {
        ArgReaderFromString::read_arg_from_string(str)
    }
}


// === Specializations ===

impl ArgMarker for bool {}
impl ArgReader for bool {
    fn read_arg(input: &str) -> Result<Self, anyhow::Error> {
        let true_values = ["true", "enabled", "yes", "1"];
        let false_values = ["false", "disabled", "no", "0"];
        if true_values.contains(&input) {
            Ok(true)
        } else if false_values.contains(&input) {
            Ok(false)
        } else {
            let allowed_values = true_values.iter().chain(false_values.iter()).join(", ");
            Err(anyhow!("Cannot parse '{input}' as bool. Allowed values: {allowed_values}."))
        }
    }
}

impl ArgMarker for f32 {}
impl ArgReader for f32 {
    fn read_arg(input: &str) -> Result<Self, anyhow::Error> {
        input.parse().map_err(|_| anyhow!("Cannot parse '{input}' as f32."))
    }
}

impl ArgMarker for semver::Version {}
impl ArgReader for semver::Version {
    fn read_arg(input: &str) -> Result<Self, anyhow::Error> {
        semver::Version::parse(input).map_err(|_| anyhow!("Cannot parse '{input}' as semver."))
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
/// ```text
/// read_args! {
///     entry: String,
///     project: String,
///     dark_theme: bool,
/// }
/// ```
///
/// The following structs will be generated (some functions omitted for clarity):
///
/// ```text
/// #[derive(Clone, Debug, Default)]
/// pub struct Args {
///     pub entry: Option<String>,
///     pub project: Option<String>,
///     pub dark_theme: Option<bool>,
/// }
///
/// lazy_static! {
///     pub static ref ARGS: Args = Args::new();
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
    ($($(#[$($attr:tt)+])* $field:ident : $field_type:ty),* $(,)?) => {
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
                    pub fn $field(&self) -> String {
                        $crate::application::args::snake_case_to_camel_case(stringify!{$field})
                    }
                )*
            }

            /// The structure containing application configs.
            #[derive(Clone,Debug,Default)]
            #[allow(missing_docs)]
            pub struct Args {
                $(
                    $(#[$($attr)*])*
                    pub $field : $field_type
                ),*
            }

            impl Args {
                /// Constructor.
                fn new() -> Self {
                    let js_app = ensogl::system::js::app::app();
                    match js_app {
                        Err(_) => {
                            error!("Cannot get the JS application. Using default arguments.");
                            default()
                        }
                        Ok(js_app) => {
                            let mut params = js_app.config().params().to_hash_map();
                            $(
                                let js_name = $crate::application::args::snake_case_to_camel_case
                                    (stringify!{$field});
                                let $field = if let Some(param) = params.remove(&js_name) {
                                    let str_value = param.value();
                                    let value = $crate::application::args::OptArgReader::read_arg(&str_value);
                                    match value {
                                        Err(err) => {
                                            let tp = stringify!{$field_type};
                                            error!("Config error. Invalid value '{str_value:?}' for parameter \
                                                '{js_name}' of type '${tp}'. {err}");
                                            default()
                                        }
                                        Ok(value) => value,
                                    }
                                } else {
                                    warn!("Config error. Rust config parameter '{js_name}' not found in \
                                        JavaScript.");
                                    default()
                                };
                            )*
                            for js_name in params.keys() {
                                warn!("Config error. JavaScript config parameter '{js_name}' not found in \
                                    Rust.");
                            }
                            Self {$($field),*}
                        }
                    }
                }

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
