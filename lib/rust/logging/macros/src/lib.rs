//! Proc macros supporting the implementation of the `enso_logging` library.

// === Features ===
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(proc_macro_span)]
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use inflector::Inflector;
use quote::quote;



/// The log levels defined in the Console Web API.
/// See: https://console.spec.whatwg.org/#loglevel-severity
const WEB_LOG_LEVELS: &[&str] = &["error", "warn", "info", "debug"];



// ==================================
// === Compile-time configuration ===
// ==================================

const LEVEL_CONFIGURATION_ENV_VARS: LevelConfiguration<&str> = LevelConfiguration {
    max_enabled:     "ENSO_MAX_LOG_LEVEL",
    max_uncollapsed: "ENSO_MAX_UNCOLLAPSED_LOG_LEVEL",
};



// =====================
// === Helper macros ===
// =====================

macro_rules! map_fns {
    ($Ty:tt, [$($covariant:tt),*]) => { map_fns!($Ty, [$($covariant),*], []); };
    ($Ty:ident, [$($covariant:ident),*], [$($invariant:ident),*]) => {
        impl<T> $Ty<T> {
            #[allow(unused)]
            fn map<F, U>(self, f: F) -> $Ty<U> where F: Fn(T) -> U {
                let Self {
                    $($covariant,)*
                    $($invariant,)*
                } = self;
                $Ty {
                    $($covariant: f($covariant),)*
                    $($invariant,)*
                }
            }

            #[allow(unused)]
            fn for_each<F>(self, mut f: F) where F: FnMut(T) {
                $(f(self.$covariant);)*
            }

            #[allow(unused)]
            fn as_ref(&self) -> $Ty<&T> {
                let Self {
                    $($covariant,)*
                    $($invariant,)*
                } = self;
                $Ty {
                    $($covariant,)*
                    $($invariant: $invariant.clone(),)*
                }
            }
        }
    };
}



// =================
// === Interface ===
// =================

/// Implement a logging API for the spcecified log levels.
#[proc_macro]
pub fn define_log_levels(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use syn::parse::Parser;
    let parser = syn::punctuated::Punctuated::<syn::Ident, syn::Token![,]>::parse_terminated;
    let args = parser.parse(ts).unwrap();
    let names: Vec<_> = args.into_iter().map(|ident| ident.to_string().to_snake_case()).collect();
    let position_in_args = |name| {
        names.iter().position(|arg| &name == arg).unwrap_or_else(|| {
            let error = "Environment variable value must correspond to a defined log level";
            panic!("{error}. Found: {name:?}, expected one of: {names:?}.")
        })
    };
    let level_configuration = LEVEL_CONFIGURATION_ENV_VARS
        .map(|var| std::env::var(var).ok().map(position_in_args).unwrap_or_default());
    logging_api(names, level_configuration)
}

struct LevelConfiguration<T> {
    max_enabled:     T,
    max_uncollapsed: T,
}
map_fns!(LevelConfiguration, [max_enabled, max_uncollapsed]);



// =====================
// === Top-level API ===
// =====================

fn logging_api(
    level_names: impl IntoIterator<Item = String>,
    config: LevelConfiguration<usize>,
) -> proc_macro::TokenStream {
    let global_logger_ident = ident("GlobalLogger");
    let global_logger_path = ident_to_path(global_logger_ident.clone());
    let levels = levels(level_names, &config, global_logger_path);
    let api: Api = [
        span_trait(),
        logger_trait(&levels),
        span_api(&levels),
        event_api(&levels),
        global_logger(global_logger_ident, &levels),
    ]
    .into_iter()
    .collect();
    api.into_library().into()
}


// === Information used to construct level-specific interfaces ===

struct Level {
    // Intrinsic properties of a level:
    name:                  String,
    enabled:               bool,
    uncollapsed:           bool,
    // Identifiers for API cross-references:
    trait_methods:         LoggerMethods<syn::Ident>,
    global_logger_methods: LoggerMethods<syn::Path>,
}

impl Level {
    fn new(name: String, enabled: bool, uncollapsed: bool, global_logger_path: &syn::Path) -> Self {
        let trait_methods = trait_methods(&name);
        let global_logger_methods =
            trait_methods.clone().map(|x| qualified(global_logger_path.clone(), x));
        Level { global_logger_methods, trait_methods, enabled, uncollapsed, name }
    }
}

fn levels(
    names: impl IntoIterator<Item = String>,
    config: &LevelConfiguration<usize>,
    global_logger_path: syn::Path,
) -> Vec<Level> {
    let enabled = |i| config.max_enabled >= i;
    let uncollapsed = |i| config.max_uncollapsed >= i;
    let level = |(i, name)| Level::new(name, enabled(i), uncollapsed(i), &global_logger_path);
    names.into_iter().enumerate().map(level).collect()
}



// ==============================
// === Representation of APIs ===
// ==============================

#[derive(Default)]
struct Api {
    implementation: proc_macro2::TokenStream,
    exports:        Vec<Export>,
}

impl Api {
    fn extend(&mut self, mut other: Self) {
        self.implementation.extend(other.implementation);
        self.exports.append(&mut other.exports);
    }
    fn into_library(self) -> proc_macro2::TokenStream {
        let reexport = |name: &syn::Ident| {
            quote! { pub use crate::internal::#name; }
        };
        let prelude: proc_macro2::TokenStream =
            self.exports.iter().filter_map(|e| e.prelude.then(|| reexport(&e.ident))).collect();
        let exports: proc_macro2::TokenStream =
            self.exports.iter().map(|e| reexport(&e.ident)).collect();
        let implementation = self.implementation;
        quote! {
            /// Exports, intended to be used by glob-import.
            pub mod prelude {
                #prelude
            }
            #exports
            /// Low-level interface, used by macro implementations.
            pub mod internal {
                #implementation
            }
        }
    }
}

impl FromIterator<Api> for Api {
    fn from_iter<T: IntoIterator<Item = Api>>(iter: T) -> Self {
        let mut collected: Api = Default::default();
        iter.into_iter().for_each(|api| collected.extend(api));
        collected
    }
}

impl From<proc_macro2::TokenStream> for Api {
    fn from(implementation: proc_macro2::TokenStream) -> Self {
        Self { implementation, ..Default::default() }
    }
}

struct Export {
    ident:   syn::Ident,
    prelude: bool,
}

impl Export {
    fn prelude(ident: syn::Ident) -> Self {
        Self { ident, prelude: true }
    }
}



// =====================
// === LogSpan trait ===
// =====================

fn span_trait() -> Api {
    let trait_name = ident("LogSpan");
    let implementation = quote! {
        /// Identifies a location in the source that may be traced in the logs.
        pub trait #trait_name {
            /// Log entry into the span, run the given closure, and log exit.
            #[inline(always)]
            fn in_scope<F, T>(self, f: F) -> T where Self: Sized, F: FnOnce() -> T {
                let _scope = self.entered();
                f()
            }
            /// Log entry into the span, and log exit when the returned value is dropped.
            #[inline(always)]
            fn entered(self) -> Entered<Self> where Self: Sized {
                Entered::new(self)
            }
            #[allow(missing_docs)]
            fn _enter(&self);
            #[allow(missing_docs)]
            fn _exit(&self);
        }
        /// RAII guard that enters a span when created and exits when dropped.
        pub struct Entered<S: #trait_name>(S);
        impl<S: #trait_name> Entered<S> {
            #[allow(missing_docs)]
            pub fn new(span: S) -> Self {
                span._enter();
                Self(span)
            }
        }
        impl<S: #trait_name> Drop for Entered<S> {
            fn drop(&mut self) {
                self.0._exit();
            }
        }
    };
    let exports = vec![Export::prelude(trait_name)];
    Api { implementation, exports }
}



// ====================
// === Logger trait ===
// ====================

fn logger_trait(levels: impl IntoIterator<Item = &Level>) -> Api {
    let mut methods = proc_macro2::TokenStream::new();
    for level in levels {
        level.trait_methods.signatures().for_each(|x| methods.extend(x))
    }
    (quote! {
        /// A type that serves as a destination for logging.
        pub trait Logger {
            #methods
        }
    })
    .into()
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LoggerMethods<T = proc_macro2::TokenStream> {
    emit_fn:  T,
    enter_fn: T,
    exit_fn:  T,
}
map_fns!(LoggerMethods, [emit_fn, enter_fn, exit_fn]);

impl LoggerMethods<syn::Ident> {
    fn signatures(&self) -> LoggerMethods {
        let LoggerMethods { emit_fn, enter_fn, exit_fn } = self;
        LoggerMethods {
            emit_fn:  quote! { #[allow(missing_docs)] fn #emit_fn(span: &str); },
            enter_fn: quote! { #[allow(missing_docs)] fn #enter_fn(span: &str); },
            exit_fn:  quote! { #[allow(missing_docs)] fn #exit_fn(); },
        }
    }

    fn with_bodies(
        &self,
        bodies: &LoggerMethods<proc_macro2::TokenStream>,
    ) -> LoggerMethods<proc_macro2::TokenStream> {
        let LoggerMethods { emit_fn, enter_fn, exit_fn } = self;
        let LoggerMethods { emit_fn: emit_body, enter_fn: enter_body, exit_fn: exit_body } = bodies;
        LoggerMethods {
            emit_fn:  quote! { #[inline] fn #emit_fn(span: &str) { #emit_body } },
            enter_fn: quote! { #[inline] fn #enter_fn(span: &str) { #enter_body } },
            exit_fn:  quote! { #[inline] fn #exit_fn() { #exit_body } },
        }
    }
}

fn trait_methods(level: &str) -> LoggerMethods<syn::Ident> {
    (LoggerMethods {
        emit_fn:  format!("emit_{level}"),
        enter_fn: format!("enter_{level}"),
        exit_fn:  format!("exit_{level}"),
    })
    .as_ref()
    .map(ident)
}



// =================
// === Event API ===
// =================

fn event_api(levels: impl IntoIterator<Item = &Level>) -> Api {
    levels
        .into_iter()
        .map(|level| event_api_for_level(&level.name, &level.global_logger_methods, level.enabled))
        .collect()
}

fn event_api_for_level(level: &str, methods: &LoggerMethods<syn::Path>, enabled: bool) -> Api {
    let event_macro = ident(level);
    let emit_fn = &methods.emit_fn;
    let level_tag = level.to_screaming_snake_case();
    let body = if enabled {
        quote! {
            use $crate::internal::Logger;
            $crate::internal::#emit_fn(
                &format!(
                    "[{}] {}:{} {}",
                    #level_tag,
                    file!(),
                    line!(),
                    format_args!($($args)*)
                ));
        }
    } else {
        quote! {
            let _unused_at_this_log_level = format_args!($($args)*);
        }
    };
    let implementation = quote! {
        /// Emit a log message, if the log-level is enabled.
        #[macro_export]
        macro_rules! #event_macro {
            ($($args:tt)*) => {{ #body }};
        }
    };
    Api { implementation, ..Default::default() }
}



// ================
// === Span API ===
// ================

fn span_api(levels: impl IntoIterator<Item = &Level>) -> Api {
    levels
        .into_iter()
        .map(|level| span_api_for_level(&level.name, &level.global_logger_methods, level.enabled))
        .collect()
}

fn span_api_for_level(level: &str, methods: &LoggerMethods<syn::Path>, enabled: bool) -> Api {
    let object_name = ident(level.to_pascal_case());
    let macro_name = ident(format!("{level}_span"));
    let object_contents = enabled.then_some(quote! { pub String }).unwrap_or_default();
    let enter_fn = &methods.enter_fn;
    let exit_fn = &methods.exit_fn;
    let enter_impl = enabled
        .then_some(quote! {
            #enter_fn(&self.0);
        })
        .unwrap_or_default();
    let exit_impl = enabled
        .then_some(quote! {
            #exit_fn();
        })
        .unwrap_or_default();
    let level_tag = level.to_screaming_snake_case();
    let creation_body = if enabled {
        quote! {
            $crate::internal::#object_name(
                format!(
                    "[{}] {}:{} {}",
                    #level_tag,
                    file!(),
                    line!(),
                    format_args!($($args)*)
                )
            )
        }
    } else {
        quote! {
            let _unused_at_this_log_level = format_args!($($args)*);
            $crate::internal::#object_name()
        }
    };
    let implementation = quote! {
        /// Refers to a region in the source code that may have associated logging.
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct #object_name(#object_contents);
        impl LogSpan for #object_name {
            #[inline(always)]
            fn _enter(&self) {
                #enter_impl
            }
            #[inline(always)]
            fn _exit(&self) {
                #exit_impl
            }
        }
        /// Create an object that identifies a location in the source code for logging purposes.
        #[macro_export]
        macro_rules! #macro_name {
            ($($args:tt)*) => {{
                #creation_body
            }}
        }
    };
    implementation.into()
}



// =========================
// === Global Logger API ===
// =========================

fn global_logger(logger: syn::Ident, levels: impl IntoIterator<Item = &Level>) -> Api {
    let console_logger = ident("NativeConsole");
    let web_logger = ident("WebConsole");
    let mut web_logger_methods = proc_macro2::TokenStream::new();
    let mut console_logger_methods = proc_macro2::TokenStream::new();
    let mut web_level = WEB_LOG_LEVELS.iter().copied().fuse();
    let mut web = web_level.next().unwrap();
    for level in levels {
        level
            .trait_methods
            .with_bodies(&level.enabled.then(console_logger_impl).unwrap_or_default())
            .for_each(|x| console_logger_methods.extend(x));
        level
            .trait_methods
            .with_bodies(&level.enabled.then_some(web_logger_impl(web, level)).unwrap_or_default())
            .for_each(|x| web_logger_methods.extend(x));
        web = web_level.next().unwrap_or(web);
    }
    (quote! {
        #[cfg(target_arch = "wasm32")]
        /// The currently-enabled global logger.
        pub type #logger = web::#web_logger;
        #[cfg(not(target_arch = "wasm32"))]
        /// The currently-enabled global logger.
        pub type #logger = native::#console_logger;
        /// Logging support for wasm environments.
        #[cfg(target_arch = "wasm32")]
        pub mod web {
            use super::*;
            /// A [`Logger`] that emits messages to the Console Web API.
            pub struct #web_logger;
            impl Logger for #web_logger {
                #web_logger_methods
            }
        }
        /// Logging support for non-wasm environments.
        #[cfg(not(target_arch = "wasm32"))]
        pub mod native {
            use super::*;
            /// A [`Logger`] that emits messages to the native console.
            pub struct #console_logger;
            thread_local! {
                static CONSOLE_INDENT: core::cell::Cell<usize> = core::cell::Cell::new(0);
            }
            impl Logger for #console_logger {
                #console_logger_methods
            }
        }
    })
    .into()
}



// =============================
// === Native console logger ===
// =============================

fn console_logger_impl() -> LoggerMethods {
    LoggerMethods {
        emit_fn:  quote! {
            let indent = CONSOLE_INDENT.get();
            println!("{:indent$}{}", "", span, indent=indent*4);
        },
        enter_fn: quote! {
            let indent = CONSOLE_INDENT.get();
            println!("{:indent$}{}", "", span, indent=indent*4);
            CONSOLE_INDENT.set(indent + 1);
        },
        exit_fn:  quote! {
            let indent = CONSOLE_INDENT.get();
            CONSOLE_INDENT.set(indent - 1);
        },
    }
}



// ======================
// === Web API logger ===
// ======================

fn web_logger_impl(web_level: &str, level: &Level) -> LoggerMethods {
    let event_fn = ident(format!("{web_level}_1"));
    let group_fn = ident(if level.uncollapsed { "group_1" } else { "group_collapsed_1" });
    LoggerMethods {
        emit_fn:  quote! {
            web_sys::console::#event_fn(&span.into());
        },
        enter_fn: quote! {
            web_sys::console::#group_fn(&span.into());
        },
        exit_fn:  quote! {
            web_sys::console::group_end();
        },
    }
}



// ============================
// === Syn-building helpers ===
// ============================

fn qualified(mut path: syn::Path, name: syn::Ident) -> syn::Path {
    path.segments.push(path_segment(name));
    path
}

fn path_segment(ident: syn::Ident) -> syn::PathSegment {
    syn::PathSegment { ident, arguments: Default::default() }
}

fn ident(name: impl AsRef<str>) -> syn::Ident {
    syn::Ident::new(name.as_ref(), proc_macro2::Span::call_site())
}

fn ident_to_path(segment: syn::Ident) -> syn::Path {
    syn::Path {
        leading_colon: Default::default(),
        segments:      std::iter::once(path_segment(segment)).collect(),
    }
}
