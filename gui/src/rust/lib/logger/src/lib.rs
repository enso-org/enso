//! This crate contains implementation of logging interface.

#![feature(cell_update)]

#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

pub mod disabled;
pub mod enabled;

use enso_prelude::*;



// ==============
// === Message ===
// ==============

/// Message that can be logged.
pub trait Message {
    /// Turns message into `&str` and passes it to input function.
    fn with<T,F:FnOnce(&str)->T>(&self, f:F) -> T;
}

impl Message for &str {
    fn with<T,F:FnOnce(&str)->T>(&self, f:F) -> T {
        f(self)
    }
}

impl<G:Fn()->S, S:AsRef<str>> Message for G {
    fn with<T,F:FnOnce(&str)->T>(&self, f:F) -> T {
        f(self().as_ref())
    }
}



// =================
// === AnyLogger ===
// =================

/// Interface common to all loggers.
pub trait AnyLogger {
    /// Owned type of the logger.
    type Owned;

    /// Creates a new logger. Path should be a unique identifier for this logger.
    fn new(path:impl Into<ImString>) -> Self::Owned;

    /// Path that is used as an unique identifier of this logger.
    fn path(&self) -> &str;

    /// Creates a new logger with this logger as a parent.
    fn sub(logger:impl AnyLogger, path:impl Into<ImString>) -> Self::Owned {
        let path       = path.into();
        let super_path = logger.path();
        if super_path.is_empty() { Self::new(path) }
        else                     { Self::new(iformat!("{super_path}.{path}")) }
    }

    /// Creates a logger from AnyLogger.
    fn from_logger(logger:impl AnyLogger) -> Self::Owned {
        Self::new(logger.path())
    }

    /// Evaluates function `f` and visually groups all logs will occur during its execution.
    fn group<T,F:FnOnce() -> T>(&self, msg:impl Message, f:F) -> T {
        self.group_begin(msg);
        let out = f();
        self.group_end();
        out
    }

    /// Log with stacktrace and info level verbosity.
    fn trace(&self, _msg:impl Message) {}

    /// Log with debug level verbosity
    fn debug(&self, _msg:impl Message) {}

    /// Log with info level verbosity.
    fn info(&self, _msg:impl Message) {}

    /// Log with warning level verbosity.
    fn warning(&self, _msg:impl Message) {}

    /// Log with error level verbosity.
    fn error(&self, _msg:impl Message) {}

    /// Visually groups all logs between group_begin and group_end.
    fn group_begin(&self, _msg:impl Message) {}

    /// Visually groups all logs between group_begin and group_end.
    fn group_end(&self) {}
}

impl<T:AnyLogger> AnyLogger for &T {
    type Owned = T::Owned;
    fn path        (&self) -> &str { T::path(self) }
    fn new         (path:impl Into<ImString>) -> Self::Owned { T::new(path) }
    fn trace       (&self, msg:impl Message) { T::trace       (self,msg) }
    fn debug       (&self, msg:impl Message) { T::debug       (self,msg) }
    fn info        (&self, msg:impl Message) { T::info        (self,msg) }
    fn warning     (&self, msg:impl Message) { T::warning     (self,msg) }
    fn error       (&self, msg:impl Message) { T::error       (self,msg) }
    fn group_begin (&self, msg:impl Message) { T::group_begin (self,msg) }
    fn group_end   (&self)                   { T::group_end   (self)     }
}



// ==============
// === Macros ===
// ==============

/// Shortcut for `|| format!(..)`.
#[macro_export]
macro_rules! fmt {
    ($($arg:tt)*) => (||(format!($($arg)*)))
}

/// Evaluates expression and visually groups all logs will occur during its execution.
#[macro_export]
macro_rules! group {
    ($logger:expr, $message:tt, {$($body:tt)*}) => {{
        let __logger = $logger.clone();
        __logger.group_begin(|| iformat!{$message});
        let out = {$($body)*};
        __logger.group_end();
        out
    }};
}

/// Logs a message on on given level.
#[macro_export]
macro_rules! log_template {
    ($method:ident $logger:expr, $message:tt $($rest:tt)*) => {
        $crate::log_template_impl! {$method $logger, iformat!($message) $($rest)*}
    };
}

/// Logs a message on on given level.
#[macro_export]
macro_rules! log_template_impl {
    ($method:ident $logger:expr, $expr:expr) => {{
        $logger.$method(|| $expr);
    }};
    ($method:ident $logger:expr, $expr:expr, $body:tt) => {{
        let __logger = $logger.clone();
        __logger.group_begin(|| $expr);
        let out = $body;
        __logger.group_end();
        out
    }};
}

/// Logs an internal error with descriptive message.
#[macro_export]
macro_rules! with_internal_bug_message { ($f:ident $($args:tt)*) => { $crate::$f! {
"This is a bug. Please report it and and provide us with as much information as \
possible at https://github.com/luna/enso/issues. Thank you!"
$($args)*
}};}

/// Logs an internal error.
#[macro_export]
macro_rules! log_internal_bug_template {
    ($($toks:tt)*) => {
        $crate::with_internal_bug_message! { log_internal_bug_template_impl $($toks)* }
    };
}

/// Logs an internal error.
#[macro_export]
macro_rules! log_internal_bug_template_impl {
    ($note:tt $method:ident $logger:expr, $message:tt $($rest:tt)*) => {
        $crate::log_template_impl! {$method $logger,
            format!("Internal Error. {}\n\n{}",iformat!($message),$note) $($rest)*
        }
    };
}

/// Log with stacktrace and level:info.
#[macro_export]
macro_rules! trace {
    ($($toks:tt)*) => {
        $crate::log_template! {trace $($toks)*}
    };
}

/// Log with level:debug
#[macro_export]
macro_rules! debug {
    ($($toks:tt)*) => {
        $crate::log_template! {debug $($toks)*}
    };
}

/// Log with level:info.
#[macro_export]
macro_rules! info {
    ($($toks:tt)*) => {
        $crate::log_template! {info $($toks)*}
    };
}

/// Log with level:warning.
#[macro_export]
macro_rules! warning {
    ($($toks:tt)*) => {
        $crate::log_template! {warning $($toks)*}
    };
}

/// Log with level:error.
#[macro_export]
macro_rules! error {
    ($($toks:tt)*) => {
        $crate::log_template! {error $($toks)*}
    };
}

/// Logs an internal warning.
#[macro_export]
macro_rules! internal_warning {
    ($($toks:tt)*) => {
        $crate::log_internal_bug_template! {warning $($toks)*}
    };
}
