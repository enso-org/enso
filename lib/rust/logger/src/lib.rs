//! Extensible logger implementation.
//!
//! ### Main Features
//!
//! This crate is designed to provide extensible and easy-to-use abstractions for logging
//! arbitrary string messages to (primarily) the DevTools console. The code is designed with the
//! following features in mind:
//!
//! - Customizable hierarchical log levels.
//! - Compile-time level filtering.
//! - Support for JS console features, like message grouping.
//! - Customizable logs processing pipeline.
//!
//! ### How to Use
//!
//! The usage is similar to the [`log`](https://docs.rs/log) logging facade, though there are some
//! important differences.
//!
//! #### Logging Macros
//!
//! First of all, logging macros (like [`info!`]) accept [`AnyLogger`] type as the first argument:
//!
//! ```text
//! # use enso_logger::DefaultInfoLogger as Logger;
//! # use enso_logger::{AnyLogger, info};
//! # use enso_prelude::iformat;
//! let logger = Logger::new("my_logger");
//! info!(logger, "Here comes some log message");
//! ```
//!
//! Each `Logger` has a so-called `path`, a unique string identifier of the logger, that is
//! attached to each log message. Loggers can create hierarchies with the help of
//! [`AnyLogger::new_sub`] constructor. This allows to semantically distinguish log messages from
//! different modules of the program.
//!
//! The second argument of the logging macros might be any type implementing [`Message`] trait:
//!
//! ```text
//! # use enso_logger::DefaultInfoLogger as Logger;
//! # use enso_logger::{AnyLogger, info};
//! # use enso_prelude::iformat;
//! # let logger = Logger::new("my_logger");
//! info!(logger, "String literal");
//! let string_variable = String::from("Owned string");
//! info!(logger, string_variable);
//! info!(logger, || "A closure returning string");
//! let some_var = 3.14;
//! info!(logger, "String interpolation using iformat! macro also works: {some_var}");
//! // will output "String interpolation using iformat! macro also works: 3.14"
//! ```
//!
//! #### Message Grouping
//!
//! Also, a third argument may be provided. It groups log messages in the DevTools console under a
//! group with the provided name. For example:
//!
//! ```text
//! # use enso_logger::DefaultInfoLogger as Logger;
//! # use enso_logger::{AnyLogger, info};
//! # use enso_prelude::iformat;
//! # let logger = Logger::new("my_logger");
//! # fn some_computation() {}
//! info!(logger, "group name", || {
//!     some_computation();
//!     info!(logger, "this message will go into group");
//! });
//! ```
//! You can also use macro-keywords `collapsed`
//! or `expanded` just before `||` to print the group collapsed or expanded by default,
//! respectively. If not provided, the [`warning`] and [`error`] group macros are collapsed by
//! default, while all other group macros are expanded by default.
//! #### Compile-time Log Level Filtering
//!
//! The crate provides a set of predefined loggers that can be used together with reexport
//! mechanism to easily control log level filtering with zero run-time overhead.
//!
//! ```text
//! mod prelude {
//!     // Reexport needed loggers in the crate prelude.
//!     pub use enso_logger::DefaultInfoLogger as Logger;
//!     pub use enso_logger::DefaultTraceLogger as DebugLogger;
//! }
//!
//! // Use it in the code.
//! // Changing this line to `use prelude::DebugLogger as Logger;`
//! // will enable debug logs for this module.
//! use prelude::Logger;
//! # use enso_logger::{AnyLogger, debug, warning};
//! # use enso_prelude::iformat;
//!
//! let logger = Logger::new("MyModule");
//! warning!(logger, "This will be printed to the logs");
//! debug!(logger, "This would not");
//! ```
//!
//! ### Extensibility
//!
//! You can define your own log levels (see [`define_levels!`], [`define_levels_group!`] and
//! [`define_compile_time_filtering_rules!`] macros), formatters (see [`processor::formatter`]), and
//! log processing pipelines (see [`processor`]).

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![allow(incomplete_features)] // To be removed, see: https://github.com/enso-org/ide/issues/1559
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![feature(specialization)]

pub mod entry;
pub mod macros;
pub mod processor;

pub use enso_prelude as prelude;
pub use entry::message::Message;

use prelude::*;

use crate::entry::DefaultFilter;
use crate::entry::DefaultLevels;
use crate::entry::Entry;
use crate::processor::DefaultProcessor;
use crate::processor::Processor;

use enso_shapely::CloneRef;

pub use ifmt::iformat;


// ==============
// === Logger ===
// ==============

/// The main logger implementation. It is parametrized by three main types:
/// - Filter, which is used for compile-time message filtering (zero runtime overhead).
/// - Processor, which defines a pipeline of what happens to the logged messages. Read the docs of
///   `Processor` to learn more.
/// - Levels, which is a structure defining all possible verbosity levels this logger should handle.
///   See the `level.rs` module to learn how to define custom verbosity levels.
///
/// In order to learn how to use the logger, please refer to the docs in `macros.rs`, where a lot
/// of logging utility macros are defined.
#[derive(CloneRef, Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct Logger<Filter = DefaultFilter, Processor = DefaultProcessor, Levels = DefaultLevels> {
    path:      ImString,
    #[derivative(Debug = "ignore")]
    filter:    PhantomData<Filter>,
    #[derivative(Debug = "ignore")]
    levels:    PhantomData<Levels>,
    #[derivative(Debug = "ignore")]
    processor: Rc<RefCell<Processor>>,
}

impl<Filter, Processor, Level> Logger<Filter, Processor, Level>
where Processor: Default
{
    /// Constructor from another logger keeping the same path.
    pub fn new_from(logger: impl AnyLogger) -> Self {
        Self::new(logger.path())
    }
}

impl<Filter, Processor, Level> AnyLogger for Logger<Filter, Processor, Level>
where Processor: Default
{
    type Owned = Self;

    fn new(path: impl Into<ImString>) -> Self {
        let path = path.into();
        let filter = default();
        let levels = default();
        let processor = default();
        Self { path, filter, levels, processor }
    }

    fn path(&self) -> &str {
        &self.path
    }
}



// =================
// === AnyLogger ===
// =================

/// A common interface for all loggers. Exposing all information needed to create a particular
/// sub-logger from a given parent logger of any type.
pub trait AnyLogger {
    /// The owned type of this logger. As this trait is implemented for logger references, this
    /// dependent type just removes the references in this case.
    type Owned;

    /// Constructor.
    fn new(path: impl Into<ImString>) -> Self::Owned;

    /// The path that is used as a unique identifier of this logger.
    fn path(&self) -> &str;

    /// Create a new logger with this logger as a parent. It can be useful when we need to create
    /// a sub-logger for a generic type parameter.
    fn sub<T>(&self, id: impl AsRef<str>) -> T
    where
        T: AnyLogger<Owned = T>,
        Self: Sized, {
        <T as AnyLogger>::new_sub(self, id)
    }

    /// Create a new logger with `logger` as a parent. It can be useful when we need to create
    /// a sub-logger for a generic type parameter.
    fn new_sub(logger: impl AnyLogger, id: impl AsRef<str>) -> Self::Owned
    where Self::Owned: AnyLogger<Owned = Self::Owned> {
        Self::Owned::new(iformat!("{logger.path()}.{id.as_ref()}"))
    }
}

impl<T: AnyLogger> AnyLogger for &T {
    type Owned = T::Owned;
    fn new(path: impl Into<ImString>) -> Self::Owned {
        T::new(path)
    }
    fn path(&self) -> &str {
        T::path(self)
    }
}



// ======================
// === Logger Aliases ===
// ======================

macro_rules! define_logger_aliases {
    ($($tp:ident $name:ident $default_name:ident;)*) => {$(
        #[doc = "A logger which compile-time filters out all messages with log levels smaller than "]
        #[doc = stringify!($tp)]
        #[doc = "."]
        pub type $name <S=DefaultProcessor,L=DefaultLevels> = Logger<entry::filter_from::$tp,S,L>;

        #[doc = "The same as "]
        #[doc = stringify!($name)]
        #[doc = ", but with all type arguments applied, for convenient usage."]
        pub type $default_name = $name;
    )*};
}

define_logger_aliases! {
    Trace   TraceLogger   DefaultTraceLogger;
    Debug   DebugLogger   DefaultDebugLogger;
    Info    InfoLogger    DefaultInfoLogger;
    Warning WarningLogger DefaultWarningLogger;
    Error   ErrorLogger   DefaultErrorLogger;
}



// =================
// === LoggerOps ===
// =================

/// Primitive operations on a logger. The type parameter allows for compile-time log level filtering
/// of the messages.
#[allow(missing_docs)]
pub trait LoggerOps<Level = DefaultLevels> {
    fn log(&self, level: Level, msg: impl Message);
    fn group_begin(&self, level: Level, collapsed: bool, msg: impl Message);
    fn group_end(&self, level: Level);
}


// === Impl for References ===

impl<T: LoggerOps<Level>, Level> LoggerOps<Level> for &T {
    fn log(&self, level: Level, msg: impl Message) {
        LoggerOps::log(*self, level, msg)
    }

    fn group_begin(&self, level: Level, collapsed: bool, msg: impl Message) {
        LoggerOps::group_begin(*self, level, collapsed, msg)
    }

    fn group_end(&self, level: Level) {
        LoggerOps::group_end(*self, level)
    }
}


// === Generic Redirection ===

impl<S, Filter, Level, L> LoggerOps<L> for Logger<Filter, S, Level>
where
    S: Processor<Entry<Level>>,
    Level: From<L>,
{
    default fn log(&self, level: L, msg: impl Message) {
        self.processor.borrow_mut().submit(Entry::message(level, self.path.clone(), msg));
    }

    default fn group_begin(&self, level: L, collapsed: bool, msg: impl Message) {
        self.processor.borrow_mut().submit(Entry::group_begin(
            level,
            self.path.clone(),
            msg,
            collapsed,
        ));
    }

    default fn group_end(&self, level: L) {
        self.processor.borrow_mut().submit(Entry::group_end(level, self.path.clone()));
    }
}


// === Compile-time Filtering ===

/// Defines specialized version of compile time filtering rules for the given filtering levels.
/// It defines specialized implementations for the default implementation above. See the usage
/// below to learn more.
#[macro_export]
macro_rules! define_compile_time_filtering_rules {
    ($(for level::from::$filter:ident remove $($level:ident),*;)*) => {$($(
        impl<S,Level> LoggerOps<entry::level::$level>
        for Logger<entry::level::filter_from::$filter,S,Level>
        where S:Processor<Entry<Level>>, Level:From<entry::level::$level> {
            fn log         (&self, _lvl:entry::level::$level, _msg:impl Message) {}
            fn group_begin (&self, _lvl:entry::level::$level, _collapsed:bool, _msg:impl Message) {}
            fn group_end   (&self, _lvl:entry::level::$level) {}
        }
    )*)*};
}


// === Compile-time filtering of built-in levels ===

define_compile_time_filtering_rules! {
    for level::from::Debug   remove Trace;
    for level::from::Info    remove Trace,Debug;
    for level::from::Warning remove Trace,Debug,Info;
    for level::from::Error   remove Trace,Debug,Info,Warning;
}
