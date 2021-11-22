//! Extensible logger implementation.

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

    /// Path that is used as an unique identifier of this logger.
    fn path(&self) -> &str;

    /// Creates a new logger with this logger as a parent. It can be useful when we need to create
    /// a sub-logger for a generic type parameter.
    fn sub<T>(&self, id: impl AsRef<str>) -> T
    where
        T: AnyLogger<Owned = T>,
        Self: Sized, {
        <T as AnyLogger>::new_sub(self, id)
    }

    /// Creates a new logger with this logger as a parent. It can be useful when we need to create
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
        /// A logger which compile-time filters out all messages with log levels smaller than $tp.
        pub type $name <S=DefaultProcessor,L=DefaultLevels> = Logger<entry::filter_from::$tp,S,L>;

        /// The same as $name, but with all type arguments applied, for convenient usage.
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
