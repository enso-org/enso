// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
//! Interface to profile data.
//!
//! # Overview
//!
//! Usage of this API starts with applying [`str::parse`] to JSON profiling data, returning a
//! [`Measurement`] which is the root of the hierarchy of profiler outputs.
//!
//! Parsing is robust to changes in the definitions of metadata types; if deserialization of some
//! metadata entries fails, the resulting error type provides access to the result of deserializing
//! all the data that succeeded (see [`Error::RecoverableFormatError`]).
//!
//! # Usage example: storing and retrieving metadata
//!
//! ```
//! use enso_profiler as profiler;
//! use enso_profiler_data as profiler_data;
//! use profiler::profile;
//!
//! // Some metadata types.
//! #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
//! struct MyDataA(u32);
//!
//! #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
//! struct MyDataB(String);
//!
//! // An activity that produces metadata.
//! struct ActivityWithMetadata {
//!     meta_logger_a: profiler::MetadataLogger<MyDataA>,
//!     meta_logger_b: profiler::MetadataLogger<MyDataB>,
//!     // ...fields for doing stuff
//! }
//! impl ActivityWithMetadata {
//!     fn new() -> Self {
//!         let meta_logger_a = profiler::MetadataLogger::new("MyDataA");
//!         let meta_logger_b = profiler::MetadataLogger::new("MyDataB");
//!         Self { meta_logger_a, meta_logger_b /* ... */ }
//!     }
//!
//!     #[profile(Objective)]
//!     fn action_producing_metadata(&self) {
//!         self.meta_logger_a.log(MyDataA(23));
//!         self.meta_logger_b.log(MyDataB("5".into()));
//!     }
//! }
//!
//! // Run the activity that produces metadata, and profile it.
//! #[profile(Objective)]
//! fn demo() {
//!     let act = ActivityWithMetadata::new();
//!     act.action_producing_metadata();
//! }
//!
//! fn store_and_retrieve_metadata() {
//!     demo();
//!
//!     // To deserialize, we define a metadata type as an enum.
//!     //
//!     // Each variant has a name and type that match the string-argument and type-parameter of a
//!     // call to `MetadataLogger::new`.
//!     #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
//!     enum MyMetadata {
//!         MyDataA(MyDataA),
//!         MyDataB(MyDataB),
//!         // In this case we've handled everything.
//!         // If we intended to handle some metadata and silently ignore anything else, we could
//!         // include a catch-all variant like:
//!         // `#[serde(other)] Other`
//!         // On the other hand, if we intend to handle every type of metadata, we can omit the
//!         // catch-all variant; unknown metadata will produce an
//!         // [`Error::RecoverableFormatError`], which we can use to emit a warning and continue.
//!     }
//!
//!     // Obtain log data directly; it could also be deserialized from a file.
//!     let log = profiler::internal::take_log();
//!     // Parse the log. Interpret metadata according to the enum defined above.
//!     let profile: profiler_data::Profile<MyMetadata> = log.parse().unwrap();
//!     // Verify the MyData objects are present and attached to the right interval.
//!     let demo = &profile[profile.root_interval().children[0]];
//!     let interval = &profile[demo.children[0]];
//!     let action = &profile[interval.measurement];
//!     assert_eq!(&action.label.name, "action_producing_metadata");
//!     assert_eq!(interval.metadata[0].data, MyMetadata::MyDataA(MyDataA(23)));
//!     assert_eq!(interval.metadata[1].data, MyMetadata::MyDataB(MyDataB("5".into())));
//!     // Marks can be used to compare the order of events.
//!     assert!(interval.metadata[0].mark < interval.metadata[1].mark);
//! }
//!
//! store_and_retrieve_metadata();
//! ```

#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

use enso_profiler as profiler;

use std::error;
use std::fmt;

pub mod parse;



// =============
// === Error ===
// =============

/// Describes an error and where it occurred.
pub enum Error<M> {
    /// Failed to deserialize the event log at all. The file is corrupt, or in a completely
    /// incompatible format.
    FormatError(serde_json::Error),
    /// Failed to deserialize some events; if this is caused by a change to a metadata type, the
    /// core data and metadata of unaffected types will still be available.
    ///
    /// For an example of handling a recoverable failure, see `tests::skip_failed_metadata`.
    RecoverableFormatError {
        /// Deserialization errors for each Event that failed to parse.
        errors:            Vec<EventError<serde_json::Error>>,
        /// The core data.
        ///
        /// If the `errors` all relate to metadata events, the remaining data will be
        /// available here, with one metadata object missing for each error.
        ///
        /// If some errors are not metadata errors (i.e. the core format has changed), the readable
        /// subset of events might not form a valid log, and this will contain an error too.
        with_missing_data: Result<Profile<M>, EventError<parse::DataError>>,
    },
    /// Failed to interpret the event log data.
    DataError(EventError<parse::DataError>),
}

impl<M> fmt::Display for Error<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// This cannot be derived because: https://github.com/rust-lang/rust/issues/26925
// Also, the debug output doesn't need to include the entire with_missing_data.
impl<M> fmt::Debug for Error<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::FormatError(e) => e.fmt(f),
            Error::RecoverableFormatError { errors, .. } => errors.fmt(f),
            Error::DataError(e) => e.fmt(f),
        }
    }
}

impl<M> error::Error for Error<M> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(match self {
            Error::FormatError(e) => e,
            Error::RecoverableFormatError { errors, .. } => &errors[0],
            Error::DataError(e) => e,
        })
    }
}

/// An error associated with a particular event in the log.
#[derive(Debug)]
pub struct EventError<E> {
    #[allow(unused)] // displayed by Debug
    /// The event's index in the log.
    log_pos: usize,
    #[allow(unused)] // displayed by Debug
    /// The error.
    error:   E,
}

impl<E: fmt::Debug> fmt::Display for EventError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<E: error::Error> error::Error for EventError<E> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.error.source()
    }
}



// ===============
// === Profile ===
// ===============

/// All the profiling information captured during one run of the application.
///
/// This is parameterized by a type that determines how metadata is interpreted. The type must be
/// an enum, with a variant for each type of metadata that is handled. Each variant's name and type
/// should correspond to the parameters supplied to [`profiler::MetadataLogger::new`]. For an
/// example, see the docs for the [`crate`].
#[derive(Clone, Debug)]
pub struct Profile<M> {
    /// The hierarchy of profilers. A parent-child relationship indicates that the child was
    /// started while the parent was running.
    pub measurements: Vec<Measurement>,
    /// The hierarchy of intervals. A parent-child relationship indicates that the child is
    /// contained within the parent.
    pub intervals:    Vec<ActiveInterval<M>>,
}

impl<M> Profile<M> {
    /// A virtual measurement containing the top-level measurements as children.
    pub fn root_measurement(&self) -> &Measurement {
        self.measurements.last().unwrap()
    }
}

impl<M> Profile<M> {
    /// A virtual interval containing the top-level intervals as children.
    pub fn root_interval(&self) -> &ActiveInterval<M> {
        self.intervals.last().unwrap()
    }
}


// === IDs and indexing ===

/// Identifies a measurement in a particular profile.
#[derive(Copy, Clone, Debug)]
pub struct MeasurementId(pub(crate) usize);

/// Identifies an interval in a particular profile.
#[derive(Copy, Clone, Debug)]
pub struct IntervalId(pub(crate) usize);

impl<M> std::ops::Index<MeasurementId> for Profile<M> {
    type Output = Measurement;
    fn index(&self, MeasurementId(index): MeasurementId) -> &Self::Output {
        &self.measurements[index]
    }
}

impl<M> std::ops::Index<IntervalId> for Profile<M> {
    type Output = ActiveInterval<M>;
    fn index(&self, IntervalId(index): IntervalId) -> &Self::Output {
        &self.intervals[index]
    }
}



// ===================
// === Measurement ===
// ===================

/// All the information produced by a profiler.
#[derive(Clone, Debug)]
pub struct Measurement {
    /// Identifies the profiler's source and scope to the user.
    pub label:     Label,
    /// Profilers started by this profiler, ordered by time created.
    pub children:  Vec<MeasurementId>,
    /// When the profiler was created.
    pub created:   Mark,
    /// Whether the profiler logged its completion at the end of its last active interval.
    pub finished:  bool,
    /// When the profiler was running.
    pub intervals: Vec<IntervalId>,
}

impl Measurement {
    /// Distinguish between classes of profilers that may need to be handled differently.
    pub fn classify(&self) -> Class {
        self.label.classify()
    }
}


// == Class ==

/// Distinguishes special profilers from normal profilers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Class {
    /// Profiler that is active during the execution of anything else, after early startup.
    OnFrame,
    /// Profiler that is run when a WebGL context is acquired or lost.
    SetContext,
    /// Any profiler that doesn't need special treatment.
    Normal,
}



// ================
// === Metadata ===
// ================

/// Wrapper adding a timestamp to dependency-injected contents.
#[derive(Clone, Debug)]
pub struct Metadata<M> {
    /// Time the data was logged.
    pub mark: Mark,
    /// The actual data.
    pub data: M,
}


// === OpaqueMetadata ===

/// Black-box metadata object, for ignoring metadata contents.
#[derive(Debug, Copy, Clone, serde::Serialize, serde::Deserialize)]
pub enum OpaqueMetadata {
    /// Anything.
    #[serde(other)]
    Unknown,
}



// ============
// === Mark ===
// ============

/// A timestamp that can be used for distinguishing event order.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Mark {
    /// Sequence number of the mark. Used to resolve timestamp collisions.
    seq:  Seq,
    /// Time of the mark.
    time: profiler::internal::Timestamp,
}

impl Mark {
    fn time_origin() -> Self {
        Self::default()
    }

    /// Time of the mark in milliseconds.
    pub fn into_ms(self) -> f64 {
        self.time.into_ms()
    }
}


// === Seq ===

/// A value that can be used to compare the order of events.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub(crate) struct Seq(u32);

impl Seq {
    fn runtime_event(event_index: u32) -> Self {
        // Seq(0) is the time origin.
        Seq(event_index.checked_add(1).unwrap())
    }
}



// ======================
// === ActiveInterval ===
// ======================

/// Represents the tree of profilers active during some interval.
#[derive(Clone, Debug)]
pub struct ActiveInterval<M> {
    /// The profiler instance that this interval belongs to.
    pub measurement: MeasurementId,
    /// The time spanned by this interval.
    pub interval:    Interval,
    /// Active intervals that occurred during this interval.
    pub children:    Vec<IntervalId>,
    /// Metadata emitted while this was the active interval.
    pub metadata:    Vec<Metadata<M>>,
}



// ================
// === Interval ===
// ================

/// A start time and an optional end time.
#[derive(Copy, Clone, Debug)]
pub struct Interval {
    /// The time the interval began.
    pub start: Mark,
    /// The time the interval ended, or None if no end was logged.
    pub end:   Option<Mark>,
}

impl Interval {
    /// Return whether this interval has a known end.
    pub fn closed(self) -> bool {
        self.end.is_some()
    }

    /// Return the duration from start to end in milliseconds, if the end is known.
    pub fn duration_ms(self) -> Option<f64> {
        self.end.map(|end| end.into_ms() - self.start.into_ms())
    }
}



// =============
// === Label ===
// =============

/// A measurement label.
#[derive(Debug, Clone)]
pub struct Label {
    /// The name of the measurement, usually a function.
    pub name: String,
    /// Location in the code the measurement originated, if compiled with line numbers enabled.
    pub pos:  Option<CodePos>,
}

impl Label {
    /// Recognize profilers with special names.
    fn classify(&self) -> Class {
        match self.name.as_str() {
            "@on_frame" => Class::OnFrame,
            "@set_context" => Class::SetContext,
            // Data producer is probably newer than consumer. Forward compatibility isn't necessary.
            name if name.starts_with('@') => panic!("Unrecognized special profiler: {:?}", name),
            _ => Class::Normal,
        }
    }
}


impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(pos) = self.pos.as_ref() {
            write!(f, "{} ({}:{})", self.name, pos.file, pos.line)
        } else {
            write!(f, "{}", self.name)
        }
    }
}


// === CodePos ===

/// Identifies a position within a specific file.
#[derive(Debug, Clone)]
pub struct CodePos {
    /// The path to the file.
    pub file: String,
    /// A line number within the file.
    pub line: u32,
}



// ==================
// === Unit tests ===
// ==================

#[cfg(test)]
mod tests {

    use crate as profiler_data;
    use crate::OpaqueMetadata;
    use enso_profiler as profiler;
    use profiler::profile;


    #[test]
    fn profile_sync() {
        #[profile(Objective)]
        fn parent() -> u32 {
            child()
        }
        #[profile(Objective)]
        fn child() -> u32 {
            4
        }
        parent();
        let profile: profiler_data::Profile<OpaqueMetadata> =
            profiler::internal::take_log().parse().unwrap();
        let roots = &profile.root_measurement().children;
        assert_eq!(roots.len(), 1);
        let parent = &profile[roots[0]];
        assert!(parent.finished);
        assert_eq!(parent.label.name, "parent");
        assert_eq!(parent.children.len(), 1);
        let child = &profile[parent.children[0]];
        assert!(child.finished);
        assert_eq!(child.label.name, "child");
        assert_eq!(child.children.len(), 0);
    }

    #[test]
    fn profile_async() {
        #[profile(Objective)]
        async fn parent() -> u32 {
            child().await
        }
        #[profile(Objective)]
        async fn child() -> u32 {
            let block = async { 4 };
            block.await
        }
        let future = parent();
        futures::executor::block_on(future);
        let profile: profiler_data::Profile<OpaqueMetadata> =
            profiler::internal::take_log().parse().unwrap();
        let roots = &profile.root_measurement().children;
        assert_eq!(roots.len(), 1);
        let parent = &profile[roots[0]];
        assert!(parent.finished);
        let parent_intervals = &parent.intervals;
        assert_eq!(parent_intervals.len(), 2);
        for interval in parent_intervals {
            assert!(profile[*interval].interval.closed());
        }
        assert!(parent.finished);
        assert_eq!(parent.label.name, "parent");
        assert_eq!(parent.children.len(), 1);
        let child = &profile[parent.children[0]];
        assert!(child.finished);
        let child_intervals = &child.intervals;
        assert_eq!(child_intervals.len(), 2);
        for interval in child_intervals {
            assert!(profile[*interval].interval.closed());
        }
        assert!(child.finished);
        assert_eq!(child.label.name, "child");
        assert_eq!(child.children.len(), 1, "{:?}", &profile);
    }

    #[test]
    fn unfinished_never_started() {
        #[profile(Objective)]
        async fn func() {}
        // Create a Future, but don't await it.
        let _future = func();
        let profile: profiler_data::Profile<OpaqueMetadata> =
            profiler::internal::take_log().parse().unwrap();
        let roots = &profile.root_measurement().children;
        assert!(!profile[roots[0]].finished);
    }

    #[test]
    fn unfinished_still_running() {
        profiler::internal::EventLog.start(
            profiler::internal::EventId::implicit(),
            profiler::internal::Label("unfinished (?:?)"),
            None,
            profiler::internal::StartState::Active,
        );
        let profile: profiler_data::Profile<OpaqueMetadata> =
            profiler::internal::take_log().parse().unwrap();
        let roots = &profile.root_measurement().children;
        assert!(!profile[roots[0]].finished);
    }

    #[test]
    fn unfinished_paused_never_resumed() {
        let id = profiler::internal::EventLog.start(
            profiler::internal::EventId::implicit(),
            profiler::internal::Label("unfinished (?:?)"),
            None,
            profiler::internal::StartState::Active,
        );
        profiler::internal::EventLog.pause(id, profiler::internal::Timestamp::now());
        let profile: profiler_data::Profile<OpaqueMetadata> =
            profiler::internal::take_log().parse().unwrap();
        let roots = &profile.root_measurement().children;
        assert!(!profile[roots[0]].finished);
    }

    /// Simulate a change to the format of a type of metadata; ensure the error is reported
    /// correctly, and all other data is still readable.
    #[test]
    fn skip_failed_metadata() {
        #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
        struct MyDataA(u32);
        #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
        struct MyDataBExpected(u32);
        #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
        struct MyDataBActual(String);

        let meta_logger_a = profiler::MetadataLogger::new("MyDataA");
        let meta_logger_b = profiler::MetadataLogger::new("MyDataB");
        meta_logger_a.log(MyDataA(23));
        meta_logger_b.log(MyDataBActual("bad".into()));

        #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
        enum MyMetadata {
            MyDataA(MyDataA),
            MyDataB(MyDataBExpected),
        }
        let log = profiler::internal::take_log();
        let root: Result<profiler_data::Profile<MyMetadata>, _> = log.parse();
        let root = match root {
            Err(profiler_data::Error::RecoverableFormatError { errors, with_missing_data }) => {
                assert_eq!(errors.len(), 1);
                assert_eq!(errors[0].log_pos, 1);
                with_missing_data.unwrap()
            }
            other => panic!("Expected RecoverableFormatError, found: {:?}", other),
        };
        assert_eq!(root.root_interval().metadata.len(), 1);
        assert_eq!(root.root_interval().metadata[0].data, MyMetadata::MyDataA(MyDataA(23)));
    }
}
