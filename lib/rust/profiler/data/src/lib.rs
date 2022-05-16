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
//! #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
//! struct MyDataA(u32);
//! profiler::metadata_logger!("MyDataA", log_data_a(u32));
//!
//! #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
//! struct MyDataB(String);
//! profiler::metadata_logger!("MyDataB", log_data_b(String));
//!
//! #[profile(Objective)]
//! fn action_producing_metadata() {
//!     log_data_a(23);
//!     log_data_b("5".into());
//! }
//!
//! fn store_and_retrieve_metadata() {
//!     action_producing_metadata();
//!
//!     // To deserialize, we define a metadata type as an enum.
//!     //
//!     // Each variant has a name and type that match the string-argument and type-parameter that
//!     // match the `profiler::metadata_logger!` definition. If the type is a newtype, the
//!     // metadata logger may accept the wrapped type for convenience; a newtype and its contents
//!     // have the same serialized form.
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
//!     let interval = &profile[profile.root_interval().children[0]];
//!     let action = &profile[interval.measurement];
//!     assert_eq!(&action.label.name, "action_producing_metadata");
//!     assert_eq!(interval.metadata[0].data, MyMetadata::MyDataA(MyDataA(23)));
//!     assert_eq!(interval.metadata[1].data, MyMetadata::MyDataB(MyDataB("5".into())));
//!     // Timestamps can be used to compare the order of events.
//!     assert!(interval.metadata[0].time < interval.metadata[1].time);
//! }
//!
//! store_and_retrieve_metadata();
//! ```

// === Features ===
#![feature(test)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use enso_profiler as profiler;
use profiler::format;
use std::error;
use std::fmt;
use std::rc::Rc;


// ==============
// === Export ===
// ==============

pub mod aggregate;
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
        /// Deserialization errors for each metadata Event that failed to parse.
        errors:            Vec<EventError<serde_json::Error>>,
        /// A profile with metadata of one or more types excluded due to format incompatibility.
        /// There is one missing metadata object for each value in `errors`.
        with_missing_data: Profile<M>,
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



// ==============================
// === Multi-process profiles ===
// ==============================

/// Parse data representing profiling information collected by multiple processes.
pub fn parse_multiprocess_profile<M: serde::de::DeserializeOwned>(
    data: &str,
) -> impl Iterator<Item = Result<Profile<M>, Error<M>>> + '_ {
    serde_json::Deserializer::from_str(data).into_iter::<Box<serde_json::value::RawValue>>().map(
        |profile| {
            let raw_parse_error = "Cannot parse input as sequence of JSON values!";
            profile.expect(raw_parse_error).get().parse()
        },
    )
}



// ===============
// === Profile ===
// ===============

/// All the profiling information captured by one process during one run of the application.
///
/// This is parameterized by a type that determines how metadata is interpreted. The type must be
/// an enum, with a variant for each type of metadata that is handled. Each variant's name and type
/// should correspond to the parameters supplied to [`profiler::metadata_logger`]. For an example,
/// see the docs for the [`crate`].
#[derive(Clone, Debug)]
pub struct Profile<M> {
    /// The hierarchy of profilers. A parent-child relationship indicates that the child was
    /// started while the parent was running.
    pub measurements: Vec<Measurement>,
    /// The hierarchy of intervals. A parent-child relationship indicates that the child is
    /// contained within the parent.
    pub intervals:    Vec<ActiveInterval<M>>,
    /// Optional information about this profile.
    pub headers:      Headers,
}

impl<M> Profile<M> {
    /// A virtual measurement containing the top-level measurements as children.
    pub fn root_measurement(&self) -> &Measurement {
        self.measurements.last().unwrap()
    }

    /// A virtual interval containing the top-level intervals as children.
    pub fn root_interval(&self) -> &ActiveInterval<M> {
        self.intervals.last().unwrap()
    }

    /// Id of a virtual measurement containing the top-level measurements as children.
    pub fn root_measurement_id(&self) -> MeasurementId {
        MeasurementId(self.measurements.len() - 1)
    }

    /// Id of a virtual interval containing the top-level intervals as children.
    pub fn root_interval_id(&self) -> IntervalId {
        IntervalId(self.intervals.len() - 1)
    }

    /// Iterate over only the metadata stored in the profile.
    pub fn iter_metadata(&self) -> impl Iterator<Item = &Timestamped<M>> {
        self.intervals.iter().flat_map(|interval| interval.metadata.iter())
    }
}


// === Headers ===

/// Information about the profile.
#[derive(Clone, Debug, Default)]
pub struct Headers {
    /// A value that can be used to translate a timestamp to system time.
    pub time_offset: Option<format::Timestamp>,
    /// An application-specific identifier used to distinguish logs from different processes.
    pub process:     Option<String>,
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
    pub label:     Rc<Label>,
    /// Profilers started by this profiler, ordered by time created.
    pub children:  Vec<MeasurementId>,
    /// When the profiler was created.
    pub created:   Timestamp,
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



// ===================
// === Timestamped ===
// ===================

/// Wrapper adding a timestamp to contents.
#[derive(Clone, Debug)]
pub struct Timestamped<M> {
    /// Time the data was logged.
    pub time: Timestamp,
    /// The actual data.
    pub data: M,
}

impl<M> Timestamped<M> {
    /// Convert from &[`Timestamped<M>`] to [`Timestamped<&M>`].
    pub fn as_ref(&self) -> Timestamped<&M> {
        let Self { time, data } = self;
        let time = *time;
        Timestamped { time, data }
    }

    /// Use a function to transform the contained data, preserving the timestamp.
    pub fn map<F, N>(self, f: F) -> Timestamped<N>
    where F: FnOnce(M) -> N {
        let Self { time, data } = self;
        let data = f(data);
        Timestamped { time, data }
    }
}


// === OpaqueMetadata ===

/// Black-box metadata object, for ignoring metadata contents.
pub type OpaqueMetadata = format::AnyMetadata;



// =================
// === Timestamp ===
// =================

/// A timestamp. Supports distinguishing order of all events within a process.
///
/// Note that while an [`Ord`] implementation is provided for convenience (e.g. for use with
/// data structures that require it), the results of comparisons should only be considered
/// meaningful when comparing [`Timestamp`]s that were recorded by the same process.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Timestamp {
    /// The time.
    time: format::Timestamp,
    /// Indicates event order; used to resolve timestamp collisions.
    seq:  Seq,
}

impl Timestamp {
    fn time_origin() -> Self {
        Self::default()
    }

    /// Offset from the time origin, in milliseconds.
    pub fn into_ms(self) -> f64 {
        self.time.into_ms()
    }
}


// === Seq ===

/// A value that can be used to compare the order of events within a process.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub(crate) struct Seq(usize);

impl Seq {
    fn runtime_event(event_index: usize) -> Self {
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
    pub metadata:    Vec<Timestamped<M>>,
}



// ================
// === Interval ===
// ================

/// A start time and an optional end time.
#[derive(Copy, Clone, Debug)]
pub struct Interval {
    /// The time the interval began.
    pub start: Timestamp,
    /// The time the interval ended, or None if no end was logged.
    pub end:   Option<Timestamp>,
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

    fn start_profiler(label: &'static str) -> profiler::internal::EventId {
        profiler::internal::EventLog.start(
            profiler::internal::EventId::implicit(),
            profiler::internal::Label(label),
            Some(profiler::internal::Timestamp::now()),
            profiler::internal::StartState::Active,
        )
    }

    #[test]
    fn unfinished_still_running() {
        start_profiler("unfinished (?:?)");
        let profile: profiler_data::Profile<OpaqueMetadata> =
            profiler::internal::take_log().parse().unwrap();
        let roots = &profile.root_measurement().children;
        assert!(!profile[roots[0]].finished);
    }

    #[test]
    fn unfinished_paused_never_resumed() {
        let id = start_profiler("unfinished (?:?)");
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
        #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
        struct MyDataA(u32);
        profiler::metadata_logger!("MyDataA", log_data_a(u32));
        #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
        struct MyDataBExpected(u32);
        #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
        struct MyDataBActual(String);
        profiler::metadata_logger!("MyDataB", log_data_b(String));

        log_data_a(23);
        log_data_b("bad".into());

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
                with_missing_data
            }
            other => panic!("Expected RecoverableFormatError, found: {:?}", other),
        };
        assert_eq!(root.root_interval().metadata.len(), 1);
        assert_eq!(root.root_interval().metadata[0].data, MyMetadata::MyDataA(MyDataA(23)));
    }
}
