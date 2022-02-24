#![allow(rustdoc::private_intra_doc_links)] // check_no_async_tasks_active
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
//!     let log = profiler::take_log();
//!     // Parse the log. Interpret metadata according to the enum defined above.
//!     let root: profiler_data::Measurement<MyMetadata> = log.parse().unwrap();
//!     // Verify the MyData object is present and attached to the right profiler.
//!     let profiler = &root.children[0].children[0];
//!     assert_eq!(&profiler.label.name, "action_producing_metadata");
//!     assert_eq!(profiler.metadata[0].data, MyMetadata::MyDataA(MyDataA(23)));
//!     assert_eq!(profiler.metadata[1].data, MyMetadata::MyDataB(MyDataB("5".into())));
//!     // Marks can be used to compare the order of events.
//!     assert!(profiler.metadata[0].mark < profiler.metadata[1].mark);
//! }
//!
//! store_and_retrieve_metadata();
//! ```
//!
//! # Limitations
//!
//! [`LogVisitor::check_no_async_task_active`] checks for a type of API misuse error, but currently
//! also disallows running an async profiler during the lifetime of a non-async parent. The only way
//! that could occur is with the use of `block_on`, which is never used in the Enso codebase except
//! in tests.

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
use profiler::id;

use std::collections;
use std::error;
use std::fmt;
use std::mem;
use std::str;

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
        /// Deserialization errors for each metadata that failed to parse.
        errors:            Vec<EventError<serde_json::Error>>,
        /// The core data. One metadata object will be missing for each error above.
        with_missing_data: Result<Measurement<M>, EventError<DataError>>,
    },
    /// Failed to interpret the event log data.
    DataError(EventError<DataError>),
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



// ===================
// === Measurement ===
// ===================

/// All the information produced by a profiler.
///
/// This is parameterized by a type that determines how metadata is interpreted. The type must be
/// an enum, with a variant for each type of metadata that is handled. Each variant's name and type
/// should correspond to the parameters supplied to [`profiler::MetadataLogger::new`]. For an
/// example, see the docs for the [`crate`].
#[derive(Clone, Debug)]
pub struct Measurement<M> {
    /// When the profiler was running.
    pub lifetime: Lifetime,
    /// Identifies the profiler's source and scope to the user.
    pub label:    Label,
    /// Profilers started by this profiler.
    pub children: Vec<Self>,
    /// Metadata attached to this profiler.
    pub metadata: Vec<Metadata<M>>,
}


// === Lifetime ===

/// Information about when a profiler was running.
#[derive(Clone, Debug)]
pub enum Lifetime {
    /// Information applicable to async profilers.
    Async(AsyncLifetime),
    /// Information applicable to non-async profilers.
    NonAsync {
        /// The interval that the profiler was running.
        active: Interval,
    },
}

impl Lifetime {
    /// Whether the task this profiler measures was completed.
    pub fn finished(&self) -> bool {
        match self {
            Lifetime::Async(lifetime) => lifetime.finished,
            Lifetime::NonAsync { active } => active.end.is_some(),
        }
    }

    /// Get a AsyncLifetime, if this Lifetime was async.
    pub fn as_async(&self) -> Option<&AsyncLifetime> {
        match self {
            Lifetime::Async(lifetime) => Some(lifetime),
            Lifetime::NonAsync { .. } => None,
        }
    }

    /// Whether this profiler recorded an async task.
    pub fn is_async(&self) -> bool {
        self.as_async().is_some()
    }
}

/// Information about when an async profiler was running.
#[derive(Clone, Debug)]
pub struct AsyncLifetime {
    /// The time a profiled `async fn` was called, if known.
    ///
    /// This will always be before the first interval in `active`, as the function must be
    /// called before it can be awaited and begin running.
    pub created:  Option<Mark>,
    /// Intervals that the profiler was running.
    pub active:   Vec<Interval>,
    /// If true: the last interval in `active` ended when the task was completed.
    /// If false: the task was awaiting or running (indicated by whether the last interval in
    /// `active` has an end) at the time the log was created.
    pub finished: bool,
}

impl AsyncLifetime {
    /// The interval from when the async profiler was created, to when it finished.
    ///
    /// If creation time is not known, it will be approximated by first start-time.
    ///
    /// If the profiler is associated with a Future that was created but never awaited,
    /// this will return None.
    pub fn create_to_finish(&self) -> Option<Interval> {
        self.active.first().map(|first| {
            let start = self.created.unwrap_or(first.start);
            let end = match self.finished {
                true => self.active.last().unwrap().end,
                false => None,
            };
            Interval { start, end }
        })
    }
}


// === Parsing ===

impl<M: serde::de::DeserializeOwned> str::FromStr for Measurement<M> {
    type Err = Error<M>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // First just decode the array structure, so we can skip any metadata events that fail.
        let log: Vec<&serde_json::value::RawValue> =
            serde_json::from_str(s).map_err(Error::FormatError)?;
        let mut errors = Vec::new();
        let mut events = Vec::with_capacity(log.len());
        for (i, entry) in log.into_iter().enumerate() {
            match serde_json::from_str::<profiler::Event<M, String>>(entry.get()) {
                Ok(event) => events.push(event),
                Err(error) => errors.push(EventError { log_pos: i, error }),
            }
        }
        let result = interpret(events);
        if errors.is_empty() {
            result.map_err(|e| Error::DataError(e))
        } else {
            Err(Error::RecoverableFormatError { errors, with_missing_data: result })
        }
    }
}

/// Process a log of events, producing a hierarchy of measurements.
///
/// Returns an error if the log cannot be interpreted.
fn interpret<M>(
    events: impl IntoIterator<Item = profiler::Event<M, OwnedLabel>>,
) -> Result<Measurement<M>, EventError<DataError>> {
    // Process log into data about each measurement, and data about relationships.
    let LogVisitor { builders, order, root_builder, .. } = LogVisitor::visit(events)?;
    // Build measurements from accumulated measurement data.
    let mut measurements: collections::HashMap<_, _> =
        builders.into_iter().map(|(k, v)| (k, v.build())).collect();
    // Organize measurements into trees.
    let mut root = root_builder.build();
    for (id, parent) in order.into_iter().rev() {
        let child = measurements.remove(&id).unwrap();
        let parent = match parent {
            id::Explicit::AppLifetime => &mut root.children,
            id::Explicit::Runtime(pos) =>
                &mut measurements
                    .get_mut(&pos)
                    .ok_or(DataError::IdNotFound)
                    .map_err(|e| EventError { log_pos: id.0 as usize, error: e })?
                    .children,
        };
        parent.push(child);
    }
    Ok(root)
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



// ============
// === Mark ===
// ============

/// A timestamp that can be used for distinguishing event order.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Mark {
    seq:  Seq,
    time: profiler::Timestamp,
}

impl Mark {
    fn time_origin() -> Self {
        Self::default()
    }
}


// === Seq ===

/// A value that can be used to compare the order of events.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct Seq(u32);

impl From<id::Runtime> for Seq {
    fn from(pos: id::Runtime) -> Self {
        // Seq(0) is the time origin.
        Seq(pos.0 + 1)
    }
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

impl str::FromStr for Label {
    type Err = Expected;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (name, pos) = s.rsplit_once(' ').ok_or(Expected(" "))?;
        Ok(Self { name: name.to_owned(), pos: CodePos::parse(pos)? })
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
impl CodePos {
    fn parse(s: &str) -> Result<Option<Self>, Expected> {
        let (file, line) = s.rsplit_once(':').ok_or(Expected(":"))?;
        let file = file.strip_prefix('(').ok_or(Expected("("))?;
        let line = line.strip_suffix(')').ok_or(Expected(")"))?;
        Ok(if file == "?" {
            None
        } else {
            Some(Self {
                file: file.to_owned(),
                line: line.parse().map_err(|_| Expected("line number"))?,
            })
        })
    }
}



// ==========================
// === MeasurementBuilder ===
// ==========================

/// Used while gathering information about a profiler.
struct MeasurementBuilder<M> {
    label:          Label,
    created_paused: Option<Mark>,
    finished:       bool,
    starts:         Vec<Mark>,
    ends:           Vec<Mark>,
    state:          State,
    metadata:       Vec<Metadata<M>>,
}

impl<M> MeasurementBuilder<M> {
    fn build(self) -> Measurement<M> {
        let MeasurementBuilder { label, starts, ends, metadata, created_paused, finished, state } =
            self;
        let mut ends = ends.into_iter().fuse();
        let active: Vec<_> =
            starts.into_iter().map(|start| Interval { start, end: ends.next() }).collect();
        let children = Vec::new();
        let is_paused = matches!(state, State::Paused(_));
        // A profiler is considered async if:
        // - It was created in a non-running state (so, `#[profile] async fn` is always async).
        // - It ever awaited (i.e. it has more than one active interval).
        // - It suspended, and never awaited.
        let lifetime = if created_paused.is_none() && active.len() == 1 && !is_paused {
            Lifetime::NonAsync { active: *active.first().unwrap() }
        } else {
            Lifetime::Async(AsyncLifetime { created: created_paused, active, finished })
        };
        Measurement { lifetime, label, children, metadata }
    }
}


// === State ===

/// Used to validate state transitions.
#[derive(Debug, Copy, Clone)]
pub enum State {
    /// Started and not paused or ended; id of most recent Start or Resume event is included.
    Active(id::Runtime),
    /// Paused. Id of Pause or StartPaused event is included.
    Paused(id::Runtime),
    /// Ended. Id of End event is included.
    Ended(id::Runtime),
}



// =================
// === DataError ===
// =================

/// A problem with the input data.
#[derive(Debug)]
pub enum DataError {
    /// A profiler was in the wrong state for a certain event to occur.
    UnexpectedState(State),
    /// A reference was not able to be resolved.
    IdNotFound,
    /// An EventId that should have been a runtime event was IMPLICIT or APP_LIFETIME.
    RuntimeIdExpected(profiler::id::Event),
    /// A parse error.
    UnexpectedToken(Expected),
    /// An event that should only occur during the lifetime of a profiler didn't find any profiler.
    ActiveProfilerRequired,
    /// An event expected to refer to a certain profiler referred to a different profiler.
    /// This can occur for events that include a profiler ID as a consistency check, but only
    /// have one valid referent (e.g. [`profiler::Event::End`] must end the current profiler.)
    WrongProfiler {
        /// The profiler that was referred to.
        found:    id::Runtime,
        /// The only valid profiler for the event to refer to.
        expected: id::Runtime,
    },
    /// Profiler(s) were active at a time none were expected.
    ExpectedEmptyStack(Vec<id::Runtime>),
    /// A profiler was expected to have started before a related event occurred.
    ExpectedStarted,
}

impl fmt::Display for DataError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl error::Error for DataError {}

impl From<Expected> for DataError {
    fn from(inner: Expected) -> Self {
        DataError::UnexpectedToken(inner)
    }
}


// === Expected ===

/// Parsing error: expected a different token.
#[derive(Debug, Copy, Clone)]
pub struct Expected(&'static str);

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl error::Error for Expected {}



// ==================
// === LogVisitor ===
// ==================

/// Gathers data while visiting a series of [`profiler::Event`]s.
struct LogVisitor<M> {
    /// Stack of active profilers, for keeping track of the current profiler.
    active:       Vec<id::Runtime>,
    /// Accumulated data pertaining to each profiler.
    builders:     collections::HashMap<id::Runtime, MeasurementBuilder<M>>,
    /// Accumulated data pertaining to the root event.
    root_builder: MeasurementBuilder<M>,
    /// Ids and parents, in same order as event log.
    order:        Vec<(id::Runtime, id::Explicit)>,
}

impl<M> Default for LogVisitor<M> {
    fn default() -> Self {
        let root_builder = MeasurementBuilder {
            label:          "APP_LIFETIME (?:?)".parse().unwrap(),
            created_paused: Default::default(),
            finished:       Default::default(),
            state:          State::Active(id::Runtime(0)), // value doesn't matter
            starts:         vec![Mark::time_origin()],
            ends:           Default::default(),
            metadata:       Default::default(),
        };
        Self {
            active: Default::default(),
            builders: Default::default(),
            root_builder,
            order: Default::default(),
        }
    }
}

impl<M> LogVisitor<M> {
    /// Convert the log into data about each measurement.
    fn visit(
        events: impl IntoIterator<Item = profiler::Event<M, OwnedLabel>>,
    ) -> Result<Self, EventError<DataError>> {
        let mut visitor = Self::default();
        for (i, event) in events.into_iter().enumerate() {
            let log_pos = id::Runtime(i as u32);
            let result = match event {
                profiler::Event::Start(event) =>
                    visitor.visit_start(log_pos, event, profiler::StartState::Active),
                profiler::Event::StartPaused(event) =>
                    visitor.visit_start(log_pos, event, profiler::StartState::Paused),
                profiler::Event::End { id, timestamp } => visitor.visit_end(log_pos, id, timestamp),
                profiler::Event::Pause { id, timestamp } =>
                    visitor.visit_pause(log_pos, id, timestamp),
                profiler::Event::Resume { id, timestamp } =>
                    visitor.visit_resume(log_pos, id, timestamp),
                profiler::Event::Metadata(metadata) => visitor.visit_metadata(log_pos, metadata),
            };
            result.map_err(|error| EventError { log_pos: i, error })?;
        }
        Ok(visitor)
    }
}


// === Handlers for each event ===

impl<M> LogVisitor<M> {
    fn visit_start(
        &mut self,
        pos: id::Runtime,
        event: profiler::Start<OwnedLabel>,
        start_state: profiler::StartState,
    ) -> Result<(), DataError> {
        let parent = match event.parent.into() {
            id::Event::Explicit(parent) => parent,
            id::Event::Implicit => self.current_profiler(),
        };
        let start = match event.start {
            Some(time) => Mark { seq: pos.into(), time },
            None => self.inherit_start(parent)?,
        };
        let state = match start_state {
            profiler::StartState::Active => State::Active(pos),
            profiler::StartState::Paused => State::Paused(pos),
        };
        let created_paused = match start_state {
            profiler::StartState::Paused => Some(start),
            profiler::StartState::Active => None,
        };
        let mut builder = MeasurementBuilder {
            label: event.label.to_string().parse()?,
            created_paused,
            finished: Default::default(),
            state,
            starts: Default::default(),
            ends: Default::default(),
            metadata: Default::default(),
        };
        if start_state == profiler::StartState::Active {
            builder.starts.push(start);
            self.active.push(pos);
        }
        self.order.push((pos, parent));
        let old = self.builders.insert(pos, builder);
        assert!(old.is_none());
        Ok(())
    }

    fn visit_end(
        &mut self,
        pos: id::Runtime,
        id: profiler::EventId,
        time: profiler::Timestamp,
    ) -> Result<(), DataError> {
        let current_profiler = self.active.pop().ok_or(DataError::ActiveProfilerRequired)?;
        check_profiler(id, current_profiler)?;
        let measurement = &mut self.builders.get_mut(&current_profiler).unwrap();
        let mark = Mark { seq: pos.into(), time };
        measurement.finished = true;
        measurement.ends.push(mark);
        match mem::replace(&mut measurement.state, State::Ended(pos)) {
            State::Active(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_pause(
        &mut self,
        pos: id::Runtime,
        id: profiler::EventId,
        time: profiler::Timestamp,
    ) -> Result<(), DataError> {
        let current_profiler = self.active.pop().ok_or(DataError::ActiveProfilerRequired)?;
        check_profiler(id, current_profiler)?;
        self.check_no_async_task_active()?;
        let mark = Mark { seq: pos.into(), time };
        let measurement = &mut self.builders.get_mut(&current_profiler).unwrap();
        measurement.ends.push(mark);
        match mem::replace(&mut measurement.state, State::Paused(pos)) {
            State::Active(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_resume(
        &mut self,
        pos: id::Runtime,
        id: profiler::EventId,
        time: profiler::Timestamp,
    ) -> Result<(), DataError> {
        let start_id = match id.into() {
            id::Event::Explicit(id::Explicit::Runtime(id)) => id,
            id => return Err(DataError::RuntimeIdExpected(id)),
        };
        self.check_no_async_task_active()?;
        self.active.push(start_id);
        let mark = Mark { seq: pos.into(), time };
        let measurement = &mut self.builders.get_mut(&start_id).ok_or(DataError::IdNotFound)?;
        measurement.starts.push(mark);
        match mem::replace(&mut measurement.state, State::Active(pos)) {
            State::Paused(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_metadata(
        &mut self,
        pos: id::Runtime,
        metadata: profiler::Metadata<M>,
    ) -> Result<(), DataError> {
        let builder = match self.active.last() {
            Some(profiler) => self.builders.get_mut(profiler).unwrap(),
            None => &mut self.root_builder,
        };
        let profiler::Metadata { timestamp, data } = metadata;
        let mark = Mark { seq: pos.into(), time: timestamp };
        builder.metadata.push(Metadata { mark, data });
        Ok(())
    }
}


// === Helper types, functions, and methods ===

type OwnedLabel = String;

fn check_profiler(found: profiler::EventId, expected: id::Runtime) -> Result<(), DataError> {
    let found = match found.into() {
        id::Event::Explicit(id::Explicit::Runtime(id)) => id,
        id => return Err(DataError::RuntimeIdExpected(id)),
    };
    if found != expected {
        return Err(DataError::WrongProfiler { found, expected });
    }
    Ok(())
}

impl<M> LogVisitor<M> {
    fn current_profiler(&self) -> id::Explicit {
        match self.active.last() {
            Some(&pos) => pos.into(),
            None => id::Explicit::AppLifetime,
        }
    }

    fn inherit_start(&self, parent: id::Explicit) -> Result<Mark, DataError> {
        Ok(match parent {
            id::Explicit::AppLifetime => Mark::time_origin(),
            id::Explicit::Runtime(pos) =>
                match self.builders.get(&pos).ok_or(DataError::IdNotFound)?.starts.first() {
                    Some(start) => *start,
                    None => return Err(DataError::ExpectedStarted),
                },
        })
    }

    fn check_empty_stack(&self) -> Result<(), DataError> {
        match self.active.is_empty() {
            true => Ok(()),
            false => Err(DataError::ExpectedEmptyStack(self.active.clone())),
        }
    }

    /// Used to validate there is never more than one async task active simultaneously, which
    /// would indicate that a low-level-profiled section has used an uninstrumented `.await`
    /// expression.
    fn check_no_async_task_active(&self) -> Result<(), DataError> {
        // This simple check is conservative; it doesn't allow certain legal behavior, like an
        // instrumented non-async function using block-on to run an instrumented async function,
        // because support for that is unlikely to be needed.
        self.check_empty_stack()
    }
}



// ==================
// === Unit tests ===
// ==================

#[cfg(test)]
mod tests {
    use crate as profiler_data;
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
        let root: profiler_data::Measurement<profiler::OpaqueMetadata> =
            profiler::take_log().parse().unwrap();
        let roots = &root.children;
        assert_eq!(roots.len(), 1);
        assert!(roots[0].lifetime.finished());
        assert!(!roots[0].lifetime.is_async(), "{:?}", &roots[0].lifetime);
        assert!(roots[0].lifetime.finished());
        assert_eq!(roots[0].label.name, "parent");
        assert_eq!(roots[0].children.len(), 1);
        let child = &roots[0].children[0];
        assert!(child.lifetime.finished());
        assert!(!child.lifetime.is_async());
        assert!(child.lifetime.finished());
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
        let root: profiler_data::Measurement<profiler::OpaqueMetadata> =
            profiler::take_log().parse().unwrap();
        let roots = &root.children;
        assert_eq!(roots.len(), 1);
        assert!(roots[0].lifetime.finished());
        let root_intervals = &roots[0].lifetime.as_async().unwrap().active;
        assert_eq!(root_intervals.len(), 2);
        for interval in root_intervals {
            assert!(interval.closed());
        }
        assert!(roots[0].lifetime.finished());
        assert_eq!(roots[0].label.name, "parent");
        assert_eq!(roots[0].children.len(), 1);
        let child = &roots[0].children[0];
        assert!(child.lifetime.finished());
        let child_intervals = &child.lifetime.as_async().unwrap().active;
        assert_eq!(child_intervals.len(), 2);
        for interval in child_intervals {
            assert!(interval.closed());
        }
        assert!(child.lifetime.finished());
        assert_eq!(child.label.name, "child");
        assert_eq!(child.children.len(), 0);
    }

    #[test]
    fn unfinished_never_started() {
        #[profile(Objective)]
        async fn func() {}
        // Create a Future, but don't await it.
        let _future = func();
        let root: profiler_data::Measurement<profiler::OpaqueMetadata> =
            profiler::take_log().parse().unwrap();
        assert!(!root.children[0].lifetime.finished());
    }

    #[test]
    fn unfinished_still_running() {
        profiler::EventLog.start(
            profiler::EventId::implicit(),
            profiler::Label("unfinished (?:?)"),
            None,
            profiler::StartState::Active,
        );
        let root: profiler_data::Measurement<profiler::OpaqueMetadata> =
            profiler::take_log().parse().unwrap();
        assert!(!root.children[0].lifetime.finished());
    }

    #[test]
    fn unfinished_paused_never_resumed() {
        let id = profiler::EventLog.start(
            profiler::EventId::implicit(),
            profiler::Label("unfinished (?:?)"),
            None,
            profiler::StartState::Active,
        );
        profiler::EventLog.pause(id, profiler::Timestamp::now());
        let root: profiler_data::Measurement<profiler::OpaqueMetadata> =
            profiler::take_log().parse().unwrap();
        assert!(!root.children[0].lifetime.finished(), "{:?}", &root);
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
        let log = profiler::take_log();
        let root: Result<profiler_data::Measurement<MyMetadata>, _> = log.parse();
        let root = match root {
            Err(profiler_data::Error::RecoverableFormatError { errors, with_missing_data }) => {
                assert_eq!(errors.len(), 1);
                assert_eq!(errors[0].log_pos, 1);
                with_missing_data.unwrap()
            }
            other => panic!("Expected RecoverableFormatError, found: {:?}", other),
        };
        assert_eq!(root.metadata.len(), 1);
        assert_eq!(root.metadata[0].data, MyMetadata::MyDataA(MyDataA(23)));
    }
}
