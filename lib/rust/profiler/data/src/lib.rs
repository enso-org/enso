//! Interface to profile data.
//!
//! # Overview
//!
//! Usage of this API starts with [`interpret`], which converts a raw log of [`profiler::Event`]s
//! into a hierarchy of [`Measurement`] values. Each [`Measurement`] contains all information
//! relating to a particular profiler. This conversion checks low-level invariants of the event log
//! format, and produces an output that is easier to use than the raw events.
//!
//! # Usage example: storing and retrieving metadata
//!
//! ```
//! use enso_profiler as profiler;
//! use enso_profiler_data as profiler_data;
//! use profiler::profile;
//!
//! // A metadata type.
//! #[derive(serde::Serialize, serde::Deserialize, PartialEq, Eq, Debug)]
//! struct MyData(u32);
//!
//! // An activity that produces metadata.
//! struct ActivityWithMetadata {
//!     meta_logger: profiler::MetadataLogger<MyData>,
//!     // ...fields for doing stuff
//! }
//! impl ActivityWithMetadata {
//!     fn new() -> Self {
//!         let meta_logger = profiler::MetadataLogger::register("MyData");
//!         Self { meta_logger /* ... */ }
//!     }
//!
//!     #[profile(Objective)]
//!     fn action_producing_metadata(&self) {
//!         let x = MyData(23);
//!         self.meta_logger.log(x);
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
//!     #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
//!     enum MyMetadata {
//!         MyData(MyData),
//!     }
//!
//!     // Obtain log data directly; it could also be deserialized from a file.
//!     let log = profiler::take_log::<MyMetadata>();
//!     // Interpret the log, converting its event-series data into a normalized format. Data
//!     // consumers will likely convert the output of this into their own formats, but this
//!     // conversion inherently checks most invariants (valid parent/child relationships,
//!     // interval starts/ends compatible, etc.) so that future steps know the input is valid.
//!     let roots = profiler_data::interpret(log).unwrap();
//!     // Verify the MyData object is present and attached to the right profiler.
//!     let profiler = &roots[0].children[0];
//!     assert_eq!(&profiler.label.name, "action_producing_metadata");
//!     // The other field of profiler.metadata[0], mark, contains timing information for the event.
//!     assert_eq!(profiler.metadata[0].data, MyMetadata::MyData(MyData(23)));
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



// =================
// === interpret ===
// =================

/// Process a log of events, producing a hierarchy of measurements.
///
/// Returns an error if the log cannot be interpreted.
pub fn interpret<M>(
    events: impl IntoIterator<Item = profiler::Event<M, OwnedLabel>>,
) -> Result<Vec<Measurement<M>>, Error> {
    // Process log into data about each measurement, and data about relationships.
    let LogVisitor { builders, order, .. } = LogVisitor::visit(events)?;
    // Build measurements from accumulated measurement data.
    let mut measurements: collections::HashMap<_, _> =
        builders.into_iter().map(|(k, v)| (k, v.build())).collect();
    // Organize measurements into trees.
    let mut roots = Vec::new();
    for (id, parent) in order.into_iter().rev() {
        let child = measurements.remove(&id).unwrap();
        let parent = match parent {
            id::Explicit::AppLifetime => &mut roots,
            id::Explicit::Runtime(pos) =>
                &mut measurements
                    .get_mut(&pos)
                    .ok_or(DataError::IdNotFound)
                    .map_err(|e| Error { log_pos: id, error: e })?
                    .children,
        };
        parent.push(child);
    }
    Ok(roots)
}



// =============
// === Error ===
// =============

/// Describes an error and where it occurred.
#[derive(Debug)]
pub struct Error {
    #[allow(unused)] // used by Debug::fmt
    /// Index in the event log indicating where the event occurred.
    log_pos: id::Runtime,
    /// The error.
    error:   DataError,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&self.error)
    }
}



// ===================
// === Measurement ===
// ===================

/// All the information produced by a profiler.
#[derive(Clone, Debug)]
pub struct Measurement<M> {
    /// The interval from the profiler's start to its end.
    pub lifetime:         Interval,
    /// Intervals in which the profiler was not paused.
    pub intervals_active: Vec<Interval>,
    /// Identifies the profiler's source and scope to the user.
    pub label:            Label,
    /// Profilers started by this profiler.
    pub children:         Vec<Self>,
    /// Metadata attached to this profiler.
    pub metadata:         Vec<Metadata<M>>,
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
    label:    Label,
    lifetime: Interval,
    starts:   Vec<Mark>,
    ends:     Vec<Mark>,
    state:    State,
    metadata: Vec<Metadata<M>>,
}

impl<M> MeasurementBuilder<M> {
    fn build(self) -> Measurement<M> {
        let MeasurementBuilder { label, lifetime, starts, ends, metadata, state: _ } = self;
        let mut ends = ends.into_iter().fuse();
        let intervals_active =
            starts.into_iter().map(|start| Interval { start, end: ends.next() }).collect();
        let children = Vec::new();
        Measurement { lifetime, intervals_active, label, children, metadata }
    }
}


// === State ===

/// Used to validate state transitions.
#[derive(Debug)]
enum State {
    Active(id::Runtime),
    Paused(id::Runtime),
    Ended(id::Runtime),
}



// =================
// === DataError ===
// =================

/// A problem with the input data.
#[derive(Debug)]
enum DataError {
    UnexpectedState(State),
    IdNotFound,
    RuntimeIdExpected(profiler::id::Event),
    UnexpectedToken(Expected),
    ActiveProfilerRequired,
    #[allow(dead_code)] // fields displayed by Debug
    WrongProfiler {
        found:    id::Runtime,
        expected: id::Runtime,
    },
    ExpectedEmptyStack(Vec<id::Runtime>),
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
    active:   Vec<id::Runtime>,
    /// Accumulated data pertaining to each profiler.
    builders: collections::HashMap<id::Runtime, MeasurementBuilder<M>>,
    /// Ids and parents, in same order as event log.
    order:    Vec<(id::Runtime, id::Explicit)>,
}

// This can't be derived without requiring M: Default, which is not otherwise needed.
// See: https://github.com/rust-lang/rust/issues/26925
impl<M> Default for LogVisitor<M> {
    fn default() -> Self {
        Self {
            active:   Default::default(),
            builders: Default::default(),
            order:    Default::default(),
        }
    }
}

impl<M> LogVisitor<M> {
    /// Convert the log into data about each measurement.
    fn visit(
        events: impl IntoIterator<Item = profiler::Event<M, OwnedLabel>>,
    ) -> Result<Self, Error> {
        let mut visitor = Self::default();
        for (id, event) in events.into_iter().enumerate() {
            let log_pos = id::Runtime(id as u32);
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
            result.map_err(|error| Error { log_pos, error })?;
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
        let mut builder = MeasurementBuilder {
            label: event.label.to_string().parse()?,
            lifetime: Interval { start, end: None },
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
        measurement.lifetime.end = Some(mark);
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
        let current_profiler = self.active.last().ok_or(DataError::ActiveProfilerRequired)?;
        let builder = self.builders.get_mut(current_profiler).unwrap();
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
                self.builders.get(&pos).ok_or(DataError::IdNotFound)?.lifetime.start,
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
        let log = profiler::take_log::<profiler::OpaqueMetadata>();
        let roots = super::interpret(log).unwrap();
        assert_eq!(roots.len(), 1);
        assert!(roots[0].lifetime.closed());
        assert_eq!(roots[0].intervals_active.len(), 1);
        for interval in roots[0].intervals_active.iter() {
            assert!(interval.closed());
        }
        assert!(roots[0].lifetime.closed());
        assert_eq!(roots[0].label.name, "parent");
        assert_eq!(roots[0].children.len(), 1);
        let child = &roots[0].children[0];
        assert!(child.lifetime.closed());
        assert_eq!(child.intervals_active.len(), 1);
        for interval in child.intervals_active.iter() {
            assert!(interval.closed());
        }
        assert!(child.lifetime.closed());
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
        let log = profiler::take_log::<profiler::OpaqueMetadata>();
        let roots = super::interpret(log).unwrap();
        assert_eq!(roots.len(), 1);
        assert!(roots[0].lifetime.closed());
        assert_eq!(roots[0].intervals_active.len(), 2);
        for interval in roots[0].intervals_active.iter() {
            assert!(interval.closed());
        }
        assert!(roots[0].lifetime.closed());
        assert_eq!(roots[0].label.name, "parent");
        assert_eq!(roots[0].children.len(), 1);
        let child = &roots[0].children[0];
        assert!(child.lifetime.closed());
        assert_eq!(child.intervals_active.len(), 2);
        for interval in child.intervals_active.iter() {
            assert!(interval.closed());
        }
        assert!(child.lifetime.closed());
        assert_eq!(child.label.name, "child");
        assert_eq!(child.children.len(), 0);
    }
}
