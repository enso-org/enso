//! Parsing implementation. `pub` contents are low-level error details.

use enso_profiler as profiler;
use std::collections;
use std::error;
use std::fmt;
use std::mem;
use std::str;



// ===========================
// === parse and interpret ===
// ===========================

impl<M: serde::de::DeserializeOwned> str::FromStr for crate::Profile<M> {
    type Err = crate::Error<M>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Parse { events, errors } = parse(s).map_err(crate::Error::FormatError)?;
        let result = interpret(events);
        if errors.is_empty() {
            result.map_err(|e| crate::Error::DataError(e))
        } else {
            Err(crate::Error::RecoverableFormatError { errors, with_missing_data: result })
        }
    }
}


// === parse ===

pub(crate) struct Parse<M> {
    pub events: Vec<profiler::internal::Event<M, OwnedLabel>>,
    pub errors: Vec<crate::EventError<serde_json::Error>>,
}

/// Deserialize a log of events.
///
/// For each entry in the log, produces either a deserialized Event or an error.
pub(crate) fn parse<M>(s: &str) -> Result<Parse<M>, serde_json::Error>
where M: serde::de::DeserializeOwned {
    // First just decode the array structure, so we can skip any metadata events that fail.
    let log: Vec<&serde_json::value::RawValue> = serde_json::from_str(s)?;
    let mut errors = Vec::new();
    let mut events = Vec::with_capacity(log.len());
    for (i, entry) in log.into_iter().enumerate() {
        match serde_json::from_str::<profiler::internal::Event<M, String>>(entry.get()) {
            Ok(event) => events.push(event),
            Err(error) => errors.push(crate::EventError { log_pos: i, error }),
        }
    }
    Ok(Parse { events, errors })
}


// === interpret ===

/// Process a log of events, producing a hierarchy of measurements and a hierarchy of active
/// intervals.
///
/// Returns an error if the log cannot be interpreted.
pub(crate) fn interpret<M>(
    events: impl IntoIterator<Item = profiler::internal::Event<M, OwnedLabel>>,
) -> Result<crate::Profile<M>, crate::EventError<DataError>> {
    // Process log into data about each measurement, and data about relationships.
    let LogVisitor { builders, order, intervals, .. } = LogVisitor::visit(events)?;
    // Build measurements from accumulated measurement data.
    let mut measurement_ids = std::collections::HashMap::with_capacity(builders.len());
    let mut measurements = Vec::with_capacity(builders.len());
    for (event_id, builder) in builders.into_iter() {
        let measurement_id = crate::MeasurementId(measurements.len());
        measurements.push(builder.into());
        measurement_ids.insert(id::Explicit::Runtime(event_id), measurement_id);
    }
    let root = crate::Measurement {
        label:     crate::Label { name: "APP_LIFETIME (?:?)".into(), pos: None },
        children:  Default::default(),
        intervals: Default::default(),
        finished:  Default::default(),
        created:   crate::Mark::time_origin(),
    };
    let root_measurement_id = crate::MeasurementId(measurements.len());
    measurements.push(root);
    measurement_ids.insert(id::Explicit::AppLifetime, root_measurement_id);
    for (child, parent) in order.into_iter() {
        let child = measurement_ids.get(&id::Explicit::Runtime(child)).unwrap();
        let parent = measurement_ids
            .get(&parent)
            .ok_or(DataError::IdNotFound)
            .map_err(|e| crate::EventError { log_pos: child.0, error: e })?;
        measurements[parent.0].children.push(*child);
    }
    let mut intervals_ = Vec::with_capacity(intervals.len());
    for builder in intervals.into_iter() {
        let IntervalBuilder { measurement, interval, children, metadata } = builder;
        let measurement = *measurement_ids.get(&measurement).unwrap();
        let id = crate::IntervalId(intervals_.len());
        intervals_.push(crate::ActiveInterval { measurement, interval, children, metadata });
        measurements[measurement.0].intervals.push(id);
    }
    Ok(crate::Profile { measurements, intervals: intervals_ })
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
    RuntimeIdExpected(id::Event),
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
pub struct Expected(pub(crate) &'static str);

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl error::Error for Expected {}



// ==========================
// === MeasurementBuilder ===
// ==========================

/// Used while gathering information about a profiler.
struct MeasurementBuilder {
    label:    crate::Label,
    created:  crate::Mark,
    state:    State,
    finished: bool,
}

impl From<MeasurementBuilder> for crate::Measurement {
    fn from(builder: MeasurementBuilder) -> Self {
        let MeasurementBuilder { label, created, finished, state: _ } = builder;
        let children = Vec::new();
        let intervals = Vec::new();
        Self { label, children, created, intervals, finished }
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

impl State {
    fn start(start_state: profiler::internal::StartState, pos: id::Runtime) -> Self {
        match start_state {
            profiler::internal::StartState::Active => Self::Active(pos),
            profiler::internal::StartState::Paused => Self::Paused(pos),
        }
    }
}



// =======================
// === IntervalBuilder ===
// =======================

/// Holds information gathered during visitation.
struct IntervalBuilder<M> {
    measurement: id::Explicit,
    interval:    crate::Interval,
    children:    Vec<crate::IntervalId>,
    metadata:    Vec<crate::Metadata<M>>,
}



// ==================
// === LogVisitor ===
// ==================

/// Gathers data while visiting a series of [`profiler::internal::Event`]s.
struct LogVisitor<M> {
    /// Accumulated data pertaining to each profiler.
    builders:  collections::HashMap<id::Runtime, MeasurementBuilder>,
    /// Ids and parents, in same order as event log.
    order:     Vec<(id::Runtime, id::Explicit)>,
    /// Intervals ended, in arbitrary order.
    intervals: Vec<IntervalBuilder<M>>,
    /// Intervals currently open, as a LIFO stack.
    active:    Vec<IntervalBuilder<M>>,
}

impl<M> Default for LogVisitor<M> {
    fn default() -> Self {
        let root_interval = IntervalBuilder {
            measurement: id::Explicit::AppLifetime,
            interval:    crate::Interval {
                start: crate::Mark::time_origin(),
                end:   Default::default(),
            },
            children:    Default::default(),
            metadata:    Default::default(),
        };
        Self {
            intervals: Default::default(),
            active:    vec![root_interval],
            builders:  Default::default(),
            order:     Default::default(),
        }
    }
}

impl<M> LogVisitor<M> {
    /// Convert the log into data about each measurement.
    fn visit(
        events: impl IntoIterator<Item = profiler::internal::Event<M, OwnedLabel>>,
    ) -> Result<Self, crate::EventError<DataError>> {
        let mut visitor = Self::default();
        let mut event_count = 0;
        for (i, event) in events.into_iter().enumerate() {
            let log_pos = id::Runtime(i as u32);
            let result = match event {
                profiler::internal::Event::Start(event) =>
                    visitor.visit_start(log_pos, event, profiler::internal::StartState::Active),
                profiler::internal::Event::StartPaused(event) =>
                    visitor.visit_start(log_pos, event, profiler::internal::StartState::Paused),
                profiler::internal::Event::End { id, timestamp } =>
                    visitor.visit_end(log_pos, id.into(), timestamp),
                profiler::internal::Event::Pause { id, timestamp } =>
                    visitor.visit_pause(log_pos, id.into(), timestamp),
                profiler::internal::Event::Resume { id, timestamp } =>
                    visitor.visit_resume(log_pos, id.into(), timestamp),
                profiler::internal::Event::Metadata(metadata) =>
                    visitor.visit_metadata(log_pos, metadata),
            };
            result.map_err(|error| crate::EventError { log_pos: i, error })?;
            event_count += 1;
        }
        visitor.finish().map_err(|error| crate::EventError { log_pos: event_count, error })?;
        Ok(visitor)
    }

    /// Perform any finalization, e.g. ending intervals implicitly if their ends weren't logged.
    fn finish(&mut self) -> Result<(), DataError> {
        // Build any ActiveIntervals that didn't have ends logged. This will always include at
        // least the root interval.
        while let Some(builder) = self.active.pop() {
            let id = crate::IntervalId(self.intervals.len());
            self.intervals.push(builder);
            // Only the root interval has no parent; the root interval is found in the last
            // position in the intervals vec.
            if let Some(parent) = self.active.last_mut() {
                parent.children.push(id);
            }
        }
        Ok(())
    }
}


// === Handlers for each event ===

impl<M> LogVisitor<M> {
    fn visit_start(
        &mut self,
        pos: id::Runtime,
        event: profiler::internal::Start<OwnedLabel>,
        start_state: profiler::internal::StartState,
    ) -> Result<(), DataError> {
        let parent = match event.parent.into() {
            id::Event::Explicit(parent) => parent,
            id::Event::Implicit => self.current_profiler(),
        };
        let start = match event.start {
            Some(time) => crate::Mark { seq: pos.into(), time },
            None => self.inherit_start(parent)?,
        };
        let builder = MeasurementBuilder {
            label:    event.label.to_string().parse()?,
            created:  start,
            state:    State::start(start_state, pos),
            finished: Default::default(),
        };
        if start_state == profiler::internal::StartState::Active {
            self.start_interval(pos, start);
        }
        self.order.push((pos, parent));
        let old = self.builders.insert(pos, builder);
        assert!(old.is_none());
        Ok(())
    }

    fn visit_end(
        &mut self,
        pos: id::Runtime,
        id: id::Event,
        time: profiler::internal::Timestamp,
    ) -> Result<(), DataError> {
        let id = id.try_into()?;
        let measurement = &mut self.builders.get_mut(&id).unwrap();
        measurement.finished = true;
        let end = crate::Mark { seq: pos.into(), time };
        match mem::replace(&mut measurement.state, State::Ended(pos)) {
            // Typical case: The current profiler ends.
            State::Active(_) => self.end_interval(id, end)?,
            // Edge case: A profiler can be dropped without ever being started if an async block
            // is created, but dropped without ever being awaited.
            State::Paused(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_pause(
        &mut self,
        pos: id::Runtime,
        id: id::Event,
        time: profiler::internal::Timestamp,
    ) -> Result<(), DataError> {
        let id = id.try_into()?;
        let mark = crate::Mark { seq: pos.into(), time };
        self.end_interval(id, mark)?;
        let measurement = &mut self.builders.get_mut(&id).unwrap();
        match mem::replace(&mut measurement.state, State::Paused(pos)) {
            State::Active(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_resume(
        &mut self,
        pos: id::Runtime,
        id: id::Event,
        time: profiler::internal::Timestamp,
    ) -> Result<(), DataError> {
        let id = id.try_into()?;
        let mark = crate::Mark { seq: pos.into(), time };
        self.start_interval(id, mark);
        let measurement = &mut self.builders.get_mut(&id).ok_or(DataError::IdNotFound)?;
        match mem::replace(&mut measurement.state, State::Active(pos)) {
            State::Paused(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_metadata(
        &mut self,
        pos: id::Runtime,
        metadata: profiler::internal::Timestamped<M>,
    ) -> Result<(), DataError> {
        let profiler::internal::Timestamped { timestamp, data } = metadata;
        let mark = crate::Mark { seq: pos.into(), time: timestamp };
        self.active.last_mut().unwrap().metadata.push(crate::Metadata { mark, data });
        Ok(())
    }
}


// === Helper types, functions, and methods ===

type OwnedLabel = String;

impl<M> LogVisitor<M> {
    fn start_interval(&mut self, id: id::Runtime, start: crate::Mark) {
        let measurement = id::Explicit::Runtime(id);
        let end = Default::default();
        let children = Default::default();
        let metadata = Default::default();
        let interval = crate::Interval { start, end };
        self.active.push(IntervalBuilder { measurement, interval, children, metadata });
    }

    fn end_interval(&mut self, id: id::Runtime, end: crate::Mark) -> Result<(), DataError> {
        let mut builder = self.active.pop().unwrap();
        builder.interval.end = Some(end);
        let expected = match builder.measurement {
            id::Explicit::Runtime(id) => id,
            id => return Err(DataError::RuntimeIdExpected(id::Event::Explicit(id))),
        };
        if id != expected {
            let found = id;
            return Err(DataError::WrongProfiler { found, expected });
        }
        let id = crate::IntervalId(self.intervals.len());
        self.intervals.push(builder);
        // This can't fail, because we checked above that we aren't trying to end the root interval,
        // and only the root interval occurs at top level.
        let parent = self.active.last_mut().unwrap();
        parent.children.push(id);
        Ok(())
    }

    fn current_profiler(&self) -> id::Explicit {
        // We can unwrap here because there is always an active interval during visiting;
        // the root interval doesn't end until visiting is finished.
        self.active.last().unwrap().measurement
    }

    fn inherit_start(&self, parent: id::Explicit) -> Result<crate::Mark, DataError> {
        Ok(match parent {
            id::Explicit::AppLifetime => crate::Mark::time_origin(),
            id::Explicit::Runtime(pos) =>
                self.builders.get(&pos).ok_or(DataError::IdNotFound)?.created,
        })
    }
}



// ======================
// === String parsing ===
// ======================

impl str::FromStr for crate::Label {
    type Err = Expected;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(if let Some((name, pos)) = s.rsplit_once(' ') {
            Self { name: name.to_owned(), pos: crate::CodePos::parse(pos)? }
        } else {
            Self { name: s.to_owned(), pos: None }
        })
    }
}

impl crate::CodePos {
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



// ==========
// === id ===
// ==========

/// Facilities for classifying an event ID into subtypes.
///
/// The [`profiler::internal::EventId`] type can be logically broken down into different subtypes,
/// but in the event log subtypes are not differentiated so that all EventIDs can be easily packed
/// into a small scalar. This module supports unpacking an EventID.
pub mod id {
    use enso_profiler as profiler;

    /// An reference to an event. This type classifies [`profiler::EventId`]; they have a 1:1
    /// correspondence.
    #[derive(Copy, Clone, Debug)]
    pub enum Event {
        /// An unspecified ID that must be inferred from context.
        Implicit,
        /// A specific, context-independent ID.
        Explicit(Explicit),
    }

    impl From<profiler::internal::EventId> for Event {
        fn from(id: profiler::internal::EventId) -> Self {
            if id == profiler::internal::EventId::IMPLICIT {
                Event::Implicit
            } else {
                Event::Explicit(if id == profiler::internal::EventId::APP_LIFETIME {
                    Explicit::AppLifetime
                } else {
                    Explicit::Runtime(Runtime(id.0))
                })
            }
        }
    }

    /// An explicit reference to an event.
    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub enum Explicit {
        /// The parent of the real roots.
        AppLifetime,
        /// An event logged at runtime.
        Runtime(Runtime),
    }

    /// An explicit reference to an event in the log.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub struct Runtime(pub u32);

    impl From<Runtime> for Explicit {
        fn from(id: Runtime) -> Self {
            Explicit::Runtime(id)
        }
    }

    impl From<Runtime> for crate::Seq {
        fn from(pos: Runtime) -> Self {
            Self::runtime_event(pos.0)
        }
    }

    impl TryFrom<Event> for Runtime {
        type Error = super::DataError;
        fn try_from(id: Event) -> Result<Self, Self::Error> {
            match id {
                Event::Explicit(Explicit::Runtime(id)) => Ok(id),
                id => Err(super::DataError::RuntimeIdExpected(id)),
            }
        }
    }
}
