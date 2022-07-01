//! Parsing implementation. `pub` contents are low-level error details.

use enso_profiler as profiler;
use profiler::format;
use std::collections;
use std::error;
use std::fmt;
use std::mem;
use std::rc::Rc;
use std::str;



// ===========================
// === Parse and interpret ===
// ===========================

impl<M: serde::de::DeserializeOwned> str::FromStr for crate::Profile<M> {
    type Err = crate::Error<M>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let events: Result<Vec<format::Event>, _> = serde_json::from_str(s);
        let events = events.map_err(crate::Error::FormatError)?;
        let Interpreted { profile, metadata_errors } =
            interpret(events).map_err(crate::Error::DataError)?;
        if metadata_errors.is_empty() {
            Ok(profile)
        } else {
            let errors = metadata_errors;
            Err(crate::Error::RecoverableFormatError { errors, with_missing_data: profile })
        }
    }
}


// === interpret ===

/// Process a log of events, producing a hierarchy of measurements and a hierarchy of active
/// intervals.
///
/// Returns an error if the log cannot be interpreted.
pub(crate) fn interpret<'a, M: serde::de::DeserializeOwned>(
    events: impl IntoIterator<Item = format::Event<'a>>,
) -> Result<Interpreted<M>, crate::EventError<DataError>> {
    // Process log into data about each measurement, and data about relationships.
    let LogVisitor {
        builders,
        order,
        intervals,
        metadata_errors,
        headers,
        root_intervals,
        root_metadata,
        ..
    } = LogVisitor::visit(events)?;
    // Build measurements from accumulated measurement data.
    let extra_measurements = 1; // Root measurement.
    let mut measurements = Vec::with_capacity(builders.len() + extra_measurements);
    let mut builders: Vec<_> = builders.into_iter().collect();
    builders.sort_unstable_by_key(|(k, _)| *k);
    measurements.extend(builders.into_iter().map(|(_, b)| b.into()));
    let mut root = crate::Measurement {
        label:     Rc::new(crate::Label { name: "APP_LIFETIME (?:?)".into(), pos: None }),
        children:  Default::default(),
        intervals: Default::default(),
        finished:  Default::default(),
        created:   crate::Timestamp::time_origin(),
    };
    let root_measurement_id = crate::MeasurementId(measurements.len());
    for (child, (log_pos, parent)) in order.into_iter().enumerate() {
        let log_pos = log_pos.0;
        let parent = match parent {
            format::ParentId::Measurement(id) => measurements
                .get_mut(id.0)
                .ok_or(DataError::MeasurementNotFound(id))
                .map_err(|e| crate::EventError { log_pos, error: e })?,
            format::ParentId::Root => &mut root,
        };
        parent.children.push(crate::MeasurementId(child));
    }
    measurements.push(root);
    let extra_intervals = 1; // Root interval.
    let mut intervals_ = Vec::with_capacity(intervals.len() + extra_intervals);
    for builder in intervals.into_iter() {
        let IntervalBuilder { measurement, interval, children, metadata } = builder;
        let id = crate::IntervalId(intervals_.len());
        let format::MeasurementId(measurement) = measurement;
        let measurement = crate::MeasurementId(measurement);
        intervals_.push(crate::ActiveInterval { measurement, interval, children, metadata });
        measurements[measurement.0].intervals.push(id);
    }
    let root = crate::ActiveInterval {
        measurement: root_measurement_id,
        interval:    crate::Interval {
            start: crate::Timestamp::time_origin(),
            end:   Default::default(),
        },
        children:    root_intervals,
        metadata:    root_metadata,
    };
    intervals_.push(root);
    let profile = crate::Profile { measurements, intervals: intervals_, headers };
    Ok(Interpreted { profile, metadata_errors })
}

/// Result of a successful [`interpret()`].
pub(crate) struct Interpreted<M> {
    profile:         crate::Profile<M>,
    metadata_errors: Vec<MetadataError>,
}



// =================
// === DataError ===
// =================

/// A problem with the input data.
#[derive(Debug)]
pub enum DataError {
    /// A profiler was in the wrong state for a certain event to occur.
    UnexpectedState(State),
    /// An ID referred to an undefined measurement.
    MeasurementNotFound(format::MeasurementId),
    /// A parse error.
    UnexpectedToken(Expected),
    /// An event that should only occur during the lifetime of a profiler didn't find any profiler.
    ActiveProfilerRequired,
    /// An event expected to refer to a certain measurement referred to a different measurement.
    /// This can occur for events that include a measurement ID as a consistency check, but only
    /// have one valid referent (e.g. [`profiler::Event::End`] must end the current measurement.)
    WrongProfiler {
        /// The measurement that was referred to.
        found:    format::MeasurementId,
        /// The only valid measurement for the event to refer to.
        expected: format::MeasurementId,
    },
    /// Profiler(s) were active at a time none were expected.
    ExpectedEmptyStack(Vec<format::MeasurementId>),
    /// A profiler was expected to have started before a related event occurred.
    ExpectedStarted,
    /// A label ID referred beyond the end of the label table (at the time is was used).
    ///
    /// This could only occur due to a logic error in the application that wrote the profile.
    UndefinedLabel(usize),
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
    label:    Rc<crate::Label>,
    created:  crate::Timestamp,
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
    Active(EventId),
    /// Paused. Id of Pause or StartPaused event is included.
    Paused(EventId),
    /// Ended. Id of End event is included.
    Ended(EventId),
}

/// An index into the event log. Mainly used for error reporting.
#[derive(Debug, Copy, Clone)]
pub struct EventId(usize);

impl From<EventId> for crate::Seq {
    fn from(id: EventId) -> Self {
        crate::Seq::runtime_event(id.0)
    }
}



// =======================
// === IntervalBuilder ===
// =======================

/// Holds information gathered during visitation.
struct IntervalBuilder<M> {
    // The interval itself.
    measurement: format::MeasurementId,
    interval:    crate::Interval,
    // Data attached to interval.
    children:    Vec<crate::IntervalId>,
    metadata:    Vec<crate::Timestamped<M>>,
}



// ==================
// === LogVisitor ===
// ==================

/// Gathers data while visiting a series of [`format::Event`]s.
#[derive(derivative::Derivative)]
#[derivative(Default(bound = ""))]
struct LogVisitor<M> {
    /// Accumulated data pertaining to each profiler.
    builders:        collections::HashMap<format::MeasurementId, MeasurementBuilder>,
    /// Ids and parents, in same order as event log.
    order:           Vec<(EventId, format::ParentId)>,
    /// Intervals ended, in arbitrary order.
    intervals:       Vec<IntervalBuilder<M>>,
    /// Intervals currently open, as a LIFO stack.
    active:          Vec<IntervalBuilder<M>>,
    /// Top-level intervals.
    root_intervals:  Vec<crate::IntervalId>,
    /// Top-level metadata.
    root_metadata:   Vec<crate::Timestamped<M>>,
    /// Errors for metadata objects that could not be deserialized as type [`M`].
    metadata_errors: Vec<MetadataError>,
    /// References to the locations in code that measurements measure.
    profilers:       Vec<Rc<crate::Label>>,
    /// Properties of the whole profile.
    headers:         crate::Headers,
}
type MetadataError = crate::EventError<serde_json::Error>;

impl<M: serde::de::DeserializeOwned> LogVisitor<M> {
    /// Convert the log into data about each measurement.
    fn visit<'a>(
        events: impl IntoIterator<Item = format::Event<'a>>,
    ) -> Result<Self, crate::EventError<DataError>> {
        let mut visitor = Self::default();
        let mut event_count = 0;
        for (i, event) in events.into_iter().enumerate() {
            let log_pos = EventId(i);
            let result = match event {
                format::Event::Start { id, timestamp } =>
                    visitor.visit_resume(log_pos, id, timestamp),
                format::Event::Create(event) => visitor.visit_create(log_pos, event),
                format::Event::End { id, timestamp } => visitor.visit_end(log_pos, id, timestamp),
                format::Event::Pause { id, timestamp } =>
                    visitor.visit_pause(log_pos, id, timestamp),
                format::Event::Metadata(metadata) => visitor.visit_metadata(log_pos, metadata),
                format::Event::Label { label } => visitor.visit_label(log_pos, label.as_ref()),
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

impl<M: serde::de::DeserializeOwned> LogVisitor<M> {
    fn visit_create(&mut self, pos: EventId, event: format::Start) -> Result<(), DataError> {
        let parent = match event.parent {
            format::Parent::Explicit(parent) => parent,
            format::Parent::Implicit => self.current_profiler(),
        };
        let start = match event.start {
            Some(time) => crate::Timestamp { seq: pos.into(), time },
            None => self.inherit_start(parent)?,
        };
        let label = event.label.id();
        let label = self.profilers.get(label).ok_or(DataError::UndefinedLabel(label))?.clone();
        let builder = MeasurementBuilder {
            label,
            created: start,
            state: State::Paused(pos),
            finished: Default::default(),
        };
        self.order.push((pos, parent));
        let id = format::MeasurementId(self.builders.len());
        let old = self.builders.insert(id, builder);
        assert!(old.is_none());
        Ok(())
    }

    fn visit_end(
        &mut self,
        pos: EventId,
        id: format::MeasurementId,
        time: format::Timestamp,
    ) -> Result<(), DataError> {
        let measurement = self.measurement_mut(id)?;
        measurement.finished = true;
        let end = crate::Timestamp { seq: pos.into(), time };
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
        pos: EventId,
        id: format::MeasurementId,
        time: format::Timestamp,
    ) -> Result<(), DataError> {
        let time = crate::Timestamp { seq: pos.into(), time };
        self.end_interval(id, time)?;
        match mem::replace(&mut self.measurement_mut(id)?.state, State::Paused(pos)) {
            State::Active(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_resume(
        &mut self,
        pos: EventId,
        id: format::MeasurementId,
        time: format::Timestamp,
    ) -> Result<(), DataError> {
        let time = crate::Timestamp { seq: pos.into(), time };
        self.start_interval(id, time);
        match mem::replace(&mut self.measurement_mut(id)?.state, State::Active(pos)) {
            State::Paused(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_metadata(
        &mut self,
        pos: EventId,
        metadata: format::Timestamped<format::AnyMetadata>,
    ) -> Result<(), DataError> {
        let format::Timestamped { time, data } = metadata;
        let time = crate::Timestamp { seq: pos.into(), time };
        if let Ok(data) = serde_json::from_str(data.get()) {
            match data {
                format::Header::Process(process) => self.headers.process = Some(process),
                format::Header::TimeOffset(offset) => self.headers.time_offset = Some(offset),
            }
        } else {
            match serde_json::from_str(data.get()) {
                Ok(data) => {
                    let container = match self.active.last_mut() {
                        Some(parent) => &mut parent.metadata,
                        None => &mut self.root_metadata,
                    };
                    container.push(crate::Timestamped { time, data });
                }
                Err(error) => {
                    let log_pos = pos.0;
                    self.metadata_errors.push(MetadataError { log_pos, error })
                }
            }
        }
        Ok(())
    }

    fn visit_label(&mut self, _pos: EventId, label: &'_ str) -> Result<(), DataError> {
        let label = label.parse()?;
        self.profilers.push(Rc::new(label));
        Ok(())
    }
}


// === Visitation helpers ===

impl<M> LogVisitor<M> {
    fn start_interval(&mut self, measurement: format::MeasurementId, start: crate::Timestamp) {
        let end = Default::default();
        let children = Default::default();
        let metadata = Default::default();
        let interval = crate::Interval { start, end };
        self.active.push(IntervalBuilder { measurement, interval, children, metadata });
    }

    fn end_interval(
        &mut self,
        id: format::MeasurementId,
        end: crate::Timestamp,
    ) -> Result<(), DataError> {
        let mut builder = self.active.pop().ok_or(DataError::ActiveProfilerRequired)?;
        builder.interval.end = Some(end);
        let expected = builder.measurement;
        if id != expected {
            let found = id;
            return Err(DataError::WrongProfiler { found, expected });
        }
        let id = crate::IntervalId(self.intervals.len());
        self.intervals.push(builder);
        let container = match self.active.last_mut() {
            Some(parent) => &mut parent.children,
            None => &mut self.root_intervals,
        };
        container.push(id);
        Ok(())
    }

    fn current_profiler(&self) -> format::ParentId {
        match self.active.last() {
            Some(interval) => format::ParentId::Measurement(interval.measurement),
            None => format::ParentId::Root,
        }
    }

    fn inherit_start(&self, parent: format::ParentId) -> Result<crate::Timestamp, DataError> {
        Ok(match parent {
            format::ParentId::Root => crate::Timestamp::time_origin(),
            format::ParentId::Measurement(pos) => self.measurement(pos)?.created,
        })
    }

    fn measurement(&self, id: format::MeasurementId) -> Result<&MeasurementBuilder, DataError> {
        self.builders.get(&id).ok_or(DataError::MeasurementNotFound(id))
    }

    fn measurement_mut(
        &mut self,
        id: format::MeasurementId,
    ) -> Result<&mut MeasurementBuilder, DataError> {
        self.builders.get_mut(&id).ok_or(DataError::MeasurementNotFound(id))
    }
}



// ======================
// === String parsing ===
// ======================

impl str::FromStr for crate::Label {
    type Err = Expected;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.rsplit_once(' ') {
            Some((name, pos)) => match crate::CodePos::parse(pos) {
                Ok(pos) => Self { name: name.to_owned(), pos },
                Err(_) => Self { name: s.to_owned(), pos: None },
            },
            None => Self { name: s.to_owned(), pos: None },
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
