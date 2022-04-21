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
    let LogVisitor { builders, order, intervals, metadata_errors, .. } = LogVisitor::visit(events)?;
    // Build measurements from accumulated measurement data.
    let mut measurements = Vec::with_capacity(builders.len());
    let mut builders: Vec<_> = builders.into_iter().collect();
    builders.sort_unstable_by_key(|(k, _)| *k);
    measurements.extend(builders.into_iter().map(|(_, b)| b.into()));
    let root = crate::Measurement {
        label:     Rc::new(crate::Label { name: "APP_LIFETIME (?:?)".into(), pos: None }),
        children:  Default::default(),
        intervals: Default::default(),
        finished:  Default::default(),
        created:   crate::Mark::time_origin(),
    };
    let root_measurement_id = crate::MeasurementId(measurements.len());
    measurements.push(root);
    for (child, (log_pos, parent)) in order.into_iter().enumerate() {
        let log_pos = log_pos.0;
        let parent = match parent {
            format::ParentId::Measurement(id) => crate::MeasurementId(id.0),
            format::ParentId::Root => root_measurement_id,
        };
        let parent = &mut measurements
            .get_mut(parent.0)
            .ok_or(DataError::IdNotFound)
            .map_err(|e| crate::EventError { log_pos, error: e })?;
        let child = crate::MeasurementId(child);
        parent.children.push(child);
    }
    let mut intervals_ = Vec::with_capacity(intervals.len());
    for builder in intervals.into_iter() {
        let IntervalBuilder { measurement, interval, children, metadata } = builder;
        let id = crate::IntervalId(intervals_.len());
        let measurement = match measurement {
            format::ParentId::Measurement(id) => crate::MeasurementId(id.0),
            format::ParentId::Root => root_measurement_id,
        };
        intervals_.push(crate::ActiveInterval { measurement, interval, children, metadata });
        measurements[measurement.0].intervals.push(id);
    }
    // TODO[kw]: Add headers to format and read this from the file.
    let time_offset = None;
    let profile = crate::Profile { measurements, intervals: intervals_, time_offset };
    Ok(Interpreted { profile, metadata_errors })
}
pub(crate) struct Interpreted<M> {
    profile: crate::Profile<M>,
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
    /// A reference was not able to be resolved.
    IdNotFound,
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
        expected: format::ParentId,
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
    Active(EventId),
    /// Paused. Id of Pause or StartPaused event is included.
    Paused(EventId),
    /// Ended. Id of End event is included.
    Ended(EventId),
}

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
    measurement: format::ParentId,
    interval:    crate::Interval,
    children:    Vec<crate::IntervalId>,
    metadata:    Vec<crate::Metadata<M>>,
}



// ==================
// === LogVisitor ===
// ==================

/// Gathers data while visiting a series of [`format::Event`]s.
struct LogVisitor<M> {
    /// Accumulated data pertaining to each profiler.
    builders:  collections::HashMap<format::MeasurementId, MeasurementBuilder>,
    /// Ids and parents, in same order as event log.
    order:     Vec<(EventId, format::ParentId)>,
    /// Intervals ended, in arbitrary order.
    intervals: Vec<IntervalBuilder<M>>,
    /// Intervals currently open, as a LIFO stack.
    active:    Vec<IntervalBuilder<M>>,
    metadata_errors: Vec<MetadataError>,
    profilers:   Vec<Rc<crate::Label>>,
}
type MetadataError = crate::EventError<serde_json::Error>;

impl<M> Default for LogVisitor<M> {
    fn default() -> Self {
        let root_interval = IntervalBuilder {
            measurement: format::ParentId::Root,
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
            metadata_errors: Default::default(),
            profilers:     Default::default(),
        }
    }
}

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
                    visitor.visit_start(log_pos, id.into(), timestamp),
                format::Event::Create(event) => visitor.visit_create(log_pos, event),
                format::Event::End { id, timestamp } =>
                    visitor.visit_end(log_pos, id.into(), timestamp),
                format::Event::Pause { id, timestamp } =>
                    visitor.visit_pause(log_pos, id.into(), timestamp),
                format::Event::Resume { id, timestamp } =>
                    visitor.visit_resume(log_pos, id.into(), timestamp),
                format::Event::Metadata(metadata) =>
                    visitor.visit_metadata(log_pos, metadata),
                format::Event::Label(label) => visitor.visit_label(log_pos, label),
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
    fn visit_create(
        &mut self,
        pos: EventId,
        event: format::Start,
    ) -> Result<(), DataError> {
        let parent = match event.parent {
            format::Parent::Explicit(parent) => parent,
            format::Parent::Implicit => self.current_profiler(),
        };
        let start = match event.start {
            Some(time) => crate::Mark { seq: pos.into(), time },
            None => self.inherit_start(parent)?,
        };
        let label = event.label.id();
        let label = self.profilers.get(label).ok_or(DataError::UndefinedLabel(label))?.clone();
        let builder = MeasurementBuilder {
            label,
            created:  start,
            state:    State::Paused(pos),
            finished: Default::default(),
        };
        self.order.push((pos, parent));
        let id = format::MeasurementId(self.builders.len());
        let old = self.builders.insert(id, builder);
        assert!(old.is_none());
        Ok(())
    }

    fn visit_start(
        &mut self,
        pos: EventId,
        id: format::MeasurementId,
        time: format::Timestamp,
    ) -> Result<(), DataError> {
        let mark = crate::Mark { seq: pos.into(), time };
        self.start_interval(id, mark);
        let measurement = &mut self.builders.get_mut(&id).unwrap();
        match mem::replace(&mut measurement.state, State::Active(pos)) {
            State::Paused(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_end(
        &mut self,
        pos: EventId,
        id: format::MeasurementId,
        time: format::Timestamp,
    ) -> Result<(), DataError> {
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
        pos: EventId,
        id: format::MeasurementId,
        time: format::Timestamp,
    ) -> Result<(), DataError> {
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
        pos: EventId,
        id: format::MeasurementId,
        time: format::Timestamp,
    ) -> Result<(), DataError> {
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
        pos: EventId,
        metadata: format::Timestamped<format::AnyMetadata>,
    ) -> Result<(), DataError> {
        let format::Timestamped { time, data } = metadata;
        let mark = crate::Mark { seq: pos.into(), time };
        match serde_json::from_str(data.get()) {
            Ok(data) =>
                self.active.last_mut().unwrap().metadata.push(crate::Metadata { mark, data }),
            Err(error) => {
                let log_pos = pos.0;
                self.metadata_errors.push(MetadataError { log_pos, error })
            },
        }
        Ok(())
    }

    fn visit_label(
        &mut self,
        _pos: EventId,
        label: &'_ str,
    ) -> Result<(), DataError> {
        let label = label.parse()?;
        self.profilers.push(Rc::new(label));
        Ok(())
    }
}


// === Visitation helpers ===

impl<M> LogVisitor<M> {
    fn start_interval(&mut self, measurement: format::MeasurementId, start: crate::Mark) {
        let measurement = format::ParentId::Measurement(measurement);
        let end = Default::default();
        let children = Default::default();
        let metadata = Default::default();
        let interval = crate::Interval { start, end };
        self.active.push(IntervalBuilder { measurement, interval, children, metadata });
    }

    fn end_interval(&mut self, id: format::MeasurementId, end: crate::Mark) -> Result<(), DataError> {
        let mut builder = self.active.pop().unwrap();
        builder.interval.end = Some(end);
        let expected = builder.measurement;
        if format::ParentId::Measurement(id) != expected {
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

    fn current_profiler(&self) -> format::ParentId {
        // We can unwrap here because there is always an active interval during visiting;
        // the root interval doesn't end until visiting is finished.
        self.active.last().unwrap().measurement
    }

    fn inherit_start(&self, parent: format::ParentId) -> Result<crate::Mark, DataError> {
        Ok(match parent {
            format::ParentId::Root => crate::Mark::time_origin(),
            format::ParentId::Measurement(pos) =>
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