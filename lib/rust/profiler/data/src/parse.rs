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

impl<M: serde::de::DeserializeOwned> str::FromStr for crate::Measurement<M> {
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

/// Process a log of events, producing a hierarchy of measurements.
///
/// Returns an error if the log cannot be interpreted.
pub(crate) fn interpret<M>(
    events: impl IntoIterator<Item = profiler::internal::Event<M, OwnedLabel>>,
) -> Result<crate::Measurement<M>, crate::EventError<DataError>> {
    // Process log into data about each measurement, and data about relationships.
    let LogVisitor { builders, order, root_builder, .. } = LogVisitor::visit(events)?;
    // Build measurements from accumulated measurement data.
    let mut measurements: collections::HashMap<_, _> =
        builders.into_iter().map(|(k, v)| (k, v.build())).collect();
    // Organize measurements into trees. Use reverse chronological order as a convenient topological
    // sort (parents are always created before children).
    let mut root = root_builder.build();
    for (id, parent) in order.into_iter().rev() {
        let child = measurements.remove(&id).unwrap();
        let parent = match parent {
            id::Explicit::AppLifetime => &mut root.children,
            id::Explicit::Runtime(pos) =>
                &mut measurements
                    .get_mut(&pos)
                    .ok_or(DataError::IdNotFound)
                    .map_err(|e| crate::EventError { log_pos: id.0 as usize, error: e })?
                    .children,
        };
        parent.push(child);
    }
    // Organize children by creation time. (Previous pass gathered them in reverse order.)
    reverse_subtrees(&mut root);
    Ok(root)
}

fn reverse_subtrees<M>(root: &mut crate::Measurement<M>) {
    root.children.reverse();
    for child in root.children.iter_mut() {
        reverse_subtrees(child);
    }
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
struct MeasurementBuilder<M> {
    label:          crate::Label,
    created:        crate::Mark,
    created_paused: bool,
    pauses:         Vec<crate::Mark>,
    resumes:        Vec<crate::Mark>,
    end:            Option<crate::Mark>,
    state:          State,
    metadata:       Vec<crate::Metadata<M>>,
}

impl<M> MeasurementBuilder<M> {
    fn build(self) -> crate::Measurement<M> {
        let MeasurementBuilder {
            label,
            pauses,
            resumes,
            metadata,
            created,
            created_paused,
            end,
            state: _,
        } = self;
        // A profiler is considered async if:
        // - It was created in a non-running state (occurs when a `#[profile] async fn` is called).
        // - It ever awaited.
        let lifetime = if created_paused || !pauses.is_empty() {
            let mut starts = resumes;
            if !created_paused {
                starts.insert(0, created);
            }
            let mut ends = pauses;
            if let Some(end) = end {
                ends.push(end);
            }
            let mut ends = ends.into_iter().fuse();
            let active: Vec<_> = starts
                .into_iter()
                .map(|start| crate::Interval { start, end: ends.next() })
                .collect();
            crate::Lifetime::Async(crate::AsyncLifetime {
                created: if created_paused { Some(created) } else { None },
                active,
                finished: end.is_some(),
            })
        } else {
            let active = crate::Interval { start: created, end };
            crate::Lifetime::NonAsync { active }
        };
        let children = Vec::new();
        crate::Measurement { lifetime, label, children, metadata }
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



// ==================
// === LogVisitor ===
// ==================

/// Gathers data while visiting a series of [`profiler::internal::Event`]s.
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
            state:          State::Active(id::Runtime(0)), // value doesn't matter
            created:        crate::Mark::time_origin(),
            created_paused: Default::default(),
            metadata:       Default::default(),
            pauses:         Default::default(),
            resumes:        Default::default(),
            end:            Default::default(),
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
        events: impl IntoIterator<Item = profiler::internal::Event<M, OwnedLabel>>,
    ) -> Result<Self, crate::EventError<DataError>> {
        let mut visitor = Self::default();
        for (i, event) in events.into_iter().enumerate() {
            let log_pos = id::Runtime(i as u32);
            let result = match event {
                profiler::internal::Event::Start(event) =>
                    visitor.visit_start(log_pos, event, profiler::internal::StartState::Active),
                profiler::internal::Event::StartPaused(event) =>
                    visitor.visit_start(log_pos, event, profiler::internal::StartState::Paused),
                profiler::internal::Event::End { id, timestamp } =>
                    visitor.visit_end(log_pos, id, timestamp),
                profiler::internal::Event::Pause { id, timestamp } =>
                    visitor.visit_pause(log_pos, id, timestamp),
                profiler::internal::Event::Resume { id, timestamp } =>
                    visitor.visit_resume(log_pos, id, timestamp),
                profiler::internal::Event::Metadata(metadata) =>
                    visitor.visit_metadata(log_pos, metadata),
            };
            result.map_err(|error| crate::EventError { log_pos: i, error })?;
        }
        Ok(visitor)
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
            label:          event.label.to_string().parse()?,
            created:        start,
            created_paused: matches!(start_state, profiler::internal::StartState::Paused),
            state:          State::start(start_state, pos),
            pauses:         Default::default(),
            resumes:        Default::default(),
            end:            Default::default(),
            metadata:       Default::default(),
        };
        if start_state == profiler::internal::StartState::Active {
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
        id: profiler::internal::EventId,
        time: profiler::internal::Timestamp,
    ) -> Result<(), DataError> {
        let current_profiler = self.active.pop().ok_or(DataError::ActiveProfilerRequired)?;
        check_profiler(id, current_profiler)?;
        let measurement = &mut self.builders.get_mut(&current_profiler).unwrap();
        measurement.end = Some(crate::Mark { seq: pos.into(), time });
        match mem::replace(&mut measurement.state, State::Ended(pos)) {
            State::Active(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_pause(
        &mut self,
        pos: id::Runtime,
        id: profiler::internal::EventId,
        time: profiler::internal::Timestamp,
    ) -> Result<(), DataError> {
        let current_profiler = self.active.pop().ok_or(DataError::ActiveProfilerRequired)?;
        check_profiler(id, current_profiler)?;
        self.check_no_async_task_active()?;
        let mark = crate::Mark { seq: pos.into(), time };
        let measurement = &mut self.builders.get_mut(&current_profiler).unwrap();
        measurement.pauses.push(mark);
        match mem::replace(&mut measurement.state, State::Paused(pos)) {
            State::Active(_) => (),
            state => return Err(DataError::UnexpectedState(state)),
        }
        Ok(())
    }

    fn visit_resume(
        &mut self,
        pos: id::Runtime,
        id: profiler::internal::EventId,
        time: profiler::internal::Timestamp,
    ) -> Result<(), DataError> {
        let start_id = match id.into() {
            id::Event::Explicit(id::Explicit::Runtime(id)) => id,
            id => return Err(DataError::RuntimeIdExpected(id)),
        };
        self.check_no_async_task_active()?;
        self.active.push(start_id);
        let mark = crate::Mark { seq: pos.into(), time };
        let measurement = &mut self.builders.get_mut(&start_id).ok_or(DataError::IdNotFound)?;
        measurement.resumes.push(mark);
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
        let builder = match self.active.last() {
            Some(profiler) => self.builders.get_mut(profiler).unwrap(),
            None => &mut self.root_builder,
        };
        let profiler::internal::Timestamped { timestamp, data } = metadata;
        let mark = crate::Mark { seq: pos.into(), time: timestamp };
        builder.metadata.push(crate::Metadata { mark, data });
        Ok(())
    }
}


// === Helper types, functions, and methods ===

type OwnedLabel = String;

fn check_profiler(
    found: profiler::internal::EventId,
    expected: id::Runtime,
) -> Result<(), DataError> {
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

    fn inherit_start(&self, parent: id::Explicit) -> Result<crate::Mark, DataError> {
        Ok(match parent {
            id::Explicit::AppLifetime => crate::Mark::time_origin(),
            id::Explicit::Runtime(pos) => {
                let measurement = self.builders.get(&pos).ok_or(DataError::IdNotFound)?;
                if measurement.created_paused {
                    *measurement.resumes.first().ok_or(DataError::ExpectedStarted)?
                } else {
                    measurement.created
                }
            }
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
        // instrumented non-async function using block_on to run an instrumented async function,
        // because support for that is unlikely to be needed.
        self.check_empty_stack()
    }
}



// ======================
// === String parsing ===
// ======================

impl str::FromStr for crate::Label {
    type Err = Expected;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (name, pos) = s.rsplit_once(' ').ok_or(Expected(" "))?;
        Ok(Self { name: name.to_owned(), pos: crate::CodePos::parse(pos)? })
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
    #[derive(Copy, Clone, Debug)]
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
}
