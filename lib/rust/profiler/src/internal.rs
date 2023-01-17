//! Implementation details not used directly in normal usage of the Profiling API.
//!
//! `pub` items in this module support two uses:
//! - They support profiling crates in interpreting raw profiling data.
//! - They are used by [the macros](../index.html#macros) that provide the public interface to
//!   `profiler`.

use crate::format;
use crate::log;
use crate::ProfilingLevel;

use std::fmt;
use std::rc;



// ======================================================
// === The global logs (EVENTS and the METADATA_LOGS) ===
// ======================================================

thread_local! {
    static EVENT_LOG: log::Log<Event> = log::Log::new();
}
/// Global log of [`Events`]s.
pub(crate) static EVENTS: log::ThreadLocalLog<Event> = log::ThreadLocalLog::new(EVENT_LOG);

thread_local! {
    static METADATA_LOG_LOG: log::Log<rc::Rc<dyn MetadataSource>> = log::Log::new();
}
/// Global registry of metadata logs.
pub(crate) static METADATA_LOGS: log::ThreadLocalLog<rc::Rc<dyn MetadataSource>> =
    log::ThreadLocalLog::new(METADATA_LOG_LOG);



// =========================
// === Capturing the log ===
// =========================

/// Produce a JSON-formatted event log from the internal event logs.
pub fn get_log() -> String {
    let LogData { events, metadata_names, mut metadata_entries } = get_raw_log();
    let mut out = LogTranslator::new();
    for (id, event) in events.into_iter().enumerate() {
        let id = EventId(id as u32);
        match event {
            Event::Metadata { timestamp, data } => {
                let ExternalMetadata { type_id } = data;
                let id = type_id as usize;
                let name = metadata_names[id];
                let data = metadata_entries[id].next().unwrap();
                out.metadata(timestamp, name, data);
            }
            Event::Start(Start { parent, start, label, .. }) => {
                out.create(start, parent, label, id);
                out.start(start.unwrap(), id);
            }
            Event::StartPaused(Start { parent, start, label, .. }) =>
                out.create(start, parent, label, id),
            Event::End { id, timestamp } => out.end(timestamp, id),
            Event::Pause { id, timestamp } => out.pause(timestamp, id),
            Event::Resume { id, timestamp } => out.start(timestamp, id),
        }
    }
    out.finish()
}


// === Capture raw log data ===

/// Obtain the data from the internal event log.
pub(crate) fn get_raw_log() -> LogData {
    let events = EVENTS.clone_all();
    let metadatas: Vec<_> = METADATA_LOGS.clone_all();
    let metadata_names: Vec<_> = metadatas.iter().map(|metadata| metadata.name()).collect();
    let metadata_entries: Vec<_> =
        metadatas.into_iter().map(|metadata| metadata.get_all()).collect();
    LogData { events, metadata_names, metadata_entries }
}

/// A snapshot of the internal event log.
/// Contains all the information necessary to produce a profile.
pub(crate) struct LogData {
    pub events:       Vec<Event>,
    metadata_names:   Vec<&'static str>,
    metadata_entries: Vec<Box<dyn Iterator<Item = Box<serde_json::value::RawValue>>>>,
}



// =====================
// === LogTranslator ===
// =====================

/// Translates [`profiler::internal`] types and IDs to [`profiler::format`] equivalents.
#[derive(Debug)]
struct LogTranslator<'a> {
    profile: format::Builder<'a>,
    ids:     std::collections::HashMap<EventId, format::MeasurementId>,
}

macro_rules! translate_transition {
    ($name:ident) => {
        fn $name(&mut self, time: Timestamp, id: EventId) {
            self.profile.$name(time.into(), self.ids[&id]);
        }
    };
}

impl<'a> LogTranslator<'a> {
    fn new() -> Self {
        let mut profile = format::Builder::new();
        profile.time_offset(Timestamp::time_offset().into());
        profile.process("Ide");
        let ids = Default::default();
        Self { profile, ids }
    }

    fn finish(self) -> String {
        self.profile.build_string()
    }

    fn metadata(&mut self, time: Timestamp, name: &'static str, data: format::AnyMetadata) {
        self.profile.metadata(time.into(), name, data);
    }

    fn create(&mut self, time: Option<Timestamp>, parent: EventId, label: Label, id: EventId) {
        let parent = match parent {
            EventId::IMPLICIT => format::Parent::implicit(),
            EventId::APP_LIFETIME => format::Parent::root(),
            id => self.ids[&id].into(),
        };
        let time = time.map(|t| t.into());
        let interval = self.profile.create(time, parent, label.0);
        self.ids.insert(id, interval);
    }

    translate_transition!(start);
    translate_transition!(end);
    translate_transition!(pause);
}



// ===================
// === MetadataLog ===
// ===================

pub(crate) struct MetadataLog<T> {
    pub name:    &'static str,
    pub entries: rc::Rc<log::Log<T>>,
}

pub(crate) trait MetadataSource {
    fn name(&self) -> &'static str;
    fn get_all(&self) -> Box<dyn Iterator<Item = Box<serde_json::value::RawValue>>>;
}

impl<T: 'static + serde::Serialize> MetadataSource for MetadataLog<T> {
    fn name(&self) -> &'static str {
        self.name
    }

    fn get_all(&self) -> Box<dyn Iterator<Item = Box<serde_json::value::RawValue>>> {
        let mut entries = Vec::with_capacity(self.entries.len());
        self.entries.for_each(|x| entries.push(serde_json::value::to_raw_value(&x).unwrap()));
        Box::new(entries.into_iter())
    }
}



// ======================
// === MetadataLogger ===
// ======================

/// An object that supports writing a specific type of metadata to the profiling log.
#[derive(Debug)]
pub struct MetadataLogger<T> {
    id:      u32,
    entries: rc::Rc<log::Log<T>>,
}

impl<T: 'static + serde::Serialize> MetadataLogger<T> {
    /// Create a MetadataLogger for logging a particular type.
    ///
    /// The name given here must match the name used for deserialization.
    pub fn new(name: &'static str) -> Self {
        let id = METADATA_LOGS.len() as u32;
        let entries = rc::Rc::new(log::Log::new());
        METADATA_LOGS.push(rc::Rc::new(MetadataLog::<T> { name, entries: entries.clone() }));
        Self { id, entries }
    }

    /// Write a metadata object to the profiling event log.
    ///
    /// Returns an identifier that can be used to create references between log entries.
    pub fn log(&self, t: T) -> EventId {
        self.entries.push(t);
        EventLog.metadata(self.id)
    }
}



// ================
// === EventLog ===
// ================

/// The log of profiling events. Data is actually stored globally.
#[derive(Copy, Clone, Debug)]
pub struct EventLog;

impl EventLog {
    /// Log the beginning of a measurement.
    #[inline]
    pub fn start(
        self,
        parent: EventId,
        label: Label,
        start: Option<Timestamp>,
        state: StartState,
        level: ProfilingLevel,
    ) -> EventId {
        let m = Start { parent, label, start, level };
        let event = match state {
            StartState::Active => Event::Start(m),
            StartState::Paused => Event::StartPaused(m),
        };
        self.log_event(event)
    }

    /// Log the end of a measurement.
    #[inline]
    pub fn end(self, id: EventId, timestamp: Timestamp) {
        self.log_event(Event::End { id, timestamp });
    }

    /// Log the beginning of an interval in which the measurement is not active.
    #[inline]
    pub fn pause(self, id: EventId, timestamp: Timestamp) {
        self.log_event(Event::Pause { id, timestamp });
    }

    /// Log the end of an interval in which the measurement is not active.
    #[inline]
    pub fn resume(self, id: EventId, timestamp: Timestamp) {
        self.log_event(Event::Resume { id, timestamp });
    }

    /// Log metadata.
    #[inline]
    pub fn metadata(self, type_id: u32) -> EventId {
        let timestamp = Timestamp::now();
        let data = ExternalMetadata { type_id };
        self.log_event(Event::Metadata { timestamp, data })
    }

    #[inline(always)]
    fn log_event(self, event: Event) -> EventId {
        let id = EventId(EVENTS.len() as u32);
        EVENTS.push(event);
        id
    }
}


// === StartState ===

/// Specifies the initial state of a profiler.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum StartState {
    /// The profiler starts in the running state.
    Active,
    /// The profiler starts in the paused state.
    Paused,
}



// =============
// === Event ===
// =============

/// An entry in the profiling log.
#[derive(Debug, Clone, Copy)]
pub enum Event {
    /// The beginning of a measurement.
    Start(Start),
    /// The beginning of a measurement that starts in the paused state.
    StartPaused(Start),
    /// The end of a measurement.
    End {
        /// Identifies the measurement by the ID of its Start event.
        id:        EventId,
        /// When the event occurred.
        timestamp: Timestamp,
    },
    /// The beginning of an interruption to a measurement, e.g. an await point.
    Pause {
        /// Identifies the measurement by the ID of its Start event.
        id:        EventId,
        /// When the event occurred.
        timestamp: Timestamp,
    },
    /// The end of an interruption to an a measurement, e.g. an await point.
    Resume {
        /// Identifies the measurement by the ID of its Start event.
        id:        EventId,
        /// When the event occurred.
        timestamp: Timestamp,
    },
    /// Metadata: wrapper with dependency-injected contents.
    Metadata {
        /// Application-specific data associated with a point in time.
        data:      ExternalMetadata,
        /// When the event occurred.
        timestamp: Timestamp,
    },
}

impl Event {
    /// Return the [`Start`] information, if this is an event that defines a profiler.
    pub fn as_start(self) -> Option<Start> {
        match self {
            Event::Start(start) | Event::StartPaused(start) => Some(start),
            _ => None,
        }
    }
}



// =============
// === Start ===
// =============

/// A measurement-start entry in the profiling log.
#[derive(Debug, Clone, Copy)]
pub struct Start {
    /// Specifies parent measurement by its [`Start`].
    pub parent: EventId,
    /// Start time, or None to indicate it is the same as `parent`.
    pub start:  Option<Timestamp>,
    /// Identifies where in the code this measurement originates.
    pub label:  Label,
    /// Identifies the importance of this event.
    pub level:  ProfilingLevel,
}


// === Label ===

/// The label of a profiler; this includes the name given at its creation, along with file and
/// line-number information.
#[derive(Debug, Clone, Copy)]
pub struct Label(pub &'static str);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}



// =================
// === Timestamp ===
// =================

/// Time elapsed since the [time origin](https://www.w3.org/TR/hr-time-2/#sec-time-origin).
///
/// Stored as the raw output of performance.now() (floating-point milliseconds).
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub struct Timestamp {
    ms: f64,
}

impl Timestamp {
    #[inline(always)]
    /// Return the current time, relative to the time origin.
    pub fn now() -> Self {
        Self { ms: now() }
    }

    /// Return the timestamp corresponding to an offset from the time origin, in ms.
    #[inline(always)]
    pub fn from_ms(ms: f64) -> Self {
        Self { ms }
    }

    /// Return the timestamp of the time origin.
    #[inline(always)]
    pub fn time_origin() -> Self {
        Self { ms: 0.0 }
    }

    /// Convert to an offset from the time origin, in ms.
    #[inline(always)]
    pub fn into_ms(self) -> f64 {
        self.ms
    }

    /// Return the offset of the time origin from a system timestamp.
    #[inline(always)]
    pub fn time_offset() -> Self {
        Self::from_ms(time_origin())
    }
}

impl Default for Timestamp {
    #[inline(always)]
    fn default() -> Self {
        Self::time_origin()
    }
}

// === FFI ===

#[cfg(target_arch = "wasm32")]
fn now() -> f64 {
    use enso_web as web;
    use enso_web::traits::*;
    web::window.performance_or_panic().now()
}
#[cfg(not(target_arch = "wasm32"))]
fn now() -> f64 {
    // Monotonically-increasing timestamp, providing slightly more realistic data for tests than
    // a constant.
    thread_local! {
        static NEXT_TIMESTAMP: std::cell::Cell<f64> = Default::default();
    }
    NEXT_TIMESTAMP.with(|timestamp| {
        let now = timestamp.get();
        timestamp.set(now + 0.1);
        now
    })
}

fn time_origin() -> f64 {
    use enso_web as web;
    use enso_web::traits::*;
    web::window.performance_or_panic().time_origin()
}


// === Conversions to related types ===

impl From<Timestamp> for format::Timestamp {
    #[inline(always)]
    fn from(time: Timestamp) -> Self {
        Self::from_ms(time.into_ms())
    }
}



// ===============
// === EventId ===
// ===============

/// Identifies an event in the profiling log.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EventId(pub u32);

impl EventId {
    /// Special value indicating that an EventId is to be inferred from context.
    pub const IMPLICIT: EventId = EventId(u32::MAX);

    /// Special value indicating the root pseudo-profiler (the parent of runtime root profilers).
    pub const APP_LIFETIME: EventId = EventId(u32::MAX - 1);

    /// Special value indicating that no explicit prior event is associated.
    ///
    /// When used to identify a parent, this indicates that the parent can be inferred to be the
    /// current profiler.
    pub const fn implicit() -> Self {
        Self::IMPLICIT
    }

    /// If the value explicitly references a profiler ID, return it; otherwise, return [`None`].
    pub fn explicit(self) -> Option<Self> {
        if self == Self::IMPLICIT {
            None
        } else {
            Some(self)
        }
    }
}



// ========================
// === ExternalMetadata ===
// ========================

/// Indicates where in the event log metadata from a particular external source should be inserted.
#[derive(Debug, Copy, Clone)]
pub struct ExternalMetadata {
    type_id: u32,
}



// ================
// === Profiler ===
// ================

/// The interface supported by profilers of all profiling levels.
pub trait Profiler {
    /// Log the beginning of a measurement.
    ///
    /// Return an object that can be used to manage the measurement's lifetime.
    fn start(parent: EventId, label: Label, time: Option<Timestamp>, start: StartState) -> Self;
    /// Log the end of a measurement.
    fn finish(self);
    /// Log the beginning of an interval in which the profiler is not active.
    fn pause(self);
    /// Log the end of an interval in which the profiler is not active.
    fn resume(self);
}



// ===============
// === Started ===
// ===============

/// A profiler that has a start time set, and will complete its measurement when dropped.
#[derive(Debug)]
pub struct Started<T: Profiler + Copy>(pub T);


impl<T: Profiler + Copy> Drop for Started<T> {
    fn drop(&mut self) {
        self.0.finish();
    }
}

impl<T, U> crate::Parent<T> for Started<U>
where
    U: crate::Parent<T> + Profiler + Copy,
    T: Profiler + Copy,
{
    fn start_child(&self, label: Label) -> Started<T> {
        self.0.start_child(label)
    }
}
