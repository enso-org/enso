//! Implementation details not used directly in normal usage of the Profiling API.
//!
//! `pub` items in this module support two uses:
//! - They support profiling crates in interpreting raw profiling data.
//! - They are used by [the macros](../index.html#macros) that provide the public interface to
//!   `profiler`.

use crate::format;
use crate::log;

use std::fmt;
use std::num;
use std::rc;



// ======================================================
// === The global logs (EVENTS and the METADATA_LOGS) ===
// ======================================================

thread_local! {
    static EVENT_LOG: log::Log<Event<ExternalMetadata, &'static str>> = log::Log::new();
}
/// Global log of [`Events`]s.
pub(crate) static EVENTS: log::ThreadLocalLog<Event<ExternalMetadata, &'static str>> =
    log::ThreadLocalLog::new(EVENT_LOG);

thread_local! {
    static METADATA_LOG_LOG: log::Log<rc::Rc<dyn MetadataSource>> = log::Log::new();
}
/// Global registry of metadata logs.
pub(crate) static METADATA_LOGS: log::ThreadLocalLog<rc::Rc<dyn MetadataSource>> =
    log::ThreadLocalLog::new(METADATA_LOG_LOG);

/// Produce a JSON-formatted event log from the internal event logs.
///
/// Consumes all events that have happened up to this point; except in testing, this should only be
/// done once.
pub fn take_log() -> String {
    let events = EVENTS.take_all();
    let metadatas = METADATA_LOGS.clone_all();
    let metadata_names: Vec<_> = metadatas.iter().map(|metadata| metadata.name()).collect();
    let mut metadata_entries: Vec<_> =
        metadatas.into_iter().map(|metadata| metadata.take_all()).collect();
    let mut profile = format::Builder::new();
    let mut id_map = std::collections::HashMap::<EventId, format::MeasurementId>::new();
    id_map.insert(EventId::IMPLICIT, EventId::IMPLICIT);
    id_map.insert(EventId::APP_LIFETIME, EventId::APP_LIFETIME);
    profile.time_offset_ms(Timestamp::time_offset());
    profile.process("Ide");
    for (id, event) in events.into_iter().enumerate() {
        let id = EventId(id as u32);
        match event {
            Event::Metadata(Timestamped { timestamp, data }) => {
                let ExternalMetadata { type_id } = data;
                let id = type_id as usize;
                let name = metadata_names[id];
                let data = metadata_entries[id].next().unwrap();
                profile.metadata(timestamp, name, data);
            }
            Event::Start(Start { parent, start, label }) => {
                let parent = id_map[&parent];
                let start = start.unwrap();
                let interval = profile.start(start, parent, label.0);
                id_map.insert(id, interval);
            }
            Event::StartPaused(Start { parent, start, label }) => {
                let parent = id_map[&parent];
                let interval = profile.start_paused(start, parent, label.0);
                id_map.insert(id, interval);
            }
            Event::End { id, timestamp } => profile.end(timestamp, id_map[&id]),
            Event::Pause { id, timestamp } => profile.pause(timestamp, id_map[&id]),
            Event::Resume { id, timestamp } => profile.resume(timestamp, id_map[&id]),
        }
    }
    profile.build_string()
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
    fn take_all(&self) -> Box<dyn Iterator<Item = Box<serde_json::value::RawValue>>>;
}

impl<T: 'static + serde::Serialize> MetadataSource for MetadataLog<T> {
    fn name(&self) -> &'static str {
        self.name
    }
    fn take_all(&self) -> Box<dyn Iterator<Item = Box<serde_json::value::RawValue>>> {
        let entries = self.entries.take_all();
        let entries =
            entries.into_iter().map(|data| serde_json::value::to_raw_value(&data).unwrap());
        Box::new(entries)
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
    pub fn start(
        self,
        parent: EventId,
        label: StaticLabel,
        start: Option<Timestamp>,
        state: StartState,
    ) -> EventId {
        let m = Start { parent, label, start };
        let event = match state {
            StartState::Active => Event::Start(m),
            StartState::Paused => Event::StartPaused(m),
        };
        let id = EVENTS.len() as u32;
        EVENTS.append(event);
        EventId(id)
    }

    /// Log the end of a measurement.
    pub fn end(self, id: EventId, timestamp: Timestamp) {
        let event = Event::End { id, timestamp };
        EVENTS.append(event);
    }

    /// Log the beginning of an interval in which the measurement is not active.
    pub fn pause(self, id: EventId, timestamp: Timestamp) {
        let event = Event::Pause { id, timestamp };
        EVENTS.append(event);
    }

    /// Log the end of an interval in which the measurement is not active.
    pub fn resume(self, id: EventId, timestamp: Timestamp) {
        let event = Event::Resume { id, timestamp };
        EVENTS.append(event);
    }

    /// Log metadata.
    pub fn metadata(self, type_id: u32) -> EventId {
        let id = EVENTS.len() as u32;
        let timestamp = Timestamp::now();
        let data = ExternalMetadata { type_id };
        let event = Event::Metadata(Timestamped { timestamp, data });
        EVENTS.append(event);
        EventId(id)
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
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Event<Metadata, LabelStorage> {
    /// The beginning of a measurement.
    Start(Start<LabelStorage>),
    /// The beginning of a measurement that starts in the paused state.
    StartPaused(Start<LabelStorage>),
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
    Metadata(Timestamped<Metadata>),
}



// =============
// === Start ===
// =============

/// A measurement-start entry in the profiling log.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Start<LabelStorage> {
    /// Specifies parent measurement by its [`Start`].
    pub parent: EventId,
    /// Start time, or None to indicate it is the same as `parent`.
    pub start:  Option<Timestamp>,
    /// Identifies where in the code this measurement originates.
    pub label:  Label<LabelStorage>,
}


// === Label ===

/// The label of a profiler; this includes the name given at its creation, along with file and
/// line-number information.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Label<Storage>(pub Storage);

impl<Storage: fmt::Display> fmt::Display for Label<Storage> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Static-str label, suitable for writing.
pub(crate) type StaticLabel = Label<&'static str>;



// =================
// === Timestamp ===
// =================

/// Time elapsed since the [time origin](https://www.w3.org/TR/hr-time-2/#sec-time-origin).
///
/// Stored in units of 100us, because that is maximum resolution of performance.now():
/// - [in the specification](https://www.w3.org/TR/hr-time-3), 100us is the limit
/// - in practice, as observed in debug consoles: Chromium 97 (100us) and Firefox 95 (1ms)
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, serde::Serialize, serde::Deserialize)]
pub struct Timestamp(num::NonZeroU64);

/// Offset used to encode a timestamp, which may be 0, in a [`NonZeroU64`].
/// To maximize the supported range, this is the smallest positive integer.
const TS_OFFSET: u64 = 1;

impl Timestamp {
    /// Return the current time, relative to the time origin.
    pub fn now() -> Self {
        Self::from_ms(now())
    }

    /// Return the timestamp corresponding to an offset from the time origin, in ms.
    #[allow(unsafe_code)]
    pub fn from_ms(ms: f64) -> Self {
        let ticks = (ms * 10.0).round() as u64;
        // Safety: ticks + 1 will not be 0 unless a Timestamp wraps.
        // It takes (2 ** 64) * 100us = 58_455_453 years for a Timestamp to wrap.
        unsafe { Self(num::NonZeroU64::new_unchecked(ticks + TS_OFFSET)) }
    }

    /// Return the timestamp of the time origin.
    pub fn time_origin() -> Self {
        Self::from_ms(0.0)
    }

    /// Convert to an offset from the time origin, in ms.
    pub fn into_ms(self) -> f64 {
        (self.0.get() - TS_OFFSET) as f64 / 10.0
    }

    /// Return the offset of the time origin from a system timestamp.
    pub fn time_offset() -> f64 {
        time_origin()
    }
}

impl Default for Timestamp {
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



// === Timestamped ===

/// Wrapper adding a timestamp to an object.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Timestamped<T> {
    /// When the event occurred.
    pub timestamp: Timestamp,
    /// The data.
    pub data:      T,
}



// ===============
// === EventId ===
// ===============

/// Identifies an event in the profiling log.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
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
}



// ========================
// === ExternalMetadata ===
// ========================

/// Indicates where in the event log metadata from a particular external source should be inserted.
#[derive(Debug, Copy, Clone)]
pub(crate) struct ExternalMetadata {
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
    fn start(
        parent: EventId,
        label: StaticLabel,
        time: Option<Timestamp>,
        start: StartState,
    ) -> Self;
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
    fn new_child(&self, label: StaticLabel) -> Started<T> {
        self.0.new_child(label)
    }

    fn new_child_same_start(&self, label: StaticLabel) -> Started<T> {
        self.0.new_child_same_start(label)
    }
}
