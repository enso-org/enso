//! Supports constructing a document in the JSON format (see [`crate::format`]).

use crate::format;
use crate::internal;



// ===============
// === Builder ===
// ===============

/// Constructs a profile document for serialization into the JSON format (see [`crate::format`]).
#[derive(Debug, Default)]
pub struct Builder<'a> {
    events: Vec<internal::Event<format::AnyMetadata, &'a str>>,
}

impl<'a> Builder<'a> {
    #[allow(missing_docs)]
    pub fn new() -> Self {
        Default::default()
    }

    /// Log a metadata event to the profile.
    pub fn metadata<M>(&mut self, time: format::Timestamp, name: &'static str, data: M)
    where M: serde::Serialize {
        let data = Variant { name, data };
        let data = serde_json::value::to_raw_value(&data).unwrap();
        let event = format::Timestamped { timestamp: time, data };
        self.events.push(internal::Event::Metadata(event));
    }

    /// Log a profiler-start event to the profile.
    pub fn start<'b: 'a>(
        &mut self,
        time: format::Timestamp,
        parent: format::OptionalMeasurementParent,
        label: &'b str,
    ) -> format::MeasurementId {
        let start = Some(time);
        let label = internal::Label(label);
        let event = internal::Start { parent, start, label };
        let id = self.events.len();
        self.events.push(internal::Event::Start(event));
        crate::EventId(id as u32)
    }

    /// Log a profiler-creation event to the profile.
    pub fn start_paused<'b: 'a>(
        &mut self,
        time: Option<format::Timestamp>,
        parent: format::OptionalMeasurementParent,
        label: &'b str,
    ) -> format::MeasurementId {
        let start = time;
        let label = internal::Label(label);
        let event = internal::Start { parent, start, label };
        let id = self.events.len();
        self.events.push(internal::Event::StartPaused(event));
        crate::EventId(id as u32)
    }

    /// Log a profiler-end event to the profile.
    pub fn end(&mut self, time: format::Timestamp, measurement: format::MeasurementId) {
        let id = measurement;
        let timestamp = time;
        self.events.push(internal::Event::End { id, timestamp });
    }

    /// Log a profiler-pause event to the profile.
    pub fn pause(&mut self, time: format::Timestamp, measurement: format::MeasurementId) {
        let id = measurement;
        let timestamp = time;
        self.events.push(internal::Event::Pause { id, timestamp });
    }

    /// Log a profiler-resume event to the profile.
    pub fn resume(&mut self, time: format::Timestamp, measurement: format::MeasurementId) {
        let id = measurement;
        let timestamp = time;
        self.events.push(internal::Event::Resume { id, timestamp });
    }

    /// Attach a header to the profile indicating the offset of the file's timestamps from system
    /// time.
    pub fn time_offset_ms(&mut self, _offset: f64) {
        // TODO[kw]
    }

    /// Attach a header to the profile identifying its process.
    pub fn process<'b: 'a>(&mut self, _process: &'b str) {
        // TODO[kw]
    }

    /// Render the profile to a file.
    pub fn build_string(self) -> String {
        serde_json::to_string(&self.events).unwrap()
    }
}



// ===============
// === Variant ===
// ===============

/// Wrapper for serializing an object as if it were a particular variant of some unspecified enum.
///
/// This allows serializing instances of one variant of an enum without knowledge of the other
/// variants.
struct Variant<T> {
    name: &'static str,
    data: T,
}

impl<T: serde::Serialize> serde::Serialize for Variant<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        serializer.serialize_newtype_variant("", 0, self.name, &self.data)
    }
}
