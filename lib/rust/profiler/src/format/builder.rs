//! Supports constructing a document in the JSON format (see [`crate::format`]).

use crate::format;



// ===============
// === Builder ===
// ===============

/// Constructs a profile document for serialization into the JSON format (see [`crate::format`]).
#[derive(Debug, Default)]
pub struct Builder<'a> {
    events: Vec<format::Event<'a>>,
    next_id: usize,
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
        let event = format::Timestamped { time, data };
        self.events.push(format::Event::Metadata(event));
    }

    /// Log a profiler-start event to the profile.
    pub fn start<'b: 'a>(
        &mut self,
        timestamp: format::Timestamp,
        parent: format::Parent,
        label: &'b str,
    ) -> format::MeasurementId {
        let id = self.start_paused(Some(timestamp), parent, label);
        self.events.push(format::Event::Start { id, timestamp });
        id
    }

    /// Log a profiler-creation event to the profile.
    pub fn start_paused<'b: 'a>(
        &mut self,
        time: Option<format::Timestamp>,
        parent: format::Parent,
        label: &'b str,
    ) -> format::MeasurementId {
        let start = time;
        let label = format::Label(label);
        let event = format::Start { parent, start, label };
        self.events.push(format::Event::Create(event));
        let id = self.next_id;
        self.next_id += 1;
        format::MeasurementId(id)
    }

    /// Log a profiler-end event to the profile.
    pub fn end(&mut self, time: format::Timestamp, measurement: format::MeasurementId) {
        let id = measurement;
        let timestamp = time;
        self.events.push(format::Event::End { id, timestamp });
    }

    /// Log a profiler-pause event to the profile.
    pub fn pause(&mut self, time: format::Timestamp, measurement: format::MeasurementId) {
        let id = measurement;
        let timestamp = time;
        self.events.push(format::Event::Pause { id, timestamp });
    }

    /// Log a profiler-resume event to the profile.
    pub fn resume(&mut self, time: format::Timestamp, measurement: format::MeasurementId) {
        let id = measurement;
        let timestamp = time;
        self.events.push(format::Event::Resume { id, timestamp });
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
