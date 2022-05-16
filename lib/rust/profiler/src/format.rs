//! Defines the JSON-based profile event-log format.
//!
//! See: https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md#profiling-data

use serde;
use serde::Deserialize;
use serde::Serialize;


// ==============
// === Export ===
// ==============

pub mod builder;

pub use builder::Builder;



/// Metadata of any type.
pub type AnyMetadata = Box<serde_json::value::RawValue>;



// =============
// === Event ===
// =============

/// An entry in the profiling log.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Event<'a> {
    /// Registers a label to be referenced by ID.
    #[serde(rename = "L")]
    Label {
        /// The text content of the label.
        #[serde(rename = "l")]
        label: &'a str,
    },
    /// The beginning of a measurement that starts in the paused state.
    #[serde(rename = "C")]
    Create(Start),
    /// The beginning of a measurement, or the continuation after an interruption.
    #[serde(rename = "S")]
    Start {
        /// Identifies the measurement.
        #[serde(rename = "i")]
        id:        MeasurementId,
        /// When the event occurred.
        #[serde(rename = "t")]
        timestamp: Timestamp,
    },
    /// The end of a measurement.
    #[serde(rename = "E")]
    End {
        /// Identifies the measurement.
        #[serde(rename = "i")]
        id:        MeasurementId,
        /// When the event occurred.
        #[serde(rename = "t")]
        timestamp: Timestamp,
    },
    /// The beginning of an interruption to a measurement, e.g. an await point.
    #[serde(rename = "P")]
    Pause {
        /// Identifies the measurement.
        #[serde(rename = "i")]
        id:        MeasurementId,
        /// When the event occurred.
        #[serde(rename = "t")]
        timestamp: Timestamp,
    },
    /// Metadata: wrapper with dependency-injected contents.
    #[serde(rename = "X")]
    Metadata(Timestamped<AnyMetadata>),
}



// =============
// === Start ===
// =============

/// A measurement-start entry in the profiling log.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Start {
    /// Specifies parent measurement.
    #[serde(rename = "p")]
    #[serde(skip_serializing_if = "Parent::is_implicit")]
    pub parent: Parent,
    /// Start time, or None to indicate it is the same as `parent`.
    #[serde(rename = "t")]
    pub start:  Option<Timestamp>,
    /// Identifies where in the code this measurement originates.
    #[serde(rename = "l")]
    pub label:  Label,
}


// === Label ===

/// The label of a profiler; this includes the name given at its creation, along with file and
/// line-number information.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Label(usize);

impl Label {
    /// Return an index into the label table.
    pub fn id(self) -> usize {
        self.0
    }
}



// ==============
// === Parent ===
// ==============

/// Specifies how the parent of a measurement is identified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Parent {
    /// Unspecified parent, to be identified from context.
    Implicit,
    /// Specific parent.
    Explicit(ParentId),
}

impl Parent {
    /// Unspecified parent, to be identified from context.
    pub fn implicit() -> Self {
        Parent::Implicit
    }

    /// Return whether the parent is implicit.
    pub fn is_implicit(&self) -> bool {
        *self == Parent::Implicit
    }

    /// Returns the special parent of top-level measurements.
    pub fn root() -> Self {
        Parent::Explicit(ParentId::Root)
    }
}

impl From<MeasurementId> for Parent {
    fn from(id: MeasurementId) -> Self {
        Parent::Explicit(ParentId::Measurement(id))
    }
}


// === ParentId ===

/// Identifies a parent for a measurement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParentId {
    /// The root of top-level measurements.
    Root,
    /// A runtime measurement.
    Measurement(MeasurementId),
}


// === Serialized representation ===

impl Serialize for Parent {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where S: serde::ser::Serializer {
        match *self {
            Parent::Implicit => ser.serialize_none(),
            Parent::Explicit(ParentId::Root) => ser.serialize_i64(-1),
            Parent::Explicit(ParentId::Measurement(MeasurementId(id))) =>
                ser.serialize_u64(id as u64),
        }
    }
}

impl<'de> Deserialize<'de> for Parent {
    fn deserialize<D>(deserializer: D) -> Result<Parent, D::Error>
    where D: serde::de::Deserializer<'de> {
        let parent: Option<i64> = Deserialize::deserialize(deserializer)?;
        Ok(match parent {
            None => Parent::Implicit,
            Some(-1) => Parent::Explicit(ParentId::Root),
            Some(id) => {
                let id = id.try_into().map_err(|_| {
                    let found = serde::de::Unexpected::Signed(id);
                    let wanted =
                        format!("an integer between 0 and {}, or the special value -1", usize::MAX);
                    serde::de::Error::invalid_value(found, &wanted.as_str())
                })?;
                Parent::Explicit(ParentId::Measurement(MeasurementId(id)))
            }
        })
    }
}



// =====================
// === MeasurementId ===
// =====================

/// ID of a measurement (runtime instance of a profiler).
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct MeasurementId(pub usize);



// =================
// === Timestamp ===
// =================

/// A relative time; when added to this profile's [`Header::TimeOffset`] (if present), yields an
/// offset from the unix epoch.
///
/// Stored in microseconds; this provides plenty of range and precision, and unlike a float supports
/// [`Cmp`] and related traits easily.
#[derive(Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
pub struct Timestamp(u64);

impl Timestamp {
    /// Return the timestamp corresponding to an offset from the time origin, in ms.
    pub fn from_ms(ms: f64) -> Self {
        let ticks = (ms * 1000.0).round() as u64;
        Self(ticks)
    }

    /// Convert to an offset from the time origin, in ms.
    pub fn into_ms(self) -> f64 {
        self.0 as f64 / 1000.0
    }
}


// === Timestamped ===

/// Wrapper adding a timestamp to an object.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Timestamped<T> {
    /// When the event occurred.
    #[serde(rename = "t")]
    pub time: Timestamp,
    /// The data.
    #[serde(rename = "d")]
    pub data: T,
}



// ==============
// === Header ===
// ==============

/// Standard file headers.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Header {
    /// Value that can be added to a [`Timestamp`] to translate it to an offset from the Unix
    /// Epoch.
    #[serde(rename = "$TimeOffset")]
    TimeOffset(Timestamp),
    /// Application-specific identifier used to distinguish log data from different processes.
    #[serde(rename = "$Process")]
    Process(String),
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate::format;

    /// Verify that the current implementation can still deserialize the format as of the first
    /// stable release.
    #[test]
    fn format_stability() {
        // Example data containing every type of event and every encoding of each field.
        const LOG: &str = "[\
{\"C\":{\"p\":-1,\"t\":5210200,\"l\":3}},\
{\"C\":{\"p\":0,\"t\":5210000,\"l\":1}},\
{\"C\":{\"t\":5196300,\"l\":0}},\
{\"C\":{\"t\":null,\"l\":0}},\
{\"E\":{\"i\":0,\"t\":5199800}},\
{\"L\":{\"l\":\"entry_point_ide (app/gui/src/lib.rs:134)\"}},\
{\"P\":{\"i\":2,\"t\":5210200}},\
{\"S\":{\"i\":0,\"t\":5196300}},\
{\"X\":{\"t\":0,\"d\":{\"$Process\":\"Ide\"}}},\
{\"X\":{\"t\":0,\"d\":{\"$TimeOffset\":1650900741301300}}}\
]";
        // Check that we can deserialize the data.
        let events: Vec<format::Event> = serde_json::from_str(LOG).unwrap();
        // Check that the deserialized structures contain all the information that was in the JSON.
        // As an easy way of implementing this, we check a stricter property here: That
        // re-serializing the data structures produces the same blob as the input.
        assert_eq!(LOG, serde_json::to_string(&events).unwrap());
    }
}
