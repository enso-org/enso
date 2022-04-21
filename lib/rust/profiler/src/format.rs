//! Defines the JSON-based profile event-log format.
//!
//! See: https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md#profiling-data

use serde;
use serde::{Serialize, Deserialize};



pub mod builder;

pub use builder::Builder;

/// Metadata of any type.
pub type AnyMetadata = Box<serde_json::value::RawValue>;



// =============
// === Event ===
// =============

/// An entry in the profiling log.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "e")]
pub enum Event<'a> {
    /// The beginning of a measurement that starts in the paused state.
    #[serde(borrow)]
    #[serde(rename = "C")]
    Create(Start<'a>),
    /// The beginning of a measurement.
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
    /// The end of an interruption to an a measurement, e.g. an await point.
    #[serde(rename = "R")]
    Resume {
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Start<'a> {
    /// Specifies parent measurement.
    #[serde(rename = "p")]
    pub parent: Parent,
    /// Start time, or None to indicate it is the same as `parent`.
    #[serde(rename = "t")]
    pub start:  Option<Timestamp>,
    /// Identifies where in the code this measurement originates.
    #[serde(borrow)]
    #[serde(rename = "l")]
    pub label:  Label<'a>,
}


// === Label ===

/// The label of a profiler; this includes the name given at its creation, along with file and
/// line-number information.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Label<'a>(&'a str);

impl<'a> Label<'a> {
    /// Get the raw string data.
    pub fn as_str(self) -> &'a str {
        self.0
    }
}



// ==============
// === Parent ===
// ==============

/// Specifies how the parent of a measurement is identified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Parent {
    Implicit,
    Explicit(ParentId),
}

impl Parent {
    pub fn implicit() -> Self {
        Parent::Implicit
    }

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
    Root,
    Measurement(MeasurementId),
}


// === Serialized representation ===

impl Serialize for Parent {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::ser::Serializer,
    {
        let parent = parent_repr::Parent::from(*self);
        Serialize::serialize(&parent, serializer)
    }
}

impl<'de> Deserialize<'de> for Parent {
    fn deserialize<D>(deserializer: D) -> Result<Parent, D::Error>
        where
            D: serde::de::Deserializer<'de>,
    {
        let parent: parent_repr::Parent = Deserialize::deserialize(deserializer)?;
        parent.try_into().map_err(|id| {
            let found = serde::de::Unexpected::Signed(id);
            let wanted = "a positive integer or the special value -1";
            serde::de::Error::invalid_value(found, &wanted)
        })
    }
}

mod parent_repr {
    use super::*;

    /// Used to derive a serialization for [`Parent`].
    #[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
    pub(super) struct Parent(
        #[serde(skip_serializing_if = "Option::is_none")]
        pub Option<i64>
    );

    impl TryFrom<Parent> for super::Parent {
        type Error = i64;
        fn try_from(parent: Parent) -> Result<Self, Self::Error> {
            Ok(match parent.0 {
                None => super::Parent::Implicit,
                Some(-1) => super::Parent::Explicit(ParentId::Root),
                Some(id) => {
                    let id = id.try_into().map_err(|_| id)?;
                    super::Parent::Explicit(ParentId::Measurement(MeasurementId(id)))
                },
            })
        }
    }

    impl From<super::Parent> for Parent {
        fn from(parent: crate::format::Parent) -> Self {
            Parent(match parent {
                super::Parent::Implicit => None,
                super::Parent::Explicit(ParentId::Root) => Some(-1),
                super::Parent::Explicit(ParentId::Measurement(MeasurementId(id))) =>
                    Some(id.try_into().expect("64-bit ID overflow should not be possible!")),
            })
        }
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
