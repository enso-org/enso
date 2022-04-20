//! Defines the JSON-based profile event-log format.
//!
//! See: https://github.com/enso-org/design/blob/main/epics/profiling/implementation.md#profiling-data



// TODO[kw]: Define the format independently of [`crate::internal`]'s types.

pub mod builder;

pub use builder::Builder;

pub use crate::internal::Timestamp;
pub use crate::internal::Timestamped;

/// Metadata of any type.
pub type AnyMetadata = Box<serde_json::value::RawValue>;
/// ID of a measurement (runtime instance of a profiler).
pub type MeasurementId = crate::EventId;
/// Identifies the parent of a measurement. May be another measurement, or certain special values.
pub type OptionalMeasurementParent = crate::EventId;
