//! Common types of JSON-RPC-based Enso services used by both Project Manager and File Manager.

/// Time in UTC time zone.
pub type UTCDateTime = chrono::DateTime<chrono::FixedOffset>;
