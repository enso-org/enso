//! Definition of strongly typed units, like `Line`, `Column`, or `Location`. Used to express type
//! level dependencies in the whole library.

use crate::prelude::*;
use ensogl::types;



// ===============
// === Exports ===
// ===============

/// Common traits.
pub mod traits {
    pub use super::bytes::Into  as TRAIT_bytes_into;
    pub use super::line::Into   as TRAIT_line_into;
    pub use super::column::Into as TRAIT_column_into;
}
pub use traits::*;



// =============
// === Bytes ===
// =============

types::unit! {
/// An offset in the buffer in bytes.
Bytes::bytes(usize)
}

impl<T:Into<Bytes>> bytes::Into for Range<T> {
    type Output = Range<Bytes>;
    fn bytes(self) -> Self::Output {
        let start = self.start.bytes();
        let end   = self.end.bytes();
        Range {start,end}
    }
}



// ============
// === Line ===
// ============

types::unit! {
/// A type representing vertical measurements.
Line::line(usize)
}



// ==============
// === Column ===
// ==============

types::unsigned_unit_proxy! {
/// A type representing horizontal measurements
Column::column(Bytes)
}


types::newtype! {
/// A type representing 2d measurements.
Location {
    line   : Line,
    column : Column,
}}
