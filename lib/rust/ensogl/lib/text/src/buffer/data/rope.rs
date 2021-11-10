//! Efficient rope implementation. Currently, the module just re-exports and renames some of the
//! `xi-rope` types.



// ===============
// === Exports ===
// ===============

/// `xi-rope` spans re-export.
pub mod spans {
    pub use xi_rope::spans::Spans;
    pub use xi_rope::spans::SpansBuilder as Builder;
    pub use xi_rope::spans::SpansInfo as Info;
}

/// `xi-rope` metric units.
pub mod metric {
    pub use xi_rope::LinesMetric as Lines;
}

pub use spans::Spans;

pub use xi_rope::interval::Interval;
pub use xi_rope::interval::IntervalBounds;
pub use xi_rope::rope::Lines;
pub use xi_rope::Cursor;
pub use xi_rope::DeltaBuilder;
pub use xi_rope::Rope;
pub use xi_rope::RopeDelta as Delta;
pub use xi_rope::RopeInfo as Info;
