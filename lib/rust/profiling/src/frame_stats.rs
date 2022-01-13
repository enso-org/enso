//! This module provides utilities for aggregating and summarizing per-frame [runtime performance
//! statistics][stats::Stats] over multiple GUI rendering frames.
//!
//! The API allows tracking the statistics over multiple distinct, possibly overlapping intervals.
//! The per-frame statistics are not collected in an automated way behind the scenes, but they need
//! to be explicitly pushed into the API on every rendering frame. On every push, they are
//! internally aggregated separately for each currently active interval. After ending a particular
//! interval, summary stats are calculated and returned based on the aggregated data.
//!
//! Example usage
//! --------------
//! ```ignore
//! // Manually start a new measurement interval.
//! let reticulating_splines = frame_stats::start_interval();
//! // ...
//!
//! // On every rendering frame, push stats for that frame into the API.
//! frame_stats::push_stats(calculatePerFrameStats());
//!
//! // ...
//! // When the interval of interest has finished, retrieve summarized stats.
//! let stats_summary = reticulating_splines.end();
//! println!("Summary stats while reticulating splines: {}", stats_summary);
//! ```
//!
//! Note: See also the [`profiling`][crate::profiling] module for a higher level API for intervals
//! profiling.

use enso_prelude::*;

use enso_data_structures::opt_vec::OptVec;
use enso_logger::DefaultWarningLogger as Logger;
use enso_logger::*;
use ensogl_core::debug::stats;



// =================
// === Intervals ===
// =================

type Intervals = OptVec<stats::Accumulator>;

thread_local! {
    static ACTIVE_INTERVALS: RefCell<Intervals> = RefCell::new(Intervals::new());
}

/// Starts a new named time interval, during which frame statistics will be collected.
pub fn start_interval() -> IntervalGuard {
    let index = ACTIVE_INTERVALS.with(|intervals| -> usize {
        let mut intervals_vec = intervals.borrow_mut();
        intervals_vec.insert(default())
    });
    IntervalGuard { index, released: false }
}

/// Object that allows ending the interval.
#[derive(Debug)]
pub struct IntervalGuard {
    index:    usize,
    released: bool,
}

impl IntervalGuard {
    /// Finishes collecting frame statistics for a specific interval. Returns aggregate data
    /// collected since the start of the the interval.
    pub fn end(mut self) -> Option<stats::Summary> {
        self.released = true;
        self.finalize()
    }

    fn finalize(&mut self) -> Option<stats::Summary> {
        ACTIVE_INTERVALS.with(|intervals| match intervals.borrow_mut().remove(self.index) {
            None => {
                let logger = Logger::new("Profiling_Stats");
                let warn_msg: &str =
                    "Trying to finalize profiling stats for a process not registered before.";
                warning!(logger, warn_msg);
                None
            }
            Some(accumulator) => accumulator.summarize(),
        })
    }
}

impl Drop for IntervalGuard {
    fn drop(&mut self) {
        if !self.released {
            let logger = Logger::new("Profiling_Stats");
            warning!(logger, "Stats profiling interval dropped without a matching `end` call.");
            let _ = self.finalize();
        }
    }
}

/// Include the provided stats snapshot into statistics for all intervals that are currently started
/// and not yet ended.
pub fn push_stats(snapshot: &stats::StatsData) {
    ACTIVE_INTERVALS.with(|intervals| {
        for interval in intervals.borrow_mut().iter_mut() {
            interval.push(snapshot);
        }
    });
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use ensogl_core::debug::stats::Stats;

    use assert_approx_eq::*;

    #[test]
    fn overlapping_intervals() {
        let guard_a = start_interval();

        let stats: Stats = default();
        stats.set_fps(55.0);
        stats.set_wasm_memory_usage(1);
        stats.set_buffer_count(1);
        push_stats(&stats.snapshot());

        let guard_b = start_interval();

        let stats: Stats = default();
        stats.set_fps(57.0);
        stats.set_wasm_memory_usage(1);
        stats.set_buffer_count(1);
        push_stats(&stats.snapshot());

        let result_a = guard_a.end().unwrap();

        let stats: Stats = default();
        stats.set_fps(59.0);
        stats.set_wasm_memory_usage(2);
        stats.set_buffer_count(2);
        push_stats(&stats.snapshot());

        let result_b = guard_b.end().unwrap();

        assert_approx_eq!(result_a.fps.min, 55.0);
        assert_approx_eq!(result_a.fps.avg, 56.0);
        assert_approx_eq!(result_a.fps.max, 57.0);
        assert_eq!(result_a.wasm_memory_usage.min, 1);
        assert_approx_eq!(result_a.wasm_memory_usage.avg, 1.0);
        assert_eq!(result_a.wasm_memory_usage.max, 1);
        assert_eq!(result_a.buffer_count.min, 1);
        assert_approx_eq!(result_a.buffer_count.avg, 1.0);
        assert_eq!(result_a.buffer_count.max, 1);

        assert_approx_eq!(result_b.fps.min, 57.0);
        assert_approx_eq!(result_b.fps.avg, 58.0);
        assert_approx_eq!(result_b.fps.max, 59.0);
        assert_eq!(result_b.wasm_memory_usage.min, 1);
        assert_approx_eq!(result_b.wasm_memory_usage.avg, 1.5);
        assert_eq!(result_b.wasm_memory_usage.max, 2);
        assert_eq!(result_b.buffer_count.min, 1);
        assert_approx_eq!(result_b.buffer_count.avg, 1.5);
        assert_eq!(result_b.buffer_count.max, 2);
    }

    #[test]
    fn empty_interval_discarded() {
        let guard = start_interval();
        assert!(guard.end().is_none());
    }
}
