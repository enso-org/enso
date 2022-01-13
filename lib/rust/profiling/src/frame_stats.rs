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

// Note [Frame Stats Active Intervals]
type Intervals = OptVec<stats::Accumulator>;

thread_local! {
    /// Tracks all currently active (started and not yet ended) intervals, and accumulates
    /// per-frame stats separately for each one.
    // Note [Frame Stats Active Intervals]
    static ACTIVE_INTERVALS: RefCell<Intervals> = RefCell::new(Intervals::new());
}

// Note [Frame Stats Active Intervals]
// ===================================
//
// The `push_stats` function is intended to be run on every frame of the GUI main rendering loop.
// As such, the function and the data structures it works on need to be written with high care for
// performance (see: Note [Main Loop Performance]). Based on that, the following design choices
// were currently taken:
//
//  - **`ACTIVE_INTERVALS` is `thread_local!`.**
//    The whole GUI is currently designed and implemented under assumption of running
//    single-threaded. Thanks to this, we are safe to use thread-local storage knowing, that only
//    one instance of it will ever be crated, and this one instance will be globally available in
//    the whole program. This allows us to avoid expensive thread synchronization primitives. On
//    the other hand, the `thread_local!` macro is not the fastest it could possibly be (see:
//    https://matklad.github.io/2020/10/03/fast-thread-locals-in-rust.html). However, we consider
//    it fast enough for this use case (taking into account it will be a single access per frame),
//    that we prefer to play it safe vs. going all the way and risking use of homemade `unsafe`
//    constructs. Notably, an alternative in the shape of the `#[thread_local]` attribute was also
//    considered, but rejected due to having unsoundness problems at the time of writing (see:
//    https://github.com/rust-lang/rust/issues/29594).
//  - **`ACTIVE_INTERVALS` entries are `stats::Accumulator` per active interval.**
//    It is assumed that not many intervals will be active simultaneously at any given time -
//    roughly: `profiling_lvls_available * intevals_overlap_factor`, which shouldn't exceed a
//    dozen. The `stats::Accumulator` is a fixed-size struct (thus avoiding overhead of memory
//    allocations/deallocations), and pushing data into it comprises of a bunch of fairly simple
//    arithmetic operations per each stat.
//  - **`ACTIVE_INTERVALS` is an `OptVec`.**
//    `OptVec` is a sparse vector type, presumed to have better performance characteristics vs.
//    `HashMap`.

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
    // Note [Frame Stats Active Intervals]
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
