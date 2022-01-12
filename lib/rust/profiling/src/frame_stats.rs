//! This module helps to aggregate various per-frame performance statistics, collected over
//! intervals spanning multiple GUI rendering frames.

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
        intervals.borrow_mut().insert(default())
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
                warning!(
                    logger,
                    "Trying to finalize profiling stats for a process not registered before."
                );
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

    use ensogl_core::debug::stats::StatsData;

    use assert_approx_eq::*;

    #[test]
    fn overlapping_intervals() {
        let guard_a = start_interval();
        push_stats(&StatsData {
            fps: 55.0,
            wasm_memory_usage: 1,
            buffer_count: 1,
            ..default()
        });
        let guard_b = start_interval();
        push_stats(&StatsData {
            fps: 57.0,
            wasm_memory_usage: 1,
            buffer_count: 1,
            ..default()
        });
        let result_a = guard_a.end().unwrap();
        push_stats(&StatsData {
            fps: 59.0,
            wasm_memory_usage: 2,
            buffer_count: 2,
            ..default()
        });
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

