//! This module helps to aggregate various per-frame performance statistics, collected over labeled
//! time intervals.
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]


use enso_prelude::*;

use crate::stats;

use enso_data_structures::opt_vec::OptVec;
use enso_logger::DefaultWarningLogger as Logger;
use enso_logger::*;



// =================
// === Intervals ===
// =================

type Intervals = OptVec<stats::StatsAccumulator>;

thread_local! {
    static ACTIVE_INTERVALS: RefCell<Intervals> = RefCell::new(Intervals::new());
}

#[derive(Debug)]
pub struct IntervalGuard {
    index: usize,
    released: bool,
}

/// Starts a new named time interval, during which frame statistics will be collected.
pub fn start_interval() -> IntervalGuard {
    let index = ACTIVE_INTERVALS.with(|intervals| -> usize {
        intervals.borrow_mut().insert(stats::StatsAccumulator::default())
    });
    IntervalGuard { index, released: false }
}

impl IntervalGuard {
    /// Finishes collecting frame statistics for a specific named interval. Returns aggregate data
    /// collected since the start of the the interval.
    /// TODO: should use IntervalGuard instead of passing usize around
    pub fn end(mut self) -> Option<stats::StatsSummary> {
        self.released = true;
        self.finalize()
    }

    fn finalize(&mut self) -> Option<stats::StatsSummary> {
        ACTIVE_INTERVALS.with(|intervals| {
            match intervals.borrow_mut().remove(self.index) {
                None => {
                    let logger = Logger::new("Profiling_Stats");
                    warning!(logger, "Trying to finalize profiling stats for a process not registered before.");
                    None
                },
                Some(accumulator) => accumulator.try_into().ok(),
            }
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

/// Include the provided stats snapshot into statistics for all intervals that are currently started and
/// not yet ended.
pub fn push(snapshot: &stats::StatsData) {
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

    use assert_approx_eq::*;

    #[test]
    fn overlapping_intervals() {
        ACTIVE_INTERVALS.with(|intervals| intervals.borrow_mut().clear());

        const INTERVAL_A: &str = "interval-A";
        const INTERVAL_B: &str = "interval-B";
        const STAT: &str = "stat";

        start_interval(INTERVAL_A);
        push(&[(STAT.into(), 1.0)]);
        start_interval(INTERVAL_B);
        push(&[(STAT.into(), 1.0)]);
        let result_a = end_interval(INTERVAL_A).unwrap();
        push(&[(STAT.into(), 2.0)]);
        let result_b = end_interval(INTERVAL_B).unwrap();

        assert_eq!(result_a.frames_count, 2);
        assert_approx_eq!(result_a.accumulators[0].min, 1.0);
        assert_approx_eq!(result_a.accumulators[0].max, 1.0);
        assert_approx_eq!(result_a.accumulators[0].sum, 2.0);

        assert_eq!(result_b.frames_count, 2);
        assert_approx_eq!(result_b.accumulators[0].min, 1.0);
        assert_approx_eq!(result_b.accumulators[0].max, 2.0);
        assert_approx_eq!(result_b.accumulators[0].sum, 3.0);
    }

    #[test]
    fn empty_interval_discarded() {
        ACTIVE_INTERVALS.with(|intervals| intervals.borrow_mut().clear());

        const INTERVAL_A: &str = "interval-A";

        start_interval(INTERVAL_A);
        assert!(end_interval(INTERVAL_A).is_none());
    }

    #[test]
    fn empty_samples_ignored() {
        ACTIVE_INTERVALS.with(|intervals| intervals.borrow_mut().clear());

        const INTERVAL_A: &str = "interval-A";
        const STAT: &str = "stat";

        start_interval(INTERVAL_A);
        push(&[]);
        push(&[(STAT.into(), 1.0)]);
        push(&[]);
        let result = end_interval(INTERVAL_A).unwrap();

        assert_eq!(result.frames_count, 1);
        assert_approx_eq!(result.accumulators[0].min, 1.0);
        assert_approx_eq!(result.accumulators[0].max, 1.0);
        assert_approx_eq!(result.accumulators[0].sum, 1.0);
    }

    #[test]
    fn multiple_metrics_collected() {
        ACTIVE_INTERVALS.with(|intervals| intervals.borrow_mut().clear());

        const INTERVAL_A: &str = "interval-A";
        const STAT_1: &str = "stat-1";
        const STAT_2: &str = "stat-2";


        start_interval(INTERVAL_A);
        push(&[(STAT_1.into(), 1.0), (STAT_2.into(), 2.0)]);
        push(&[(STAT_1.into(), 1.0), (STAT_2.into(), 2.0)]);
        push(&[(STAT_1.into(), 1.0), (STAT_2.into(), 2.0)]);
        let result = end_interval(INTERVAL_A).unwrap();


        assert_eq!(result.frames_count, 3);

        assert_eq!(result.accumulators[0].label.as_str(), STAT_1);
        assert_approx_eq!(result.accumulators[0].min, 1.0);
        assert_approx_eq!(result.accumulators[0].max, 1.0);
        assert_approx_eq!(result.accumulators[0].sum, 3.0);

        assert_eq!(result.accumulators[1].label.as_str(), STAT_2);
        assert_approx_eq!(result.accumulators[1].min, 2.0);
        assert_approx_eq!(result.accumulators[1].max, 2.0);
        assert_approx_eq!(result.accumulators[1].sum, 6.0);
    }
}
