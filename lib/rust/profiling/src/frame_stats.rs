//! This module helps to aggregate various per-frame performance statistics, collected over labeled
//! time intervals.
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]


use enso_prelude::*;

use enso_data_structures::opt_vec::OptVec;
use enso_logger::DefaultWarningLogger as Logger;
use enso_logger::*;
use serde::Deserialize;
use serde::Serialize;



// =================
// === Intervals ===
// =================

type Intervals = OptVec<Vec<StatsSnapshot>>;

thread_local! {
    static ACTIVE_INTERVALS: RefCell<Intervals> = RefCell::new(Intervals::new());
}

#[derive(Clone, Debug, Default)]
pub struct StatsSnapshot {
    pub frame_time           : f64,
    pub fps                  : f64,
    pub wasm_memory_usage    : f64,
    pub gpu_memory_usage     : u32,
    pub draw_call_count      : usize,
    pub buffer_count         : usize,
    pub data_upload_count    : usize,
    pub data_upload_size     : u32,
    pub sprite_system_count  : usize,
    pub sprite_count         : usize,
    pub symbol_count         : usize,
    pub mesh_count           : usize,
    pub shader_count         : usize,
    pub shader_compile_count : usize,
}

/// Starts a new named time interval, during which frame statistics will be collected.
pub fn start_interval() -> usize {
    ACTIVE_INTERVALS.with(|intervals| -> usize {
        intervals.borrow_mut().insert(Vec::new())
    })
}

/// Finishes collecting frame statistics for a specific named interval. Returns aggregate data
/// collected since the start of the the interval.
/// TODO: should use IntervalGuard instead of passing usize around
pub fn end_interval(index: usize) -> Option<StatsAggregate> {
    ACTIVE_INTERVALS.with(|intervals| {
        match intervals.borrow_mut().remove(index) {
            None => {
                let logger = Logger::new("Profiling_Stats");
                warning!(logger, "Trying to finalize profiling stats for a process not registered before.");
                None
            },
            Some(snapshots) if snapshots.is_empty() => None,
            Some(snapshots) => Some(StatsAggregate::new(snapshots)),
        }
    })
}

pub struct StatsAggregate {
}

impl StatsAggregate {
    fn new(snapshots: Vec<StatsSnapshot>) -> Self {
    }
}

/// Include the provided stats snapshot into statistics for all intervals that are currently started and
/// not yet ended.
pub fn push(snapshot: StatsSnapshot) {
    ACTIVE_INTERVALS.with(|intervals| {
        for interval in intervals.borrow_mut().iter_mut() {
            interval.push(snapshot.clone());
        }
    });
}



// =====================
// === MetricSummary ===
// =====================

/// Summarized data for a single metric.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MetricSummary<T> {
    min:   T,
    max:   T,
    avg:   f64,
}

impl<T> MetricSummary<T> {
    fn summarize(samples: impl Iterator<Item=T>) -> Option<Self> {
        match samples.next() {
            None => None,
            Some(first) => {
                let mut min = first;
                let mut max = first;
                let mut sum = first as f64;
                let mut n: usize = 1;
                for sample in samples {
                    min = min!(min, sample);
                    max = max!(max, sample);
                    sum += sample as f64;
                    n += 1;
                }
                Some(Self {
                    min, max,
                    avg: sum / (n as f64),
                })
            }
        }
    }
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
