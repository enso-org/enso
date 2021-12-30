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
#[allow(missing_docs)]
pub struct StatsSnapshot {
    pub frame_time           : f64,
    pub fps                  : f64,
    pub wasm_memory_usage    : u32,
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
            Some(snapshots) => StatsAggregate::aggregate(snapshots),
        }
    })
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[allow(missing_docs)]
#[serde(rename_all = "camelCase")]
pub struct StatsAggregate {
    pub frame_time           : MetricSummary<f64>,
    pub fps                  : MetricSummary<f64>,
    pub wasm_memory_usage    : MetricSummary<u32>,
    pub gpu_memory_usage     : MetricSummary<u32>,
    pub draw_call_count      : MetricSummary<usize>,
    pub buffer_count         : MetricSummary<usize>,
    pub data_upload_count    : MetricSummary<usize>,
    pub data_upload_size     : MetricSummary<u32>,
    pub sprite_system_count  : MetricSummary<usize>,
    pub sprite_count         : MetricSummary<usize>,
    pub symbol_count         : MetricSummary<usize>,
    pub mesh_count           : MetricSummary<usize>,
    pub shader_count         : MetricSummary<usize>,
    pub shader_compile_count : MetricSummary<usize>,
}

macro_rules! summarize {
    ($first:expr, $iter:expr, $field_name:tt) => {
        summarize($first.$field_name, $iter.clone().map(|x| x.$field_name))
    };
}

impl StatsAggregate {
    fn aggregate(snapshots: Vec<StatsSnapshot>) -> Option<Self> {
        let mut iter = snapshots.iter();
        match iter.next() {
            None => None,
            Some(first) => Some(Self {
                frame_time           : summarize!(first, iter, frame_time),
                fps                  : summarize!(first, iter, fps),
                wasm_memory_usage    : summarize!(first, iter, wasm_memory_usage),
                gpu_memory_usage     : summarize!(first, iter, gpu_memory_usage),
                draw_call_count      : summarize!(first, iter, draw_call_count),
                buffer_count         : summarize!(first, iter, buffer_count),
                data_upload_count    : summarize!(first, iter, data_upload_count),
                data_upload_size     : summarize!(first, iter, data_upload_size),
                sprite_system_count  : summarize!(first, iter, sprite_system_count),
                sprite_count         : summarize!(first, iter, sprite_count),
                symbol_count         : summarize!(first, iter, symbol_count),
                mesh_count           : summarize!(first, iter, mesh_count),
                shader_count         : summarize!(first, iter, shader_count),
                shader_compile_count : summarize!(first, iter, shader_compile_count),
            })
        }
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

fn summarize<T: Clone + MinMax>(first: T, rest: impl Iterator<Item=T>) -> MetricSummary<T> {
    let mut min = first.clone();
    let mut max = first.clone();
    let mut sum: f64 = first.to_f64();
    let mut n: usize = 1;
    for sample in rest {
        min = min.min(sample.clone());
        max = max.max(sample.clone());
        sum += sample.to_f64();
        n += 1;
    }
    MetricSummary {
        min, max,
        avg: sum / (n as f64),
    }
}

trait MinMax {
    fn min(&self, other: Self) -> Self;
    fn max(&self, other: Self) -> Self;
    fn to_f64(&self) -> f64;
}

impl MinMax for f64 {
    fn min(&self, other: f64) -> f64 { f64::min(*self, other) }
    fn max(&self, other: f64) -> f64 { f64::max(*self, other) }
    fn to_f64(&self) -> f64 { *self }
}

impl MinMax for u32 {
    fn min(&self, other: Self) -> Self { std::cmp::min(*self, other) }
    fn max(&self, other: Self) -> Self { std::cmp::max(*self, other) }
    fn to_f64(&self) -> f64 { *self as f64 }
}

impl MinMax for usize {
    fn min(&self, other: Self) -> Self { std::cmp::min(*self, other) }
    fn max(&self, other: Self) -> Self { std::cmp::max(*self, other) }
    fn to_f64(&self) -> f64 { *self as f64 }
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
