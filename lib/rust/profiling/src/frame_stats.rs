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

#[derive(Clone, Debug)]
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

// /// Starts a new named time interval, during which frame statistics will be collected.
// pub fn start_interval(label: &str) {
//     let logger = Logger::new("Profiling_Stats");
//     ACTIVE_INTERVALS.with(|intervals| {
//         let found = intervals.borrow_mut().insert(label.to_string(), Bundle::default());
//         if found.is_some() {
//             warning!(logger, "Trying to collect profiling stats for a process with same label as already existing one - values will be skewed for: {label:?}");
//         }
//     });
// }

// /// Finishes collecting frame statistics for a specific named interval. Returns aggregate data
// /// collected since the start of the the interval.
// pub fn end_interval(label: &str) -> Option<Bundle> {
//     let logger = Logger::new("Profiling_Stats");
//     ACTIVE_INTERVALS.with(|intervals| {
//         match intervals.borrow_mut().remove(label) {
//             None => {
//                 warning!(logger, "Trying to finalize profiling stats for a process with a label not registered before: {label:?}");
//                 None
//             },
//             Some(bundle) if bundle.frames_count == 0 => None,
//             Some(bundle) => Some(bundle),
//         }
//     })
// }

/// Include the provided stats snapshot into statistics for all intervals that are currently started and
/// not yet ended.
pub fn push(snapshot: StatsSnapshot) {
    ACTIVE_INTERVALS.with(|intervals| {
        for interval in intervals.borrow_mut().iter_mut() {
            interval.push(snapshot.clone());
        }
    });
}



// // ==============
// // === Bundle ===
// // ==============

// /// Statistics of various metrics, collected over several frames.
// #[derive(Clone, Debug, Default, Serialize, Deserialize)]
// pub struct Bundle {
//     /// Aggregated data of each metric.
//     pub accumulators: Vec<MetricAccumulator>,
//     /// Over how many frames the data was aggregated.
//     pub frames_count: u32,
// }

// impl Bundle {
//     /// Aggregate the provided samples into statistics.
//     /// Note: empty samples will be ignored.
//     fn push(&mut self, samples: &[LabeledSample]) {
//         if samples.len() == 0 {
//             return;
//         }

//         if self.frames_count == 0 {
//             self.accumulators = Vec::with_capacity(samples.len());
//             for (label, sample) in samples {
//                 self.accumulators.push(MetricAccumulator::new(label.clone(), *sample));
//             }
//         } else {
//             // FIXME: verify vec lengths match & labels match, and log an error if not
//             for (acc, (_label, sample)) in self.accumulators.iter_mut().zip(samples) {
//                 acc.push(*sample);
//             }
//         }
//         self.frames_count += 1;
//     }
// }



// // =========================
// // === MetricAccumulator ===
// // =========================

// /// Accumulated data for a single metric.
// #[derive(Clone, Debug, Serialize, Deserialize)]
// pub struct MetricAccumulator {
//     label: ImString,
//     min:   f64,
//     max:   f64,
//     sum:   f64,
// }

// impl MetricAccumulator {
//     fn new(label: ImString, initial_sample: f64) -> Self {
//         Self {
//             label,
//             min:   initial_sample,
//             max:   initial_sample,
//             sum:   initial_sample,
//         }
//     }

//     fn push(&mut self, new_sample: f64) {
//         self.min = self.min.min(new_sample);
//         self.max = self.max.max(new_sample);
//         self.sum += new_sample;
//     }
// }



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
