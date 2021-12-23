#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]


use enso_prelude::*;

use enso_logger::DefaultWarningLogger as Logger;
use enso_logger::*;
use serde::Deserialize;
use serde::Serialize;



// ====================
// === IntervalsMap ===
// ====================
// TODO(akavel): naming: Metrics? Stats? Correlates? Health? Impact?

type IntervalsMap = HashMap<String, Bundle>;

thread_local! {
    static ACTIVE_INTERVALS: RefCell<IntervalsMap> = RefCell::new(IntervalsMap::new());
}

pub fn start_interval(label: &str) {
    let logger = Logger::new("Profiling_Stats");
    ACTIVE_INTERVALS.with(|intervals| {
        let found = intervals.borrow_mut().insert(label.to_string(), Bundle::default());
        if found.is_some() {
            warning!(logger, "Trying to collect profiling stats for a process with same label as already existing one - values will be skewed for: {label:?}");
        }
    });
}

pub fn end_interval(label: &str) -> Option<Bundle> {
    let logger = Logger::new("Profiling_Stats");
    ACTIVE_INTERVALS.with(|intervals| {
        match intervals.borrow_mut().remove(label) {
            None => {
                warning!(logger, "Trying to finalize profiling stats for a process with a label not registered before: {label:?}");
                None
            },
            Some(bundle) if bundle.frames_count == 0 => None,
            Some(bundle) => Some(bundle),
        }
    })
}

type LabeledSamples = Vec<(&'static str, f64)>;

pub fn push(samples: &LabeledSamples) {
    ACTIVE_INTERVALS.with(|intervals| {
        for interval in intervals.borrow_mut().values_mut() {
            interval.push(samples);
        }
    });
}

// FIXME(akavel): do we need Clone?
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Bundle {
    pub accumulators: Vec<MetricAccumulator>,
    pub frames_count: u32,
}

impl Bundle {
    fn push(&mut self, samples: &LabeledSamples) {
        // FIXME: naming - stats, stat, samples, ... - I'm already confused myself
        if self.frames_count == 0 {
            self.accumulators = Vec::with_capacity(samples.len());
            for (label, sample) in samples {
                self.accumulators.push(MetricAccumulator::new(*label, *sample));
            }
        } else {
            // FIXME: verify vec lengths match & labels match, and log an error if not
            for (acc, (_label, sample)) in self.accumulators.iter_mut().zip(samples) {
                acc.push(*sample);
            }
        }
        self.frames_count += 1;
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MetricAccumulator {
    label: CowString,
    min:   f64,
    max:   f64,
    sum:   f64,
}

impl MetricAccumulator {
    fn new(label: impl Into<CowString>, initial_sample: f64) -> Self {
        Self {
            label: label.into(),
            min:   initial_sample,
            max:   initial_sample,
            sum:   initial_sample,
        }
    }

    fn push(&mut self, new_sample: f64) {
        self.min = self.min.min(new_sample);
        self.max = self.max.max(new_sample);
        self.sum += new_sample;
    }
}
