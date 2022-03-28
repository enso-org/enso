//! Supports aggregating interval data by profiler to analyze total time spent, abstracting away
//! *when* intervals occurred.

use enso_prelude::*;

use std::collections;



// ==================
// === Aggregator ===
// ==================

/// Aggregate time spent in different functions.
#[derive(Default, Debug)]
pub struct Aggregator {
    stack: Vec<ImString>,
    root:  Frame,
}

impl Aggregator {
    /// Add data from a profile to the tree.
    pub fn add_profile<Metadata>(&mut self, profile: &crate::Profile<Metadata>) {
        for child in &profile.root_interval().children {
            self.visit_interval(profile, *child);
        }
    }

    /// Add the interval to a [`Frame`]; recurse into children.
    fn visit_interval<Metadata>(
        &mut self,
        profile: &crate::Profile<Metadata>,
        active: crate::IntervalId,
    ) {
        let active = &profile[active];
        let label = profile[active.measurement].label.to_string().into();
        self.stack.push(label);
        match active.interval.duration_ms() {
            Some(duration) if duration > 0.0 => {
                self.log_interval(duration);
                for child in &active.children {
                    self.visit_interval(profile, *child);
                }
            }
            _ => (),
        };
        self.stack.pop();
    }

    /// Add the interval to the total for the current stack.
    fn log_interval(&mut self, duration: f64) {
        let stack = &self.stack;
        let mut frame = &mut self.root;
        for id in stack {
            frame = frame.children.entry(id.clone()).or_default();
        }
        frame.duration += duration;
        frame.intervals += 1;
    }
}

impl From<Aggregator> for Frame {
    fn from(Aggregator { root, .. }: Aggregator) -> Self {
        root
    }
}



// =============
// === Frame ===
// =============

/// Aggregated info about all occurrences of a particular stack of profilers.
#[derive(Default, Debug)]
pub struct Frame {
    duration:     f64,
    /// Aggregated intervals that ran as children of this profiler.
    pub children: collections::HashMap<ImString, Self>,
    intervals:    usize,
}

impl Frame {
    /// Return the duration spent in this profiler's intervals, exclusive of time in child
    /// intervals.
    pub fn self_duration(&self) -> f64 {
        let children_duration: f64 = self.children.values().map(Frame::total_duration).sum();
        self.duration - children_duration
    }

    /// Return the duration spent in this profiler's intervals.
    pub fn total_duration(&self) -> f64 {
        self.duration
    }

    /// Return the number of intervals this aggregate represents.
    pub fn interval_count(&self) -> usize {
        self.intervals
    }
}
