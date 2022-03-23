//! Supports aggregating interval data by profiler to analyze total time spent, abstracting away
//! *when* intervals occurred.

use std::collections;
use std::rc;

/// Aggregate time spent in different functions.
#[derive(Default, Debug)]
pub struct Aggregator {
    stack: Vec<std::rc::Rc<String>>,
    root:  Frame,
}

impl Aggregator {
    /// Add data from a profile to the tree.
    pub fn visit_profile<Metadata>(&mut self, profile: &crate::Profile<Metadata>) {
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
        let label = std::rc::Rc::new(profile[active.measurement].label.to_string());
        self.stack.push(label);
        match active.interval.duration_ms() {
            Some(duration) if duration > 0.0 => {
                self.log_duration(duration);
                for child in &active.children {
                    self.visit_interval(profile, *child);
                }
            }
            _ => (),
        };
        self.stack.pop();
    }

    /// Add the duration to the total for the current stack.
    fn log_duration(&mut self, duration: f64) {
        let stack = &self.stack;
        let mut frame = &mut self.root;
        for id in stack {
            frame = frame.children.entry(id.clone()).or_default();
        }
        frame.duration += duration;
    }
}

impl From<Aggregator> for Frame {
    fn from(Aggregator { root, .. }: Aggregator) -> Self {
        root
    }
}

/// Aggregated info about all occurrences of a particular stack of profilers.
#[derive(Default, Debug)]
pub struct Frame {
    duration:     f64,
    /// Aggregated intervals that ran as children of this profiler.
    pub children: collections::HashMap<rc::Rc<String>, Self>,
}

impl Frame {
    /// Return the duration spent in this profiler's intervals, exclusive of time in child
    /// intervals.
    pub fn self_duration(&self) -> f64 {
        let mut duration = self.duration;
        for child in self.children.values() {
            duration -= child.duration;
        }
        duration
    }

    /// Return the duration spent in this profiler's intervals.
    pub fn total_duration(&self) -> f64 {
        self.duration
    }
}
