//! This module contains functionality that allows the profiling framework to
//! generate the data required to render a flame graph. This means creating data for each block
//! that is supposed to be rendered, with start time, end time and labels.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use enso_profiler as profiler;
use enso_profiler_data as data;



// ==================
// === Block Data ===
// ==================

/// A `Block` contains the data required to render a single block of a frame graph.
#[derive(Clone, Debug)]
pub struct Block {
    /// Start x coordinate of the block.
    pub start: f64,
    /// End x coordinate of the block.
    pub end:   f64,
    /// Row that the block should be placed in.
    pub row:   u32,
    /// The label to be displayed with the block.
    pub label: String,
}

impl Block {
    /// Width of the block.
    pub fn width(&self) -> f64 {
        self.end - self.start
    }
}



// ==================
// === Graph Data ===
// ==================

/// Contains the information required to render a graph, i.e., the data for all blocks that make up
/// the graph.
#[derive(Debug, Default)]
pub struct Graph {
    /// Collection of all blocks making up the flame graph.
    pub blocks: Vec<Block>,
}

impl Graph {
    /// Create a callgraph from the given data.
    pub fn new_callgraph<Metadata>(profile: &data::Profile<Metadata>) -> Self {
        CallgraphBuilder::run(profile)
    }

    /// Create a rungraph from the given data.
    pub fn new_rungraph<Metadata>(profile: &data::Profile<Metadata>) -> Self {
        RungraphBuilder::run(profile)
    }

    /// Create a hybrid rungraph-callgraph from the given data.
    pub fn new_hybrid_graph<Metadata>(profile: &data::Profile<Metadata>) -> Self {
        new_hybrid_graph(profile)
    }

    /// Gather and remove all logged measurements and return them as a `Graph`.
    pub fn take_from_log() -> Self {
        let profile: Result<data::Profile<data::OpaqueMetadata>, _> =
            profiler::internal::take_log().parse();
        if let Ok(profile) = profile {
            new_hybrid_graph(&profile)
        } else {
            eprintln!("Failed to deserialize profiling event log.");
            Graph::default()
        }
    }
}



// ==================
// === Callgraphs ===
// ==================

/// Build a graph that illustrates the call stack over time.
struct CallgraphBuilder<'p, Metadata> {
    profile: &'p data::Profile<Metadata>,
    blocks:  Vec<Block>,
}

impl<'p, Metadata> CallgraphBuilder<'p, Metadata> {
    /// Create a callgraph for the given profile.
    fn run(profile: &'p data::Profile<Metadata>) -> Graph {
        let blocks = Default::default();
        let mut builder = Self { profile, blocks };
        // We skip the root node APP_LIFETIME, which is not a real measurement.
        for child in &profile.root_interval().children {
            builder.visit_interval(*child, 0);
        }
        let Self { blocks, .. } = builder;
        Graph { blocks }
    }
}

impl<'p, Metadata> CallgraphBuilder<'p, Metadata> {
    /// Create a block for an interval; recurse into children.
    fn visit_interval(&mut self, active: data::IntervalId, row: u32) {
        let active = &self.profile[active];
        let start = active.interval.start.into_ms();
        let end = active.interval.end.map(|mark| mark.into_ms()).unwrap_or(f64::MAX);
        // Optimization: can't draw zero-width blocks anyway.
        if end == start {
            return;
        }
        let label = self.profile[active.measurement].label.to_string();
        self.blocks.push(Block { start, end, label, row });
        for child in &active.children {
            self.visit_interval(*child, row + 1);
        }
    }
}



// =================
// === Rungraphs ===
// =================

/// Build a graph that illustrates async tasks over time.
struct RungraphBuilder<'p, Metadata> {
    profile:  &'p data::Profile<Metadata>,
    blocks:   Vec<Block>,
    next_row: u32,
}

impl<'p, Metadata> RungraphBuilder<'p, Metadata> {
    /// Create a rungraph for the given profile.
    fn run(profile: &'p data::Profile<Metadata>) -> Graph {
        let blocks = Default::default();
        let next_row = Default::default();
        let mut builder = Self { profile, blocks, next_row };
        // We skip the root node APP_LIFETIME, which is not a real measurement.
        for child in &profile.root_measurement().children {
            builder.visit_measurement(*child);
        }
        let Self { blocks, .. } = builder;
        Graph { blocks }
    }
}

impl<'p, Metadata> RungraphBuilder<'p, Metadata> {
    /// Create blocks for a measurement's intervals; recurse into children.
    fn visit_measurement(&mut self, measurement: data::MeasurementId) {
        let measurement = &self.profile[measurement];
        // We're only interested in tasks that await other tasks, i.e. have at least 2 intervals.
        if measurement.intervals.len() >= 2 {
            let row = self.next_row;
            self.next_row += 1;
            for active in &measurement.intervals {
                let active = &self.profile[*active];
                let start = active.interval.start.into_ms();
                let mut end = active.interval.end.map(|mark| mark.into_ms()).unwrap_or(f64::MAX);
                // We want to show every wakeup of the task, even if it is momentary.
                const DURATION_FLOOR_MS: f64 = 0.5;
                if end < start + DURATION_FLOOR_MS {
                    end = start + DURATION_FLOOR_MS;
                }
                let label = self.profile[active.measurement].label.to_string();
                self.blocks.push(Block { start, end, label, row });
            }
        }
        for child in &measurement.children {
            self.visit_measurement(*child);
        }
    }
}


// === hybrid graph ===

/// Create a rungraph+callgraph for the given profile.
fn new_hybrid_graph<Metadata>(profile: &data::Profile<Metadata>) -> Graph {
    let blocks = Default::default();
    let next_row = Default::default();
    let mut rungraph = RungraphBuilder { profile, blocks, next_row };
    for child in &profile.root_measurement().children {
        rungraph.visit_measurement(*child);
    }
    let RungraphBuilder { blocks, next_row, .. } = rungraph;
    let mut callgraph = CallgraphBuilder { profile, blocks };
    for child in &profile.root_interval().children {
        callgraph.visit_interval(*child, next_row);
    }
    let CallgraphBuilder { blocks, .. } = callgraph;
    Graph { blocks }
}



// ===================
// === Flamegraphs ===
// ===================

/// Build a graph that illustrates aggregate time spent in different functions.
#[derive(Default, Debug)]
pub struct FlamegraphBuilder {
    stack: Vec<std::rc::Rc<String>>,
    root:  AggregateFrame,
}

impl FlamegraphBuilder {
    /// Add data from a profile to the graph.
    pub fn visit_profile<Metadata>(&mut self, profile: &data::Profile<Metadata>) {
        for child in &profile.root_interval().children {
            self.visit_interval(profile, *child);
        }
    }
}

impl From<FlamegraphBuilder> for Graph {
    fn from(builder: FlamegraphBuilder) -> Self {
        let mut blocks = Default::default();
        let mut time = 0.0;
        for (label, frame) in &builder.root.children {
            frame.visit(&mut blocks, &mut time, 0, label.to_string());
        }
        Self { blocks }
    }
}


// === FlamegraphBuilder implementation ===

#[derive(Default, Debug)]
struct AggregateFrame {
    duration: f64,
    children: std::collections::HashMap<std::rc::Rc<String>, AggregateFrame>,
}

impl AggregateFrame {
    /// Build a Block from self; recurse into children.
    fn visit(&self, blocks: &mut Vec<Block>, time: &mut f64, row: u32, label: String) {
        let start = *time;
        let end = *time + self.duration;
        blocks.push(Block { start, end, label, row });
        for (label, frame) in &self.children {
            frame.visit(blocks, time, row + 1, label.to_string());
        }
        *time = end;
    }
}

impl FlamegraphBuilder {
    /// Add the interval to an AggregatedFrame; recurse into children.
    fn visit_interval<Metadata>(
        &mut self,
        profile: &data::Profile<Metadata>,
        active: data::IntervalId,
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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use profiler::profile;

    #[profile(Objective)]
    pub fn profiled_a() {
        profiled_b()
    }
    #[profile(Objective)]
    pub fn profiled_b() {}

    #[test]
    fn check_flame_graph_creation() {
        profiled_a();

        let profile: data::Profile<data::OpaqueMetadata> =
            profiler::internal::take_log().parse().unwrap();
        let flame_graph = Graph::new_callgraph(&profile);
        assert_eq!(flame_graph.blocks.len(), 2);

        assert_eq!(flame_graph.blocks[1].row, 1);
        assert!(flame_graph.blocks[1].label.contains("profiled_b"));
        assert_eq!(flame_graph.blocks[0].row, 0);
        assert!(flame_graph.blocks[0].label.contains("profiled_a"));
    }
}
