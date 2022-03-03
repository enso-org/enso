//! This module contains functionality that allows the profiling framework to
//! generate the data required to render a flame graph. This means creating data for each block
//! that is supposed to be rendered, with start time, end time and labels.


use enso_profiler as profiler;
use enso_profiler_data as data;
use enso_profiler_data::Interval;
use enso_profiler_data::Lifetime;
use enso_profiler_data::Measurement;


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



// ========================
// === Flame Graph Data ===
// ========================

/// A `FlameGraph`, contains the information required to render a flame graph, i.e., the data for
/// all blocks that make up the flame graph.
#[derive(Debug, Default)]
pub struct FlameGraph {
    /// Collection of all blocks making up the flame graph.
    pub blocks: Vec<Block>,
}

fn blocks_from_measurement<Metadata>(measurement: &Measurement<Metadata>, row: u32) -> Vec<Block> {
    match &measurement.lifetime {
        Lifetime::Async(lifetime) => lifetime
            .active
            .iter()
            .map(|interval| block_from_interval(&measurement.label, row, interval))
            .collect(),
        Lifetime::NonAsync { active } => vec![block_from_interval(&measurement.label, row, active)],
    }
}

fn block_from_interval(label: &data::Label, row: u32, interval: &Interval) -> Block {
    let start = interval.start.into_ms();
    let end = interval.end.map(|mark| mark.into_ms()).unwrap_or(f64::MAX);
    let label = label.to_string();
    let row = row;
    Block { start, end, label, row }
}

impl<Metadata> From<data::Measurement<Metadata>> for FlameGraph {
    fn from(root: data::Measurement<Metadata>) -> Self {
        let mut blocks = Vec::default();
        // We skip the root node, which is the app lifetime, and store the measurements with
        // their depth. Newly added children of visited nodes, will be added one row above their
        // parents.
        let mut to_parse: Vec<_> = root.children.iter().map(|m| (m, 0)).collect();

        loop {
            let measurement = to_parse.pop();
            match measurement {
                Some((measurement, row)) => {
                    let target_row = if measurement.lifetime.is_async() { 0 } else { row };
                    let measurement_blocks = blocks_from_measurement(measurement, target_row);
                    blocks.extend(measurement_blocks);
                    to_parse.extend(measurement.children.iter().map(|m| (m, target_row + 1)));
                }
                None => break,
            }
        }
        Self { blocks }
    }
}

impl FlameGraph {
    /// Gather and remove all logged measurements and return them as a `FlameGraph`.
    pub fn take_from_log() -> Self {
        let root: Result<data::Measurement<data::OpaqueMetadata>, _> =
            profiler::internal::take_log().parse();
        if let Ok(root) = root {
            root.into()
        } else {
            eprintln!("Failed to deserialize profiling event log.");
            FlameGraph::default()
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod compile_tests {
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

        let flame_graph = FlameGraph::take_from_log();
        assert_eq!(flame_graph.blocks.len(), 2);

        assert_eq!(flame_graph.blocks[1].row, 1);
        assert!(flame_graph.blocks[1].label.contains("profiled_b"));
        assert_eq!(flame_graph.blocks[0].row, 0);
        assert!(flame_graph.blocks[0].label.contains("profiled_a"));
    }
}
