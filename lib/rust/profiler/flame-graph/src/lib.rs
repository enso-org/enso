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

fn visit_interval<Metadata>(
    profile: &data::Profile<Metadata>,
    active: data::IntervalId,
    blocks: &mut Vec<Block>,
    row: u32,
) {
    let active = &profile[active];
    let start = active.interval.start.into_ms();
    let end = active.interval.end.map(|mark| mark.into_ms()).unwrap_or(f64::MAX);
    // Optimization: can't draw zero-width blocks anyway.
    if end == start {
        return;
    }
    let label = profile[active.measurement].label.to_string();
    blocks.push(Block { start, end, label, row });
    for child in &active.children {
        visit_interval(profile, *child, blocks, row + 1);
    }
}

impl<Metadata> From<data::Profile<Metadata>> for FlameGraph {
    fn from(profile: data::Profile<Metadata>) -> Self {
        let mut blocks = Vec::default();
        for child in &profile.root_interval().children {
            visit_interval(&profile, *child, &mut blocks, 0);
        }
        Self { blocks }
    }
}

impl FlameGraph {
    /// Gather and remove all logged measurements and return them as a `FlameGraph`.
    pub fn take_from_log() -> Self {
        let profile: Result<data::Profile<data::OpaqueMetadata>, _> =
            profiler::internal::take_log().parse();
        if let Ok(profile) = profile {
            profile.into()
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
